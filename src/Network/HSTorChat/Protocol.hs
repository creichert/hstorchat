{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Network.HSTorChat.Protocol where

import Control.Applicative
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Typeable
import Data.Word
import Graphics.QML
import Prelude hiding (take)
import qualified Prelude as P
import Network
import Network.Socket
import Network.Socks5
import System.IO
import System.Random

type Onion  = T.Text
type Cookie = T.Text

data TorChat = TorChat
        { _myonion  :: Onion
        , _mystatus :: BuddyStatus
        , _buddies  :: MVar (M.Map Onion (ObjRef Buddy))
        , _pending  :: MVar [PendingConnection]
        } deriving Typeable

data Buddy = Buddy
           { _onion   :: Onion -- ^ Buddy onion address.
           , _inConn  :: Handle
           , _outConn :: Handle
           , _cookie  :: Cookie -- ^ Cookie sent to buddy.
           , _status  :: BuddyStatus -- ^ Buddy status
           , _msgs    :: MVar [ObjRef ChatMsg]
           } deriving (Typeable)

data BuddyStatus = Offline
                 | Handshake
                 | Available
                 | Away
                 | Xa -- ^ Extended Away
                 deriving (Eq, Read, Show)

data PendingConnection = PendingConnection
                       { _pcookie    :: Cookie
                       , _ponion     :: Onion
                       , _pouthandle :: Handle
                       } deriving Show

data ProtocolMsg = Ping Onion Cookie
                 | Pong T.Text
                 | Client T.Text
                 | Version T.Text
                 | Status BuddyStatus
                 | ProfileName
                 | ProfileText
                 | AvatarAlpha
                 | ProfileAvatar
                 | AddMe
                 | RemoveMe
                 | Message T.Text
                 | Filename
                 | Filedata
                 | FiledataOk
                 | FiledataError
                 | FileStopSending
                 | FileStopReceiving
                 deriving Show

data ChatMsg = ChatMsg
             { text   :: T.Text
             , buddy  :: T.Text
             , fromme :: Bool
             } deriving (Show, Typeable)

gencookie :: StdGen -> Cookie
gencookie g = T.pack . concatMap show $ P.take 3 (randoms g :: [Word64])

torSocksPort :: PortNumber
torSocksPort = 22209

-- Hidden service port.
hstorchatHSPort :: PortNumber
hstorchatHSPort = 11009

-- Port inside Socks5 tunnel.
hstorchatLocalPort :: PortNumber
hstorchatLocalPort = 22009

hstorchatHost :: String
hstorchatHost = "127.0.0.1"

hstorchatOutConn :: Onion -> IO (Maybe Handle)
hstorchatOutConn onion = do
    handle <- E.try $ socksConnectWith hstcConf (T.unpack onion) $ PortNumber hstorchatHSPort
    case handle of
        Left e  -> print (e :: SocksError) >> return Nothing
        Right o -> do oHdl <- socketToHandle o ReadWriteMode
                      hSetBuffering oHdl LineBuffering
                      return $ Just oHdl
  where hstcConf = defaultSocksConf hstorchatHost torSocksPort

-- | Format a message to send over a Socket.
formatMsg :: ProtocolMsg -> String
formatMsg AddMe = "add_me"
formatMsg (Message m) = "message " ++ T.unpack m
formatMsg m = map C.toLower . filter (/= '"') . show $ m

-- | Return a BuddyList given the M.Map
buddylist :: M.Map Onion (ObjRef Buddy) -> [ObjRef Buddy]
buddylist bs = snd . unzip $ M.toList bs

parseResponse :: Parser ProtocolMsg
parseResponse = choice [ parsePingPong
                       , parseVersion
                       , parseClient
                       , parseStatus
                       , parseAddMe
                       , parseMessage
                       ]

parsePingPong :: Parser ProtocolMsg
parsePingPong =  try parsePing <|> try parsePong

parsePing :: Parser ProtocolMsg
parsePing = do
    string "ping"
    skipSpace
    -- parse onion address.
    bdy <- take 16
    skipSpace
    -- parse secret cookie.
    cky <- takeText
    return $ Ping bdy cky

parsePong :: Parser ProtocolMsg
parsePong = liftM Pong $ string "pong" >> skipSpace >> takeText

parseVersion :: Parser ProtocolMsg
parseVersion = liftM Version $ string "version" >> skipSpace >> takeText

parseClient :: Parser ProtocolMsg
parseClient = liftM Client $ string "client" >> skipSpace >> takeText

parseStatus :: Parser ProtocolMsg
parseStatus = do
    string "status" >> skipSpace
    st <- takeText
    return $ Status (read $ capitalized (T.unpack st) :: BuddyStatus)
  where
    capitalized [] = []
    capitalized (x:xs) = C.toUpper x : xs

parseAddMe :: Parser ProtocolMsg
parseAddMe = string "add_me" >> skipSpace >> return AddMe

parseMessage :: Parser ProtocolMsg
parseMessage = liftM Message $ string "message" >> skipSpace >> takeText
