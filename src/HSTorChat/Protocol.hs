{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module HSTorChat.Protocol where

import Control.Applicative
import Control.Concurrent
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

-- Hidden service
hstorchatHSPort :: PortNumber
hstorchatHSPort = 11009

hstorchatLocalPort :: PortNumber
hstorchatLocalPort = 22009

hstorchatHost :: String
hstorchatHost = "127.0.0.1"

hstorchatOutConn :: Onion -> IO Handle
hstorchatOutConn onion = do
    outsock <- socksConnectWith hstcConf (T.unpack onion) $ PortNumber hstorchatHSPort
    oHdl    <- socketToHandle outsock ReadWriteMode
    hSetBuffering oHdl LineBuffering
    return oHdl
  where hstcConf = defaultSocksConf hstorchatHost torSocksPort

-- | Format a message to send over a Socket.
formatMsg :: ProtocolMsg -> String
formatMsg AddMe = "add_me"
formatMsg m     = map C.toLower . filter (/= '"') . show $ m

-- | Return a BuddyList given the M.Map
buddylist :: M.Map Onion (ObjRef Buddy) -> [ObjRef Buddy]
buddylist bs = snd . unzip $ M.toList bs

parseResponse :: Parser ProtocolMsg
parseResponse =  try parsePingPong
             <|> try parseVersion
             <|> try parseClient
             <|> try parseStatus
             <|> try parseAddMe
             <|> parseMsg

parsePingPong :: Parser ProtocolMsg
parsePingPong =  try parsePing
             <|> try parsePong

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
parsePong = do
    string "pong"
    skipSpace
    -- parse secret key.
    key <- takeText
    return $ Pong key

parseVersion :: Parser ProtocolMsg
parseVersion = do
    string "version"
    skipSpace
    v <- takeText
    return $ Version v

parseClient :: Parser ProtocolMsg
parseClient = do
    string "client"
    skipSpace
    c <- takeText
    return $ Client c

parseStatus :: Parser ProtocolMsg
parseStatus = do
    string "status"
    skipSpace
    st <- takeText
    return $ Status (read $ capitalized (T.unpack st) :: BuddyStatus)
  where
    capitalized [] = []
    capitalized (x:xs) = C.toUpper x : xs

parseAddMe :: Parser ProtocolMsg
parseAddMe = do
    string "add_me"
    skipSpace
    return AddMe

parseMsg :: Parser ProtocolMsg
parseMsg = do
    string "message"
    skipSpace
    msg <- takeText
    return $ Message msg
