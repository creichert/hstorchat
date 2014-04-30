
{-# LANGUAGE DeriveDataTypeable, TypeFamilies, OverloadedStrings #-}
module GUI where

import Control.Concurrent
import qualified Data.Char as C
import Data.Tagged
import qualified Data.Text as T
import Data.Typeable
import Graphics.QML
import System.IO

import Protocol

data UI = UI
        { _onion   :: String
        , _status  :: Status
        , _buddies :: MVar [ Buddy ]
        } deriving (Typeable)

data Buddy = Buddy
           { _addy     :: String
           , _in_conn  :: Handle
           , _out_conn :: Handle
           } deriving Show

instance Object UI where
    classDef = defClass [
          -- | Return Onion address for this instance of HSTorChat.
          defMethod "onion" (
                (\o -> return $ _onion $ fromObjRef o) :: ObjRef UI -> IO String)
          -- | Send a message to a buddy.
        , defMethod "sendMsg" sendMsg
        , defPropertyRO "self" ((\x -> return x) :: ObjRef UI -> IO (ObjRef UI))
        , defPropertyRO "status" getStatus
        , defSignal (Tagged "msgReady" :: Tagged MsgReady String)
        ]

data Msg = Msg
         { text  :: String
         , buddy :: String
         } deriving Typeable

instance Object Msg where
    classDef = defClass [
          defPropertyRO "buddy" (return . buddy . fromObjRef)
        , defPropertyRO "text" (return . text . fromObjRef) ]

instance Marshal Msg where
    type MarshalMode Msg = ValObjToOnly Msg
    marshaller = objSimpleMarshaller

data MsgReady deriving Typeable
instance SignalKey MsgReady where
    type SignalParams MsgReady = ObjRef Msg -> IO ()

data Status = Online | Offline
    deriving Typeable

instance Object Status where
    classDef = defClass []

instance Marshal Status where
    type MarshalMode Status = ValObjToOnly Status
    marshaller = objSimpleMarshaller

getStatus :: ObjRef UI -> IO (ObjRef Status)
getStatus = undefined

lowercase :: String -> String
lowercase []     = ""
lowercase (x:xs) = C.toLower x : xs

-- | This method is called when the user enters
-- a msg in a chat window. The handle for the buddy
-- is accessed and used to send the message.
--
-- TODO: Pass Buddy instead of Text -> Text
sendMsg :: ObjRef UI -> T.Text -> T.Text -> IO ()
sendMsg ui onion msg = do
    let ui' = fromObjRef ui
    buds <- readMVar $ _buddies ui'
    sendMsgTo buds
  where
    sendMsgTo []   = putStrLn "Unable to send msg: no buddies."
    sendMsgTo buds = do
        -- Filter proper buddy from list.
        let thebuddy = head $ filter (\b -> _addy b == T.unpack onion) buds
        -- | TODO: Reschedule if send is not succesful.
        hPutStrLn (_out_conn thebuddy) $ lowercase $ filter (/= '"') $ show $ Message msg
        return ()
