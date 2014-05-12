{-# LANGUAGE DeriveDataTypeable, TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSTorChat.GUI where

import Control.Concurrent
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Proxy
import Data.Typeable
import Graphics.QML
import System.IO
import System.Random

import HSTorChat.Protocol

data TorChat = TorChat
        { _myonion  :: Onion
        , _mystatus :: BuddyStatus
        , _buddies  :: MVar (M.Map Onion (ObjRef Buddy))
        , _pending  :: MVar [PendingConnection]
        } deriving Typeable

-- Signals
data BuddiesChanged deriving Typeable
data NewChatMsg deriving Typeable
data BuddyChanged deriving Typeable

instance DefaultClass TorChat where
    classMembers = [
          defPropertySigRO "buddies" (Proxy :: Proxy BuddiesChanged) buddies
          -- | Return Onion address for this instance of HSTorChat.
        , defMethod "onion" (return . _myonion . fromObjRef :: ObjRef TorChat -> IO Onion)
          -- | Send a message to a buddy.
        , defMethod "sendMsg" sendMsg
          -- | Add a new buddy.
        , defMethod "newBuddy" newBuddy
        , defMethod "setStatus" statusChanged
        ]
      where
        buddies :: ObjRef TorChat -> IO [ObjRef Buddy]
        buddies tc = return . buddylist =<< (readMVar . _buddies $ fromObjRef tc)

instance DefaultClass ChatMsg where
    classMembers = [
          defPropertyRO "buddy" (return . buddy . fromObjRef)
        , defPropertyRO "text" (return . text . fromObjRef)
        , defPropertyRO "fromme" (return . fromme . fromObjRef)
        ]

instance DefaultClass Buddy where
    classMembers = [
          defPropertyRO "onion" (return . _onion . fromObjRef)
        , defPropertySigRO "status" (Proxy :: Proxy BuddyChanged) status
        , defPropertySigRO "msgs" (Proxy :: Proxy NewChatMsg) messages
        ]
      where
        messages :: ObjRef Buddy -> IO [ObjRef ChatMsg]
        messages = readMVar . _msgs . fromObjRef
        status = return . T.pack . show .  _status . fromObjRef

instance Marshal ChatMsg where
    type MarshalMode ChatMsg c d = ModeObjFrom ChatMsg c
    marshaller = fromMarshaller fromObjRef

instance Marshal Buddy where
    type MarshalMode Buddy c d = ModeObjFrom Buddy c
    marshaller = fromMarshaller fromObjRef

instance SignalKeyClass BuddiesChanged where
    type SignalParams BuddiesChanged = IO ()

instance SignalKeyClass NewChatMsg where
    type SignalParams NewChatMsg = IO ()

instance SignalKeyClass BuddyChanged where
    type SignalParams BuddyChanged = IO ()

-- | This function is called when the user enters
-- a msg in a chat window. The handle for the buddy
-- is accessed and used to send the message.
sendMsg :: ObjRef TorChat -> ObjRef Buddy -> T.Text -> IO ()
sendMsg _ bud msg
    | null (T.unpack msg) = putStrLn "Ignoring empty request."
    -- Check if buddy is offline
    | (_status . fromObjRef) bud == Offline = putStrLn "[delayed] msg not supported yet."
    -- buddy should be able to receive the message
    -- TODO: Implement gaurd to check _outConn status.
    | otherwise = do
          saveMsg $ ChatMsg msg (_onion $ fromObjRef bud) True
          fireSignal (Proxy :: Proxy NewChatMsg) bud
          hPutStrLn (_outConn $ fromObjRef bud) $ formatMsg $ Message msg
        where
          saveMsg cmsg = modifyMVar_ (_msgs $ fromObjRef bud) (\ms -> do m <- newObjectDC cmsg
                                                                         return (m:ms))

newBuddy :: ObjRef TorChat -> T.Text -> IO ()
newBuddy tc onion = do
    putStrLn $ "Requesting buddy connection: " ++ T.unpack onion
    oHdl <- hstorchatOutConn $ onion `T.append` ".onion"
    gen <- getStdGen
    let  cky = gencookie gen
         tc' = fromObjRef tc

    -- Add to list of pending connection.
    modifyMVar_ (_pending tc')
        $ \p -> return $ PendingConnection cky onion oHdl : filter ((/= onion) . _ponion) p
    hPutStrLn oHdl $ formatMsg $ Ping (_myonion tc') cky

statusChanged :: ObjRef TorChat -> T.Text -> IO ()
statusChanged tc status
    | T.unpack status == "Away" = alert Away
    | T.unpack status == "Extended Away" = alert Xa
    | otherwise = alert Available
  where
    alert st = do bs' <- readMVar . _buddies $ fromObjRef tc
                  bl <- return . map fromObjRef $ buddylist bs'
                  -- tell online buddies status.
                  tell (online bl) $ Status st
    online = filter $ (/= Offline) . _status
    tell [] _ = return ()
    tell (Buddy _ _ oHdl _ _ _:bs) st = hPutStrLn oHdl (formatMsg st) >> tell bs st
