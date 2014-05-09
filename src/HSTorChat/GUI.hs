{-# LANGUAGE DeriveDataTypeable, TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSTorChat.GUI where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Attoparsec.Text hiding (take)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Proxy
import Data.Typeable
import Graphics.QML
import System.IO
import System.IO.Error
import System.Random

import HSTorChat.Protocol

-- Main UI Object.
data UI = UI
        { _myonion  :: Onion
        , _mystatus :: BuddyStatus
        , _buddies  :: MVar (M.Map Onion (ObjRef Buddy))
        , _pending  :: MVar [PendingConnection]
        } deriving Typeable

-- Signals
data BuddiesChanged deriving Typeable
data ChatMsgReady deriving Typeable
data BuddyChanged deriving Typeable

instance DefaultClass UI where
    classMembers = [
          defPropertySigRO "buddies" (Proxy :: Proxy BuddiesChanged) buddies
          -- | Return Onion address for this instance of HSTorChat.
        , defMethod "onion" (return . _myonion . fromObjRef :: ObjRef UI -> IO Onion)
          -- | Send a message to a buddy.
        , defMethod "sendMsg" sendMsg
          -- | Add a new buddy.
        , defMethod "newBuddy" newBuddy
        , defMethod "setStatus" statusChanged
        ]
      where
        buddies :: ObjRef UI -> IO [ObjRef Buddy]
        buddies ui = do bs <- readMVar . _buddies $ fromObjRef ui
                        return . snd . unzip $ M.toList bs

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
        , defPropertySigRO "msgs" (Proxy :: Proxy ChatMsgReady) messages
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

instance SignalKeyClass ChatMsgReady where
    type SignalParams ChatMsgReady = IO ()

instance SignalKeyClass BuddyChanged where
    type SignalParams BuddyChanged = IO ()

-- | This function is called when the user enters
-- a msg in a chat window. The handle for the buddy
-- is accessed and used to send the message.
sendMsg :: ObjRef UI -> ObjRef Buddy -> T.Text -> IO ()
sendMsg _ bud msg
    | null (T.unpack msg) = putStrLn "Ignoring empty request."
    -- Check if buddy is offline
    | (_status . fromObjRef) bud == Offline = putStrLn "[delayed] msg not supported yet."
    -- buddy should be able to receive the message
    -- TODO: Implement gaurd to check _outConn status.
    | otherwise = do
          saveMsg $ ChatMsg msg (_onion $ fromObjRef bud) True
          fireSignal (Proxy :: Proxy ChatMsgReady) bud
          hPutStrLn (_outConn $ fromObjRef bud) $ formatMsg $ Message msg
        where
          saveMsg cmsg = modifyMVar_ (_msgs $ fromObjRef bud) (\ms -> do m <- newObjectDC cmsg
                                                                         return (m:ms))

newBuddy :: ObjRef UI -> T.Text -> IO ()
newBuddy ui onion = do
    putStrLn $ "Requesting buddy connection: " ++ T.unpack onion

    oHdl <- hstorchatOutConn $ onion `T.append` ".onion"

    gen <- getStdGen
    let  cky = gencookie gen
         ui' = fromObjRef ui

    -- Add to list of pending connection.
    modifyMVar_ (_pending ui') $ \p -> return $ PendingConnection cky onion oHdl : filter ((/= onion) . _ponion) p
    hPutStrLn oHdl $ formatMsg $ Ping (_myonion ui') cky

statusChanged :: ObjRef UI -> T.Text -> IO ()
statusChanged ui status
    | T.unpack status == "Away" = alert Away
    | T.unpack status == "Extended Away" = alert Xa
    | otherwise = alert Available
  where
    alert st = do bs' <- readMVar . _buddies $ fromObjRef ui
                  bs  <- return $ map fromObjRef $ snd . unzip $ M.toList bs'
                  -- tell online buddies status.
                  tell (online bs) $ Status st
    online = filter $ (/= Offline) . _status
    tell [] _ = return ()
    tell (Buddy _ _ oHdl _ _ _:bs) st = hPutStrLn oHdl (formatMsg st) >> tell bs st

-- | This loop handles the initial Ping.
handleRequest :: ObjRef UI -> Handle -> IO ()
handleRequest ui iHdl = do

    let ui' = fromObjRef ui

    txt <- hGetLine iHdl
    case parseOnly parsePingPong (T.pack txt) of

        Left e  -> putStr "Error parsing incoming connection: " >> print e

        -- A Ping here means there is a new connection request.
        Right (Ping onion key) -> do

            gen  <- getStdGen
            p'   <- readMVar $ _pending ui'
            oHdl <- hstorchatOutConn $ onion `T.append` ".onion"

            let cky = gencookie gen
                sendping = not $ pending p' key -- Already pending

            when sendping $ hPutStrLn oHdl $ formatMsg $ Ping (_myonion ui') cky
            mapM_ (hPutStrLn oHdl . formatMsg) [ Pong key
                                               , Client "HSTorChat"
                                               , Version "0.1.0.0"
                                               , AddMe
                                               , Status Available
                                               ]
            modifyMVar_ (_pending ui') $ \_ -> return $ PendingConnection cky onion oHdl : filter ((/= onion) . _ponion) p'
            handleRequest ui iHdl

        -- All buddies must authenticate using the cookie we sent.
        Right (Pong key) -> do
            p <- readMVar $ _pending ui'
            pendingConnection $ filter ((== key) . _pcookie) p

        _ -> putStrLn "Buddy is not authenticated yet. Ignoring message."
  where
    pending ps key = any ((== key) . _pcookie) ps
    -- offline Nothing = True
    -- offline (Just b) = (_status . fromObjRef) b == Offline
    pendingConnection :: [PendingConnection] -> IO ()
    pendingConnection [] = putStrLn "Security Warning: Attempted connection with unidentified cookie."
    -- | A pending connection exists. Verify and start the buddy
    pendingConnection (PendingConnection cke o oHdl:_) = do

        ms <- newMVar []
        b <- newObjectDC $ Buddy o iHdl oHdl cke Offline ms
        let ui' = fromObjRef ui

        -- Filter this connection.
        modifyMVar_ (_pending ui') $ \ps -> return $ filter ((/= cke) . _pcookie) ps
        modifyMVar_ (_buddies ui') $ \bs -> return $ M.insert o b bs
        fireSignal (Proxy :: Proxy BuddiesChanged) ui

        runBuddy ui b

runBuddy :: ObjRef UI -> ObjRef Buddy -> IO ()
runBuddy ui objb = do
    let b    = fromObjRef objb
        iHdl = _inConn b
        oHdl = _outConn b
        oni  = _onion b

    txt <- hGetLine iHdl `catch` errorHandler
    case parseOnly parseResponse (T.pack txt) of

        Left e -> print ("Error parsing incoming message: " ++ e) >> runBuddy ui objb

        Right (Ping _ key') -> do
            mapM_ (hPutStrLn oHdl . formatMsg) [ Pong key'
                                               , Client "HSTorChat"
                                               , Version "0.1.0.0"
                                               , AddMe
                                               , Status Available
                                               ]
            runBuddy ui objb

        Right (Message msg) -> do
            cmsg <- newObjectDC $ ChatMsg msg (_onion b) False
            modifyMVar_ (_msgs b) (\ms -> return (cmsg:ms))
            fireSignal (Proxy :: Proxy ChatMsgReady) objb
            runBuddy ui objb

        Right (Status Offline) -> do
            nb <- newObjectDC $ b { _status = Offline }
            modifyMVar_ (_buddies $ fromObjRef ui)
                                  $ \bs -> return $ M.insert oni nb bs
            fireSignal (Proxy :: Proxy BuddiesChanged) ui
            -- Cleanup handles.
            hClose iHdl
            hClose oHdl

        Right (Status st) -> do
            nb <- newObjectDC $ b { _status = st }
            modifyMVar_ (_buddies $ fromObjRef ui)
                                  $ \bs -> return $ M.insert oni nb bs
            -- For now, it is necessary to reload the entire buddylist.
            -- The buddylist must be notified of the new buddy first (since
            -- each ObjRef Buddy in the buddylist is immutable.
            fireSignal (Proxy :: Proxy BuddiesChanged) ui
            -- Run the new buddy loop.
            runBuddy ui nb

        Right p -> print p >> runBuddy ui objb
  where
      errorHandler e
        | isEOFError e = return "status offline"
        | otherwise    = ioError e
