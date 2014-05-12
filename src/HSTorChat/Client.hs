{-# LANGUAGE OverloadedStrings #-}
module HSTorChat.Client (newConnectionRequest
                        ) where

import Control.Concurrent
import Control.Exception
import Data.Attoparsec.Text hiding (take)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Proxy
import Graphics.QML
import System.IO
import System.IO.Error
import System.Random

import HSTorChat.Protocol
import HSTorChat.GUI

-- | This loop handles a new Buddy connection request.
newConnectionRequest :: ObjRef TorChat -> Handle -> IO ()
newConnectionRequest tc iHdl = do

    bs  <- buds
    txt <- hGetLine iHdl
    case parseOnly parsePingPong (T.pack txt) of

        Left e -> putStr "Error parsing incoming connection: " >> print e

        Right (Ping onion key)
            -- Can this interrupt a legitimate connection?
            | buddyonline (M.lookup onion bs) -> putStrLn $ "Already a connection to: " ++ T.unpack onion
            | otherwise -> pending >>= initiateConn onion key . filter ((== onion) . _ponion)

        -- When a Pong is received an attempt is made
        -- to authenticate using the cookie we sent.
        Right (Pong cke) -> pending >>= authorizePendingConnection tc iHdl . filter ((== cke) . _pcookie)

        _ -> putStrLn "Buddy is not authenticated yet. Ignoring message."
  where
    -- | Initiate a new connection from scratch.
    initiateConn o k [] = do oHdl <- hstorchatOutConn $ o `T.append` ".onion"
                             gen  <- getStdGen
                             let cky = gencookie gen
                             reply (PendingConnection cky o oHdl) $ Ping myonion cky : stdrply k
    -- | Complete an existing pending connection.
    initiateConn _ k (pconn:_) = reply pconn $ stdrply k
    reply p msgs = do mapM_ (hPutStrLn (_pouthandle p) . formatMsg) msgs
                      modifyMVar_ (_pending $ fromObjRef tc)
                          $ \ps -> return $ p : filter ((/= _ponion p) . _ponion) ps
                      newConnectionRequest tc iHdl
    buddyonline Nothing = False
    buddyonline (Just bud) = _status (fromObjRef bud) /= Offline
    gettc = fromObjRef tc
    pending = readMVar $ _pending gettc
    buds = readMVar $ _buddies gettc
    myonion = _myonion gettc
    stdrply k = [ Pong k
                , Client "HSTorChat"
                , Version "0.1.0.0"
                , AddMe
                , Status Available ]

authorizePendingConnection :: ObjRef TorChat -> Handle -> [PendingConnection] -> IO ()
authorizePendingConnection _ _ [] = putStrLn "Security Warning: Attempted connection with unidentified cookie."
-- | A pending connection exists. Verify and start the buddy
authorizePendingConnection tc iHdl (PendingConnection cke o oHdl : _) = do

        let tc' = fromObjRef tc
        bs <- readMVar $ _buddies tc'
        bud <- constructBuddy $ M.lookup o bs

        -- Filter this connection.
        modifyMVar_ (_pending tc') $ \ps -> return $ filter ((/= cke) . _pcookie) ps
        modifyMVar_ (_buddies tc') $ \buds -> return $ M.insert o bud buds
        fireSignal (Proxy :: Proxy BuddiesChanged) tc

        runBuddyConnection tc bud
  where
    constructBuddy :: Maybe (ObjRef Buddy) -> IO (ObjRef Buddy)
    constructBuddy Nothing  = do ms <- newMVar []
                                 newObjectDC $ Buddy o iHdl oHdl cke Handshake ms
    constructBuddy (Just b) = let b' = fromObjRef b in
                                            newObjectDC $ b' { _inConn = iHdl
                                                             , _outConn = oHdl
                                                             , _cookie = cke
                                                             , _status = Handshake
                                                             , _msgs = _msgs b' }

runBuddyConnection :: ObjRef TorChat -> ObjRef Buddy -> IO ()
runBuddyConnection tc objb = do
    let b    = fromObjRef objb
        iHdl = _inConn b
        oHdl = _outConn b
        oni  = _onion b

    txt <- hGetLine iHdl `catch` errorHandler
    case parseOnly parseResponse (T.pack txt) of

        Left e -> print ("Error parsing incoming message: " ++ e) >>
                  runBuddyConnection tc objb

        Right (Message msg) -> do
            cmsg <- newObjectDC $ ChatMsg msg (_onion b) False
            modifyMVar_ (_msgs b) (\ms -> return (cmsg:ms))
            fireSignal (Proxy :: Proxy NewChatMsg) objb
            runBuddyConnection tc objb

        Right (Status Offline) -> do
            nb <- newObjectDC $ b { _status = Offline }
            modifyMVar_ (_buddies $ fromObjRef tc)
                                  $ \bs -> return $ M.insert oni nb bs
            fireSignal (Proxy :: Proxy BuddiesChanged) tc
            -- Cleanup handles.
            hClose iHdl
            hClose oHdl

        Right (Status st) -> do
            nb <- newObjectDC $ b { _status = st }
            modifyMVar_ (_buddies $ fromObjRef tc)
                                  $ \bs -> return $ M.insert oni nb bs
            fireSignal (Proxy :: Proxy BuddiesChanged) tc
            -- Run the new buddy loop.
            runBuddyConnection tc nb

        Right p -> print p >> runBuddyConnection tc objb
  where
      errorHandler e
        | isEOFError e = return "status offline"
        | otherwise    = ioError e
