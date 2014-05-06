{-# LANGUAGE DeriveDataTypeable, TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSTorChat.GUI where

import Control.Concurrent
import Control.Monad
import Data.Attoparsec.Text hiding (take)
import qualified Data.Text as T
import Data.Proxy
import Data.Typeable
import Graphics.QML
import System.IO
import System.Random

import HSTorChat.Protocol

-- Main UI Object.
data UI = UI
        { _myonion  :: Onion
        , _mystatus :: BuddyStatus
        , _buddies  :: MVar [Buddy]
        , _pending  :: MVar [PendingConnection]
        } deriving Typeable

-- Signals
data ChatMsgReady deriving Typeable

instance DefaultClass UI where
    classMembers = [
          -- | Return Onion address for this instance of HSTorChat.
          defMethod "onion" (return . _myonion . fromObjRef :: ObjRef UI -> IO Onion)
          -- | Send a message to a buddy.
        , defMethod "sendMsg" sendMsg
          -- | Add a new buddy.
        , defMethod "newBuddy" newBuddy
          -- | Access the context object (ObjRef UI) in qml callbacks.
        , defPropertyRO "self" (return :: ObjRef UI -> IO (ObjRef UI))
          -- | Called when a new message arrives from a buddy.
        , defSignal "msgReady" (Proxy :: Proxy ChatMsgReady)
        ]

instance DefaultClass ChatMsg where
    classMembers = [
          defPropertyRO "buddy" (return . T.pack . buddy . fromObjRef)
        , defPropertyRO "text" (return . T.pack . text . fromObjRef)
        ]

instance Marshal ChatMsg where
    type MarshalMode ChatMsg c d = ModeObjFrom ChatMsg c
    marshaller = fromMarshaller fromObjRef

instance SignalKeyClass ChatMsgReady where
    type SignalParams ChatMsgReady = ObjRef ChatMsg -> IO ()

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
    sendMsgTo []   = putStrLn "Unable to send message. No buddies."
    sendMsgTo buds = do
        -- Filter proper buddy from list.
        let thebuddy = head $ filter (\b -> _onion b == T.unpack onion) buds

        -- | TODO: Reschedule if send is not succesful.
        hPutStrLn (_outConn thebuddy) $ formatMsg $ Message msg

newBuddy :: ObjRef UI -> T.Text -> IO ()
newBuddy ui onion = do
        putStrLn $ "Requesting buddy connection: " ++ T.unpack onion

        oHdl <- hstorchatOutConn $ onion `T.append` ".onion"

        gen <- getStdGen
        let  cky = gencookie gen
             ui' = fromObjRef ui

        -- TODO: Add this buddy to the list now and set Offline.

        -- Add to list of pending connection.
        -- TODO: Only add unique onion to this list.
        modifyMVar_ (_pending ui') (\p -> return (PendingConnection cky onion oHdl:p))
        hPutStrLn oHdl $ formatMsg $ Ping (_myonion ui') cky

-- | This loop handles the initial Ping.
handleRequest :: ObjRef UI -> Handle -> IO ()
handleRequest ui iHdl = do

        txt <- hGetLine iHdl
        case parseOnly parsePingPong (T.pack txt) of

            Left e  -> putStr "Error parsing incoming connection: " >> print e

            -- A Ping here means there is a new connection request.
            Right (Ping onion key) -> do

                gen <- getStdGen
                let cky = gencookie gen
                    ui' = fromObjRef ui

                oHdl <- hstorchatOutConn $ onion `T.append` ".onion"

                p' <- readMVar (_pending ui')
                b' <- readMVar (_buddies ui')
                -- Send Ping if this Buddy is new or Offline.
                when ((not $ any ((== key) . _pcookie) p') &&
                      (not $ any ((/= Offline) . _status) b')
                     ) $ hPutStrLn oHdl $ formatMsg $ Ping (_myonion ui') cky

                mapM_ (hPutStrLn oHdl . formatMsg) [ Pong key
                                                   , Client "HSTorChat"
                                                   , Version "0.1.0.0"
                                                   , AddMe
                                                   , Status Available
                                                   ]

                modifyMVar_ (_pending ui') (\p -> return (PendingConnection cky onion oHdl:p))

                handleRequest ui iHdl

            -- All buddies must authenticate using the cookie we sent.
            Right (Pong key) -> do

                let ui' = fromObjRef ui
                p <- readMVar (_pending ui')

                -- Filter all matching keys. Very important for security
                pendingConnection $ filter ((== key) . _pcookie) p

            _ -> putStrLn "Buddy is not authenticated yet. Ignoring message."
  where
    pendingConnection :: [PendingConnection] -> IO ()
    pendingConnection [] = putStrLn "Security Warning: Attempted connection with unidentified cookie."
    -- | A pending connection exists. Verify and start the buddy
    pendingConnection (PendingConnection cke o oHdl:pcs) = do

                let b   = Buddy (T.unpack o) iHdl oHdl cke Available
                    ui' = fromObjRef ui

                modifyMVar_ (_buddies ui') (\bs -> return (b:bs))
                -- effectively, remove this pending connection from
                -- _pending.
                modifyMVar_ (_pending ui') (\_ -> return pcs)

                -- A new Buddy has been identified.
                m <- newObjectDC $ ChatMsg ("A connection to " ++ T.unpack o ++ " has been established.") $ T.unpack o

                -- TODO: Emit the `ProtocolMsg Message` constructor directly.
                --       Remove the Msg class and modify MsgReady sig.
                fireSignal (Proxy :: Proxy ChatMsgReady) ui m

                runBuddy ui b

runBuddy :: ObjRef UI -> Buddy -> IO ()
runBuddy ui (Buddy onion iHdl oHdl cky st) = do

        txt <- hGetLine iHdl
        rdy <- hReady oHdl
        when rdy $ do out_txt <- hGetLine oHdl
                      putStrLn $ "Outgoing connection message: " ++ out_txt

        case parseOnly parseResponse (T.pack txt) of

            Left e -> putStr "Error parsing incoming message: " >> print e >> hClose iHdl

            Right (Ping _ key') -> mapM_ (hPutStrLn oHdl . formatMsg) [ Pong key'
                                                                      , Client "HSTorChat"
                                                                      , Version "0.1.0.0"
                                                                      , AddMe
                                                                      , Status Available
                                                                      ]
            Right (Message msg) -> do
                -- TODO: Emit the `ProtocolMsg Message` directly.
                m <- newObjectDC $ ChatMsg (T.unpack msg) onion
                fireSignal (Proxy :: Proxy ChatMsgReady) ui m

            Right p -> print p

        hFlush iHdl
        hFlush oHdl
        runBuddy ui $ Buddy onion iHdl oHdl cky st
