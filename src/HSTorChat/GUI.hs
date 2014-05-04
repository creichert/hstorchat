{-# LANGUAGE DeriveDataTypeable, TypeFamilies, OverloadedStrings #-}
module HSTorChat.GUI where

import Control.Concurrent
import Control.Monad
import Data.Attoparsec.Text hiding (take)
import Data.Tagged
import qualified Data.Text as T
import Data.Typeable
import Graphics.QML
import System.IO
import System.Random

import HSTorChat.Protocol

data UI = UI
        { _myonion  :: Onion
        , _mystatus :: BuddyStatus
        , _buddies  :: MVar [Buddy]
        , _pending  :: MVar [PendingConnection]
        } deriving (Typeable)

instance Object UI where
    classDef = defClass [
          -- | Return Onion address for this instance of HSTorChat.
          defMethod "onion" (return . T.unpack . _myonion . fromObjRef :: ObjRef UI -> IO String)
          -- | Send a message to a buddy.
        , defMethod "sendMsg" sendMsg
          -- | Add a new buddy.
        , defMethod "newBuddy" newBuddy
          -- | Access the context object (ObjRef UI) in qml callbacks.
        , defPropertyRO "self" (return :: ObjRef UI -> IO (ObjRef UI))
          -- | Called when a new message arrives from a buddy.
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
                mapM_ (hPutStrLn oHdl . formatMsg) [ Ping (_myonion ui') cky
                                                   , Pong key
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

                let b   = Buddy (T.unpack o) iHdl oHdl cke Offline
                    ui' = fromObjRef ui

                modifyMVar_ (_buddies ui') (\bs -> return (b:bs))
                -- effectively, remove this pending connection from
                -- _pending.
                modifyMVar_ (_pending ui') (\_ -> return pcs)

                -- A new Buddy has been identified.
                m <- newObject $ Msg ("A connection to " ++ T.unpack o ++ " has been established.") $ T.unpack o

                -- TODO: Emit the `ProtocolMsg Message` constructor directly.
                --       Remove the Msg class and modify MsgReady sig.
                fireSignal (Tagged ui :: Tagged MsgReady (ObjRef UI)) m

                runBuddy ui b

runBuddy :: ObjRef UI -> Buddy -> IO ()
runBuddy ui (Buddy onion iHdl oHdl cky st) = forever $ do

        txt <- hGetLine iHdl
        rdy <- hReady oHdl

        when rdy $ do out_txt <- hGetLine oHdl
                      putStrLn $ "Outgoing connection message: " ++ out_txt

        let ui' = fromObjRef ui

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
                m <- newObject $ Msg (T.unpack msg) onion
                fireSignal (Tagged ui :: Tagged MsgReady (ObjRef UI)) m

            Right p -> print p

        hFlush iHdl
        hFlush oHdl
        runBuddy ui $ Buddy onion iHdl oHdl cky st
