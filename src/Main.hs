{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad
import Data.Attoparsec.Text hiding (take)
import Data.Tagged
import qualified Data.Text as T
import Graphics.QML
import qualified Network as N
import Network.Socket
import Network.Socks5
import System.IO
import System.Process

import GUI
import Protocol

-- TODO: Replace this key with a random number generator.
mykey :: T.Text
mykey = "66241890815914920502101090367586695907312936797864657576118321050486943686966"

runBuddy :: ObjRef UI -> Buddy -> IO ()
runBuddy ui (Buddy onion iHdl oHdl) = forever $ do

        -- remove this buddy from _buddies ui if this fails
        txt <- hGetLine iHdl
        rdy <- hReady oHdl

        if rdy then do out_txt <- hGetLine oHdl
                       putStrLn $ "_out_conn msg: " ++ out_txt
               else return ()

        case parseOnly parseResponse (T.pack txt) of
            -- There was an error parsing the T.packet. Close the socket and
            -- destroy this thread.
            --
            -- TODO: Close socket and destroy thread.
            Left e  -> do putStr $ "Error parsing: " ++ show e

            --Right (Ping _ key') -> mapM_ (hPutStrLn oHdl)
            Right (Ping _ key') -> do
                mapM_ (hPutStrLn oHdl) $ map formatMsg [ Pong key'
                                                       -- echo message
                                                       , Client "HSTorChat"
                                                       , Version "0.9.9.666"
                                                       -- , AddMe need show to be add_me
                                                       , Status "available"
                                                       ]
            Right (Pong _)          -> putStrLn "pongelong received"
            Right (AddMe)           -> putStrLn "addme"
            Right (Client c)        -> print c
            Right (Status _)        -> putStrLn "status"
            Right (Version _)       -> putStrLn "version"
            Right (Message msg)     -> do
                -- echo message
                -- hPutStrLn oHdl $ lowercase $ Prelude.filter (/= '"') $ show $ Message msg
                m <- newObject $ Msg (T.unpack msg) onion
                -- TODO: Emit the `ProtocolMsg Message` directly. Might be able to
                -- do this with more ProtocolMsg constructors.
                fireSignal (Tagged ui :: Tagged MsgReady (ObjRef UI)) m

            Right p  -> putStr "Uknown ProtocolMsg type: " >> print p
  where
      formatMsg :: ProtocolMsg -> String
      formatMsg = lowercase . filter (/= '"') . show

-- | Begin Authentication for Buddy by sending a Ping to the advertised
-- onion.
--
-- Authentication works using Tor hidden services. When there is a request
-- to connect we immediately open a new socket to the requesters onion
-- address to verify that they are who they say they are. Tor guarantees
-- that we will arrive at the correct hiddern service.
--
-- If successfully authenticated, a socket for the out_connection is
-- returned. This is the other piece needed to create a Buddy.
authenticate :: T.Text -> T.Text -> IO Socket
authenticate onion key = do

    -- TODO: Test security issues.

    print $ "New authentication request: " `T.append` onion `T.append` " " `T.append` key

    let onion' = T.unpack $ onion `T.append` ".onion"
    print onion'
    sock <- socksConnectWith (defaultSocksConf "127.0.0.1" 22209) onion' (N.PortNumber 11009)

    me <- readFile "hidden_service/hostname"

    -- TODO: Not all of these messages need to be sent the first time.
    mapM_ (send sock) $ map formatMsg [ pingdata me
                                      , pongdata
                                      , clientdata
                                      , versiondata
                                      , "add_me" ++ "\n"
                                      , "status available" ++ "\n"
                                      ]

    print $ "Authenticated " ++ onion' ++ "."
    return sock
  where
    pingdata me = (show $ Ping (T.pack $ take 16 me) (T.init mykey)) ++ "\n"
    pongdata = (show $ Pong (T.init key)) ++ "\n"
    clientdata = (show $ Client "TorChat") ++ "\n"
    versiondata =  (show $ Version "0.9.9.553") ++ "\n"
    formatMsg :: String -> String
    formatMsg = lowercase . filter (/= '"')

-- | This loop handles the initial Ping.
handleRequest :: (Socket, SockAddr) -> ObjRef UI -> IO ()
handleRequest (sock, _) ui = do

        -- get initial Ping
        txt <- recv sock 512

        -- The ProtocolMsg was not a Ping. Close the socket and destroy
        -- this thread.
        --
        -- TODO: Close socket and destroy thread.
        case parseOnly parsePing (T.pack txt) of
            Left e  -> do putStr "Error parsing: "
                          sClose sock
                          print e

            -- A Ping here means there is a new connection request.
            Right (Ping onion key) -> do

                    -- Attempt to authenticate Buddy by sending a Pong to
                    -- it's advertised onion address. The socket returned
                    -- is returned to use as the _out_conn for the Buddy
                    -- if the authentication is succesful
                    sock' <- authenticate onion key

                    iHdl <- socketToHandle sock ReadWriteMode
                    oHdl <- socketToHandle sock' ReadWriteMode

                    hSetBuffering iHdl NoBuffering
                    hSetBuffering oHdl NoBuffering

                    let b = Buddy (T.unpack onion) iHdl oHdl
                    let ui' = fromObjRef ui

                    -- Use withMVar to update
                    buds' <- takeMVar $ _buddies ui'
                    putMVar (_buddies ui') (b:buds')
                    --withMVar (_buddies ui') $ \buds' -> do
                    --    let bs = (buddy:buds')
                    --    print $ show bs
                    --    return bs

                    -- A new Buddy has been identified.
                    runBuddy ui b

            _ -> putStrLn "Uknown ProtocolMsg type."

        return ()

main :: IO ()
main = withSocketsDo $ do

    createProcess $ proc "tor" ["-f", "torrc"]

    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1

    li <- inet_addr "127.0.0.1"
    bindSocket sock $ SockAddrInet 22009 li
    listen sock 2

    buddies <- newMVar []

    onion <- readFile "hidden_service/hostname"
    putStr $ "Hello " ++ onion

    ui <- newObject $ UI onion Online buddies

    forkIO $ forever $ do
        conn <- accept sock
        forkIO $ handleRequest conn ui

    runEngineLoop defaultEngineConfig {
        initialURL = filePathToURI "qml/HSTorChat.qml"
      , initialWindowState = ShowWindowWithTitle "HSTorChat"
      , contextObject = Just $ anyObjRef ui
    }
