{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Map as M
import Data.Text as T
import Graphics.QML
import Network.Socket
import System.IO
import System.Process

import Network.HSTorChat.Client
import Network.HSTorChat.Protocol

import Paths_hstorchat


gentorrc :: String -> IO ()
gentorrc dd = writeFile (dd ++ "/torrc") $ Prelude.unlines
                [ "SocksPort " ++ socksport
                -- we don't use the control port currently, so leave this alone
                --, "ControlPort 11119"
                -- INCOMING connections for the hidden service arrive at 11009
                -- and will be forwarded to TorChat at 127.0.0.1:22009
                , "HiddenServiceDir " ++ dd ++ "/hidden_service"
                , "HiddenServicePort " ++ hsport ++ " 127.0.0.1:" ++ hstcport
                -- where should tor store it's cache files
                , "DataDirectory " ++ dd ++ " tor_data"
                -- some tuning
                , "AvoidDiskWrites 1"
                , "LongLivedPorts " ++ hstcport
                , "FetchDirInfoEarly 1"
                , "CircuitBuildTimeout 30"
                , "NumEntryGuards 6"
                -- You can uncomment the lines below to log Tor's activity to the
                -- console or to a log file. Use this only during debugging!
                -- Turning off SaveLogging will leave sensitive information on your disk,
                -- the built in default is save logging turned on (set to 1).
                -- so don't remove the # from that line unless you need it
                -- and remember to put it in again, after you are done.
                --, "Log info File tor.log"
                --, "Log info stdout"
                --, "SafeLogging 0"
                ]
  where hsport    = show hstorchatHSPort
        hstcport  = show hstorchatLocalPort
        socksport = show torSocksPort

-- | Wait until the hidden service hostname file
-- is ready
hiddenServiceName :: IO String
hiddenServiceName = catch (do dd <- getDataDir
                              readFile $ dd ++ "/hidden_service/hostname")
                          (\e -> print (e :: IOException)
                              >> threadDelay 5000
                              >> hiddenServiceName)

main :: IO ()
main = withSocketsDo $ do

    dd <- getDataDir
    gentorrc dd
    _ <- createProcess $ proc "tor" ["-f", dd ++ "/torrc"]

    onion <- hiddenServiceName
    let myonion = T.take 16 $ T.pack onion
    putStr $ "Hello " ++ onion
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1

    li <- inet_addr hstorchatHost
    bindSocket sock $ SockAddrInet hstorchatLocalPort li
    listen sock 2

    buddies <- newMVar M.empty
    p <- newMVar []

    tc <- newObjectDC $ TorChat myonion Available buddies p

    _ <- forkIO $ forever $ do
            (insock,_) <- accept sock
            iHdl <- socketToHandle insock ReadWriteMode
            hSetBuffering iHdl LineBuffering
            forkIO $ newConnectionRequest tc iHdl

    doc <- getDataFileName "qml/HSTorChat.qml"
    runEngineLoop defaultEngineConfig {
      initialDocument = fileDocument doc
    , contextObject   = Just $ anyObjRef tc
    }
