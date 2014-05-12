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

-- | Wait until the hidden service hostname file
-- is ready
hiddenServiceName :: IO String
hiddenServiceName = catch (readFile "hidden_service/hostname")
                          (\e -> print (e :: IOException)
                              >> threadDelay 5000
                              >> hiddenServiceName)

main :: IO ()
main = withSocketsDo $ do

    _ <- createProcess $ proc "tor" ["-f", "torrc"]

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
