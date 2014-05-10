{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.Map as M
import Data.Text as T
import Graphics.QML
import Network.Socket
import System.IO
import System.Process

import HSTorChat.Client
import HSTorChat.GUI
import HSTorChat.Protocol

main :: IO ()
main = withSocketsDo $ do

    _ <- createProcess $ proc "tor" ["-f", "torrc"]

    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1

    li <- inet_addr hstorchatHost
    bindSocket sock $ SockAddrInet hstorchatLocalPort li
    listen sock 2

    buddies <- newMVar M.empty
    p <- newMVar []

    onion <- readFile "hidden_service/hostname"
    let myonion = T.take 16 $ T.pack onion

    putStr $ "Hello " ++ onion

    ui <- newObjectDC $ UI myonion Available buddies p

    _ <- forkIO $ forever $ do
        (insock,_) <- accept sock
        iHdl <- socketToHandle insock ReadWriteMode
        hSetBuffering iHdl LineBuffering
        forkIO $ newConnectionRequest ui iHdl

    runEngineLoop defaultEngineConfig {
        initialDocument    = fileDocument "qml/HSTorChat.qml"
      , contextObject      = Just $ anyObjRef ui
    }
