{-# LANGUAGE OverloadedStrings #-}
module Protocol ( ProtocolMsg(..)
                , parseResponse
                , parsePing
                ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text hiding (take)
import Prelude hiding (take)

data ProtocolMsg = Ping Text Text
                 | Pong Text
                 | Client Text
                 | Version Text
                 | Status Text
                 | ProfileName
                 | ProfileText
                 | AvatarAlpha
                 | ProfileAvatar
                 | AddMe
                 | RemoveMe
                 | Message Text
                 | Filename
                 | Filedata
                 | FiledataOk
                 | FiledataError
                 | FileStopSending
                 | FileStopReceiving
                 deriving Show

parseResponse :: Parser ProtocolMsg
parseResponse =  try parsePing
             <|> try parsePong
             <|> try parseVersion
             <|> try parseClient
             <|> try parseStatus
             <|> try parseAddMe
             <|> try parseDelayedMsg
             <|> parseMsg

parsePing :: Parser ProtocolMsg
parsePing = do
    _ <- string "ping"
    skipSpace
    -- parse onion address.
    bdy <- take 16
    skipSpace
    -- parse secret key.
    key <- takeText
    return $ Ping bdy key

parsePong :: Parser ProtocolMsg
parsePong = do
    string "pong"
    skipSpace
    -- parse secret key.
    key <- takeText
    return $ Pong key

parseVersion :: Parser ProtocolMsg
parseVersion = do
    string "version"
    skipSpace
    v <- takeText
    return $ Version v

parseClient :: Parser ProtocolMsg
parseClient = do
    string "client"
    skipSpace
    c <- takeText
    return $ Client c

parseStatus :: Parser ProtocolMsg
parseStatus = do
    string "status"
    skipSpace
    st <- takeText
    return $ Status st

parseAddMe :: Parser ProtocolMsg
parseAddMe = do
    string "add_me"
    skipSpace
    return $ AddMe

parseDelayedMsg :: Parser ProtocolMsg
parseDelayedMsg = do
    string "message"
    skipSpace
    string "[delayed]"
    skipSpace
    msg <- takeText
    return $ Message msg

parseMsg :: Parser ProtocolMsg
parseMsg = do
    _ <- string "message"
    skipSpace
    msg <- takeText
    return $ Message msg
