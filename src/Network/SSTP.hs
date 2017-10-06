module Network.SSTP
    ( dispatchEvent
    , simpleSSTP
    , Request (..)
    , emptyRequest
    ) where

import Network
import Network.SSTP.Notify
import System.IO


-- |
-- Notice an event has happen.
-- dispatchEvent "EventName"
-- >>> dispatchEvent "OnTalk"
--
dispatchEvent :: String -> IO ()
dispatchEvent event = withSocketsDo $ do
                    hSetBuffering stdout NoBuffering
                    h <- connectTo "127.0.0.1" (PortNumber 9801)
                    hSetBuffering h LineBuffering
                    hPutStrLn h $ "NOTIFY SSTP/1.1\r\nCharset:UTF-8\r\nSender:Processing\r\nEvent:" ++ event ++ "\r\n\r\n"
                    s <- hGetLine h
                    hClose h
                    print s

simpleSSTP :: String -> IO String
simpleSSTP s = withSocketsDo $ do
                 hSetBuffering stdout NoBuffering
                 h <- connectTo "127.0.0.1" (PortNumber 9801)
                 hSetBuffering h LineBuffering
                 hPutStrLn h s
                 r <- hGetLine h
                 hClose h
                 return r

sendMessage :: String -> IO String
sendMessage msg = withSocketsDo $ do
                    hSetBuffering stdout (BlockBuffering $ Just 1)
                    h <- connectTo "127.0.0.1" (PortNumber 9801)
                    hSetBuffering h LineBuffering
                    hPutStrLn h "NOTIFY SSTP/1.1\r\nCharset:Shift-JIS\r\nSender:Processing\r\nEvent:OnShellChanging\r\n\r\n"
                    s <- hGetLine h
                    hClose h
                    return s
