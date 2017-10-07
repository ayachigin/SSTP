module Network.SSTP.Notify where

import Data.List

type RequestString = String

data Request = Request { _charset   :: String
                       , _sender    :: String
                       , _event     :: String
                       , _reference :: [String]
                       , _script    :: String
                       , _entry     :: String
                       , _option    :: String
                       , _ifGhost   :: String
                       , _hWnd      :: String
                       , _locale    :: String
                       , _marker    :: String}
             deriving (Ord, Eq, Read)

join :: String -> [String] -> String
join sep = concat . intersperse sep

instance Show Request where
    show r = (++"\r\n") $ join "\r\n"
             [ "NOTIFY SSTP/1.1"
             , "Charset: " ++ _charset r
             , "Sender: " ++ _sender r
             , "Event: " ++ _event r
             , "Script: " ++ _script r
             , "Entry: " ++ _entry r
             , "Option: " ++ _option r
             , "IfGhost: " ++ _ifGhost r
             , "hWnd: " ++ _hWnd r
             , "Locale: " ++ _locale r
             , "Maker: " ++ _marker r
             , join "\r\n" references
             ]
        where
          references = map (\(ref, n) -> "Reference" ++ show n ++ ": " ++ ref) (zip (_reference r) [0..])

-- |
-- Make request string from Request.
-- >>> mkRequestString $ emptyRequest{_event="OnTalk",_reference=["hoge", "fuga"]}
-- "NOTIFY SSTP/1.1\r\nCharset: UTF-8\r\nSender: Haskell SSTP\r\nEvent: OnTalk\r\nScript: \r\nEntry: \r\nOption: \r\nIfGhost: \r\nhWnd: \r\nLocale: \r\nMaker: \r\nReference1: hoge\r\nReference2: fuga\r\n"
mkRequestString :: Request -> RequestString
mkRequestString = show

-- |
-- >>> _event emptyRequest{_event="Foo"}
-- "Foo"
emptyRequest :: Request
emptyRequest = Request { _charset="UTF-8"
                       , _sender="Haskell SSTP"
                       , _event=""
                       , _reference=[]
                       , _script=""
                       , _entry=""
                       , _option=""
                       , _ifGhost=""
                       , _hWnd=""
                       , _locale=""
                       , _marker=""}

event :: String -> RequestString
event = mkRequestString.event_

event_ :: String -> Request
event_ e = emptyRequest {_event=e}
