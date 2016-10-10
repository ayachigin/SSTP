module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "Network/SSTP.hs"
               , "Network/SSTP/Notify.hs"]
