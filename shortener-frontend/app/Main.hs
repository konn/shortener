module Main (main) where

import Web.URL.Shortener.Frontend (defaultMain)

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = defaultMain
