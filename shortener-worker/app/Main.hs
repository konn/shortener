module Main (main, handlers) where

import Web.URL.Shortener.Worker

foreign export javascript "handlers" handlers :: IO JSHandlers

main :: IO ()
main = pure ()
