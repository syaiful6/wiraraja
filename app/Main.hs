module Main where

import Network.Wai.Handler.Warp (run)

import Web.Wiraraja.Application (application)

main :: IO ()
main = run 3000 application
