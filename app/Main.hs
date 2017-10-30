{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.HtmlRewrite
import Text.HTML.TagSoup

main :: IO ()
main = runEnv 3000 $ rewrite notWow $ \req respond -> respond $ responseLBS ok200
  [("Content-Type", "text/html")
  ]
  "<html><head></head><body><p>WOW</p><br>Bop</body>"

notWow :: [Tag L.ByteString] -> [Tag L.ByteString]
notWow = map changer
  where
    -- changer :: Tag L.ByteString -> Tag L.ByteString
    changer t = case t of
      TagText t -> TagText "wibble"
      other -> other
