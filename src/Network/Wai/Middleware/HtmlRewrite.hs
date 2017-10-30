module Network.Wai.Middleware.HtmlRewrite (
  rewrite
) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Network.Wai
import Text.HTML.TagSoup

rewrite :: ([Tag L.ByteString] -> [Tag L.ByteString]) -> Middleware
rewrite f app req respond = app req $ \resp -> do
  lbs <- responseToLBS resp
  let rewritten = f $ parseTags lbs
  respond $ responseLBS (responseStatus resp) (responseHeaders resp) $ renderTags rewritten

responseToLBS :: Response -> IO L.ByteString
responseToLBS response = do
  let (_,_,f) = responseToStream response
  f $ \streamingBody -> do
    builderRef <- newIORef mempty
    let add :: Builder -> IO ()
        add b = atomicModifyIORef builderRef $ \builder -> (builder `mappend` b,())
        flush :: IO ()
        flush = return ()
    streamingBody add flush
    fmap toLazyByteString (readIORef builderRef)
