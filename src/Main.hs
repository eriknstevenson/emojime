{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Default
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Lucid.Svg
import qualified Lucid.Svg.Attributes as A (filter_)
import Network.HTTP.Types.Status
import Network.Wai.Middleware.RequestLogger
import qualified Network.Wreq as Wreq
import System.Random
import Web.Scotty


data Dimension a = Dimension a a deriving (Eq, Read)

instance Show a => Show (Dimension a) where
  show (Dimension w h) = show w <> " X " <> show h


data Config = Config
  { _size :: Dimension Int
  , _bgColor :: Text
  , _viewBox :: Dimension Int
  , _emoji :: Text
  }
makeLenses ''Config

instance Default Config where
  def = Config (Dimension 300 300) "gray" (Dimension 500 500) "1f61c"


main :: IO ()
main = do

  resp <- Wreq.get emojiListURL

  let codes = resp ^.. Wreq.responseBody.members.peopleCodes._String
      --I dont think this is actually necessary.
      --splitCodes = nub . concatMap (map T.pack . splitOn "-" . T.unpack) $ rawCodes
      codeCount = length codes

  putStrLn $ show codeCount <> " emojis found."

  scotty 3000 $ do
    middleware logStdoutDev
    get "/" $ do
      status ok200
      raw $ runReader (renderBST genSvg) def
    get "/:width/:height/" $ do
      status ok200
      (w, h) <- getRequestedSize
      code <- liftIO $ (codes !!) <$> randomRIO (0, codeCount)
      raw $ runReader (renderBST genSvg) (Config (Dimension w h) "gray" (Dimension 500 500) code)
    get "/:width/:height/:color" $ do
      status ok200
      (w, h) <- getRequestedSize
      color <- param "color"
      code <- liftIO $ (codes !!) <$> randomRIO (0, codeCount)
      raw $ runReader (renderBST genSvg) (Config (Dimension w h) color (Dimension 500 500) code)
    notFound $ status notFound404

emojiListURL :: String
emojiListURL =
  "https://raw.githubusercontent.com/Ranks/emojione/master/emoji.json"

emojiCDN :: Text -> Text
emojiCDN code =
  "https://cdn.jsdelivr.net/emojione/assets/svg/" <> code <> ".svg"

peopleCodes :: Traversal' Value Value
peopleCodes =
  belowEmojiOrder 205 . justPeople . key "unicode"

justPeople :: Traversal' Value Value
justPeople =
  filtered (\e -> e ^? key "category" . _String == Just "people")

belowEmojiOrder :: Int -> Traversal' Value Value
belowEmojiOrder n =
  filtered (\e -> fmap (read . T.unpack) (e ^? key "emoji_order" . _String) < Just n)

testSvg :: IO ()
testSvg = print $ runReader (renderTextT genSvg) def

getRequestedSize :: ActionM (Int, Int)
getRequestedSize = do
  w <- param "width"
  h <- param "height"
  return (w, h)

genSvg ::  SvgT (Reader Config) ()
genSvg = do
  doctype_
  s <- lift getSize
  vb <- lift getViewbox
  svg11_ (background >> pasteEmoji >> label) `with` (vb : s)


background :: SvgT (Reader Config) ()
background = do
  color <- lift $ view bgColor
  rect_ [y_ "-10000", x_ "-10000", stroke_width_ "2", stroke_ "#000000", fill_ color, width_ "20000", height_ "20000"]


pasteEmoji :: SvgT (Reader Config) ()
pasteEmoji = do
  code <- lift . view $ emoji
  image_ [ width_ "100%", height_ "100%"
         , xlinkHref_ $ emojiCDN code]


label :: SvgT (Reader Config) ()
label = do
  defs_ $
    filter_' [id_ "blurfilter", x_ "0", y_ "0"] $
      feGaussianBlur_ [in_ "SourceGraphic", stdDeviation_ "0.75"]
  buildText `with` [A.filter_ "url(#blurfilter)", y_ "75.5%", x_ "50.5%", fill_ "black"]
  buildText `with` [y_ "75%", x_ "50%", fill_ "white"]

  where
    -- temporary while waiting for PR on lucid-svg package.
    -- see: https://github.com/jeffreyrosenbluth/lucid-svg/pull/11
    filter_' = term "filter"

    buildText = do
      caption <- lift $ toHtml . show <$> view size
      text_ [text_anchor_ "middle", font_size_ "64"] caption


dimensionToAttr :: Show a => Dimension a -> [Attribute]
dimensionToAttr (Dimension w h) =
  [width_ . convert $ w, height_ . convert $ h]
  where
    convert :: (Show a) => a -> Text
    convert = T.pack . show


getSize :: Reader Config [Attribute]
getSize = dimensionToAttr <$> view size


getViewbox :: Reader Config Attribute
getViewbox =
  viewBox_ . convert <$> view viewBox
  where
    convert :: (Num a, Show a) => Dimension a -> Text
    convert (Dimension w h) = T.pack . unwords . map show $ [0, 0, w, h]