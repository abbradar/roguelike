module Roguelike.Dice
       ( Dice(..)
       , roll
       ) where

import Control.Applicative
import Text.Read
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Control.Monad.Random

data Dice = D !Int !Int
          deriving (Eq)

infix 5 `D`

instance Show Dice where
  show (n `D` s) = show n ++ " `D` " ++ show s

instance Read Dice where
  readPrec = prec 5 $ do
    n <- readPrec
    l <- mapM (const lexP) [1..3]
    unless (l == [Punc "`", Ident "D", Punc "`"]) $ fail "invalid Dice"
    s <- readPrec
    return $ n `D` s

instance ToJSON Dice where
  toJSON (n `D` s) = String $ T.pack $ show n ++ "d" ++ show s

instance FromJSON Dice where
  parseJSON = withText "dice" $ \(T.breakOn "d" -> (n', s')) ->
    maybe return (fail "invalid dice") $ do
      n <- readMaybe $ T.unpack n'
      s <- case T.unpack s' of
        "" -> Nothing
        _:t -> readMaybe t
      return $ n `D` s

roll :: MonadRandom m => Dice -> m Int
roll (n `D` s) = sum <$> replicateM n (getRandomR (1, s))
