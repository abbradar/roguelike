module Data.Aeson.Dynamic
       ( SomeObject(..)
       , someObject
       , SomeObjectProxy(..)
       , someP
       , AllSomes
       , parseSomeJSON
       , toSomeJSON
       , parseSomesJSON
       , toSomesJSON
       ) where

import Control.Arrow
import Data.Maybe
import Data.Proxy
import Data.Typeable
import Data.Text (Text)
import Control.Lens
import Data.Aeson
import Data.Bimap (Bimap)
import qualified Data.Bimap as B
import qualified Data.HashMap as H

import Data.IndexedSet (IndexedSet, IndexKey(..))
import qualified Data.IndexedSet as I

type SomeObjectable a = (Typeable a, FromJSON a, ToJSON a)

data SomeObject :: (* -> Constraint) -> * where
  SomeObject :: (constr a, SomeObjectable a) => a -> SomeObject constr

someObject :: (constr a, SomeObjectable a, constr b, SomeObjectable b) => Prism (SomeObject a) (SomeObject b) a b
someObject = prism' SomeObject (\(SomeObject a) -> cast a) 

data SomeObjectProxy :: (* -> Constraint) -> * where
  SomeObjectProxy :: (constr a, SomeObjectable a) => Proxy a -> SomeObjectProxy constr

someP :: forall a. (constr a, SomeObjectable a) => a -> SomeObjectProxy constr
someP _ = SomeObjectProxy (Proxy :: Proxy a)

type AllSomes constr = Bimap Text (SomeObjectProxy constr)

toProxy :: a -> Proxy a
toProxy _ = Proxy

parseSomeJSON' :: AllSomes constr -> Text -> Value -> Parser (SomeObject constr)
parseSomeJSON' alls text val = do
  (SomeObjectProxy p) <- maybe mzero return $ alls `B.lookup` text
  r <- parseJSON val
  return $ SomeObject $ r `asProxyTypeOf` p

parseSomeJSON :: AllSomes constr -> Value -> Parser (SomeObject constr)
parseSomeJSON alls (Object vs) = do
  t <- vs .: "type"
  v <- vs .: "value"
  parseSomeJSON' alls t v

lookupText :: AllSomes constr -> SomeObject -> Text
lookupText obj = fromJust $ SomeObjectProxy (toProxy obj) `B.lookupR` alls

toSomeJSON :: AllSomes constr -> SomeObject -> Value
toSomeJSON alls s@(SomeObject obj) = object ["type" :. lookupText alls s, "value" :. obj]

instance IndexKey TypeRep (SomeObject constr) where
  toIndex (SomeObject a) = typeRep $ toProxy a

parseSomesJSON :: AllSomes constr -> Value -> Parser (IndexedSet TypeRep (SomeObject constr))
parseSomesJSON alls (Object vs) = I.fromList <$> mapM (uncurry $ parseSomeJSON alls) (H.toList vs)
parseSomesJSON _ _ = mzero

toSomesJSON :: AllSomes constr -> IndexedSet TypeRep (SomeObject constr) -> Value
toSomesJSON alls objs = object $ map find $ I.toList objs
  where find s@(SomeObject obj) = lookupText alls s :. obj
