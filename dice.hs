{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, TemplateHaskell, NoMonomorphismRestriction #-}
import Control.Applicative
import Control.DeepSeq
import Data.Attoparsec.Text
import Data.Attoparsec.Expr
import Data.Text (pack)
import Data.Text.IO
import Prelude (Int, Eq, Show, Ord, fmap, ($), (.), seq)
import qualified Prelude as P
import System.Environment

import qualified Criterion as C
import Criterion.Main (defaultMain)

show = pack . P.show

data Dice = Die Int | Constant Int | Dice :+: Dice | Dice :*: Dice | Negate Dice
  deriving (Eq, Show, Ord)

instance NFData Dice where
  rnf (Die i) = rnf i `seq` ()
  rnf (Constant i) = rnf i `seq` ()
  rnf (Negate i) = rnf i `seq` ()
  rnf (a :+: b) = rnf (a,b) `seq` ()
  rnf (a :*: b) = rnf (a,b) `seq` ()

nDm :: P.Int -> P.Int -> Dice
nDm n m = P.foldl1 (:+:) . P.replicate n $ Die m

table = [ [prefix "-" Negate]
        , [binary "*" (:*:) AssocLeft]
        , [binary "+" (:+:) AssocLeft, binary "-" (\x y -> x :+: Negate y) AssocLeft]
        ]
  where 
    binary  name fun assoc = Infix (do{ string name; P.return fun }) assoc
    prefix  name fun       = Prefix (do{ string name; P.return fun })

expr = buildExpressionParser table (atom <|> parens expr)

parens p = string "(" *> p <* string ")"

die = nDm <$> option 1 decimal <*> (string "d" *> decimal)

const = Constant <$> decimal

atom = die <|> const

oExpr = expr <* endOfInput

main = do
  args <- getArgs
  let mkBench s = C.bench s $ C.nf (parseOnly oExpr) (pack s)
      parseBench = fmap mkBench args
  defaultMain parseBench
