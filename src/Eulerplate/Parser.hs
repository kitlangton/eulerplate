{-# LANGUAGE OverloadedStrings #-}

module Eulerplate.Parser where

import Data.Text (Text)
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Attoparsec.Text hiding (take)

type TypeList = [TestType]

data TestType
  = TInt
  | TDouble
  | TList TestType
  deriving (Show)

parseTypes :: Text -> Either String TypeList
parseTypes = parseOnly testFunction

testFunction :: Parser TypeList
testFunction = testType `sepBy1` endOfLine

testType :: Parser TestType
testType = choice [tList, tDouble, tInt]

tList :: Parser TestType
tList = do
   t <- choice [tDouble, tInt]
   char ' '
   _ <- choice [tDouble, tInt]
   _ <- takeTill isEndOfLine
   return (TList t)

tInt :: Parser TestType
tInt = decimal >> return TInt

tDouble :: Parser TestType
tDouble = decimal >> char '.' >> decimal >> return TDouble

renderArguments :: Text -> TypeList -> Text
renderArguments separator typeList = T.intercalate separator . fmap (T.pack . (: [])) $ Prelude.take (length typeList) ['a'..]

renderTypeList :: Text -> TypeList -> Text
renderTypeList separator typeList = T.intercalate separator (render <$> typeList)

renderInputs :: TypeList -> Text
renderInputs typeList =
  T.intercalate "\n  " inputs
  where
    variables = T.pack . (:" <- ") <$> take (length typeList) ['a'..]
    inputs = zipWith (<>) variables (renderInput <$> typeList)


renderInput :: TestType -> Text
renderInput tlist@(TList _) = "fmap read . words <$> getLine :: IO " <> render tlist
renderInput a = "read <$> getLine :: IO " <> render a

render :: TestType -> Text
render TInt = "Int"
render TDouble = "Double"
render (TList x) = T.concat ["[", render x, "]"]
