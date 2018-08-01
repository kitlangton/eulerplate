{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Eulerplate.Parser where

import           Data.Text                      ( Text )
import           Data.List                      ( intersperse )
import qualified Data.Text                     as T
import           Data.Attoparsec.Text    hiding ( take )
import           Control.Monad

type TypeList = [TestType]

data TestType
  = TInt Int
  | TDouble Double
  | TString Text
  | TList [TestType]
  deriving (Show)

parseTypes :: Text -> Either String TypeList
parseTypes = parseOnly testFunction

testFunction :: Parser TypeList
testFunction = testType `sepBy1` endOfLine

testType :: Parser TestType
testType = choice [tList, tDouble, tInt, tString]

tList :: Parser TestType
tList = do
  first <- tValue
  char ' '
  more <- tValue `sepBy1` char ' '
  return $ TList (first : more)
  where tValue = choice [tDouble, tInt, tString]

tString :: Parser TestType
tString = do
  text <- takeTill isEndOfLine
  guard $ T.length text > 0
  return $ TString text

tInt :: Parser TestType
tInt = TInt <$> decimal

tDouble :: Parser TestType
tDouble = do
  doubleValue <- double
  guard $ doubleValue /= fromIntegral (round doubleValue)
  return $ TDouble doubleValue

-- Type Rendering

maybeSurround :: Bool -> Text -> Text -> Text -> Text
maybeSurround shouldSurround open close body =
  if shouldSurround then open <> body <> close else body

renderArguments :: Text -> TypeList -> Text
renderArguments separator typeList =
  T.intercalate separator . fmap (T.pack . (: [])) $ Prelude.take
    (length typeList)
    ['a' ..]

renderTuple :: TypeList -> Text
renderTuple typeList =
  maybeSurround (length typeList > 1) "(" ")" $ renderTypeList ", " typeList

renderTypeList :: Text -> TypeList -> Text
renderTypeList separator typeList =
  T.intercalate separator (renderType <$> typeList)

renderOutputs :: TypeList -> Text
renderOutputs typeList = T.intercalate "\n  " outputs
 where
  outputs = T.pack . ("print " <>) . (: []) <$> take (length typeList) ['a' ..]

renderInputs :: TypeList -> Text
renderInputs typeList = T.intercalate "\n  " inputs
 where
  variables = T.pack . (: " <- ") <$> take (length typeList) ['a' ..]
  inputs    = zipWith (<>) variables (renderInput <$> typeList)

renderInput :: TestType -> Text
renderInput tlist@(TList _) =
  "fmap read . words <$> getLine :: IO " <> renderType tlist
renderInput a = "read <$> getLine :: IO " <> renderType a

renderType :: TestType -> Text
renderType (TInt    _       ) = "Int"
renderType (TDouble _       ) = "Double"
renderType (TString _       ) = "String"
renderType (TList   (x : xs)) = "[" <> renderType x <> "]"

renderValue :: TestType -> Text
renderValue (TInt    val) = T.pack . show $ val
renderValue (TDouble val) = T.pack . show $ val
renderValue (TString val) = T.pack . show $ val
renderValue (TList xs) = "[" <> T.intercalate ", " (renderValue <$> xs) <> "]"

renderValueTuple :: TypeList -> Text
renderValueTuple typeList = maybeSurround (length typeList > 1) "(" ")" body
  where body = T.intercalate ", " (renderValue <$> typeList)

renderFunction :: Text -> TypeList -> Text
renderFunction functionName typeList =
  T.intercalate " " $ functionName : (renderValue <$> typeList)

-- Value Rendering
