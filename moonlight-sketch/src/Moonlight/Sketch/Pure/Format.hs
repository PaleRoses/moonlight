module Moonlight.Sketch.Pure.Format
  ( matchFormat,
    matchSemanticFormat,
    matchFormatElement,
    matchCharClass,
    isUuid,
    isEmail,
    isUrl,
    isIsoDate,
    isIsoDateTime,
    isIp,
  )
where

import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Read as TextRead
import Moonlight.Sketch.Pure.Types
  ( CharClass (..),
    FormatElement (..),
    Quantifier (..),
    SemanticFormat (..),
    StringFormat (..),
  )

matchFormat :: StringFormat -> Text -> Bool
matchFormat format textValue =
  case format of
    Semantic semanticFormat -> matchSemanticFormat semanticFormat textValue
    Structural element -> matchFormatElement element textValue

matchSemanticFormat :: SemanticFormat -> Text -> Bool
matchSemanticFormat semanticFormat textValue =
  case semanticFormat of
    FUuid -> isUuid textValue
    FEmail -> isEmail textValue
    FUrl -> isUrl textValue
    FIsoDate -> isIsoDate textValue
    FIsoDateTime -> isIsoDateTime textValue
    FIp -> isIp textValue
    FStartsWith prefix -> Text.isPrefixOf prefix textValue
    FEndsWith suffix -> Text.isSuffixOf suffix textValue
    FContains infixValue -> Text.isInfixOf infixValue textValue
    FOneOf values -> textValue `elem` values

matchFormatElement :: FormatElement -> Text -> Bool
matchFormatElement element input =
  any Text.null (runFormatElement element input)

runFormatElement :: FormatElement -> Text -> [Text]
runFormatElement element input =
  case element of
    FLiteral literalText ->
      if Text.isPrefixOf literalText input
        then [Text.drop (Text.length literalText) input]
        else []
    Chars charClass quantifier ->
      matchQuantified quantifier (matchSingleChar charClass) input
    Sequence elements ->
      foldl
        (\remainders currentElement -> concatMap (runFormatElement currentElement) remainders)
        [input]
        elements
    Choice alternatives ->
      concatMap (\alternative -> runFormatElement alternative input) alternatives
    Group inner quantifier ->
      matchQuantified quantifier (runFormatElement inner) input

matchSingleChar :: CharClass -> Text -> [Text]
matchSingleChar charClass input =
  case Text.uncons input of
    Nothing -> []
    Just (character, remainder) ->
      if matchCharClass charClass character
        then [remainder]
        else []

matchQuantified :: Quantifier -> (Text -> [Text]) -> Text -> [Text]
matchQuantified quantifier atom =
  let (lowerBound, upperBound) = quantifierBounds quantifier
   in matchRepetitions lowerBound upperBound atom

quantifierBounds :: Quantifier -> (Int, Maybe Int)
quantifierBounds quantifier =
  case quantifier of
    Exact count -> (count, Just count)
    Range lowerBound upperBound -> (lowerBound, upperBound)
    Plus -> (1, Nothing)
    Star -> (0, Nothing)
    Optional -> (0, Just 1)

matchRepetitions :: Int -> Maybe Int -> (Text -> [Text]) -> Text -> [Text]
matchRepetitions lowerBound upperBound atom input =
  go 0 input
  where
    go count remainder =
      let includeCurrent =
            [ remainder
              | count >= lowerBound && withinUpperBound count
            ]
          continue =
            if canAdvance count
              then
                concatMap
                  (go (count + 1))
                  (filter (/= remainder) (atom remainder))
              else
                []
       in includeCurrent <> continue

    canAdvance count =
      case upperBound of
        Nothing -> True
        Just upper -> count < upper

    withinUpperBound count =
      case upperBound of
        Nothing -> True
        Just upper -> count <= upper

matchCharClass :: CharClass -> Char -> Bool
matchCharClass charClass character =
  case charClass of
    Digit -> isDigit character
    Lower -> isLower character
    Upper -> isUpper character
    Alpha -> isAlpha character
    Alnum -> isAlphaNum character
    Hex -> isDigit character || character `elem` ("abcdefABCDEF" :: String)
    Word -> isAlphaNum character || character == '_'
    Whitespace -> isSpace character
    LiteralChars chars -> character `elem` Text.unpack chars
    CharUnion classes -> any (\charClassValue -> matchCharClass charClassValue character) classes
    CharNegate inner -> not (matchCharClass inner character)

isUuid :: Text -> Bool
isUuid textValue =
  case Text.splitOn "-" textValue of
    [a, b, c, d, e] ->
      map Text.length [a, b, c, d, e] == [8, 4, 4, 4, 12]
        && all (Text.all isHexChar) [a, b, c, d, e]
    _ -> False

isEmail :: Text -> Bool
isEmail textValue =
  case Text.breakOn "@" textValue of
    (localPart, domainPartWithAt)
      | Text.null localPart -> False
      | Text.null domainPartWithAt -> False
      | otherwise ->
          let domainPart = Text.drop 1 domainPartWithAt
           in not (Text.null domainPart)
                && Text.isInfixOf "." domainPart

isUrl :: Text -> Bool
isUrl textValue =
  Text.isPrefixOf "http://" textValue
    || Text.isPrefixOf "https://" textValue
    || Text.isPrefixOf "ftp://" textValue

isIsoDate :: Text -> Bool
isIsoDate textValue =
  case Text.splitOn "-" textValue of
    [yearPart, monthPart, dayPart] ->
      Text.length yearPart == 4
        && Text.length monthPart == 2
        && Text.length dayPart == 2
        && all (Text.all isDigit) [yearPart, monthPart, dayPart]
    _ -> False

isIsoDateTime :: Text -> Bool
isIsoDateTime textValue =
  case Text.breakOn "T" textValue of
    (datePart, timeWithSeparator)
      | Text.null timeWithSeparator -> False
      | otherwise ->
          let timePart = Text.drop 1 timeWithSeparator
           in isIsoDate datePart
                && Text.length timePart >= 8
                && Text.count ":" timePart >= 2

isIp :: Text -> Bool
isIp textValue =
  case Text.splitOn "." textValue of
    [a, b, c, d] -> all isValidOctet [a, b, c, d]
    _ -> False

isValidOctet :: Text -> Bool
isValidOctet octetText =
  case TextRead.decimal octetText of
    Right (value, remainder) ->
      Text.null remainder
        && value >= (0 :: Int)
        && value <= 255
    Left _ -> False

isHexChar :: Char -> Bool
isHexChar character = isDigit character || character `elem` ("abcdefABCDEF" :: String)
