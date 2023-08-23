{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lexer (
  lexText,
  TokenList (..),
  UnspannedToken (..),
  displayToken,
) where

import qualified Data.Char as Char
import Data.Coerce

import Debug.Trace

import Data.Text.Array (Array)

import qualified Data.Text as Txt
import qualified Data.Text.Array as Array
import qualified Data.Text.Internal as Text
import qualified Data.Text.Internal.Encoding.Utf16 as Utf16
import qualified Data.Text.Internal.Unsafe.Char as Char
import qualified Position
import Protolude hiding (State, ord, state, trace)
import qualified Span
import qualified UTF16

data TokenList
  = Empty
  | Token !Position.LineColumn !Span.Absolute !UnspannedToken !TokenList
  deriving (Show, Generic, NFData)

data UnspannedToken
  = -- Identifiers
    Identifier !Text
  | -- Reserved identifiers
    Let
  | In
  | Data
  | Where
  | Forall
  | Case
  | Of
  | Underscore
  | -- Operators
    Operator !Text
  | -- Reserved operators
    Equals
  | Dot
  | Colon
  | Pipe
  | RightArrow
  | QuestionMark
  | Forced
  | -- Special
    Number !Integer
  | Lambda
  | LeftParen
  | RightParen
  | LeftImplicitBrace
  | RightImplicitBrace
  | Error
  deriving (Eq, Show, Generic, NFData)

displayToken :: UnspannedToken -> Text
displayToken token =
  case token of
    Identifier name -> name
    Let -> "let"
    In -> "in"
    Data -> "data"
    Where -> "where"
    Forall -> "forall"
    Case -> "case"
    Of -> "of"
    Underscore -> "_"
    Operator name -> name
    Equals -> "="
    Dot -> "."
    Colon -> ":"
    Pipe -> "|"
    RightArrow -> "->"
    QuestionMark -> "?"
    Forced -> "~"
    Number int -> show int
    Lambda -> "\\"
    LeftParen -> "("
    RightParen -> ")"
    LeftImplicitBrace -> "@{"
    RightImplicitBrace -> "}"
    Error -> "[error]"

-- >>> lexText' (" moo ")
-- Token (LineColumn 0 1) (Absolute (Absolute 1) (Absolute 4)) (Identifier "moo") Empty

lexText' :: [Char] -> TokenList
lexText' str = lexText (Txt.pack str)

lexText :: Text -> TokenList
lexText (Text.Text array offset length_) =
  lex
    State
      { input = array
      , position = coerce offset
      , lineColumn = Position.LineColumn 0 0
      , end = coerce $ offset + length_
      }

txt2Array :: Text -> Array 
txt2Array (Text.Text array _ _) = array

-- >>> Text.Text (txt2Array (Txt.pack "moo")) 0 5

-------------------------------------------------------------------------------

data State = State
  { input :: !Array
  , position :: !Position.Absolute
  , lineColumn :: !Position.LineColumn
  , end :: !Position.Absolute
  }

--instance Show Array where
--  show x =
--    "arraytext"

stateRender :: State -> [Char]
stateRender (State _ pos lineCol end) =
  "State(<input>, " ++ show pos ++ ", " ++ show lineCol ++ ", " ++ show end ++ ")"

lex :: State -> TokenList
lex state@State {..}
  | position >= end =
      trace ("[lex: empty " ++ stateRender state ++ "]") Empty
  | otherwise =
      case index input position of
        -------------------------------------------------------------------------
        -- Parens
        [UTF16.unit1|(|] ->
          token1 LeftParen $ lex state1
        [UTF16.unit1|)|] ->
          token1 RightParen $ lex state1
        -------------------------------------------------------------------------
        -- Comments
        [UTF16.unit1|-|]
          | position1 < end
          , [UTF16.unit1|-|] <- index input position1 ->
              singleLineComment state2
        [UTF16.unit1|{|]
          | position1 < end
          , [UTF16.unit1|-|] <- index input position1 ->
              multiLineComment state2 1
        -------------------------------------------------------------------------
        -- Whitespace
        [UTF16.unit1| |] ->
          trace ("[lex: lexing whitespace .. " ++ stateRender state1 ++ "]") (lex state1)
        [UTF16.unit1|	|] ->
          trace ("[lex: lexing weird whitespace .. " ++ stateRender state1 ++ "]") 
          (lex state1)

        [UTF16.unit1|
|] ->
            trace ("[lex: lexing newln .. " ++ stateRender state1 ++ "]") (lex state1 {lineColumn = Position.addLine lineColumn})
        -------------------------------------------------------------------------
        -- Number (NOTE: I think he meant 'lambda' here -dtw)
        [UTF16.unit1|\|] ->
          token1 Lambda $ lex state1
        -------------------------------------------------------------------------
        -- Number
        -- 'guarded patterns' below.. see:
        -- https://stackoverflow.com/questions/46509876/what-does-a-comma-in-the-guard-syntax-do)
        [UTF16.unit1|-|] -- unary minus
          | position1 < end
          , c <- index input position1
          , isNumeric c ->
              number position lineColumn state2 True (fromIntegral $ c - [UTF16.unit1|0|])
        c -- or just a num c
          | isNumeric c ->
              number position lineColumn state1 False (fromIntegral $ c - [UTF16.unit1|0|])
        -------------------------------------------------------------------------
        -- Implicit braces
        [UTF16.unit1|@|]
          | position1 < end
          , [UTF16.unit1|{|] <- index input position1 ->
              token2 LeftImplicitBrace $ lex state2
        [UTF16.unit1|}|] ->
          token1 RightImplicitBrace $ lex state1
        -------------------------------------------------------------------------
        -- Operator or identifier
        c
          | isASCIIIdentifierStart c ->
              trace ("[lex: lexing identifier .. " ++ stateRender state ++ "]") (identifier position lineColumn state1)

        c
          | isASCIIOperator c ->
              trace ("[lex: lexing operator .. " ++ stateRender state ++ "]") operator position lineColumn state1
        c
          | c >= 128
          , Utf16.validate1 c
          , c' <- Char.unsafeChr c
          , Char.isAlpha c' ->
             trace ("HERE1 ") identifier position lineColumn state1
        c
          | c >= 128
          , Utf16.validate1 c
          , c' <- Char.unsafeChr c
          , Char.isSymbol c' || Char.isPunctuation c' ->
              trace ("HERE2 ")  operator position lineColumn state1
        c1
          | position1 < end
          , c2 <- index input position1
          , Utf16.validate2 c1 c2
          , c <- Utf16.chr2 c1 c2
          , Char.isAlpha c ->
              trace ("HERE3 ") identifier position lineColumn state2
        c1
          | position1 < end
          , c2 <- index input position1
          , Utf16.validate2 c1 c2
          , c <- Utf16.chr2 c1 c2
          , Char.isSymbol c || Char.isPunctuation c ->
              trace ("HERE4 ")  operator position lineColumn state2
        -------------------------------------------------------------------------
        -- Error
        _ ->
          token1 Error $ lex state1
  where
    state1 =
      state
        { position = position1
        , lineColumn = Position.addColumns lineColumn 1
        }

    state2 =
      state
        { position = position2
        , lineColumn = Position.addColumns lineColumn 2
        }

    position1 =
      Position.add position 1

    position2 =
      Position.add position 2

    token1 =
      Token lineColumn $ Span.Absolute position position1

    token2 =
      Token lineColumn $ Span.Absolute position position2

-------------------------------------------------------------------------------
index :: Array -> Position.Absolute -> Word16
index =
  coerce Array.unsafeIndex

isNumeric :: Word16 -> Bool
isNumeric c =
  [UTF16.unit1|0|] <= c && c <= [UTF16.unit1|9|]

isASCIIIdentifierStart :: Word16 -> Bool
isASCIIIdentifierStart c =
  [UTF16.unit1|a|] <= c && c <= [UTF16.unit1|z|]
    || [UTF16.unit1|A|] <= c && c <= [UTF16.unit1|Z|]
    || c == [UTF16.unit1|_|]

isASCIIIdentifierCont :: Word16 -> Bool
isASCIIIdentifierCont c =
  isASCIIIdentifierStart c
    || isNumeric c
    || c == [UTF16.unit1|'|]

isASCIIOperator :: Word16 -> Bool
isASCIIOperator c =
  case c of
    [UTF16.unit1|!|] -> True
    [UTF16.unit1|#|] -> True
    [UTF16.unit1|$|] -> True
    [UTF16.unit1|%|] -> True
    [UTF16.unit1|&|] -> True
    [UTF16.unit1|*|] -> True
    [UTF16.unit1|+|] -> True
    [UTF16.unit1|,|] -> True
    [UTF16.unit1|-|] -> True
    [UTF16.unit1|.|] -> True
    [UTF16.unit1|/|] -> True
    [UTF16.unit1|:|] -> True
    [UTF16.unit1|;|] -> True
    [UTF16.unit1|<|] -> True
    [UTF16.unit1|=|] -> True
    [UTF16.unit1|>|] -> True
    [UTF16.unit1|?|] -> True
    [UTF16.unit1|@|] -> True
    [UTF16.unit1|\|] -> True
    [UTF16.unit1|^|] -> True
    [UTF16.unit1|`|] -> True
    [UTF16.unit1|||] -> True
    [UTF16.unit1|~|] -> True
    _ -> False

-------------------------------------------------------------------------------

-- i.e.: identifierOrReservedWordToken
identifier
  :: Position.Absolute
  -> Position.LineColumn
  -> State
  -> TokenList
identifier !startPosition !startLineColumn state@State {..}
  | position >= end =
      trace ("[identifier: lexing identifier/keyword (end) " ++ stateRender state ++ "]") 
            (identifierToken input startPosition startLineColumn position Empty)
  | otherwise =
      case index input position of
        c
          | isASCIIIdentifierCont c ->
              trace ("[identifier: lex id continue " ++ stateRender state ++ "]") 
              identifier startPosition startLineColumn state1
        [UTF16.unit1|.|] ->
          dotIdentifier startPosition startLineColumn position lineColumn state1
        c
          | Utf16.validate1 c
          , Char.isAlpha $ Char.unsafeChr c ->
              trace ("[identifier (isAlpha) " ++ stateRender state ++ "]") 
              identifier startPosition startLineColumn state1
        c1
          | position1 < end
          , c2 <- index input position1
          , Utf16.validate2 c1 c2
          , Char.isAlpha $ Utf16.chr2 c1 c2 ->
              trace ("[identifier (c1 c2 case) " ++ stateRender state ++ "]") 
              identifier startPosition startLineColumn state2
        _ ->
          trace ("[identifier: id/keyword (default) " ++ stateRender state ++ "]") 
          identifierToken input startPosition startLineColumn position $
            lex state
  where
    state1 =
      state
        { position = position1
        , lineColumn = Position.addColumns lineColumn 1
        }

    state2 =
      state
        { position = position2
        , lineColumn = Position.addColumns lineColumn 2
        }

    position1 =
      Position.add position 1

    position2 =
      Position.add position 2

dotIdentifier
  :: Position.Absolute
  -> Position.LineColumn
  -> Position.Absolute
  -> Position.LineColumn
  -> State
  -> TokenList
dotIdentifier !startPosition !startLineColumn !dotPosition !dotLineColumn state@State {..}
  | position >= end =
      identifierToken input startPosition startLineColumn position $
        Token dotLineColumn (Span.Absolute dotPosition position) Dot Empty
  | otherwise =
      case index input position of
        c
          | isASCIIIdentifierCont c ->
              identifier startPosition startLineColumn state1
        c
          | isASCIIOperator c ->
              operator startPosition startLineColumn state1
        c
          | c >= 128
          , Utf16.validate1 c
          , c' <- Char.unsafeChr c
          , Char.isAlpha c' ->
              identifier startPosition startLineColumn state1
        c
          | c >= 128
          , Utf16.validate1 c
          , c' <- Char.unsafeChr c
          , Char.isSymbol c' || Char.isPunctuation c' ->
              operator startPosition startLineColumn state1
        c1
          | position1 < end
          , c2 <- index input position1
          , Utf16.validate2 c1 c2
          , c <- Utf16.chr2 c1 c2
          , Char.isAlpha c ->
              identifier startPosition startLineColumn state2
        c1
          | position1 < end
          , c2 <- index input position1
          , Utf16.validate2 c1 c2
          , c <- Utf16.chr2 c1 c2
          , Char.isSymbol c || Char.isPunctuation c ->
              operator startPosition startLineColumn state2
        _ ->
          identifierToken input startPosition startLineColumn dotPosition $
            Token dotLineColumn (Span.Absolute dotPosition position) Dot $
              lex state
  where
    state1 =
      state
        { position = position1
        , lineColumn = Position.addColumns lineColumn 1
        }

    state2 =
      state
        { position = position2
        , lineColumn = Position.addColumns lineColumn 2
        }

    position1 =
      Position.add position 1

    position2 =
      Position.add position 2


identifierToken
  :: Array
  -> Position.Absolute
  -> Position.LineColumn
  -> Position.Absolute
  -> TokenList
  -> TokenList
identifierToken !input !startPosition !startLineColumn !position =
  Token startLineColumn (Span.Absolute startPosition position) $
    case index input startPosition of
      [UTF16.unit1|_|] | len == 1 -> Underscore

      -- note: thinking the "let" <- stuff is just to say explicitly in the
      -- code what we're matching
      [UTF16.unit1|l|] | "let" <- str -> Let
      [UTF16.unit1|i|] | "in" <- str -> In
      [UTF16.unit1|d|] | "data" <- str -> Data
      [UTF16.unit1|w|] | "where" <- str -> Where
      [UTF16.unit1|f|] | "forall" <- str -> Forall
      [UTF16.unit1|c|] | "case" <- str -> Case
      [UTF16.unit1|o|] | "of" <- str -> Of
      _ -> Identifier str
  where
    len =
      position - startPosition

    str =
      Text.Text
        input
        (coerce startPosition)
        (coerce position - coerce startPosition) -- just giving us what str.length will in scala.. (other than perhaps the position index) 

-- >>> Text.Text (txt2Array (Txt.pack " true")) 1 (5 - 1)
-- "true"

-------------------------------------------------------------------------------

operator
  :: Position.Absolute
  -> Position.LineColumn
  -> State
  -> TokenList
operator !startPosition !startLineColumn state@State {..}
  | position >= end =
      identifierToken input startPosition startLineColumn position Empty
  | otherwise =
      case index input position of
        c
          | isASCIIOperator c ->
              operator startPosition lineColumn state1
        c
          | c >= 128
          , Utf16.validate1 c
          , c' <- Char.unsafeChr c
          , Char.isSymbol c' || Char.isPunctuation c' ->
              operator startPosition lineColumn state1
        c1
          | position1 < end
          , c2 <- index input position1
          , Utf16.validate2 c1 c2
          , c <- Utf16.chr2 c1 c2
          , Char.isSymbol c || Char.isPunctuation c ->
              operator startPosition lineColumn state2
        _ ->
          operatorToken input startPosition startLineColumn position $
            lex state
  where
    state1 =
      state
        { position = position1
        , lineColumn = Position.addColumns lineColumn 1
        }

    state2 =
      state
        { position = position2
        , lineColumn = Position.addColumns lineColumn 2
        }

    position1 =
      Position.add position 1

    position2 =
      Position.add position 2

operatorToken
  :: Array
  -> Position.Absolute
  -> Position.LineColumn
  -> Position.Absolute
  -> TokenList
  -> TokenList
operatorToken !input !startPosition !startLineColumn !position =
  Token startLineColumn (Span.Absolute startPosition position) $
    case index input startPosition of
      [UTF16.unit1|=|] | len == 1 -> Equals
      [UTF16.unit1|.|] | len == 1 -> Dot
      [UTF16.unit1|:|] | len == 1 -> Colon
      [UTF16.unit1|||] | len == 1 -> Pipe
      [UTF16.unit1|-|] | "->" <- str -> RightArrow
      [UTF16.unit1|?|] | len == 1 -> QuestionMark
      [UTF16.unit1|~|] | len == 1 -> Forced
      _ -> Operator str
  where
    len =
      position - startPosition

    str =
      Text.Text
        input
        (coerce startPosition)
        (coerce position - coerce startPosition)

-------------------------------------------------------------------------------

number
  :: Position.Absolute
  -> Position.LineColumn
  -> State
  -> Bool
  -> Integer
  -> TokenList
number !startPosition !startLineColumn state@State {..} !shouldNegate !acc
  | position >= end =
      token Empty
  | otherwise =
      case index input position of
        c
          | isNumeric c -> do
              let acc' =
                    acc * 10 + fromIntegral (c - [UTF16.unit1|0|])
              number startPosition startLineColumn state1 shouldNegate acc'
        _ ->
          token $ lex state
  where
    token =
      Token startLineColumn (Span.Absolute startPosition position) $
        Number $
          if shouldNegate then negate acc else acc

    state1 =
      state
        { position = Position.add position 1
        , lineColumn = Position.addColumns lineColumn 1
        }

-------------------------------------------------------------------------------

singleLineComment :: State -> TokenList
singleLineComment state@State {..}
  | position >= end =
      Empty
  | otherwise =
      case index input position of
        [UTF16.unit1|
|] ->
            lex state {lineColumn = Position.addLine lineColumn}
        _ ->
          singleLineComment state1
  where
    state1 =
      state {position = Position.add position 1}

multiLineComment :: State -> Int -> TokenList
multiLineComment !state 0 =
  lex state
multiLineComment state@State {..} !depth
  | position >= end =
      Empty
  | otherwise =
      case index input position of
        [UTF16.unit1|{|]
          | position1 < end
          , [UTF16.unit1|-|] <- index input position1 ->
              multiLineComment state2 $ depth + 1
        [UTF16.unit1|-|]
          | position1 < end
          , [UTF16.unit1|}|] <- index input position1 ->
              multiLineComment state2 (depth - 1)
        [UTF16.unit1|
|] ->
            multiLineComment
              state1
                { lineColumn = Position.addLine lineColumn
                }
              depth
        _ ->
          multiLineComment state1 depth
  where
    state1 =
      state
        { position = position1
        , lineColumn = Position.addColumns lineColumn 1
        }

    state2 =
      state
        { position = position2
        , lineColumn = Position.addColumns lineColumn 2
        }

    position1 =
      Position.add position 1

    position2 =
      Position.add position 2

-- TODO: Fuzz tests for
--  length, line column
--  qualified identifiers
