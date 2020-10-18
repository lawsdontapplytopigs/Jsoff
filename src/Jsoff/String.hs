{-# LANGUAGE InstanceSigs #-}
module Jsoff.String 
    ( JString
    , Character(..)
    , EscapedChar(..)
    , character
    , isHex
    )
    where

import Parsix

type JString = [ Character ]

data Character
    = UnescapedCharacter Char
    | EscapedCharacter EscapedChar
    deriving (Show, Eq)

data EscapedChar
    = DoubleQuote
    | Backslash
    | Forwardslash
    | Backspace
    | Formfeed
    | Linefeed
    | CarriageReturn
    | HorizontalTab
    | Hex Char Char Char Char
    deriving (Show, Eq)

character :: Parser Character
character =
    oneOf
        [ fmap (\c -> UnescapedCharacter c) (validChar unescapedCharacter)
        , escapedCharacter
        ]

unescapedCharacter :: Char -> Bool
unescapedCharacter ch =
    case ch of
        '"' ->
            False
        '\\' ->
            False
        _ ->
            True

escapedCharacter :: Parser Character
escapedCharacter =
    (pure EscapedCharacter)
        |. (char '\\')
        |= oneOf
            [ fmap (\_ -> DoubleQuote) (char '"')
            , fmap (\_ -> Backslash) (char '\\')
            , fmap (\_ -> Forwardslash) (char '/')
            , fmap (\_ -> Backspace) (char 'b')
            , fmap (\_ -> Formfeed) (char 'f')
            , fmap (\_ -> Linefeed) (char 'n')
            , fmap (\_ -> CarriageReturn) (char 'r')
            , fmap (\_ -> HorizontalTab) (char 't')
            , (pure Hex)
                |. char 'u'
                |= validChar isHex
                |= validChar isHex
                |= validChar isHex
                |= validChar isHex
            ]

isHex :: Char -> Bool
isHex c =
    any (\ch -> ch == c) ['0'..'9'] 
    || any (\ch -> ch == c) ['a' .. 'f']
    || any (\ch -> ch == c) ['A' .. 'F']

