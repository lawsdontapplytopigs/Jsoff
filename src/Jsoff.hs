{-# LANGUAGE InstanceSigs #-}
module Json 
    (json)
    where

import Parsix
import qualified Text.Read -- why doesn't the default `read` function return a Maybe???
import qualified Jsoff.String as String
import qualified Jsoff.Number as Number

import Control.Applicative

data JsonValue 
    = JsonNull
    | JsonBool Bool
    | JsonNumber Number.Number
    | JsonString String.JString
    | JsonArray [ JsonValue ]
    | JsonObject [ (String.JString, JsonValue )]
    deriving (Show, Eq)

whitespace :: Parser Char
whitespace =
    oneOf
        [ char ' '
        , char '\n'
        , char '\r'
        , char '\t'
        ]

json :: Parser [ JsonValue ]
json = zeroOrMore jsonValue

jsonValue :: Parser JsonValue
jsonValue =
    oneOf
        [ jsonNull
        , jsonBool
        , jsonString
        , jsonNumber
        , jsonArray
        , jsonObject
        ]

jsonNull :: Parser JsonValue
jsonNull = fmap (\_ -> JsonNull) (keyword "null")

jsonBool :: Parser JsonValue
jsonBool =
    let
        parser = oneOf [ keyword "true" , keyword "false" ]

        trueOrFalse inside =
            if inside == "true" then
                JsonBool True
            else
                JsonBool False
    in
        fmap (\ins -> trueOrFalse ins) parser

jsonNumber :: Parser JsonValue
jsonNumber = fmap (\n -> JsonNumber n) Number.number

jsonString :: Parser JsonValue
jsonString =
    (pure JsonString)
        |. char '"'
        |= zeroOrMore String.character
        |. char '"'

jsonArray :: Parser JsonValue
jsonArray =
    let
        emptyArray =
            (pure (JsonArray []))
                |. char '['
                |. zeroOrMore whitespace
                |. char ']'

        elementsArray =
            (pure JsonArray)
                |. char '['
                |= elements
                |. char ']'

        elements =
            (pure (\v vs -> v : vs))
                |= element
                |= (zeroOrMore $ char ',' *> element)

    in
        oneOf
            [ emptyArray
            , elementsArray
            ]

element =
    (pure (\v -> v))
        |. zeroOrMore whitespace 
        |= jsonValue 
        |. zeroOrMore whitespace

jsonObject :: Parser JsonValue
jsonObject =
    let
        emptyObject =
            (pure (JsonObject []))
                |. char '{'
                |. zeroOrMore whitespace
                |. char '}'

        membersObject =
            (pure JsonObject)
                |. char '{'
                |= members
                |. char '}'

        members =
            (pure (\m ms -> m : ms))
                |= member
                |= (zeroOrMore $ char ',' *> member)

        member =
            (pure (\key value -> (key, value)))
                |. zeroOrMore whitespace
                |. char '"'
                |= zeroOrMore String.character
                |. char '"'
                |. zeroOrMore whitespace
                |. char ':'
                |= element
    in
        oneOf
            [ emptyObject
            , membersObject
            ]

