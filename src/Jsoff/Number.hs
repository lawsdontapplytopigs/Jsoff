{-# LANGUAGE InstanceSigs #-}
module Jsoff.Number where

import qualified Parsix as P
import Parsix ((|.), (|=))

import Prelude hiding (exponent)

data Number 
    = Number 
        { integer :: Integer
        , fraction :: Maybe Double
        , exponent :: Maybe Integer
        }
        deriving (Eq)


instance Show Number where
    show :: Number -> String
    show (Number i_ f_ e_) =
        let
            integer_ = show (i_)
            fraction_ =
                case f_ of
                    Just doubl ->
                        show doubl
                    Nothing ->
                        ""
            exponent_ =
                case e_ of
                    Just e ->
                        if e < 0 then
                            ("e+" ++ show e)
                        else
                            ("e-" ++ show e)
                    Nothing ->
                        ""
        in
            integer_ ++ fraction_ ++ exponent_

number :: P.Parser Number
number =
    (pure Number)
        |= int
        |= P.optional frac
        |= P.optional expon


int :: P.Parser Integer
int =
    fmap P.readInt
        $ P.oneOf
            [ P.oneOrMore digit
            , (pure (\minus digits -> minus : digits))
                |= (P.char '-')
                |= P.oneOrMore digit
            ]

frac :: P.Parser Double
frac =
    fmap P.readDouble
        $ (pure (\digits -> '0' : '.' : digits))
            |. (P.char '.')
            |= P.oneOrMore digit

expon :: P.Parser Integer
expon =
    let
        withSign =
            (pure (\sign digits -> if sign == '+' then digits else sign : digits ))
                |. P.oneOf [ P.char 'e', P.char 'E' ]
                |= P.oneOf [ P.char '+', P.char '-']
                |= P.oneOrMore digit

        withoutSign =
            (pure (\digits -> digits))
                |. P.oneOf [ P.char 'e', P.char 'E' ]
                |= P.oneOrMore digit
    in
        fmap P.readInt
            $ P.oneOf
                [ withSign
                , withoutSign
                ]

digit =
    P.oneOf $ fmap P.char [ '0' .. '9' ]

