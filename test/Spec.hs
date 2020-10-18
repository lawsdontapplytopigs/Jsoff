
import qualified Json
import qualified String
import qualified Parsix
import qualified Number

import Test.Hspec (hspec, describe, it, shouldBe)
import Test.QuickCheck (property)

main :: IO ()
main = hspec $ do
    describe "Json.jsonString" $ do
        it "parses empty strings" $ do
            (Parsix.run Json.jsonString "\"\"") `shouldBe` (Just ("", Json.JsonString []))
        it "parses an escaped double quote" $ do
            (Parsix.run Json.jsonString "\"\\\"\"") `shouldBe` (Just ("", Json.JsonString [(String.EscapedCharacter String.DoubleQuote)]))
        it "parses an escaped backslash" $ do
            (Parsix.run Json.jsonString "\"\\\\\"") `shouldBe` (Just ("", Json.JsonString [(String.EscapedCharacter String.Backslash)]))
        it "parses an escaped forwardslash" $ do
            (Parsix.run Json.jsonString "\"\\/\"") `shouldBe` (Just ("", Json.JsonString [(String.EscapedCharacter String.Forwardslash)]))
        it "parses an escaped backspace" $ do
            (Parsix.run Json.jsonString "\"\\b\"") `shouldBe` (Just ("", Json.JsonString [(String.EscapedCharacter String.Backspace)]))
        it "parses an escaped formfeed" $ do
            (Parsix.run Json.jsonString "\"\\f\"") `shouldBe` (Just ("", Json.JsonString [(String.EscapedCharacter String.Formfeed)]))
        it "parses an escaped linefeed (newline)" $ do
            (Parsix.run Json.jsonString "\"\\n\"") `shouldBe` (Just ("", Json.JsonString [(String.EscapedCharacter String.Linefeed)]))
        it "parses an escaped carriage return" $ do
            (Parsix.run Json.jsonString "\"\\r\"") `shouldBe` (Just ("", Json.JsonString [(String.EscapedCharacter String.CarriageReturn)]))
        it "parses an escaped horizontal tab" $ do
            (Parsix.run Json.jsonString "\"\\t\"") `shouldBe` (Just ("", Json.JsonString [(String.EscapedCharacter String.HorizontalTab)]))
        it "parses escaped hex digits" $ do
            property $ \digit -> 
                (Parsix.run Json.jsonString ("\"\\u" ++ [digit, digit, digit, digit] ++ "\"")) == 
                    case String.isHex digit of
                        True ->
                            (Just ("", Json.JsonString [(String.EscapedCharacter (String.Hex digit digit digit digit))]))
                        False ->
                            Nothing

        it "parses integers" $ do
            (Parsix.run Json.jsonNumber "123")
                `shouldBe`
                    (Just ("", Json.JsonNumber (Number.Number { Number.integer = 123, Number.fraction = Nothing, Number.exponent = Nothing })))
        it "parses floating point numbers" $ do
            (Parsix.run Json.jsonNumber "123.0243504")
                `shouldBe`
                    (Just ("", Json.JsonNumber (Number.Number { Number.integer = 123, Number.fraction = Just 0.0243504, Number.exponent = Nothing })))
        it "parses floating point numbers with exponents" $ do
            (Parsix.run Json.jsonNumber "884.4325e+12")
                `shouldBe`
                    (Just ("", Json.JsonNumber (Number.Number { Number.integer = 884, Number.fraction = Just 0.4325, Number.exponent = Just 12})))

        it "parses empty arrays" $ do
            (Parsix.run Json.jsonArray "[]")
                `shouldBe`
                    (Just ("", Json.JsonArray []))

        it "parses integer arrays" $ do
            (Parsix.run Json.jsonArray "[1, 2,3]")
                `shouldBe`
                    (Just ("", Json.JsonArray $ fmap (\n -> Json.JsonNumber (Number.Number n Nothing Nothing)) [ 1, 2, 3 ]))

