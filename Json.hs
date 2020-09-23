-- Json  Parser

-- use: 2 json cripts - ELAB

module Main (main) where

import System.Environment   
import System.IO
import qualified Data.Map as M
import Data.Char

import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers

-- JsonDocument
data JsonDocument = JsonDocument [JsonObject]
    deriving (Show)

-- JsonObject
data JsonObject = JsonObject [JsonMember]
    deriving (Show)

-- JsonMember
data JsonMember = JsonMember String JsonValue
    deriving (Show)

-- JsonValue
data JsonValue =
      JsonNull
    | JsonBool Bool
    | JsonNumber Double
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObjectValue JsonObject
    deriving (Show)


-- SO WE CAN USE THINGS LIKE |> and <& to forget about stuff like ":" and "\""", etc (right associative)

-- GO THROUGH PARSERS AND ADD NOFAILS (SEE EXAMPLE) (we can also add error messages)


-- Copy BLOCK from lecture grammar for the members thingo
-- Lexers -------------------------------------------------------------

symbolL :: Lexer
symbolL = literalL '{' <|> literalL '}' <|> literalL '[' <|>
          literalL ']' <|> literalL ',' <|> literalL '=' <|>
          literalL ':'


-- For identifying keywords such as "true", "false", "null"
keywordL :: Lexer
keywordL = 
       some (satisfyL isAlpha "")
   &%> "keyword"


jsonL :: Lexer
jsonL = dropWhite $ nofail $ total $ listL
    [whitespaceL, symbolL, keywordL, stringL, signedFloatL]

-- Parsers ------------------------------------------------------------

arrayP :: Parser JsonValue
arrayP = 
        literalP "'['" "["
     &> optional ((    valueP 
                   <&> many (   literalP "','" ","
                             &> valueP)
                   @> (\(v, vs)-> (v:vs))))
    <&  literalP "']'" "]"
    @> (\(vs:_)-> JsonArray vs)

memberP :: Parser JsonMember
memberP = 
        tagP "string"
     <&> literalP "':'" ":"
      &> valueP
      @> (\((_,s,_), v) -> JsonMember s v)

objectP :: Parser JsonObject
objectP = 
        literalP "'{'" "{"
     &> optional ((    memberP 
                   <&> many (   literalP "','" ","
                             &> memberP)
                   @> (\(m, ms)-> (m:ms))))
    <&  literalP "'}'" "}"
    @> (\(ms:_) -> JsonObject ms)
    
    

valueP :: Parser JsonValue
valueP = 
        tagP "string"
        @> (\(_,s,_) -> JsonString s)
    <|> tagP "signedFloat"
        @> (\(_,n,_) -> JsonNumber (read n))
    <|> literalP "keyword" "true"
        @> (\_ -> JsonBool True)
    <|> literalP "keyword" "false"
        @> (\_ -> JsonBool False)
    <|> literalP "keyword" "null"
        @> (\_ -> JsonNull)
    <|> objectP
        @> (\o -> JsonObjectValue o)
    <|> arrayP


jsonP :: Parser JsonDocument
jsonP = nofail $ total $ 
            optional ((    objectP
                       <&> many (   literalP "','" ","
                                 &> objectP)
                   @> (\(o, os)-> (o:os))))
            @> (\(os:_) -> JsonDocument os)

-- main :: IO ()
-- main = do
--     args <- getArgs
--     case args of 
--         [json, verification] -> interpret 
--         _                    -> error "Two arguments expected.."


main :: IO ()
main = do
    args <- getArgs
    case args of 
        [json] -> interpret json
        _      -> error "Two arguments expected.."

run :: IO ()
run = interpret "tst.json"

interpret :: FilePath -> IO ()
interpret path = do
    source <- readFile path
    let cps = preLex source -- chars and positions
    case jsonL cps of
        Error pos msg -> putStr $ errMsg pos msg source 
        OK (tlps,_) -> do
            print tlps -- tags lexemes positions
            case jsonP tlps of
                Error pos msg -> putStr $ errMsg pos msg source
                OK (json,_) -> do
                    print json
                    -- verify json TODO


-- verify :: 