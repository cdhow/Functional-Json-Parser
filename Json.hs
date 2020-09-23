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
    | JsonObject [JsonMember]
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
    @> (\(vs) -> case vs of
        [] -> JsonArray []
        (vs':_) -> JsonArray vs'
       )

memberP :: Parser JsonMember
memberP = 
        tagP "string"
     <&> literalP "':'" ":"
      &> valueP
      @> (\((_,s,_), v) -> JsonMember s v)

objectP :: Parser JsonValue
objectP = 
        literalP "'{'" "{"
     &> optional ((    memberP 
                   <&> many (   literalP "','" ","
                             &> memberP)
                   @> (\(m, ms)-> (m:ms))))
    <&  literalP "'}'" "}"
    @> (\ms -> case ms of 
        []     -> JsonObject []
        (ms':_) -> JsonObject ms'
       )
    
    

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
    <|> arrayP


jsonP :: Parser JsonValue
jsonP = nofail $ total $ valueP

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
        [json, schema] -> interpret json schema
        _      -> error "Two arguments expected.."

run :: IO ()
run = interpret "tst.json" "tst1.json"

-- CHANGE TO NOT REPEAT TODOO
interpret :: FilePath -> FilePath -> IO ()
interpret d schema = do
    source <- readFile d
    let cps = preLex source -- chars and positions
    case jsonL cps of
        Error pos msg -> putStr $ errMsg pos msg source 
        OK (tlps,_) -> do
            putStrLn "----- Data Document ------"
            print tlps -- tags lexemes positions
            case jsonP tlps of
                Error pos msg -> putStr $ errMsg pos msg source
                OK (json,_) -> do
                    print json
                    schemaDoc <- readFile schema
                    let cps' = preLex schemaDoc -- chars and positions
                    case jsonL cps' of
                        Error pos msg -> putStr $ errMsg pos msg schemaDoc 
                        OK (tlps',_) -> do
                            putStrLn "----- Schema Document ------"
                            print tlps' -- tags lexemes positions
                            case jsonP tlps' of
                                Error pos msg -> putStr $ errMsg pos msg schemaDoc
                                OK (jsonSchema,_) -> do
                                    print jsonSchema

verify :: JsonValue -> JsonValue -> IO ()
verify data schema = undefined