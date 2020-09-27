-- Json  Parser

-- use: 2 json cripts - ELAB

module Main (main) where

import System.Environment   
import System.IO
import Data.Char

import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers

import JsonVerifier (verify)
import JsonTypes

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

main :: IO ()
main = undefined
-- main = do
--     args <- getArgs
--     case args of 
--         [json, schema] -> interpret json
--         _      -> error "Two arguments expected.."

run :: IO ()
run = do
    putStrLn "----- Json Data Document ------"
    j <- interpret "data_doc.json"
    putStrLn "----- Json Schema Document ------"
    s <- interpret "schema_doc.json"
    if verify j s
        then putStrLn "----- Verification Successful ------"
        else putStrLn "----- Verification Failed ------"

interpret :: FilePath -> IO JsonValue
interpret doc = do
    source <- readFile doc
    let cps = preLex source -- chars and positions
    putStrLn "----- Chars and Positions ------"
    print cps
    case jsonL cps of
        Error pos msg -> do
            print $ errMsg pos msg source
            return JsonNull
        OK (tlps,_) -> do
            putStrLn "----- Lexemes ------"
            print tlps -- tags lexemes positions
            case jsonP tlps of
                Error pos msg -> do
                    print $ errMsg pos msg source
                    return JsonNull
                OK (json,_) -> do
                    print json
                    return json