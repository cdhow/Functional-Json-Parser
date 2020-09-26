module JsonVerifier (verify) where
import JsonTypes


-- JsonSchemaType
data JsonSchemaType = 
      SchemaNull 
    | SchemaBool 
    | SchemaInt 
    | SchemaFloat
    | SchemaString 
    | SchemaArray 
    | SchemaObject 
    deriving (Show, Eq)

-- JsonSchema
data JsonSchema = 
    JsonAll
  | JsonSchemaValue JsonSchemaType
  | JsonSchemaArray JsonSchema
  | JsonSchemaObject [(String, JsonSchema)]
  deriving (Show, Eq)

compileSchema :: JsonValue -> JsonSchema
compileSchema (JsonObject [JsonMember "\"type\"" (JsonString "\"null\"")] ) = JsonSchemaValue SchemaNull
compileSchema (JsonObject [JsonMember "\"type\"" (JsonString "\"string\"")] ) = JsonSchemaValue SchemaString
compileSchema (JsonObject [JsonMember "\"type\"" (JsonString "\"int\"")] ) = JsonSchemaValue SchemaInt
compileSchema (JsonObject [JsonMember "\"type\"" (JsonString "\"bool\"")] ) = JsonSchemaValue SchemaBool
compileSchema (JsonObject [JsonMember "\"type\"" (JsonString "\"array\""), JsonMember "\"element\"" e]) =
    JsonSchemaArray (compileSchema e)
compileSchema (JsonObject ((JsonMember "\"type\"" (JsonString "\"object\"")):props) ) =
    JsonSchemaObject . map (\(JsonMember name memberSchema) -> (name, compileSchema memberSchema) ) $ props
compileSchema s = error $ "failed to compile - " ++ show s

validate :: JsonSchema -> JsonValue -> Bool
validate (JsonSchemaValue SchemaNull) JsonNull = True
validate (JsonSchemaValue SchemaBool) (JsonBool _) = True
validate (JsonSchemaValue SchemaInt) (JsonNumber _) = True -- actually validate x for being an integral
validate (JsonSchemaValue SchemaString) (JsonString _) = True
validate (JsonSchemaArray elementSchema) (JsonArray elements) = all (== True) . map (validate elementSchema) $ elements
validate (JsonSchemaObject memberSchemas) (JsonObject members) = 
    all (== True) . map validateMember $ members
    where
      validateMember (JsonMember name value) =
        case lookup name memberSchemas of
          Just memberSchema -> validate memberSchema value
          Nothing -> False

validate _ _ = error "not implemented"


verify :: JsonValue -> JsonValue -> Bool
verify dataDoc schemaDoc = validate schema dataDoc
  where schema = compileSchema schemaDoc