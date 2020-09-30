module JsonValidator (validator) where
import Data.List (partition)
import JsonTypes

data JsonSchemaType = 
      SchemaNull 
    | SchemaBool 
    | SchemaInt 
    | SchemaFloat
    | SchemaString 
    | SchemaArray 
    | SchemaObject
    | SchemaCustom String
    deriving (Show, Eq)

type MemberSchemas = [(String, JsonSchema)] -- String is the member (property) name, schema is the type
type CustomSchemas = [(String, JsonSchema)] -- String is the custom type name, schema is the schema for that custom type

-- A schema defining a custom type would look e.g.:
-- JsonSchemaObject 
--   [("name", JsonSchemaValue (SchemaCustom "customNameType"))]
--   [("customNameType", JsonSchemaValue SchemaString)]

data JsonSchema = 
    JsonAll
  | JsonSchemaArray
  | JsonSchemaValue JsonSchemaType
  | JsonSchemaElementArray JsonSchema
  | JsonSchemaObject MemberSchemas CustomSchemas
  deriving (Show, Eq)


compileSchema :: JsonValue -> JsonSchema
-- Empty
compileSchema (JsonObject []) =
    JsonAll
-- Null
compileSchema (JsonObject [JsonMember "\"type\"" (JsonString "\"null\"")])   = 
    JsonSchemaValue SchemaNull
-- String
compileSchema (JsonObject [JsonMember "\"type\"" (JsonString "\"string\"")]) =
    JsonSchemaValue SchemaString
-- Int
compileSchema (JsonObject [JsonMember "\"type\"" (JsonString "\"int\"")])    = 
    JsonSchemaValue SchemaInt
-- Float
compileSchema (JsonObject [JsonMember "\"type\"" (JsonString "\"float\"")])  = 
    JsonSchemaValue SchemaFloat
-- Bool
compileSchema (JsonObject [JsonMember "\"type\"" (JsonString "\"bool\"")])   = 
    JsonSchemaValue SchemaBool
-- Array with element type specification
compileSchema (JsonObject [JsonMember "\"type\"" (JsonString "\"array\""), JsonMember "\"elements\"" e]) =
    JsonSchemaElementArray (compileSchema e)
-- Array
compileSchema (JsonObject [JsonMember "\"type\"" (JsonString "\"array\"")]) =
    JsonSchemaArray
-- Object
compileSchema (JsonObject ((JsonMember "\"type\"" (JsonString "\"object\"")):props) ) =
    let
      (props', schemas) = partition (\(JsonMember name _) -> name /= "\"schemas\"") props
      schemas' = 
        case schemas of
          ((JsonMember _ (JsonObject customSchemaTypes)) : _) -> map compileMember customSchemaTypes
          [] -> []
          _ -> error "Invalid schemas definition value"
    in
      JsonSchemaObject (map compileMember props') schemas'
    where
      compileMember (JsonMember name memberSchema) = (name, compileSchema memberSchema)
-- Custom
compileSchema (JsonObject [JsonMember "\"type\"" (JsonString custom)]) = 
    JsonSchemaValue (SchemaCustom custom)

-- Invalid schema
compileSchema s = error $ "failed to compile - " ++ (show s) ++ "\n*** Invalid json schema."

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

validate :: JsonSchema -> JsonValue -> Bool
validate  JsonAll                     _                = True
validate (JsonSchemaValue SchemaNull) JsonNull         = True
validate (JsonSchemaValue SchemaBool) (JsonBool _)     = True
validate (JsonSchemaValue SchemaInt) (JsonNumber x)    = isInt $ x
validate (JsonSchemaValue SchemaFloat) (JsonNumber x)  = not $ isInt $ x
validate (JsonSchemaValue SchemaString) (JsonString _) = True
validate (JsonSchemaElementArray elementSchema) (JsonArray elements) =
    all (== True) . map (validate elementSchema) $ elements
validate (JsonSchemaArray) (JsonArray _) = True
validate (JsonSchemaObject memberSchemas customTypes) (JsonObject members) = 
    -- An object is validated if all of its members are validated
    --all (== True) . map validateMember $ members
    -- To make it 1:1 (disallow missing)
    all (== True) . map validateMemberSchema $ memberSchemas
    where
      -- To validate a member schema for the presence of the members
      validateMemberSchema (name, memberSchema) = 
        -- look up the member within the member list
        case lookupInMember name members of
          -- If it's there, validate its value against the schema
          Just value -> validateMemberWith memberSchema value customTypes
          -- Missing members not allowed
          Nothing -> False 

      -- To validate a member
      validateMember (JsonMember name value) =
        -- We first need to find a schema for that member
        case lookup name memberSchemas of
          -- If the schema is there, validate the member value against the schema
          Just memberSchema -> validateMemberWith memberSchema value customTypes
          -- Uknown members aren't allowed
          Nothing -> False

      -- Special validate function that only works for members
      -- And makes use of local custom types.

      -- If the member is defined as a custom type,
      validateMemberWith (JsonSchemaValue (SchemaCustom customTypeName)) v customTypes =
        -- try to find the custom type definition
        case lookup customTypeName customTypes of
          -- if it's there, validate the member against the custom type schema
          Just customSchema -> validate customSchema v
          -- If the custom type for the member is unknown, fail validation
          Nothing -> False

      -- If the member is not defined as a custom type, just validate it as any other value
      validateMemberWith memberSchema v _ = validate memberSchema v

validator :: JsonValue -> JsonValue -> Bool
validator dataDoc schemaDoc = validate schema dataDoc
  where schema = compileSchema schemaDoc

-- display :: JsonValue -> IO ()
-- display s = print $ compileSchema s



