module JsonTypes where

-- JsonMember
data JsonMember = JsonMember String JsonValue
    deriving (Show)

-- Added because the validator requires a lookup for type JsonMember.
lookupInMember :: String -> [JsonMember] -> Maybe JsonValue
lookupInMember n ms = lookup n ms'
  where
    ms' = map toTuple ms
    toTuple (JsonMember n v) = (n,v)

-- JsonValue
data JsonValue =
      JsonError
    | JsonNull
    | JsonBool Bool
    | JsonNumber Double
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [JsonMember]
    deriving (Show)
