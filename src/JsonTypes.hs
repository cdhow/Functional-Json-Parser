module JsonTypes where

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
