module Api exposing (..)

import Json.Decode as D
import Json.Encode as E

import Utils

type Session
  = SignedIn { session : String, username : String }
  | SignedOut

type alias UserInfo =
  { username : String
  , name : String
  , password : String
  , email : String
  , bio : String
  }

createUser : UserInfo -> Utils.Msg String msg -> Cmd msg
createUser info msg =
  Utils.post "create-user" Nothing msg (E.object
    [ ("username", E.string info.username)
    , ("name", E.string info.name)
    , ("password", E.string info.password)
    , ("email", E.string info.email)
    ])
    (D.field "session" D.string)

login : String -> String -> Utils.Msg String msg -> Cmd msg
login username password msg =
  Utils.post "login" Nothing msg (E.object
    [ ("username", E.string username)
    , ("password", E.string password)
    ])
    (D.field "session" D.string)
