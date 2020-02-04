module Api exposing (..)

import Json.Decode as D
import Json.Encode as E
import Http

import Utils

type alias SessionID = String

type Session
  = SignedIn { session : SessionID, username : String }
  | SignedOut

type SessionOrCmd msg
  = Command (Cmd msg)
  | ChangeSession Session

type alias UserInfo =
  { username : String
  , name : String
  , password : String
  , email : String
  , bio : String
  }

-- Authenticate

createUser : UserInfo -> (String -> Result Utils.HttpError SessionID -> msg) -> Cmd msg
createUser info msg =
  Utils.post "create-user" Nothing (msg info.username) (E.object
    [ ("username", E.string info.username)
    , ("name", E.string info.name)
    , ("password", E.string info.password)
    , ("email", E.string info.email)
    ])
    (D.field "session" D.string)

login : String -> String -> (String -> Result Utils.HttpError SessionID -> msg) -> Cmd msg
login username password msg =
  Utils.post "login" Nothing (msg username) (E.object
    [ ("username", E.string username)
    , ("password", E.string password)
    ])
    (D.field "session" D.string)

-- Requires authentication

logout : SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
logout session msg =
  Utils.post "logout" (Just session) msg (E.object []) (D.succeed ())

type alias UserSettingsInfo =
  { name : String
  , bio : String
  , email : String
  }

getSettings : SessionID -> (Result Utils.HttpError UserSettingsInfo -> msg) -> Cmd msg
getSettings session msg =
  Utils.get "user-settings" (Just session) msg <|
    D.map3 UserSettingsInfo
      (D.field "name" D.string)
      (D.field "bio" D.string)
      (D.field "email" D.string)

setSettings : E.Value -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
setSettings changes session msg =
  Utils.post "user-settings" (Just session) msg changes (D.succeed ())

type alias GameInfo =
  { name : String
  , description : String
  , password : String
  }

type alias GameID = String

createGame : GameInfo -> SessionID -> (Result Utils.HttpError GameID -> msg) -> Cmd msg
createGame gameInfo session msg =
  Utils.post "create-game" (Just session) msg (E.object
    [ ("name", E.string gameInfo.name)
    , ("description", E.string gameInfo.description)
    , ("password", E.string gameInfo.password)
    ])
    (D.field "game" D.string)

getGameSettings : GameID -> SessionID -> (Result Utils.HttpError GameInfo -> msg) -> Cmd msg
getGameSettings game session msg =
  Utils.get ("game-settings?game=" ++ game) (Just session) msg <|
    D.map3 GameInfo
      (D.field "name" D.string)
      (D.field "description" D.string)
      (D.field "password" D.string)

setGameSettings : E.Value -> GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
setGameSettings changes game session msg =
  Utils.post ("game-settings?game=" ++ game) (Just session) msg changes (D.succeed ())

join : String -> GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
join password game session msg =
  Utils.post ("join?game=" ++ game) (Just session) msg (E.object [("password", E.string password)]) (D.succeed ())

leave : GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
leave game session msg =
  Utils.post ("leave?game=" ++ game) (Just session) msg (E.object []) (D.succeed ())

kick : String -> GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
kick target game session msg =
  Utils.post ("kick?game=" ++ game) (Just session) msg (E.object [("target", E.string target)]) (D.succeed ())

start : GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
start game session msg =
  Utils.post ("start?game=" ++ game) (Just session) msg (E.object []) (D.succeed ())

shuffle : GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
shuffle game session msg =
  Utils.post ("shuffle?game=" ++ game) (Just session) msg (E.object []) (D.succeed ())

status : GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
status game session msg =
  Utils.post ("status?game=" ++ game) (Just session) msg (E.object []) (D.succeed ())

kill : String -> GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
kill code game session msg =
  Utils.post ("kill?game=" ++ game) (Just session) msg (E.object [("code", E.string code)]) (D.succeed ())

-- Public

-- TODO: API provides more info, but I forgot what
type alias User =
  { name : String
  , bio : String
  }

type alias Game =
  { name : String
  , description : String
  }

getUser : String -> (Result Utils.HttpError User -> msg) -> Cmd msg
getUser user msg =
  Utils.get ("user?user=" ++ user) Nothing msg <|
    D.map2 User
      (D.field "name" D.string)
      (D.field "bio" D.string)

getGame : GameID -> (Result Utils.HttpError Game -> msg) -> Cmd msg
getGame game msg =
  Utils.get ("game?game=" ++ game) Nothing msg <|
    D.map2 Game
      (D.field "name" D.string)
      (D.field "description" D.string)
