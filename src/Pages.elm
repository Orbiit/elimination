module Pages exposing (..)

import Utils.Request as Request

type Page
  = FrontPage
  | Terms
  | Privacy
  | About
  | User
  | Game
  | UserSettings
  | GameSettings
  | Loading
  | Error Request.HttpError
