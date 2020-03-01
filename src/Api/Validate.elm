module Api.Validate exposing (..)

import Regex
import Email

import Utils

username = Utils.makeRegex "^[a-zA-Z0-9_-]{3,20}$"

usernameLabel = "Only letters, digits, underscores, and hyphens are allowed. This cannot be changed later."

usernameOk : String -> Maybe String
usernameOk string =
  if String.length string > 20 then
    Just "Username cannot be over 20 characters long."
  else if Regex.contains username string then
    Nothing
  else
    Just "Usernames must be at least three characters long and may only contain lowercase letters, digits, underscores, and hyphens."

nameLabel = "Lets others be able to find and eliminate you, which makes the game fair."

nameOk : String -> Maybe String
nameOk string =
  if String.length string > 50 then
    Just "Names cannot be over 50 characters long. Sorry if your name is actually over 50 characters long!"
  else if String.isEmpty string then
    Just "You need to have a name."
  else
    Nothing

passwordLabel = "Must be at least 6 characters long. We recommend using a memorable sentence, such as \"Do not use example passwords.\""

passwordOk : String -> Maybe String
passwordOk string =
  if string == "Do not use example passwords." then
    Just "Yeah, don't."
  else if String.length string > 200 then
    Just "Passwords can't be over 200 characters."
  else if String.length string < 6 then
    Just "Password too short. It must be at least six characters long."
  else
    Nothing

emailLabel = "For password reset forms. You can also turn on email notifications if you want."

emailOk : String -> Maybe String
emailOk string =
  if String.length string > 320 then
    Just "Emails can't be over 320 characters. (Some article I read says it actually can't be over 254 characters, so...)"
  else if Email.isValid string then
    Nothing
  else
    Just "That does not look like an email."
