module Phenomenology where

data Condition = Friend
               | Foe
               | FriendWithFood
               | FoeWithFood
               | Food
               | Rock
               | Marker Int
               | FoeMarker
               | Home
               | FoeHome
               deriving (Show, Eq, Read)
