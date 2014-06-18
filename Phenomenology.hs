module Phenomenology where

import Chemistry

data Condition = Friend
               | Foe
               | FriendWithFood
               | FoeWithFood
               | Food
               | Rock
               | Marker Marker
               | FoeMarker
               | Home
               | FoeHome
               deriving (Show, Eq, Read)
