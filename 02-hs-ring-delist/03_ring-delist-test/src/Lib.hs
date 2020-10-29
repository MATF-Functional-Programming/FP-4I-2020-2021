module Lib ( fromList
           , toList
           , focusNext
           , focusPrev
           , closeWindow
           , openWindow
           ) where


data Ring t = MkRing [t] [t]


fromList :: [t] -> Ring t
fromList xs = MkRing xs []


toList :: Ring t -> [t]
toList (MkRing ls rs) = ls ++ reverse rs


focusNext :: Ring t -> Ring t
focusNext (MkRing [] [])     = MkRing [] []
focusNext (MkRing [l] [])    = MkRing [l] []
focusNext (MkRing [l] rs)    = MkRing (reverse rs) [l]
focusNext (MkRing (l:ls) rs) = MkRing ls (l:rs)


focusPrev :: Ring t -> Ring t
focusPrev (MkRing [] [])    = MkRing [] []
focusPrev (MkRing ls [])    = focusPrev $ MkRing [] (reverse ls)
focusPrev (MkRing ls (r:rs)) = MkRing (r:ls) rs


closeWindow :: Ring t -> Ring t
closeWindow (MkRing [] [])     = MkRing [] []
closeWindow (MkRing [w] rs)    = MkRing (reverse rs) []
closeWindow (MkRing (l:ls) rs) = MkRing ls rs


openWindow :: t -> Ring t -> Ring t
openWindow w (MkRing ls rs) = MkRing (w:ls) rs


