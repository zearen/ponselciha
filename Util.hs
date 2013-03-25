{-
 - Zachary Weaver <zearen.wover@gmail.com> 2013
 - Util.hs
 -
 - Various utility functions and operators that I enjoy
 -}

module Util
    ( (><)
    , (.:)
    , (??)
    ) where

(><) :: a -> b -> (a, b)
a >< b = (a, b)
infix 1 ><

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)
infixr 9 .:

(??) :: a -> a -> Bool -> a
(l ?? r) b = if b then l else r
infix 1 ?? 
