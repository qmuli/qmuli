module Qi.Core.Curry where

import           Protolude


-- | Curry `f x y = g $ h x y` to `f = g .: h`
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- | Curry `f x y z = g $ h x y z` to `f = g .:: h`
(.::) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.::) = (.:) . (.)

-- | Curry `f x y z z' = g $ h x y z z'` to `f = g .::: h`
(.:::) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.:::) = (.::) . (.)

(.::::) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
(.::::) = (.:::) . (.)

(.:::::) :: (g -> h) -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> h
(.:::::) = (.::::) . (.)

(.::::::)  :: (h -> i) -> (a -> b -> c -> d -> e -> f -> g -> h) -> a -> b -> c -> d -> e -> f -> g -> i
(.::::::) = (.:::::) . (.)
