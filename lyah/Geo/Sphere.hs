-- from learn you a haskell ch 7

module Geo.Sphere  
( volume  
, area  
) where  
  
volume :: Float -> Float  
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
  
area :: Float -> Float  
area radius = 4 * pi * (radius ^ 2)  
