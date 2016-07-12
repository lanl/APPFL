module Test where

import AppflPrelude


data C = C Int Bool

c i b =
  let f = C i in
    case i of
      2 -> f True
      _ -> f False


main = 3239487129834912834710293847
