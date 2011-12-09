#lang racket

(require 
 pure-lands/utilities/dict-state-monad
 pure-lands/utilities/partial-application
 pure-lands/chapter-01/agrilex/state-commands
 pure-lands/chapter-01/agrilex/parameters
 pure-lands/chapter-01/agrilex/initial-state
 pure-lands/chapter-01/agrilex/randomness
 pure-lands/chapter-01/agrilex/field-commands)

((create-plot "Test Plot") initial-state)

((build
  (create-plot "Test Plot")
  (set 'weather 'rainy)
  (handle-weather)
  )
 initial-state)