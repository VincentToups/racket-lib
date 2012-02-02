#lang racket

(require 
 (planet schematics/schemeunit:3)
 pure-lands/utilities/dict-state-monad
 pure-lands/utilities/partial-application
 pure-lands/chapter-01/agrilex/state-commands
 pure-lands/chapter-01/agrilex/parameters
 pure-lands/chapter-01/agrilex/initial-state
 pure-lands/chapter-01/agrilex/randomness
 pure-lands/chapter-01/agrilex/field-commands)

((build (create-plot "Test Plot")
	   (get* 'field "Test Plot"))
 initial-state)

((build
  (create-plot "Test Plot")
  (set 'weather 'raining)
  handle-weather
  (get-plot "Test Plot"))
 initial-state)



