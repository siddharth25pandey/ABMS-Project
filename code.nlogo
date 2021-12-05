breed [cows1 cow1]
breed [cows2 cow2]
breed [wolves wolf]
turtles-own [energy runspeed vision repcost]
cows1-own [dosage]
cows2-own [dosage]
patches-own [grass-amount regrowth-rate]

globals [dom_run dom_vis dom_rep l_run l_vis l_rep dayvar]

to-report daytime?
  let t int(ticks / 50)
  report t mod 2 = 0
end

to setup-cows
  create-cows1 initial-cow1
  [
    setxy random-xcor random-ycor
    set color white
    set runspeed cow1-speed
    set dosage cow1-dosage
    set vision cow1-vision
    set repcost cow1-repcost
    set size 2
    set energy 5
  ]
  create-cows2 initial-cow2
  [
    setxy random-xcor random-ycor
    set color black
    set runspeed cow1-speed
    set dosage cow2-dosage
    set vision cow2-vision
    set repcost cow2-repcost
    set size 2
    set energy 5
  ]

end

to setup-wolves
  create-wolves initial-wolves [
    setxy random-xcor random-ycor
    set color yellow
    set size 2
    set repcost wolf-repcost
    set runspeed 2
    set energy 10
    set vision wolf-vision
  ]
end

to color-grass
  ifelse daytime?
  [
  set pcolor scale-color green grass-amount -20 30
  if grass-amount < 2
  [set pcolor brown]
  ]
  [
    set pcolor scale-color blue grass-amount -20 30
    if grass-amount < 2
    [set pcolor black]
  ]
end

to setup-patches
  ask patches [
   set grass-amount random-float 10.0
   set regrowth-rate random-float 2
   color-grass
  ]
end

to setup-var
  set dom_run random 2
  set dom_vis random 2
  set dom_rep random 2
  if dom_run + dom_vis + dom_rep = 0
  [set dom_run 1]
  if dom_run + dom_vis + dom_rep = 3
  [set dom_run 0]
  set dayvar 0
  ;;show dom_run
  ;;show dom_vis
  ;;show dom_rep
end

to setup-list
  set l_vis list cow1-vision cow2-vision
  set l_run list cow1-speed cow2-speed
  set l_rep list cow1-repcost cow2-repcost
end

;; -----------------------------------------------
to-report grass
  let val sum [grass-amount] of patches
  set val val / count patches
  report val
end
;; -----------------------------------------------

to setup
  clear-all
  reset-ticks
  set-default-shape cows1 "cow"
  set-default-shape cows2 "cow"
  set-default-shape wolves "ghost"
  setup-cows
  setup-wolves
  setup-patches
  setup-var
  setup-list
end

to drift
  ask n-of random 20 cows1
  [
    set vision cow2-vision
  ]
  ask n-of random 20 cows2
  [
    set runspeed cow2-speed
  ]
end

to go
  go-white-cows
  go-black-cows
  go-wolves
  recolor-world
  regrow-grass
  tick
end

to recolor-world
  ifelse daytime?
  [
    ask patches
    [
      set pcolor scale-color green grass-amount -20 30
      if grass-amount < 4
      [set pcolor brown]
    ]
    ask cows1
    [
      set vision cow1-vision
      set runspeed cow1-speed
    ]
    ask cows2
    [
      set vision cow2-vision
      set runspeed cow2-speed
    ]
    ask wolves
    [
      set vision wolf-vision
      set runspeed 2
    ]
  ]
  [
    if dayvar = 0
    [
      set dayvar 1
      ask turtles with [breed != wolves]
      [
        set vision vision - 2
        set runspeed runspeed - 1
      ]
      ask wolves
      [
        set vision vision + 2
        set runspeed runspeed + 1
      ]
    ]
    ask patches
    [
      set pcolor scale-color blue grass-amount -20 30
      if grass-amount < 2
      [set pcolor black]
    ]
  ]
end

to go-white-cows
  ask cows1[
    move
    ;;print "moved"
    eat-grass
    ;;print "eaten"
    flee-from-predator
    ;;print "fleed"
    reproduce-cows
    ;;print "reproduced"
    check-death
  ]
end

to go-black-cows
  ask cows2[
    move
    eat-grass
    flee-from-predator
    reproduce-cows
    check-death
  ]
end

to go-wolves
  ask wolves[
    move-w
    eat-cows
    reproduce
    check-death
  ]
end

to move
    rt random 360
    fd random 3
    set energy energy - energy-loss-from-moving
end

to move-w
    rt random 360
    fd random 3
    set energy energy - (energy-loss-from-moving / 2)
end

to eat-cows
  ifelse count my-links = 0
  [
    let myen energy
    ifelse any? turtles with [breed != wolves and energy < myen] in-radius 1
    [
      let str one-of turtles with [breed != wolves] in-radius 1
      ifelse [breed] of str = cows1
      [set energy energy + energy-gain-from-cow1][set energy energy + energy-gain-from-cow2]
      ask str [die]
    ]
    [
      ifelse daytime?
      [
        let target one-of turtles in-radius vision with [breed != wolves and (pcolor != brown or color != brown) and (count my-links = 0 and energy < myen)]
        if target != nobody
        [
          create-link-with target
        ]
      ]
      [
        let target one-of turtles in-radius vision with [breed != wolves and (pcolor != black or color != black ) and (count my-links = 0 and energy < myen)]
        if target != nobody
        [
          create-link-with target
        ]
      ]
    ]
  ]
  [
    let target one-of link-neighbors
    if target != nobody
    [
      face target
      ifelse distance target > vision
      [
        ask my-links with [(end1 = self and end2 = target) or (end2 = self and end1 = target)]
        [die]
      ]
      [
        fd runspeed
        let mypatch patch-here
        if [patch-here] of target = mypatch
        [
          ifelse [breed] of target = cows1
          [set energy energy + energy-gain-from-cow1][set energy energy + energy-gain-from-cow2]
          ask target [die]
        ]
      ]
    ]
  ]
end


to flee-from-predator
  if any? wolves in-radius vision
  [
    let yamraj one-of wolves in-radius vision
    face yamraj
    rt 90
    fd runspeed * (1 - (grass-amount / 10))
  ]
  if count my-links != 0
  [
    let predator one-of link-neighbors
    if distance predator <= vision
    [
      face predator
      rt 180
      fd runspeed * (1 - (grass-amount / 10))
    ]
  ]
end

to eat-grass
      ifelse grass-amount <= dosage
      [
        ;;print "Eating less"
        let v energy-from-grass * (grass-amount / dosage)
        set energy energy + v
        ;;show energy
        set grass-amount  0
      ]
      [
        ;;print "Eating ok"
        set grass-amount grass-amount - dosage
        set energy energy + energy-from-grass
        ;;show energy
      ]
      color-grass

end


to reproduce
  let mate one-of other wolves in-radius 5 with [energy > wolf-repcost]
  if energy > wolf-repcost and mate != nobody
    [
     set energy energy - repcost
     ask mate [set energy energy - repcost]
     hatch 1
      [
        set energy 10
      ]
    ]
end


to reproduce-cows
  let mate turtles in-radius 5 with [breed = cows1 or breed = cows2]
  let v max-one-of mate [energy]
  let e1 -1
  let e2 -1

  if energy > repcost and [energy] of v > [repcost] of v
  [
    set energy (energy - repcost)
    ask v [set energy (energy - repcost)]
    let c1 0
    let c2 0
    let vis item dom_vis l_vis
    let rep item dom_rep l_rep
    let runsp item dom_run l_run
    let vi 0
    let repi 0
    let runi 0

    if [vision] of v = vis
    [
      set c2 c2 + 1
    ]
    if [vision] of self = vis
    [
      set c1 c1 + 1
    ]
    if [vision] of v != vis and [vision] of self != vis
    [
      set vi 1
    ]

    if [runspeed] of v = runsp
    [
      set c2 c2 + 1
    ]
    if [runspeed] of self = runsp
    [
      set c1 c1 + 1
    ]
    if [runspeed] of v != runsp and [runspeed] of self != runsp
    [
      set runi 1
    ]

    if [repcost] of v = rep
    [
      set c2 c2 + 1
    ]
    if [repcost] of self = rep
    [
      set c1 c1 + 1
    ]
    if [repcost] of v != rep and [repcost] of self != rep
    [
      set repi 1
    ]

    ifelse [breed] of v != [breed] of self
    ;; here characteristic allotment would be there
    [
      if random 100 > RII
      [
        ifelse c1 > c2
        [
          hatch 1
          [
            set energy 5
            ifelse vi = 1
            [set vision item (1 - dom_vis) l_vis][set vision item dom_vis l_vis]
            ifelse repi = 1
            [set repcost item (1 - dom_rep) l_rep][set repcost item dom_rep l_rep]
            ifelse runi = 1
            [set runspeed item (1 - dom_run) l_run][set runspeed item dom_run l_run]
            ifelse random 2 = 1
            [set color brown]
            [set color black]
          ]
        ]
        [
          ifelse [breed] of v = cows1
          [
            hatch-cows1 1
            [
              set energy 5
              ifelse vi = 1
              [set vision item (1 - dom_vis) l_vis][set vision item dom_vis l_vis]
              ifelse repi = 1
              [set repcost item (1 - dom_rep) l_rep][set repcost item dom_rep l_rep]
              ifelse runi = 1
              [set runspeed item (1 - dom_run) l_run][set runspeed item dom_run l_run]
              ifelse random 2 = 1
              [set color brown]
              [set color black]
            ]
          ]
          [
            hatch-cows2 1
            [
              set energy 5
              ifelse vi = 1
              [set vision item (1 - dom_vis) l_vis][set vision item dom_vis l_vis]
              ifelse repi = 1
              [set repcost item (1 - dom_rep) l_rep][set repcost item dom_rep l_rep]
              ifelse runi = 1
              [set runspeed item (1 - dom_run) l_run][set runspeed item dom_run l_run]
              ifelse random 2 = 1
              [set color brown]
              [set color black]
            ]
          ]
        ]
      ]
    ]
    ;; if breeds are same
    [
      hatch 1
      [
        set energy 5
        ifelse vi = 1
        [set vision item (1 - dom_vis) l_vis][set vision item dom_vis l_vis]
        ifelse repi = 1
        [set repcost item (1 - dom_rep) l_rep][set repcost item dom_rep l_rep]
        ifelse runi = 1
        [set runspeed item (1 - dom_run) l_run][set runspeed item dom_run l_run]
      ]
    ]

  ]
end


to check-death
   if energy <= 0
   [
    ;;print "died"
    die
   ]
end

to regrow-grass
  ask patches with [grass-amount < 10]
  [
    let v grass-amount + regrowth-rate
    set grass-amount min (list 10 v )
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
10
10
695
563
-1
-1
13.275
1
10
1
1
1
0
1
1
1
-25
25
-20
20
1
1
1
ticks
30.0

BUTTON
824
339
887
372
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
710
47
882
80
initial-cow1
initial-cow1
0
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
901
47
1073
80
initial-cow2
initial-cow2
0
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
1096
45
1269
78
initial-wolves
initial-wolves
0
100
50.0
1
1
NIL
HORIZONTAL

BUTTON
899
339
962
372
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
709
94
883
127
energy-gain-from-cow1
energy-gain-from-cow1
0
10
9.0
1
1
NIL
HORIZONTAL

SLIDER
901
96
1074
129
energy-gain-from-cow2
energy-gain-from-cow2
0
10
8.0
1
1
NIL
HORIZONTAL

SLIDER
1296
43
1470
76
energy-from-grass
energy-from-grass
0
3
1.5
0.1
1
NIL
HORIZONTAL

SLIDER
900
144
1074
177
cow2-repcost
cow2-repcost
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
709
144
884
177
cow1-repcost
cow1-repcost
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
1296
93
1471
126
energy-loss-from-moving
energy-loss-from-moving
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
1095
145
1271
178
wolf-repcost
wolf-repcost
0
10
8.0
1
1
NIL
HORIZONTAL

PLOT
1095
193
1473
417
Population Check
time
Population
0.0
100.0
0.0
100.0
true
true
"" ""
PENS
"Brown Cow" 1.0 0 -2674135 true "" "plot count turtles with [color = brown]"
"Wolf" 1.0 0 -13791810 true "" "plot count wolves"
"Grass" 1.0 0 -13840069 true "" "plot grass"
"Black Cow" 1.0 0 -16777216 true "" "plot count turtles with [color = black]"

SLIDER
1296
146
1473
179
RII
RII
0
100
100.0
1
1
NIL
HORIZONTAL

TEXTBOX
731
24
881
42
Cow subspecies 1 controls
11
0.0
1

TEXTBOX
921
24
1071
42
Cow supspecies 2 controls
11
0.0
1

TEXTBOX
1144
24
1294
42
Wolf controls
11
0.0
1

SLIDER
709
192
884
225
cow1-dosage
cow1-dosage
0
10
5.0
0.2
1
NIL
HORIZONTAL

SLIDER
900
192
1075
225
cow2-dosage
cow2-dosage
0
10
6.2
0.2
1
NIL
HORIZONTAL

SLIDER
709
237
884
270
cow1-speed
cow1-speed
0.5
10
1.5
0.25
1
NIL
HORIZONTAL

SLIDER
900
237
1075
270
cow2-speed
cow2-speed
0.5
10
2.0
0.25
1
NIL
HORIZONTAL

SLIDER
708
281
884
314
cow1-vision
cow1-vision
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
900
281
1075
314
cow2-vision
cow2-vision
0
10
7.0
1
1
NIL
HORIZONTAL

TEXTBOX
1334
23
1484
41
Global-variables
11
0.0
1

SLIDER
1095
97
1272
130
wolf-vision
wolf-vision
0
10
7.0
1
1
NIL
HORIZONTAL

MONITOR
1096
430
1178
475
cows1
count turtles with [color = brown]
0
1
11

MONITOR
1194
430
1276
475
cows2
count turtles with [color = black]
0
1
11

MONITOR
1291
430
1377
475
NIL
count wolves
0
1
11

MONITOR
1393
430
1456
475
NIL
daytime?
17
1
11

BUTTON
735
339
815
372
Mutation
setup-var
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
974
339
1037
372
NIL
drift
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
# OOD

## Purpose
The model’s purpose is to study and observe the impact of reproductive isolation in the population dynamics of an isolated ecoystem. Specifically, it aims to answer how the species of the crossbreed offspring would be decided on the amount of genes inherited from the particular parent species

## Entities, state variables, and scales
Model entities are the square spatial units or habitat cells comprising the landscape, black and white cows with territories. Also an exteranl dynamic agent to make control over the cows population.

## Process overview and scheduling

## Design concepts

## Initialization

## Input data

## Submodels

## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cow skull
false
0
Polygon -7500403 true true 150 90 75 105 60 150 75 210 105 285 195 285 225 210 240 150 225 105
Polygon -16777216 true false 150 150 90 195 90 150
Polygon -16777216 true false 150 150 210 195 210 150
Polygon -16777216 true false 105 285 135 270 150 285 165 270 195 285
Polygon -7500403 true true 240 150 263 143 278 126 287 102 287 79 280 53 273 38 261 25 246 15 227 8 241 26 253 46 258 68 257 96 246 116 229 126
Polygon -7500403 true true 60 150 37 143 22 126 13 102 13 79 20 53 27 38 39 25 54 15 73 8 59 26 47 46 42 68 43 96 54 116 71 126

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

ghost
false
0
Polygon -7500403 true true 30 165 13 164 -2 149 0 135 -2 119 0 105 15 75 30 75 58 104 43 119 43 134 58 134 73 134 88 104 73 44 78 14 103 -1 193 -1 223 29 208 89 208 119 238 134 253 119 240 105 238 89 240 75 255 60 270 60 283 74 300 90 298 104 298 119 300 135 285 135 285 150 268 164 238 179 208 164 208 194 238 209 253 224 268 239 268 269 238 299 178 299 148 284 103 269 58 284 43 299 58 269 103 254 148 254 193 254 163 239 118 209 88 179 73 179 58 164
Line -16777216 false 189 253 215 253
Circle -16777216 true false 102 30 30
Polygon -16777216 true false 165 105 135 105 120 120 105 105 135 75 165 75 195 105 180 120
Circle -16777216 true false 160 30 30

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

moose
false
0
Polygon -7500403 true true 196 228 198 297 180 297 178 244 166 213 136 213 106 213 79 227 73 259 50 257 49 229 38 197 26 168 26 137 46 120 101 122 147 102 181 111 217 121 256 136 294 151 286 169 256 169 241 198 211 188
Polygon -7500403 true true 74 258 87 299 63 297 49 256
Polygon -7500403 true true 25 135 15 186 10 200 23 217 25 188 35 141
Polygon -7500403 true true 270 150 253 100 231 94 213 100 208 135
Polygon -7500403 true true 225 120 204 66 207 29 185 56 178 27 171 59 150 45 165 90
Polygon -7500403 true true 225 120 249 61 241 31 265 56 272 27 280 59 300 45 285 90

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

rabbit
false
0
Polygon -7500403 true true 61 150 76 180 91 195 103 214 91 240 76 255 61 270 76 270 106 255 132 209 151 210 181 210 211 240 196 255 181 255 166 247 151 255 166 270 211 270 241 255 240 210 270 225 285 165 256 135 226 105 166 90 91 105
Polygon -7500403 true true 75 164 94 104 70 82 45 89 19 104 4 149 19 164 37 162 59 153
Polygon -7500403 true true 64 98 96 87 138 26 130 15 97 36 54 86
Polygon -7500403 true true 49 89 57 47 78 4 89 20 70 88
Circle -16777216 true false 37 103 16
Line -16777216 false 44 150 104 150
Line -16777216 false 39 158 84 175
Line -16777216 false 29 159 57 195
Polygon -5825686 true false 0 150 15 165 15 150
Polygon -5825686 true false 76 90 97 47 130 32
Line -16777216 false 180 210 165 180
Line -16777216 false 165 180 180 165
Line -16777216 false 180 165 225 165
Line -16777216 false 180 210 210 240

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

wolf 6
false
0
Polygon -7500403 true true 105 75 105 45 45 0 30 45 45 60 60 90
Polygon -7500403 true true 45 165 30 135 45 120 15 105 60 75 105 60 180 60 240 75 285 105 255 120 270 135 255 165 270 180 255 195 255 210 240 195 195 225 210 255 180 300 120 300 90 255 105 225 60 195 45 210 45 195 30 180
Polygon -16777216 true false 120 300 135 285 120 270 120 255 180 255 180 270 165 285 180 300
Polygon -7500403 true true 195 75 195 45 255 0 270 45 255 60 240 90
Polygon -16777216 true false 225 75 210 60 210 45 255 15 255 45 225 60
Polygon -16777216 true false 75 75 90 60 90 45 45 15 45 45 75 60
Circle -16777216 true false 88 118 32
Circle -16777216 true false 178 118 32

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
