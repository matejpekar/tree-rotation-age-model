globals [
  ;; constants
  π
  eps
  tan-30

  ;; variables
  max-height
  logging-period
  max-logging-percentage

  ;; statistics
  logging-volume         ;; m^3
  logging-percentage
  logging-dead
  logging-unsuitable
  profit
  average-tree-profit
]

breed [trees tree]

trees-own [
  age        ;; years
  diameter   ;; diameter at breast height (meters)
  height     ;; meters
  dead?      ;; bool
]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all

  set π (1 + sqrt 5) / 2
  set eps 0.000001
  set tan-30 (sqrt 3) / 3


  ; Set the size of each patch to represent 1 square meter
  let patch-size-meters tree-distance
  ; Calculate the number of patches in each dimension for 1 ha
  let area 900 ;; m2
  let num-patches round(sqrt (area) / patch-size-meters) - 1
  resize-world 0 num-patches 0 num-patches

  ;; visuals
  set-patch-size 18 * tree-distance ;; the visual size of the patches
  set-default-shape turtles "circle"
  ask patches [ set pcolor brown ] ;; brown background


  set max-height 40
  set logging-period 10
  set max-logging-percentage 0.33

  plant-trees
  draw-trees
  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TICK STEP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  grow-trees
  ask trees with [not dead?] [
    if random-float 1.0 < tree-mortality-probability [
      set dead? true
    ]
  ]

  if ticks mod logging-period = 0 [
    logging
    plant-trees
  ]

  draw-trees
;  print [stand-basal-area (max-height * tan-30)] of one-of trees
  tick
end

to grow-trees
  ask trees [
    if not dead? [
      set age age + 1
      set height height + height-growth
      set diameter diameter + diameter-growth
    ]
  ]
end

to chop-tree
  set logging-volume logging-volume + tree-wood-volume
  ifelse diameter >= 0.1 [
    set profit profit + (tree-wood-volume * wood-price)
  ] [
    set logging-unsuitable logging-unsuitable + 1
  ]
  die
end

to logging
  let chopped-trees 0
  let max-trees-to-cut round (max-logging-percentage * (count trees))

  set logging-volume 0
  set profit 0

  set logging-dead 0
  set logging-unsuitable 0

  ;; first dead trees
  let dead-trees trees with [dead?]
  ask up-to-n-of max-trees-to-cut dead-trees [
    set chopped-trees chopped-trees + 1
    set logging-dead logging-dead + 1
    chop-tree
  ]

  if max-trees-to-cut - chopped-trees > 0 [
    ;; mature trees
    let mature-trees trees with [age >= rotation-age and not dead?]
    ask up-to-n-of (max-trees-to-cut - chopped-trees) mature-trees [
      set chopped-trees chopped-trees + 1
      chop-tree
    ]
  ]


  set logging-percentage chopped-trees / (count patches)
  if chopped-trees != 0 [
    set average-tree-profit profit / chopped-trees - seedling-price
    set logging-dead logging-dead / chopped-trees
    set logging-unsuitable logging-unsuitable / chopped-trees
  ]
end


;; https://www.pavelburda.cz/wp-content/uploads/2023/01/sazenice-jaro-2023.pdf
to plant-trees
  ask patches with [not any? turtles-here] [
      sprout-trees 1 [
        set age 3
        set diameter 0.005
        set height 0.26 + random-float 0.09
        set dead? false
        set profit profit - seedling-price
      ]
   ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROWTH MODELS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://user.mendelu.cz/drapela/Dendrometrie/Lesnicke_tabulky/Taxacni%20tabulky/smrk.jpg
to-report height-growth
  let K 40.641708
  let N 5.3923839
  let r 0.0488635

  let heigth-at-age K * N / ((K - N) * exp(- r * age) + N)
  let height-growth-potential heigth-at-age * r * (1 - heigth-at-age / K)

  report height-growth-potential * competition-height-growth-effect
end

to-report diameter-growth
  ;; https://acta.mendelu.cz/pdfs/acu/2004/04/06.pdf
  let average-ring-size 0.0025 ;; meters
  let diameter-growth-potential 2 * average-ring-size

  report diameter-growth-potential * competition-diameter-growth-effect
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MORTALITY MODELS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; annual probability of mortality
;; https://www.cifor.org/publications/pdf_files/articles/ASunderland2003.pdf
;; copetition doesn't influence mortality
to-report tree-mortality-probability
  let neighbor-mortality-percentage count (turtles-on neighbors) with [dead?] / 8

  let competition-mortality exp(0.01 * neighbor-mortality-percentage) - 1
  let size-mortality 1 - gaussian-function diameter 0.25 2
  report competition-mortality + size-mortality
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPETITION MODELS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report tree-basal-area
  report circle-area (diameter / 2)
end

to-report stand-basal-area [radius]
  report sum [tree-basal-area] of trees in-radius (radius / tree-distance)
end

to-report competition-height-growth-effect
  report gaussian-function (stand-basal-area ((max-height - height)  * tan-30)) 1.2 1
end

to-report competition-diameter-growth-effect
  report gaussian-function (stand-basal-area (max-height * tan-30)) 0 1.5
end

to-report gaussian-function [x m w]
  report exp(- (((x - m) ^ 2) / w)) ;; Gaussian function
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YIELD MODELS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://bg.copernicus.org/articles/11/6711/2014/bg-11-6711-2014.pdf
to-report tree-wood-volume
  report (π / 8) * (diameter ^ 2) * height
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to draw-trees
  ask trees [
    ifelse dead? [
      set color red
    ] [
      set color scale-color green age rotation-age 0 ; map the age to a green color
    ]

    let crown-diameter sqrt(tree-crown-area / π) * 2
    set size crown-diameter
  ]
end

;; projected crown area
;; https://bg.copernicus.org/articles/11/6711/2014/bg-11-6711-2014.pdf
to-report tree-crown-area
  report 0.2 * π * diameter * height
end

to-report circle-area [radius]
  report π * (radius ^ 2)
end
@#$#@#$#@
GRAPHICS-WINDOW
224
17
784
578
-1
-1
39.6
1
10
1
1
1
0
1
1
1
0
13
0
13
1
1
1
years
10.0

BUTTON
92
369
161
405
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
9
369
79
405
setup
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
14
33
186
66
rotation-age
rotation-age
1.0
150.0
150.0
1.0
1
years
HORIZONTAL

PLOT
1195
26
1395
176
Logging volume
years
NIL
0.0
100.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot logging-volume"

PLOT
846
384
1143
543
Tree age
years
age
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"average" 1.0 0 -16777216 true "" "plot mean [age] of trees"
"max" 1.0 0 -2674135 true "" "plot max [age] of trees"

PLOT
1192
208
1423
379
Average tree wood volume
years
volume (m^3)
0.0
10.0
0.0
0.1
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [tree-wood-volume] of trees"

SLIDER
14
71
186
104
tree-distance
tree-distance
1
5
2.2
0.1
1
meters
HORIZONTAL

PLOT
842
24
1147
179
Tree diameter
years
cm
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"average" 1.0 0 -16777216 true "" "plot mean [diameter * 100] of trees"
"max" 1.0 0 -2674135 true "" "plot max [diameter * 100] of trees"

PLOT
845
200
1141
363
Tree hight
years
m
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"mean" 1.0 0 -16777216 true "" "plot mean [height] of trees"
"max" 1.0 0 -2674135 true "" "plot max [height] of trees"

INPUTBOX
12
136
161
196
wood-price
500.0
1
0
Number

INPUTBOX
10
205
159
265
seedling-price
10.0
1
0
Number

PLOT
1158
388
1375
548
Profit
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot profit"

PLOT
1247
564
1447
714
Average profit per tree
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 10.0 0 -16777216 true "" "if ticks mod logging-period = 0 [\nplot average-tree-profit\n]"

PLOT
846
574
1224
724
Logging percetage
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"percentage of chopped trees" 1.0 0 -16777216 true "" "plot logging-percentage"
"dead trees" 1.0 0 -2674135 true "" "plot logging-dead"
"unsuitable trees" 1.0 0 -13791810 true "" "plot logging-unsuitable"

@#$#@#$#@
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
set density 60.0
setup
repeat 180 [ go ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="final-parameter-search" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>profit</metric>
    <metric>logging-volume</metric>
    <metric>average-tree-profit</metric>
    <metric>logging-percentage</metric>
    <metric>logging-dead</metric>
    <metric>logging-unsuitable</metric>
    <metric>mean [age] of trees</metric>
    <metric>max [age] of trees</metric>
    <metric>mean [diameter] of trees</metric>
    <metric>max [diameter] of trees</metric>
    <metric>mean [height] of trees</metric>
    <metric>max [height] of trees</metric>
    <enumeratedValueSet variable="rotation-age">
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
      <value value="110"/>
      <value value="120"/>
      <value value="130"/>
      <value value="140"/>
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tree-distance">
      <value value="1"/>
      <value value="1.25"/>
      <value value="1.5"/>
      <value value="1.75"/>
      <value value="2"/>
      <value value="2.25"/>
      <value value="2.5"/>
      <value value="2.75"/>
      <value value="3"/>
      <value value="3.25"/>
      <value value="3.5"/>
      <value value="3.75"/>
      <value value="4"/>
      <value value="4.25"/>
      <value value="4.5"/>
      <value value="4.75"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wood-price">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seedling-price">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
