extensions [

]

globals [
  Labor
  Total_Capital
  k
  GDP
  y

  Return_of_TotalCapital
  Return_of_UnitCaptial

  sd_saving_rate
  mean_Saving_Rate

  sd_sup_rate
  mean_sup_Rate

  mean_c1
  mean_c2

  u

  A
]

;;
__includes[
  "./libs/ProbOffspring.nls"
  "./libs/ConsumptionTheory.nls"
]

turtles-own[
  cohort
  age
  wage
  asset

  saving_rate
  sup_rate

  father
  children_list

  c1
  c2
  utility

  rho
  theta
]

to go-once
  production

  let pre_asset sum [asset] of turtles with [age = 1]

  allocation

  ; checkAlloc pre_asset

  if do-sup [

    oldAgeSup
  ]

  oldConsumptionAndUtility

  youngAdj

  youngSaving


  updateCounter

  tick

  aging
  ;; aging过后，年龄层次为(age = 1, age = 2)
  ;; 其中age = 2的人数为num_pop，age = 1的人数为num_pop * (1 + 人口出身率)

  ;; 最老一代人死亡(age = 2)
  die_too_old

  ;; 此时把age=1的人口数量修正为num_pop
  popCtrl

  raisingChildren

  updateWorld

  if (count turtles) < 10 [stop]

end

to oldAgeSup
  ask turtles with [age = 0][
    let sup wage * sup_rate
    set wage wage - sup
    ask father [
      set asset asset + sup
    ]

  ]
end

to updateWorld
  set A  A * (1 + tech_growth_rate)
end

to die_too_old
  ask turtles [
    if (age >= 2) [die]
  ]
end

to popCtrl
  let cc count turtles with [age = 1]
  if(cc > num_pop ) [
    ask n-of (cc - num_pop) turtles [die]
  ]
end

to oldConsumptionAndUtility
  ask turtles with [age = 1][

    set c2  asset

    set utility (oldUntilityFun c1 c2 rho theta)

  ]
end

to youngAdj
  let r Return_of_UnitCaptial

  ask turtles with [age = 0][
    if method = "Rational" [
       set saving_rate (calSavingRate r theta rho)
    ]

    if method = "Imitate" [
       if(random-float 1) < learning_rate [

        let target nobody
        if learn-from = "best" [
          set target max-one-of turtles with [age = 1] [utility]
        ]

        if learn-from = "any" [
          set target one-of turtles with [age = 1]
        ]

        set saving_rate [saving_rate] of target
        set sup_rate [sup_rate] of target
      ]
    ]

    if method = "Adjust" [
       ;; use learning
      if(random-float 1) < 1 [
        let target father

        if target != nobody [
            ;;if ([unity] of father) < mid_unity_2 [
          let _c1 [c1] of target
          let _c2 [c2] of target

          if (_c1 * (Return_of_UnitCaptial / (1 + rho))) >  (_c2 * 1) and saving_rate < (1 - step)  [
            set saving_rate saving_rate + step
            ;;print saving_rate
          ]

          if (_c1 * (Return_of_UnitCaptial / (1 + rho))) <  (_c2 * 1) and saving_rate > step  [
            set saving_rate saving_rate - step
          ]
        ]
      ]
    ]
  ]

end

to youngSaving
  ask turtles with [age = 0][
    set asset saving_rate * wage
    set c1  (wage - asset)
  ]
end

to updateCounter

  set mean_Saving_Rate mean [saving_rate] of turtles with [age = 0]
  set sd_saving_rate standard-deviation [saving_rate] of turtles with [age = 0]

  set mean_sup_Rate mean [sup_rate] of turtles with [age = 0]
  set sd_sup_rate standard-deviation [sup_rate] of turtles with [age = 0]



  set mean_c1 mean [c1] of turtles with [age = 1]
  set mean_c2 mean [c2] of turtles with [age = 1]

  set u mean [utility] of turtles with [age = 1]



end

to-report calSavingRate [_r _theta  _rho]
  ;; 这个函数依赖于同质性 + 完全理性假定

  let part1 (1 + _r) ^ ((1 - _theta ) / _theta )
  let part2 (1 + _rho) ^ (1 / _theta )

  let saveing_rate part1 / ( part1 + part2)

  report saveing_rate
end

to checkAlloc [pa]
  ;; 因为是完全折旧，生产和分配过后，老人手中的资本都是：“新生产出来的”
  ;; 因此存量资产和工资之和，应该等于gdp，而不是增量资产和工资的和

  let tw sum [wage] of turtles with [age = 0]
  ;; let ta (sum [asset] of turtles with [age = 1]) - pa
  let ta (sum [asset] of turtles with [age = 1])

  if(precision (tw + ta) 2 !=  (precision GDP 2))[
    print ( word "wage " precision  tw  2 )
    print ( word "new captial " precision  ta  2 )
    print ( word "total " precision  (tw + ta)  2 )
    print ( word "gdp " precision  GDP  2 )
  ]
end

to production
  ifelse production_fun = "cd" [
    production_cd
  ][
    production_h
  ]

end

to production_cd
  ;;CD生产函数

  set Labor count turtles with [age = 0]
  set Total_Capital sum [asset] of turtles with [age = 1] ; K = sum(s)
  set k Total_Capital / (A * Labor) ; k = K/(AL)

  set GDP  (Total_Capital ^ alpha) * (A * Labor)^ ( 1 - alpha)
  set y GDP / (A * Labor)
end

to production_h
  ;;人力生产函数

  set Labor count turtles with [age = 0]
  set Total_Capital sum [asset] of turtles with [age = 1] ; K = sum(s)
  set k Total_Capital / (A * Labor) ; k = K/(AL)

  set GDP  A * Labor
  set y GDP / (A * Labor)
end

to allocation
  ;; 资本的总报酬是中产出中的alpha比例
  set Return_of_TotalCapital alpha * GDP

  ;; 那么每单位的资本报酬就是:

  ifelse production_fun = "cd" [
    set Return_of_UnitCaptial Return_of_TotalCapital / Total_Capital
  ][
    set Return_of_UnitCaptial 0 ;;注意，设置为0，表示不存在可以长期储存价值的手段。什么东西存40年都没了。
  ]


  ask turtles with [age = 1][
    set asset Return_of_UnitCaptial * asset

  ]

  let Total_Wage (GDP - Return_of_TotalCapital)

  ;; 有效工资率是单位有效人力可以获得的工资
  let Effect_Wage Total_Wage / (A * Labor)

  ;; 分配下来
  ask turtles with [age = 0][
    set wage A * Effect_Wage
  ]


end

to setup
  clear-all

  setupPop

  checkPop

  setupWorld

  reset-ticks
end

to setupWorld
  set A init_A
end

to setupPop

  ;; old
  crt num_pop [
    p.init
    set asset 0.001
    set c1 0.001

    set cohort -1

    t.setupVars
  ]

  aging

  raisingChildren

end

to t.setupVars
  set saving_rate random-float 1
  set sup_rate random-float 1
end

to checkPop
  ask turtles with [age = 1][
    ask children_list [
      if father != myself [
        print "pop errpr"
      ]
    ]
  ]
end

to p.init
  carefully [set cohort ticks]
  [set cohort 0]

  set age 0
  set wage 0
  set asset 0

  set theta init_theta
  set rho init_rho
end

to raisingChildren
  ask turtles with [age = 1][
    ; the n should be he best chose
    hatch (randomInt (1 + reproduction_rate)) [
      p.init
      set father myself
      ;if (useLearning?)[
        t.mutation
      ;]
    ]

    set children_list turtles with [father = myself]
  ]
end

to t.mutation
  if(random-float 1) < mutation_rate [
    set saving_rate random-float 1

  ]
  if(random-float 1) < mutation_rate [
    set sup_rate random-float 1

  ]
end

to aging
  ask turtles [
    set age age + 1
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
255
10
398
154
-1
-1
4.1
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
5
10
129
44
setup (normal)
let seed new-seed\n;;show word \"new-seed : \" seed \nset rs seed\nrandom-seed seed\nsetup
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
165
10
245
44
go
go-once
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
165
165
245
199
NIL
go-once
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
165
45
245
80
go 100
reset-timer \nrepeat 100 [go-once]\nshow word \"Time used: \" timer
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
5
80
130
140
rs
1.019976101E9
1
0
Number

BUTTON
5
45
140
78
setup (use seed)
show word \"use seed : \" rs \nrandom-seed rs\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
280
35
362
80
population
count turtles
17
1
11

BUTTON
165
80
245
113
go 300
reset-timer \nrepeat 300 [go-once]\nshow word \"Time used: \" timer
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
5
140
80
200
num_pop
500.0
1
0
Number

PLOT
405
10
670
130
ln Population
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"all" 1.0 0 -16777216 true "" "plot ln count turtles"
"a0" 1.0 0 -10899396 true "" "plot ln count turtles with [age = 0]"
"a1" 1.0 0 -13840069 true "" "plot ln count turtles with [age = 1]"
"a2" 1.0 0 -14835848 true "" "plot ln count turtles with [age = 2]"
"a3" 1.0 0 -11221820 true "" "plot count turtles with [age = 3]"

PLOT
670
10
840
130
Hist of Age
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
"default" 1.0 1 -16777216 true "" "histogram [age] of  turtles"

SLIDER
130
265
302
298
reproduction_rate
reproduction_rate
0
5
0.0
0.1
1
NIL
HORIZONTAL

PLOT
405
130
640
250
ln wage
NIL
NIL
0.0
10.0
0.0
0.01
true
false
"" ""
PENS
"wage" 1.0 0 -16777216 true "" "plot ln (mean ([wage] of turtles with [age = 1]))"

INPUTBOX
0
320
60
380
init_theta
1.0
1
0
Number

INPUTBOX
65
320
120
380
init_rho
0.0
1
0
Number

SLIDER
130
300
302
333
tech_growth_rate
tech_growth_rate
0
0.1
0.0
0.005
1
NIL
HORIZONTAL

PLOT
405
250
640
370
ln Tech
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"A" 1.0 0 -16777216 true "" "plot ln A"

PLOT
640
130
840
250
ln Captial
NIL
NIL
0.0
10.0
0.0
0.01
true
false
"" ""
PENS
"Captial" 1.0 0 -16777216 true "" "plot ln Total_Capital"

BUTTON
400
495
687
528
NIL
inspect one-of turtles with [age = 0]\n
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
400
530
687
563
NIL
inspect one-of turtles with [age = 1]\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
65
380
120
440
Alpha
0.4
1
0
Number

PLOT
640
250
840
370
ln GDP
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ln GDP"

PLOT
860
55
1060
205
R
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot min list Return_of_UnitCaptial 5"

BUTTON
165
130
245
163
go 15
reset-timer \nrepeat 15 [go-once]\nshow word \"Time used: \" timer
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
405
370
640
490
k
NIL
NIL
0.0
10.0
0.0
0.01
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot k"

MONITOR
360
395
417
440
k
precision k 4
17
1
11

PLOT
640
370
840
490
y
NIL
NIL
0.0
10.0
0.0
0.05
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot y"

PLOT
860
205
1025
325
saving_rate
NIL
NIL
0.0
10.0
0.0
0.5
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean_Saving_Rate"
"pen-1" 1.0 0 -7500403 true "" "plot mean_Saving_Rate + sd_saving_rate"
"pen-2" 1.0 0 -2674135 true "" "plot mean_Saving_Rate - sd_saving_rate"
"pen-3" 1.0 0 -955883 true "" "plot 0"

TEXTBOX
255
170
405
188
8代左右到均衡
12
0.0
1

SWITCH
5
205
117
238
fixPop?
fixPop?
0
1
-1000

PLOT
860
335
1060
485
u
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ln u"

MONITOR
880
10
1037
55
Return_of_UnitCaptial
precision  Return_of_UnitCaptial 2
17
1
11

INPUTBOX
170
490
322
550
learning_rate
0.05
1
0
Number

MONITOR
1060
335
1117
380
u
precision u 2
17
1
11

INPUTBOX
15
490
167
550
mutation_rate
0.001
1
0
Number

MONITOR
1035
240
1122
285
saving_rate
precision mean [saving_rate] of turtles 5
17
1
11

CHOOSER
140
365
278
410
method
method
"Rational" "Adjust" "Imitate"
2

PLOT
1080
60
1280
210
h_saving
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 0.01 1 -16777216 true "" "histogram [saving_rate] of  turtles with [age = 0] "

INPUTBOX
170
425
322
485
init_A
1000.0
1
0
Number

INPUTBOX
90
565
242
625
step
0.004
1
0
Number

PLOT
715
495
915
645
A
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
"default" 1.0 0 -16777216 true "" "plot A"

PLOT
1135
290
1335
440
c1
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
"default" 1.0 0 -16777216 true "" "plot mean_c1"

PLOT
1095
455
1295
605
c2
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
"default" 1.0 0 -16777216 true "" "plot mean_c2"

MONITOR
1290
455
1347
500
c2
precision mean_c2 2
17
1
11

MONITOR
1335
295
1392
340
c1
precision mean_c1 2
17
1
11

CHOOSER
320
590
458
635
production_fun
production_fun
"cd" "h"
1

PLOT
980
610
1180
760
sup
NIL
NIL
0.0
10.0
0.0
0.5
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean_sup_rate"
"pen-1" 1.0 0 -7500403 true "" "plot mean_sup_rate + sd_sup_rate"
"pen-2" 1.0 0 -2674135 true "" "plot mean_sup_rate - sd_sup_rate"

MONITOR
880
680
982
725
sup_rate
precision mean_sup_rate 5
17
1
11

SWITCH
340
665
443
698
do-sup
do-sup
0
1
-1000

CHOOSER
495
615
633
660
learn-from
learn-from
"best" "any"
0

PLOT
1180
610
1380
760
h_sup
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 0.01 1 -16777216 true "" "histogram [sup_rate] of  turtles with [age = 0] "

@#$#@#$#@
# Class OLG Model 

## sup_0.1.0 base on v0.03.1_renew
1. no k
2. no return_of_k -> no saving
3. sup replace saving

## v0.03.1
add adjuster (from big model)

## v0.03
Evolutionary Editoion : saving rate = learning saving rate

### TODO
1. (done)calc all variables
2. (done)where to tick?


## v0.02
Rational Edition : saving rate = rational saving rate


### TODO
1. (**Not this time**)death rate (by real data or math model?)
2. (done)standard OLG model



### Change 
1. agents live for 4 periods = [0 1 2 3], approximately 20 years for 1 period. (so 100 ticks means 100 * 20 = 2000 years)
2. at p1 and p2, agents get paid (exogenous wage = 1)
3. at p0 and p3, do nothing
4. at p1, give birth by reproduction_rate

## v0.01

### Basic framework

1. a olg model with exogenous **reproduction_rate**, aka *n*.
2. change the **reproduction_rate**,  the evolution of population will be change.

## Base on Papers
1. Population, Technology, and Growth: From Maithusian Stagnation to the Demographic Transition and Beyond (Galor and Weil, 2000)
2. **Natural Selection and the Origin of Economic Growth (Galor and Moav, 2002)**
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
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
1
@#$#@#$#@
