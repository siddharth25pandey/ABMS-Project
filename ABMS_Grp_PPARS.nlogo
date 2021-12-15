breed [rabbits1 rabbit1]
breed [rabbits2 rabbit2]
breed [wolves wolf]
turtles-own [energy runspeed vision repcost]
rabbits1-own [dosage]
rabbits2-own [dosage]
patches-own [grass-amount regrowth-rate]

globals [dom_run dom_vis dom_rep l_run l_vis l_rep dayvar]


to-report daytime?
  let t int(ticks / 50)
  report t mod 2 = 0
end

to setup-rabbits
  create-rabbits1 initial-rabbit1
  [
    setxy random-xcor random-ycor
    set color brown
    set runspeed rabbit1-speed
    set dosage rabbit1-dosage
    set vision rabbit1-vision
    set repcost rabbit1-repcost
    set size 2
    set energy 5
  ]
  create-rabbits2 initial-rabbit2
  [
    setxy random-xcor random-ycor
    set color black
    set runspeed rabbit2-speed
    set dosage rabbit2-dosage
    set vision rabbit2-vision
    set repcost rabbit2-repcost
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
  set l_vis list rabbit1-vision rabbit2-vision
  set l_run list rabbit1-speed rabbit2-speed
  set l_rep list rabbit1-repcost rabbit2-repcost
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
  set-default-shape rabbits1 "rabbit"
  set-default-shape rabbits2 "rabbit"
  set-default-shape wolves "wolf"
  setup-rabbits
  setup-wolves
  setup-patches
  setup-var
  setup-list
end

to drift
  ask n-of random (count rabbits1) rabbits1
  [
    set vision rabbit2-vision
    set runspeed rabbit2-speed
  ]
  ask n-of random (count rabbits2) rabbits2
  [
    set runspeed rabbit1-speed
    set vision rabbit1-vision
  ]
end

to go
  if not any? wolves or not any? turtles with [breed != wolves] [ stop ]
  go-white-rabbits
  go-black-rabbits
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

to go-white-rabbits
  ask rabbits1[
    move
    ;;print "moved"
    eat-grass
    ;;print "eaten"
    flee-from-predator
    ;;print "fleed"
    reproduce-rabbits
    ;;print "reproduced"
    check-death
  ]
end

to go-black-rabbits
  ask rabbits2[
    move
    eat-grass
    flee-from-predator
    reproduce-rabbits
    check-death
  ]
end

to go-wolves
  ask wolves[
    move-w
    eat-rabbits
    reproduce
    check-death
  ]
end

to move
    rt random 360
    fd random 4
    set energy energy - energy-loss-from-moving
end

to move-w
    rt random 360
    fd random 2
    set energy energy - (energy-loss-from-moving / 2)
end

to eat-rabbits
  ifelse count my-links = 0
  [
    let myen energy
    ifelse any? turtles with [breed != wolves and energy < myen] in-radius 1
    [
      let str one-of turtles with [breed != wolves] in-radius 1
      ifelse [breed] of str = rabbits1
      [set energy energy + energy-gain-from-rabbit1][set energy energy + energy-gain-from-rabbit2]
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
          ifelse [breed] of target = rabbits1
          [set energy energy + energy-gain-from-rabbit1][set energy energy + energy-gain-from-rabbit2]
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


to reproduce-rabbits
  let mate turtles in-radius 5 with [breed = rabbits1 or breed = rabbits2]
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
          ifelse [breed] of v = rabbits1
          [
            hatch-rabbits1 1
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
            hatch-rabbits2 1
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

;;---------------------------------------------------------------------------------------------------

to-report rabbit1trait
  let val1 count turtles with [breed != wolves and vision = rabbit1-vision]
  let val2 count turtles with [breed = rabbits1 and repcost = rabbit1-repcost]
  let val3 count turtles with [breed != wolves and runspeed = rabbit1-speed]
  let val4 count rabbits1
  let val 0
  ifelse val4 = 0
  [set val 0]
  [set val ((val1 + val2 + val3) / (3 * val4))]
  report val * 100
end

to-report rabbit2trait
  let val1 count turtles with [breed != wolves and vision = rabbit2-vision]
  let val2 count turtles with [breed = rabbits2 and repcost = rabbit2-repcost]
  let val3 count turtles with [breed != wolves and runspeed = rabbit2-speed]
  let val4 count rabbits2
  let val 0
  ifelse val4 = 0
  [set val 0]
  [set val ((val1 + val2 + val3) / (3 * val4))]
  report val * 100
end


;;-----------------------------------------------------------------------------------------------------
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
1226
239
1289
272
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
initial-rabbit1
initial-rabbit1
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
initial-rabbit2
initial-rabbit2
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
1301
239
1364
272
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
energy-gain-from-rabbit1
energy-gain-from-rabbit1
0
10
7.0
1
1
NIL
HORIZONTAL

SLIDER
901
96
1074
129
energy-gain-from-rabbit2
energy-gain-from-rabbit2
0
10
6.0
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
2.5
0.1
1
NIL
HORIZONTAL

SLIDER
900
144
1074
177
rabbit2-repcost
rabbit2-repcost
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
rabbit1-repcost
rabbit1-repcost
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
0.4
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
10.0
1
1
NIL
HORIZONTAL

PLOT
1097
326
1475
550
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
"Rabbit 1" 1.0 0 -10402772 true "" "plot count rabbits1"
"Wolf" 1.0 0 -1184463 true "" "plot count wolves"
"Grass" 1.0 0 -13840069 true "" "plot grass * 10"
"Rabbit 2" 1.0 0 -16777216 true "" "plot count rabbits2"

SLIDER
1296
146
1473
179
RII
RII
0
100
0.0
1
1
NIL
HORIZONTAL

TEXTBOX
731
24
881
42
Rabbit subspecies 1 controls
11
0.0
1

TEXTBOX
921
24
1071
42
Rabbit supspecies 2 controls
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
rabbit1-dosage
rabbit1-dosage
0
10
10.0
0.2
1
NIL
HORIZONTAL

SLIDER
900
192
1075
225
rabbit2-dosage
rabbit2-dosage
0
10
9.0
0.2
1
NIL
HORIZONTAL

SLIDER
709
237
884
270
rabbit1-speed
rabbit1-speed
0.5
10
2.0
0.25
1
NIL
HORIZONTAL

SLIDER
900
237
1075
270
rabbit2-speed
rabbit2-speed
0.5
10
2.5
0.25
1
NIL
HORIZONTAL

SLIDER
708
281
884
314
rabbit1-vision
rabbit1-vision
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
rabbit2-vision
rabbit2-vision
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
4.0
1
1
NIL
HORIZONTAL

MONITOR
1098
563
1180
608
rabbits1
count rabbits1
0
1
11

MONITOR
1196
563
1278
608
rabbits2
count rabbits2
0
1
11

MONITOR
1293
563
1379
608
NIL
count wolves
0
1
11

MONITOR
1395
563
1458
608
NIL
daytime?
17
1
11

BUTTON
1137
239
1217
272
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
1376
239
1439
272
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

PLOT
706
326
1077
549
Trait preservation
time
percentage
0.0
100.0
0.0
100.0
true
true
"" ""
PENS
"Rabbit 1 Traits" 1.0 0 -10899396 true "" "plot rabbit1trait"
"Rabbit 2 Traits" 1.0 0 -13345367 true "" "plot rabbit2trait"
"Total Eqlbm" 1.0 0 -16777216 true "" "plot (rabbit1trait + rabbit2trait) / 2"

MONITOR
797
561
1012
606
Survival Index
(rabbit2trait + rabbit1trait) / 2
17
1
11

@#$#@#$#@
# OOD

## Model Desciption
The model description follows the ODD (Overview, Design con-
cepts, Details) protocol for describing agent-based models (Grimm et al., 2010, 2006). The model was implemented in NetLogo 6.0.2(Wilensky, 1999) and the program used to simulate the wolf - rabbit  population is available in the Supplementary Material.

## Purpose
The proximate purpose of the model is to study and observe the impact of reproductive isolation in the population dynamics of an isolated ecosystem.It aims to answer how the species of the crossbreed offspring would be decided on the amount of genes inherited from the particular parent species and how natural selection plays a role in deciding the dominant and recessive traits. The ultimate aim of the model presented is to study the change in genetic structure of a particular species living in the natural ecosystem due to reproductive isolation.


## Entities, state variables, and scales
Model entities are the square spatial units or habitat cells comprising the landscape, grass,  black and brown rabbits with territories.These are characterised by five continuous state variables “energy, run speed, vision, reproduction cost and dosage”.  Also an external dynamic agent, wolves,  to take control over the rabbits population.  The RII count refers to the liberty of inter-breeding: 0 means complete liberty and 100 means complete restriction. One time step of the model represents the time in which all individuals have randomly chosen another individual and possibly interacted with it. Simulations run until one of the rabbit or the wolf species population becomes extinct or zero.
Factors to add randomness in the model:.
* Genetic Drift - A sudden change in the genetic patterns across a species 
* Natural Selection - Advantage due to a natural trait that prefers survival 
* Mutation - A complete alteration in a particular trait of a subspecies
* Dominant Traits - Trait domination as explained in the mendelian theory 

R1
Initial number of white rabbit
R2
Initial number of black rabbit
W
Initial number of wolves
R1e
Initial energy of white rabbit
R2e
Initial energy of black rabbit
We
Initial energy of wolf
R1r
Running speed of white rabbit
R2r
Running speed of black rabbit
Wr
Running speed of wolf
Wv
Wolf vision range
R1v
White rabbit vision range
R2v
Black rabbit vision range
R1g
Energy gain from food for white rabbit
R2g
Energy gain from food for black rabbit
Wg
Energy gain from food for wolf
RII
Reproductive Isolation Index




## Process overview and scheduling
Each time step the following processes are processed in the given order.Model entities are processed in a random-ized order, unless stated otherwise, and changes in state variables are updated immediately. The submodels implementing these processes are described in detail below in the submodels section. There are mutual links between wolves and rabbits which have to be updated every time if the rabbit dies, flees away or the wolf dies. 

#### Chasing :

Wolves have a vision range, and if they see a rabbit in that range, they start running towards them. Similarly, rabbits have a vision range and if they see a wolf in that range they start running away.

```chasing() :

If(rabbits in radius of vision wolf) :
    -- eat-rabbit()
If(wolves in radius of vision rabbit):
    -- flee-from-predator()

#Reproduce - Wolf :

Having sufficient energy , wolves start looking for mates in specific radius to reproduce new ones.

reproduce(agentset of wolves in the x meter radius of the function caller wolf):
-- The agentset consists of wolves breed here

Mate = random wolf from the agentset

If(energy of mate > wolf-reproduction-cost) and (Mate!=nobody):
     -- energy of caller wolf = energy - reproduction cost
    -- energy of mate = energy - reproduction cost
    -- produce a new wolf
    -- set the initial energy

end function

```


#### Reproduce - Rabbit :

Having sufficient energy , rabbits start looking for mates in specific radius to reproduce new ones. Since there are two breeds or species of rabbits they can interbreed or even crossbreed with other rabbits.

```reproduce(agentset of rabbits in the x meter radius of the function caller rabbit):
-- The agentset consists of rabbits of both breeds here

Final_rabbit = random rabbit from the agentset 
If breed(final_rabbit) != breed(caller object rabbit):
-- cross-breed case
	-- Here crossbreed success probability is seen
-- Crossbreed_ratio = x% say suppose
temp_num = random number from 1 to 100
If (temp_num > crossbreed_ratio):
-- Hatch one offspring of crossbreed
-- It’s genetic traits will be decided
	-- randomly based on parent’s dominant trait
-- e.g out of five traits, trait 1,2,4 may be
-- copied from parent 1 and 3,5 from parent 2
	-- since breed 1 has 1,2,4 dominant traits and
-- breed 2 has 3,5 dominant traits.
-- Based on which parent got more similarity,
-- The breed of child is decided as 1 or 2.
-- We need to write a function for this
Set energy of both parents = energy - cost of mating   	
Else:
	-- same breed case
	-- even here the two parents may be of same breed but
	-- some of their traits may vary, so the child breed 
	-- is fixed here, but traits are decided in the same manner 
	-- as done in the crossbreed case. Just the breed is fixed 
	-- unlike the earlier case.

end function
```

#### Flee-from-predator :

Rabbits have a vision range, and if they see a wolf in that range they start running in the opposite direction to flee away from the predator.

```flee-from-predator():

If(wolves in radius vision):
   -- Link with one of wolves
   -- turn in opposite direction
   -- start running away
   -- set run-speed = run-speed *(1 - grass-amount/10), because the more the grass , it will be more difficult to run.



#### Eat - Grass : 

Rabbits have a fixed amount of grass to eat(rabbit-dosage) to gain energy.

eat-grass():

If(grass-amount > dosage):
    -- set grass-amount = grass-amount -  dosage
    -- set energy = energy + energy-from-grass
Else: (since grass is less than needed ,so the rabbit will eat less)
    -- set v = energy-from-grass * (grass-amount/dosage)
    -- set energy = energy + v
    -- set grass-amount = 0

```

#### Eat - Rabbit :

Wolves have a vision range, and if they see a rabbit in that range, they start running towards them , in order to hunt them down.

``eat-rabbits():

If(link=0) && (rabbits in radius vision) :
      -- link to rabbit with energy less than wolf energy
      If(breed of rabbit = rabbit1):
         -- set energy = energy + energy-from-rabbit1
      Else:
         -- set energy = energy + energy-from-rabbit2
     If(daytime=true):
         --link with black rabbit
     Else:
         --link with brown rabbit
     chase()
     -- die rabbit
```
#### Trait - Selection :

For crossbreeding and genetic mutation, we are performing trait - selection in our model. Each breed of rabbit has three traits; run-speed, vision, reproduction - cost.

```trait-selection():
     -- randomly we are selecting dominant traits, i.e.
        which breed has dominant traits for 
        run-speed, vision, reproduction cost.
If(all three traits are dominant of same breed):
     -- run-speed = trait value of other breed, to maintain equilibrium

```
#### Check - Death :

If the energy of any particular breed goes below a certain limit, they die.

``` check-death():
If(energy <= 0):
    -- die
end function
```
![Flow Chart](https://drive.google.com/file/d/1ZpdzfNERJU2OtFSu1-1KPE5uLzXzcCLn/view)


## Design concepts

* Basic Principle: This model reflects fundamental ecological relationships between population dynamics of species and their environment and replicate an isolated environment with two subspecies of any generic herbivore (rabbit in our case), their source of nutrition (a point of competition), and a predator. As we are dealing with complex phenomena by integrating the modeling and simulation based environment, we can say that agent based modelling focuses on the individual active components of a system. This is in contrast to both the more abstract system dynamics approach and process-focused discrete method.

* Emergence: Rabbit population size emerges from demographic process and more abstract territory dynamics. If the Reproductive Isolation Index is zero then, we can say that the traits of both rabbit species are completely intermatched & if both species are completely isolated then the traits of individuals are not transferred, hence resulting in a drastic change in the genetic patterns of the population.

* Sensing: Wolves can sense through their vision for total prey available to them within their territory range. Similarly Rabbit species also have their own sense of vision for escaping from the predators. Along with vision sensing, the brown coloured rabbits have the ability to hide from the predators due to their body colour in day time, and same natural selection favours black rabbits to hide at night.

* Interaction: Competition for habitat cells (grass) is a direct interaction for both species of rabbits. Similarly, Wolves also interact directly with the rabbit species as a predator to gain access to their habitat territory cells and their livelihood. Due to the rise of survival competition the one who runs faster and has greater visibility range will survive more in a longer duration of time.

* Objective: Our main objective is to make balance between the ecological system of population dynamics of species and their environment. Like for herbivore species such as rabbits, we observe that, when rabbits have greater visibility and high running speed, they can escape easily from predators. Similarly for the survival of carnivores species such as wolves, they have to catch the prey (rabbits) with the weakest individual traits.In this way, they can maintain balance between their individual species.


* Adaptation: To increase the survival chance and having greater adaptation, the rabbit species only try to reproduce with the partner having stronger traits selection. Stronger the traits of the offspring, greater the chance of survival, hence leads to high adaptation rate in their territories.

* Stochasticity: Stochasticity was incorporated into many processes to account for natural variation. Dominant traits are the trait domination as explained in the mendelian theory. Mutation is a complete alteration in a particular trait of a subspecies. Natural Selection is the advantage due to a natural trait that prefers survival, Genetic Drift  is a sudden change in the genetic patterns across a species.

* Observation: Individual and population-level species were observed through These included reproduction (due to genetic drive, mutation,selection & dominant trait choice), resource selection (grass eating capacity on day and night time), black and brown rabbit land tenure (i.e., time that breeding animal held onto territory before dying or dispersing),territory size and spatial distribution, and rabbit population size and age structure.



## Initialization
Simulations are run with a random number of individuals, of which a specified initial proportion, rabbit1 are brown rabbits; rabbit2 are black rabbits and initial-wolf are wolves. Each individual originates at a random location. Initially , each individual has initial energies set for survival, like vision-range, speed, gain-from-food etc. The dominant and recessive traits are chosen randomly for the herbivore traits.
The model is run for two extreme cases - one when RII is 0 and other when RII is 100. 

## Input data
All the variables defined are to be input from the user using sliders, so that the different nuances are balanced to reach a state of equilibrium in the model, so that further studies on our hypothesis can be carried out further.



## Submodels
All the models parameters are listed in the below table :


R1
Initial number of white rabbit
R2
Initial number of black rabbit
W
Initial number of wolves
R1e
Initial energy of white rabbit
R2e
Initial energy of black rabbit
We
Initial energy of wolf
R1r
Running speed of white rabbit
R2r
Running speed of black rabbit
Wr
Running speed of wolf
Wv
Wolf vision range
R1v
White rabbit vision range
R2v
Black rabbit vision range
R1g
Energy gain from food for white rabbit
R2g
Energy gain from food for black rabbit
Wg
Energy gain from food for wolf
RII
Reproductive Isolation Index
R1rc
Reproduction cost for rabbit1
R2rc
Reproduction cost for rabbit2
Rd 
Rabbits dosage
Gr
Grass regrowth rate



#### Chasing :

Wolves have a specific vision range Wv which can be set using a slider. Similarly, rabbit species also have a specific vision range R1v, R2v which can also be set using a slider.When wolves see a rabbit in their vision range, they start running towards them. Similarly when rabbits find a wolf in their vision range they start running away from it in the opposite direction.
7 cases are possible now:

* Wv = R1v = R2v : In this case there is an equal chance of both the rabbits getting hunted by the wolf or fleeing away from the wolf.
Wv > R1v = R2v : In this case there is an equal chance of of both the rabbits getting hunted by the wolf what’s different in this case is there is less chance of rabbits getting away from the wolf vision as it depends on the so many factors like the energy and no of hunters around.
* Wv > R1v > R2v: In this case species 1 has an advantage on species 2 of rabbits in fleeing away from the wolf and now it depends on the surrounding factors to decide whether the wolf will be able to hunt down the rabbit of species 2 or not.
* Wv > R2v > R1v: In this case species 2 has an advantage on species 1 of rabbits in fleeing away from the wolf and now it depends on the surrounding factors to decide whether the wolf will be able to hunt down the rabbit of species 1 or not.
* R1v > Wv > R2v: In this case rabbit of species 1 have higher chances of not being hunted by the wolves because now rabbit of species 1 can now see the wolves before coming into the vision of wolves and start running.
* R2v > Wv >R1v: In this case rabbit of species 2 have higher chances of not being hunted by the wolves because now rabbit of species 2 can now see the wolves before coming into the vision of wolves and start running.
* R1v & R2v > Wv: In this case the ecosystem will be unstable as there is no one to balance the population of rabbits and also there’s no food for the wolves to eat which will result in their extinction.



#### Fleeing :

Rabbits have a vision range,R1v and R2v, and if they see a wolf in that range they start running in the opposite direction to flee away from the predator.
The following steps are involved while fleeing away :

* If they see wolves in their vision, they link with one of the wolves among them.(with the nearest one)
* They face in a direction to that of the wolf.
* Turn 180 degrees to face the opposite direction.
* They start running away from it.
* The run-speed(R1r or R2r) is affected due to the amount of grass(grass-amount) present in the patch where they are running.
* If they are at a distance greater than wolf-vision(Wv), the link is broken and they escape.

#### Grass - Regrowth : 

There are 2 factors affecting the grass in the environment.

* Rd :Rabbits dosage i.e. both the species have different amounts of appetite to fulfill their needs for energy and reproduction.
Gr: Grass regrowth rate is fixed in the entire run.

* So if any of the species has higher dosage i.e Rd then they might end up eating all the grass in the nearby patches and since the grass regrowth i.e Gr is constant it may result in the land being barren i.e land without grass and this might cause the death of specific specie in the long run.





#### Wolf Reproduction : 

When wolves' energy reaches above a certain limit they start looking for mates in a particular radius for reproduction. These are the steps involved in selecting mates and reproducing :

* The wolf identifies a wolf in a radius wolf-vision(Wv). We have specified the radius limit as 5km, because this is the maximum distance a wolf in our model can travel using initial wolf-energy(We).
* Among the list of mates in the radius, the wolf selects a mate with wolf-energy(We) less than wolf-reproduction-cost(Wr).
* If it finds such a mate as listed in point 2 above, reproduction happens.
* All the initial variables are set for the new wolf.


#### Rabbit Reproduction :
With sufficient energy , rabbits start looking for mates in specific radius to reproduce new ones. Since there are two breeds or species of rabbits they can interbreed or even crossbreed with other rabbits.

2 cases are possible here either they breed within themselves or they breed with the other species.

* Crossbreed: The genetic traits of the offspring will be decided randomly based on the parent's dominant trait. Let’s suppose There are 5 traits and out of five traits, let’s suppose  trait 1,2,4 are being copied  from parent1 and 3,5 from parent 2.Since breed 1 has 1,2,4 dominant traits and breed 2 has 3,5 dominant traits.Based on the fact The breed of child is decided as 1 or 2.

* Same breed: Here the parents are of the same breed so the offspring will be of the same breed that their parents belong to. But here also their traits will vary just as explained with an example in the cross breed.Here just the breed is fixed unlike the earlier case.
  	

#### Trait Selection :

For crossbreeding and genetic mutation, we are performing trait - selection in our model. Each breed of rabbit has three traits; run-speed, vision, reproduction - cost. 
According to genetics :
Dominant + Dominant = Dominant
Dominant + Recessive = Dominant
Recessive + Recessive = Recessive

* We randomly choose the dominant traits among the two species.

* If all the dominant traits are from the same species we define run-speed to be of different species.

* After this cross-breeding happens and the new one inherits the dominant traits of different species.

* Based on the number of traits inherited from an individual parent, the breed of the offspring is decided.
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

cat
false
0
Line -7500403 true 285 240 210 240
Line -7500403 true 195 300 165 255
Line -7500403 true 15 240 90 240
Line -7500403 true 285 285 195 240
Line -7500403 true 105 300 135 255
Line -16777216 false 150 270 150 285
Line -16777216 false 15 75 15 120
Polygon -7500403 true true 300 15 285 30 255 30 225 75 195 60 255 15
Polygon -7500403 true true 285 135 210 135 180 150 180 45 285 90
Polygon -7500403 true true 120 45 120 210 180 210 180 45
Polygon -7500403 true true 180 195 165 300 240 285 255 225 285 195
Polygon -7500403 true true 180 225 195 285 165 300 150 300 150 255 165 225
Polygon -7500403 true true 195 195 195 165 225 150 255 135 285 135 285 195
Polygon -7500403 true true 15 135 90 135 120 150 120 45 15 90
Polygon -7500403 true true 120 195 135 300 60 285 45 225 15 195
Polygon -7500403 true true 120 225 105 285 135 300 150 300 150 255 135 225
Polygon -7500403 true true 105 195 105 165 75 150 45 135 15 135 15 195
Polygon -7500403 true true 285 120 270 90 285 15 300 15
Line -7500403 true 15 285 105 240
Polygon -7500403 true true 15 120 30 90 15 15 0 15
Polygon -7500403 true true 0 15 15 30 45 30 75 75 105 60 45 15
Line -16777216 false 164 262 209 262
Line -16777216 false 223 231 208 261
Line -16777216 false 136 262 91 262
Line -16777216 false 77 231 92 261

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

eagle
false
0
Polygon -7500403 true true 135 165 90 270 120 300 180 300 210 270 165 165
Rectangle -7500403 true true 120 105 180 237
Polygon -7500403 true true 135 105 120 75 105 45 121 6 167 8 207 25 257 46 180 75 165 105
Circle -16777216 true false 128 21 42
Polygon -7500403 true true 163 116 194 92 212 86 230 86 250 90 265 98 279 111 290 126 296 143 298 158 298 166 296 183 286 204 272 219 259 227 235 240 241 223 250 207 251 192 245 180 232 168 216 162 200 162 186 166 175 173 171 180
Polygon -7500403 true true 137 116 106 92 88 86 70 86 50 90 35 98 21 111 10 126 4 143 2 158 2 166 4 183 14 204 28 219 41 227 65 240 59 223 50 207 49 192 55 180 68 168 84 162 100 162 114 166 125 173 129 180

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

mouse
false
0
Polygon -7500403 true true 38 162 24 165 19 174 22 192 47 213 90 225 135 230 161 240 178 262 150 246 117 238 73 232 36 220 11 196 7 171 15 153 37 146 46 145
Polygon -7500403 true true 289 142 271 165 237 164 217 185 235 192 254 192 259 199 245 200 248 203 226 199 200 194 155 195 122 185 84 187 91 195 82 192 83 201 72 190 67 199 62 185 46 183 36 165 40 134 57 115 74 106 60 109 90 97 112 94 92 93 130 86 154 88 134 81 183 90 197 94 183 86 212 95 211 88 224 83 235 88 248 97 246 90 257 107 255 97 270 120
Polygon -16777216 true false 234 100 220 96 210 100 214 111 228 116 239 115
Circle -16777216 true false 246 117 20
Line -7500403 true 270 153 282 174
Line -7500403 true 272 153 255 173
Line -7500403 true 269 156 268 177

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

squirrel
false
0
Polygon -7500403 true true 87 267 106 290 145 292 157 288 175 292 209 292 207 281 190 276 174 277 156 271 154 261 157 245 151 230 156 221 171 209 214 165 231 171 239 171 263 154 281 137 294 136 297 126 295 119 279 117 241 145 242 128 262 132 282 124 288 108 269 88 247 73 226 72 213 76 208 88 190 112 151 107 119 117 84 139 61 175 57 210 65 231 79 253 65 243 46 187 49 157 82 109 115 93 146 83 202 49 231 13 181 12 142 6 95 30 50 39 12 96 0 162 23 250 68 275
Polygon -16777216 true false 237 85 249 84 255 92 246 95
Line -16777216 false 221 82 213 93
Line -16777216 false 253 119 266 124
Line -16777216 false 278 110 278 116
Line -16777216 false 149 229 135 211
Line -16777216 false 134 211 115 207
Line -16777216 false 117 207 106 211
Line -16777216 false 91 268 131 290
Line -16777216 false 220 82 213 79
Line -16777216 false 286 126 294 128
Line -16777216 false 193 284 206 285

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

wolf 4
false
0
Polygon -7500403 true true 105 75 105 45 45 0 30 45 45 60 60 90
Polygon -7500403 true true 45 165 30 135 45 120 15 105 60 75 105 60 180 60 240 75 285 105 255 120 270 135 255 165 270 180 255 195 255 210 240 195 195 225 210 255 180 300 120 300 90 255 105 225 60 195 45 210 45 195 30 180
Polygon -16777216 true false 120 300 135 285 120 270 120 255 180 255 180 270 165 285 180 300
Polygon -16777216 true false 240 135 180 165 180 135
Polygon -16777216 true false 60 135 120 165 120 135
Polygon -7500403 true true 195 75 195 45 255 0 270 45 255 60 240 90
Polygon -16777216 true false 225 75 210 60 210 45 255 15 255 45 225 60
Polygon -16777216 true false 75 75 90 60 90 45 45 15 45 45 75 60

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
<experiments>
  <experiment name="experiment" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>(rabbit1trait + rabbit2trait) / 2</metric>
    <enumeratedValueSet variable="RII">
      <value value="0"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-vision">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-rabbit2">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rabbit2-vision">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-repcost">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-grass">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-gain-from-rabbit1">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-gain-from-rabbit2">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rabbit2-repcost">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rabbit1-speed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rabbit1-dosage">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rabbit2-speed">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-rabbit1">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rabbit1-vision">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rabbit1-repcost">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rabbit2-dosage">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-loss-from-moving">
      <value value="0.4"/>
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
