__includes [ "plots.nls" "productivite.nls" "partition.nls" "anim_betail.nls" "demographie.nls" "echanges.nls" "fertilite.nls" "troupeau.nls" "engrais.nls" "update_g_variables.nls"]

extensions [gini.jar set.jar]

;; on utilise l'extension fp (fonctionnal programming) https://github.com/NetLogo/FP-Extension

breed [cuisines cuisine]
breed [couverts couvert]
breed [betails betail]
breed [engrais engrai]
cuisines-own [
  taille
  entrants
  sortants
  famille
  besoin-nourriture
  nb-patch-dispo
  nourriture-autosuffisante
  bilan-nourriture
  idmyParcellesSorted
  idmyParcellesCultive
  tropParcelles?
  taille-troupeau
  kg-engrais
]

patches-own [
  zone
  couvert-type
  proprietaire
  fertilite
  cycle
  parcelle-id
  myDistFromCuisine
  cultived?

]

globals [
  case-offset
  taille-bande
  cycle-jachere-courante
  COS-par-tete
  COS-champ-case-moy
  COS-champ-case-sd
  COS-champ-brousse-moy
  COS-champ-brousse-sd
  surface-de-patch
  betail-par-ha
  kg-mil-par-ha
  kg-mil-par-patch
  kg-arachide-par-ha
  kg-arachide-par-patch

  transhumants
  conso-carbone-culture
  spl-champ-brousse-par-cuisine
  spl-champ-case-par-cuisine
  kg-nourriture-par-pers-jour

  seuil-gini ;; tolérance entre gini souhaité et gini calculé
  last-tick  ;; defined the end of the simu a init

  ;; demographie
  min-taille-cuisine
  population-totale
  fertilite-global
  fertilite-init
  bilan-nourriture-g-init
  bilan-nourriture-g
  population-troupeau
  delta-nourriture
  delta-population
  delta-fertilite
]

to setup-i
  random-seed myseed-i
end

to setup
  ca
  reset-ticks
  set cycle-jachere-courante 1
  set last-tick 10
  ;;no-display

  ;; paramètre interne

  ;; en m²
  set surface-de-patch patch-area


  set seuil-gini 0.01
  ;;

  set spl-champ-brousse-par-cuisine 5
  set spl-champ-case-par-cuisine 5



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; paramètres calibrés depuis les données
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; fumier en kgs par tete par an
  set COS-par-tete 250

  ;; en kg par m²
  set COS-champ-case-moy 1.26
  set COS-champ-case-sd 0.125
  set COS-champ-brousse-moy 0.84
  set COS-champ-brousse-sd  0.06

  ;; en tete par hectare
  set betail-par-ha 1


  ;; paramètres libres pour modèle à l'équilibre

  ;; en kg de COS par m²
  set conso-carbone-culture 0.39 / 3 ;; source : simulation de chapitre 8 de "carbone des sols en afrique" tableau 2



  ;; croissance demographiuqe
  set min-taille-cuisine 3

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; paramètres calibrés depuis les acteurs
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  set kg-mil-par-ha 600
  set kg-mil-par-patch (kg-mil-par-ha *  (surface-de-patch / 10000))
  set kg-arachide-par-ha 400
  set kg-arachide-par-patch (kg-arachide-par-ha *  (surface-de-patch / 10000))
  set kg-nourriture-par-pers-jour 0.75


;; repartition des terres intiales
  ask patches [
    set proprietaire -99
  ]

  ask patches with [pxcor < 50 and pycor < 50]
  [
    set pcolor 6
    set zone "case"

  ]

  ask patches with [pxcor > 50 and pycor < 50]
  [
    set zone "J1"
    set cycle 1
  ]

  ask patches with [pxcor > 50 and pycor > 50]
  [
    set zone "J2"
      set cycle 2
  ]

  ask patches with [pxcor < 50 and pycor > 50]
  [
    set zone "J3"
    set cycle 3
  ]



 ask n-of nb-cuisines patches with [zone = "case" and pxcor <= 25 and pycor <= 25]  [
    set proprietaire "zone cuisine"
    sprout-cuisines 1 [
      set shape "house"
      set color (who + 1) * 10 + 6
    ]
  ]
  distiribution-population-par-cuisine
  affectation-initiale-troupeau-par-cuisine
  update-cuisine-size

  ask cuisines [
    set idmyParcellesCultive []
  ]

  ;; attriubut des bordures de zones proprietaire =  bordure

  ask patches with [ pxcor = 50 or pycor = 50 ]
  [
    set pcolor blue
    set proprietaire "bordures"
  ]

  ;; attribut proprietaire de la zone de cases

  ask patches with [pxcor <= 25 and pycor <= 25]
  [
    set proprietaire "zone cuisine"
  ]

  partition-init
  partition-iteration
  etalement-parcelle

  init-fertilite-a-la-parcelle

  ordre-parcelles
  affectation-initiale-troupeau-par-cuisine

  let repartition-init []
  ask cuisines [
    set repartition-init lput taille-troupeau repartition-init
  ]

  repartition-troupeau

  ;; troupeau pour mettre à l'équilibre probablement faux
  ;;set troupeau ceiling (((conso-carbone-culture * surface-de-patch) * count patches with [ proprietaire != "bordures" and proprietaire != "zone cuisine"] * 2 / 3 ) / COS-par-tete)

  ;; équilibre empirique pour gini faible (0.02)
  ;; set troupeau 130


 ask cuisines [
    set besoin-nourriture calcul-besoin-nourriture  [taille] of self
  ]

  init-sacs-engrais-cuisines
end ;; setup




to go
  demographie
  update-cuisine-size
  update-inti-tick
  ordre-parcelles
  planif-culture
  chercher-parcelles
  mise-en-culture

  ;; GUI cosmétique
  ask betails [die]
   ask n-of 15 patches with [cycle = 3 and zone != "case" and proprietaire != "bordures"]
  [
    sprout-betails 1 [ set shape "cow" set size 4 set color white ]
  ]


  ask patches with [ zone != "case" and proprietaire != "bordures" and proprietaire != "zone cuisine"]
  [
    set cycle (cycle + 1)
  ]

  ask patches with [cycle = 4]
  [
    set cycle 1
  ]


  epandage-engrais
  MAJ-fertilite


  MAJ-teinte
  updatePlots
  calcul-bilan
  update-end ;; update at last tick
  tick
end









to-report calcul-gini
  let surfaces [] ; pour stocker les surfaces
  ask cuisines [
    set surfaces lput count patches with [proprietaire != "bordures" and proprietaire != "zone cuisine" and proprietaire = myself] surfaces
  ]

  report gini.jar:gini surfaces
end



to-report calcul-besoin-nourriture [my-taille]
   report my-taille * kg-nourriture-par-pers-jour * 365
 end

to-report countMyCultivetedPlots ;cuisine context
  let idP 0
    ifelse ticks < 1 [
      set idP map [id -> count patches with[parcelle-id = id]] idmyParcellesSorted ;; ATTENTION il faudra que ce soit seulement les id des parcelles cultivé
    ][
      set idP map [id -> count patches with[parcelle-id = id]] idmyParcellesCultive ;; ATTENTION il faudra que ce soit seulement les id des parcelles cultivé
    ]
  report idP
end

to calcul-bilan

  ask cuisines [
    set besoin-nourriture calcul-besoin-nourriture  [taille] of self

    ;;let sumIdP sum countMyCultivetedPlots
    ;set nb-patch-dispo count patches with [(proprietaire = myself and parcelle-id =   or zone = "case" ) ] ;; selection sur la liste des parcelle cultive
    ;;set nourriture-autosuffisante (sumIdP * surface-de-patch  /  10000) * kg-cereale-par-ha

    set nourriture-autosuffisante production self
    set bilan-nourriture nourriture-autosuffisante - besoin-nourriture
  ]


end

to update-cuisine-size
  ask cuisines [
    set size taille / 2 + 2
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
295
43
808
557
-1
-1
5.0
1
10
1
1
1
0
0
0
1
0
100
0
100
0
0
1
ticks
30.0

BUTTON
15
14
88
47
setup
setup-i\nsetup
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
13
48
160
81
go 10
repeat last-tick [ go ]
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
848
46
946
79
paturage
random-walk-betail
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
835
104
948
137
NIL
eclatement
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
845
430
946
475
fertilite totale
sum [fertilite] of patches
1
1
11

PLOT
1019
238
1219
388
fertilité globale
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
"default" 1.0 0 -16777216 true "" "plot fertilitesize"
"pen-1" 1.0 0 -7500403 true "" "let fertilite-totale 0\nask cuisines [\n    let myParcelles patches with [member? parcelle-id [idmyParcellesCultive] of myself]\n    set fertilite-totale fertilite-totale + sum [fertilite] of myParcelles\n    ]\nplot fertilite-totale"

MONITOR
838
384
910
429
NIL
troupeau
17
1
11

PLOT
1227
85
1427
235
patchs par parcelle
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
"default" 1.0 1 -16777216 true "" ""

SLIDER
958
50
1136
83
ratio-arachide-riz
ratio-arachide-riz
0
2
0.9
.1
1
NIL
HORIZONTAL

SLIDER
1
341
186
374
kg-cereale-par-ha
kg-cereale-par-ha
100
800
620.0
10
1
NIL
HORIZONTAL

PLOT
1023
85
1223
235
bilan nourriture global
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
"default" 1.0 0 -16777216 true "" "plot bilan-nourriture-g"
"pen-1" 1.0 0 -7500403 true "" "plot 0"

MONITOR
835
190
920
235
besoins tot
sum [besoin-nourriture] of cuisines
1
1
11

MONITOR
835
140
1010
185
surface tot. dispo
sum [nb-patch-dispo] of cuisines
0
1
11

MONITOR
835
235
944
280
production tot.
sum [nourriture-autosuffisante] of cuisines
0
1
11

MONITOR
920
190
1000
235
bilan moyen
mean [bilan-nourriture] of cuisines
1
1
11

PLOT
1020
390
1220
540
bilan cuisine
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
"besoin 1" 1.0 2 -16777216 true "" ""

PLOT
1224
240
1424
390
patchs par cuisine
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
"default" 1.0 1 -16777216 true "" ""

MONITOR
835
335
913
380
NIL
calcul-gini
4
1
11

SLIDER
1
384
186
417
gini-parcelles
gini-parcelles
0.0
0.6
0.58
0.01
1
NIL
HORIZONTAL

SLIDER
1
427
188
460
croissance-demographique
croissance-demographique
0
1.0
0.02
0.01
1
NIL
HORIZONTAL

PLOT
1225
390
1425
540
emigration/immigration
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
"default" 1.0 2 -16777216 true "" ""

SLIDER
0
475
187
508
troupeau
troupeau
count cuisines
200
195.0
10
1
NIL
HORIZONTAL

SLIDER
0
632
187
665
malus-fertilite
malus-fertilite
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
3
672
178
705
malus-in-jachere
malus-in-jachere
0
1
0.4
0.01
1
NIL
HORIZONTAL

BUTTON
88
14
151
47
NIL
go
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
2
150
174
183
nb-cuisines
nb-cuisines
1
25
20.0
1
1
NIL
HORIZONTAL

SLIDER
3
196
227
229
moyenne-ppc
moyenne-ppc
1
20
11.0
1
1
NIL
HORIZONTAL

SLIDER
2
292
174
325
patch-area
patch-area
10
500
20.0
10
1
m2
HORIZONTAL

SWITCH
3
722
138
755
absorption
absorption
0
1
-1000

SLIDER
4
242
176
275
sd-ppc
sd-ppc
0
5
0.0
0.1
1
NIL
HORIZONTAL

PLOT
1430
85
1630
235
Population totale
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
"default" 1.0 0 -16777216 true "" "plot population-totale"

MONITOR
835
285
1004
330
Taille moyenne des cuisines
mean [taille] of cuisines
2
1
11

SLIDER
6
530
178
563
gini-troupeau
gini-troupeau
0
1
1.0
0.01
1
NIL
HORIZONTAL

PLOT
1430
240
1630
390
Troupeaux par cuisine
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
"default" 1.0 1 -16777216 true "" ""

SWITCH
170
10
381
43
update-fertilite-teinte
update-fertilite-teinte
1
1
-1000

SLIDER
193
234
226
384
max-sacs
max-sacs
0
10
2.0
1
1
NIL
VERTICAL

CHOOSER
7
580
169
625
strategie-paturage
strategie-paturage
"mixte" "collectif" "par cuisine"
0

PLOT
1430
395
1630
545
Fertilite cuisine
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
"default" 1.0 2 -16777216 true "" ""

TEXTBOX
250
580
410
655
Nb cuisine 16 (de 10 personnes)\nmalus fertilie 0\nkg cereale par ha 620
12
0.0
1

INPUTBOX
195
95
270
155
myseed-i
77.0
1
0
Number

PLOT
875
585
1075
735
Plot des varibale OM
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
"nourriture" 1.0 0 -16777216 true "" "if ticks > 3 [\nplot delta-nourriture\n]"
"population" 1.0 0 -7500403 true "" "if ticks > 3 [\nplot delta-population\n]"
"fertilite" 1.0 0 -8053223 true "" "if ticks > 3 [ plot delta-fertilite]"

BUTTON
870
505
942
538
setup
setup-i\nsetup
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
885
550
947
583
go
repeat last-tick [ go ]
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
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="plan-complet-diohine022020" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="25"/>
    <metric>population-totale</metric>
    <metric>fertilite-global</metric>
    <metric>population-troupeau</metric>
    <metric>bilan-nourriture-g</metric>
    <enumeratedValueSet variable="malus-in-jachere">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="malus-fertilite">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gini-parcelles">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-sacs">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gini-troupeau">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="moyenne-ppc">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kg-cereale-par-ha">
      <value value="620"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="absorption">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-fertilite-teinte">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="croissance-demographique">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ratio-arachide-riz">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-cuisines">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-ppc">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-area">
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategie-paturage">
      <value value="&quot;mixte&quot;"/>
      <value value="&quot;collectif&quot;"/>
      <value value="&quot;par cuisine&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="troupeau">
      <value value="195"/>
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
1
@#$#@#$#@
