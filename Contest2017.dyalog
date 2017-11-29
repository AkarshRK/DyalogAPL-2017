:Namespace Contest2017

(⎕IO ⎕ML ⎕WX)←1 0 3

:Namespace geom
(⎕IO ⎕ML ⎕WX)←1 0 3

 ConvexHull←{
     ⍝ Stub function for 2017 APL Problem Solving Competition Computational Geometry Problem Set Problem 2, Task 1
     ⍝ Your code goes below...
     
          ⍝ Algorithm Implemented: GRAHAM SCAN (I found that it is simple, easy and efficient in handling degenerate cases such as collinearity )
     
     isFar←{</+/¨2*⍨(⍵[3],⍵[2])-⍵[1]}  ⍝ this Dfn retains the farthest point in a group of collinear points from the BOTTOM most point
     
     BottomSortAndEliminate←{(1⌷poly),poly⌷⍨⊂in/⍨~∨/[2](1↓poly)∘.{(orientation(p←poly[⍵],⊂⍺))=0:isFar p ⋄ 0}{1 ⍵}¨in←1↓⍳⍴poly←poly[⍋↑⌽¨poly←⍵]} ⍝ now this function does TWO important things:
                                                                                                                                                     ⍝ One, it finds the BOTTOM most point
                                                                                                                                                     ⍝ Second, in case of group of collinear points from the BOTTOM point, it retains only the farthest point
     
     SortByPolar←{PolarIn←1,2++/[1](1↓p)∘.{((orientation p[⍵],⊂⍺)=1)}{1 ⍵}¨1↓⍳⍴p←⍵ ⋄ p[⍋PolarIn,[2]↑p]}  ⍝ Sorts the points according to its polar angle by just using the 'Dfn orientaion'
                                                                                                              ⍝ and there is NO TRIGONOMETRIC FUNCTION USED to sort the points (efficiency is important!)
     
     ElimClkTurns←{(∨/k←1∊⍨{orientation ⍵}¨tri←3,/⊂¨⍵)=0:⍵ ⋄ ∇ ⍵~2⌷⊃tri⌷⍨⊃(⍳⍴tri)/⍨k} ⍝ After sorting the points by it's polar angle, this Dfns removes ONLY the points responsible for concavity
     
     ElimClkTurns SortByPolar BottomSortAndEliminate ⍵   ⍝ finds the convex hull in ANTI CLOCK WISE direction
 }

 EntryAndExitPoints←{
     polyA polyB poly1 poly2←⍺,⍵     ⍝ polyA and polyB are the polygons poly1 and poly2 respectively along with the intersection points inserted among the edges in clockwise order
     
     Entry←{Pol←⍵ ⋄ point←3,/⊂¨(¯1↑⍺),⍺,1↑⍺ ⋄ (⊂0 1 1)⍳⍨{⍵∘.isInside⊂Pol}¨point} ⍝ Dfn to determine the ENTRY point of polygon ⍺ (left argument) in polygon ⍵ (right argument)
     
     Exit←{Pol←⍵ ⋄ point←3,/⊂¨(¯1↑⍺),⍺,1↑⍺ ⋄ (⊂1 1 0)⍳⍨{⍵∘.isInside⊂Pol}¨point}  ⍝ Dfn to determine the EXIT point of polygon ⍺ (left argument) in polygon ⍵ (right argument)
     
     (Entry/¨(polyA poly2)(polyB poly1)),Exit/¨(polyA poly2)(polyB poly1)   ⍝ this statement claculates and returns the Entry and Exit points of poly1 and poly2
     
 }

 Intersects←{
     ⍝ Stub function for 2017 APL Problem Solving Competition Computational Geometry Problem Set Problem 1, Task 1
     ⍝ Your code goes below...
     
     triad←{⍺∘.{orientation ⍵,⊂⍺}⊂⍵}    ⍝ suppose left and right argument of triad are (p1 q1) and (p2 q2) then
                                             ⍝ triad returns the orientation of (p1,q1,p2) and (p1,q1,q2)
     
     (≠/p←⍺ triad ⍵)∧(≠/q←⍵ triad ⍺):1  ⍝ line segments intersects if orientation of triads (p1,q1,p2) and (p1,q1,q2) are different
                                             ⍝ and also orientation of triads (p2,q2,p1) and (p2,q2,q1) are different
     
     (0=+/p,q)∧(∨/⍺∘.OnLine⊂⍵)  ⍝ now if the orientaion of all the four triads are collinear:
                                     ⍝ then it checks if the line segments touch or overlap
 }

 OnLine←{
      ⍝ Syntax: point Online LineSegment
      ⍝ provided the three points are collinear, it returns 1 if the point lies on the given LineSegment otherwise 0
     ∧/(⍺[1]≤⌈/2↑A)(⍺[1]≥⌊/2↑A)(⍺[2]≤⌈/¯2↑A)(⍺[2]≥⌊/¯2↑A←,⍉↑⍵)
 }

 PointOfInt←{
      ⍝ syntax: seg1 PointOfInt seg2
     
     triad←{⍺∘.{orientation ⍵,⊂⍺}⊂⍵}   ⍝ suppose left and right argument of triad are (p1 q1) and (p2 q2) then
                                            ⍝ triad returns the orientation of (p1,q1,p2) and (p1,q1,q2)
     
     0=(∨/⍺ triad ⍵)∧(∨/⍵ triad ⍺):0   ⍝ returns 0 if the seg1 and seg2  overlap or don't touch each other at all
     
     (⊃⍺[1])+(⊃⍺[2]-⍺[1])×1↑⌽,(⌹⍉↑(⍵[2]-⍵[1]),⍺[1]-⍺[2])(+.×)(2 1⍴↑⍺[1]-⍵[1])  ⍝ returns the intersection point of line segments seg1 and seg2
     
 }

 PolyAND←{
     ⍝ Stub function for 2017 APL Problem Solving Competition Computational Geometry Problem Set Problem 3, Task 2
     ⍝ Your code goes below...
     
     poly1 poly2←⍺ ⍵
     
     polyA polyB←poly1 PolyInterPoints poly2  ⍝ finds the intersecting points in clockwise order
     
     EnPtA EnPtB ExPtA ExPtB←polyA polyB EntryAndExitPoints poly1 poly2   ⍝ finds the entry and exit points of poly1 in poly2 and also the entry and exit points of poly2 in poly1
     
     ∪(polyA⌷⍨⊂Series EnPtA,ExPtA,(⍴polyA)),(polyB⌷⍨⊂Series EnPtB,ExPtB,(⍴polyB))  ⍝ now that we know the entry and exit points we can find the points of the REQUIRED NEW POLYGON as follows:
                                                                                        ⍝ for AND operation we can perform the following steps:
                                                                                        ⍝ Step1: For polygon A (i.e poly1) include only the points starting from ENTRY point till EXIT point (inclusive)
                                                                                        ⍝ Step2: Similarly for polygon B (i.e poly2) include only the points starting from ENTRY point till EXIT point (inclusive)
                                                                                        ⍝ the resulting points will be the new polygon created by performing an AND operation on the polygons in CLOCK WISE order
 }

 PolyInterPoints←{
          ⍝ Function to calculate intersecting points of edges of poly1 and poly2
     
     PolySides←{2,/⊂¨⍵,1⌷⍵}    ⍝ to generate edges of the polygon in the given Clock Wise order
     
     ClockWise←{R←{⍺+1×(⍳1+⍵-⍺)-1} ⋄ P←⍵ ⋄ ∪↑,/SortLinePoints¨{P[⍵]}¨↑,/R/¨{2,/⊂¨⍵}((P∊⍺)/(⍳⍴P)),⍴P}    ⍝ To sort the intersecting points on the polygon edges in Clock Wise order
     
     SortLinePoints←{(⍴,↑⍵)=2:⍵ ⋄ (x←(1 1)⊃⍵)<(y←((⍴⍵)1)⊃⍵):⍵[⍋↑⍵] ⋄ (x>y):⍵[⍒↑⍵] ⋄ (x=y):⌽¨SortLinePoints(⌽¨⍵)}    ⍝ to sort points on a POLYGON EDGE according to the direction of EDGE as found
                                                                                                                         ⍝ when tracing it in Clockwise order
     
     IntMtrx←(PolySides ⍺)∘.{(⍺ Intersects ⍵):⍺ PointOfInt ⍵ ⋄ 0}(PolySides ⍵)    ⍝ 'IntMtrx' is a matrix consisting of ONLY intersecting points of poly1 and poly2
     
     polyA polyB←0~⍨¨∪¨(,⍺,[2]IntMtrx)(,⍵,[2]⍉IntMtrx)       ⍝ here polyA and polyB are polygons poly1 and poly2 respectively along with the intersecting points
                                                                  ⍝ inserted in between the original points of poly1 and poly2
     
     (⍺ ClockWise polyA)(⍵ ClockWise polyB)   ⍝ returns two polygons with points of intersection inserted accordingly in clock wise order
 }

 PolyNOT←{
     ⍝ Stub function for 2017 APL Problem Solving Competition Computational Geometry Problem Set Problem 3, Task 3
     ⍝ Your code goes below...
     
     poly1 poly2←⍺ ⍵
     
     polyA polyB←poly1 PolyInterPoints poly2  ⍝ finds the intersecting points in clockwise order
     
     EnPtA EnPtB ExPtA ExPtB←polyA polyB EntryAndExitPoints poly1 poly2  ⍝ finds the entry and exit points of poly1 in poly2 and also the entry and exit points of poly2 in poly1
     
     ∪(polyA⌷⍨⊂Series ExPtA,EnPtA,(⍴polyA)),(⌽polyB⌷⍨⊂Series EnPtB,ExPtB,(⍴polyB)) ⍝ now that we know the entry and exit points we can find the points of the REQUIRED NEW POLYGON as follows:
                                                                                        ⍝ for NOT operation we can perform the following steps:
                                                                                        ⍝ Step1: For polygon A (i.e poly1) include only the points starting from EXIT point till ENTRY point (inclusive)
                                                                                        ⍝ Step2: For polygon B (i.e poly2) include only the points starting from ENTRY point till EXIT point, but in REVERSE ORDER  (since NOT operation is not commutative)
                                                                                        ⍝ the resulting points will be the new polygon created by performing a NOT operation on the polygons in CLOCK WISE order
 }

 PolyOR←{
     ⍝ Stub function for 2017 APL Problem Solving Competition Computational Geometry Problem Set Problem 3, Task 1
     ⍝ Your code goes below...
     
     poly1 poly2←⍺ ⍵
     
     polyA polyB←poly1 PolyInterPoints poly2   ⍝ finds the intersecting points in clockwise order
     
     EntA EntB ExtA ExtB←polyA polyB EntryAndExitPoints poly1 poly2    ⍝ finds the entry and exit points of poly1 in poly2 and also the entry and exit points of poly2 in poly1
     
     ∪(polyA⌷⍨⊂Series ExtA,EntA,(⍴polyA)),(polyB⌷⍨⊂Series ExtB,EntB,(⍴polyB))      ⍝ now that we know the entry and exit points we can find the points of the REQUIRED NEW POLYGON as follows:
                                                                                        ⍝ for OR operation we can perform the following steps:
                                                                                        ⍝ Step1: For polygon A (i.e poly1) include only the points starting from EXIT point till ENTRY point
                                                                                        ⍝ Step2: Similarly for polygon B (i.e poly2) include only the points starting from EXIT point till ENTRY point
                                                                                        ⍝ the resulting points will be the new polygon created by performing an OR operation in CLOCKWISE ORDER
 }

 Series←{
     ⍝ Where have I used this function?
     ⍝ I have used this in PolyOR,PolyAND and PolyNOT to find all the points lying between ENTRY and EXIT point in the same order
     
     R←{⍺+1×(⍳1+⍵-⍺)-1} ⋄ ⍵[1]≤⍵[2]:⍵[1]R ⍵[2] ⋄ (⍵[1]R ⍵[3]),(1 R ⍵[2])
 }

 isInside←{
     ⍝ Stub function for 2017 APL Problem Solving Competition Computational Geometry Problem Set Problem 1, Task 2
     ⍝ Your code goes below...
     
     point nodes←⍺ ⍵
     PolLineSegs←2{⍺ ⍵}/nodes,1⌷nodes    ⍝ Dfn to create edges of the polygon in ClockWise order
     
     InfLine←(⊂⍺),⊂999,2⌷⍺   ⍝ represents the Infinite Line drawn from the point
     
     ∨/(⊂InfLine)∘.{(⍺ Intersects ⍵)∧(0=orientation ⍵,⊂point)∧(point OnLine ⍵)}PolLineSegs:1  ⍝ this statement checks if the point lies on the polygon or not
     
     2|+/(⊂InfLine)∘.{⍺ Intersects ⍵}PolLineSegs      ⍝ this satement returns 1 if the number of intersection of Infinite Line drawn
                                                           ⍝ from the point with the edges is odd or returns 0 if the number of intersections is even
 }

 orientation←{
      ⍝ Finds the orientation of an ordered triplet of points in a plane which returns:
      ⍝  1 for Clockwise, 0 for Collinear and 2 for Anti-CLockwise
     (A←-/⊃((⍵[3]-⍵[2])×⊂⌽⊃(⍵[2]-⍵[1])))>0:1 ⋄ A=0:0 ⋄ 2
 }

:EndNamespace 
:Namespace hlth
(⎕IO ⎕ML ⎕WX)←1 0 3

 DrugD←{
     ⍝ Stub function for 2017 APL Problem Solving Competition Healthcare Problem Set Problem 1, Task 4
     ⍝ Your code goes below...
     
     
     years←1                   ⍝ DEFAULT number of years to run the projection, you can change it to any number of years
     dropRate a b c←(⍺×0.01),⍵ ⍝ convert dropRate into fraction
     d←0                       ⍝ Initially there are NO PATIENTS for therapy in Drug D for INITIAL EIGHT MONTHS
                                    ⍝ it takes time for the new patients to migrate from drug A to B to C and then finally to Drug D
     
      ⍝ Few changes are made here, and what are those?
      ⍝ Now, since Drug B is effective in 35 % , then 65 % percent move from Drug B to Drug C, therefore following Parameters of Drug B changes:
      ⍝ respondB ← 0.35×0.9×moveAtoB
      ⍝ moveBtoC ← 0.65×0.9×moveAtoB
     
      ⍝ Now, since Drug C is effective in 40 % , then 60 % percent move from Drug C to Drug D, therefore following Parameters of Drug C changes:
      ⍝ respondC ← 0.4×0.8×moveBtoC
      ⍝ moveCtoD ← 0.6×0.8×moveBtoC
     
      ⍝ For Drug D:
      ⍝ (1-dropRate) of newly moved patients from Drug C are responsive and stay on it until they enter remission or die
     
      ⍝ NewA, Newb,NewC and NewD are numeric vectors containing the number of new patients that are identified every month
     NewA←(12×years)Random 1000 3000
     NewB←(¯2↓NewA),2/0
     NewC←(¯5↓NewA),5/0
     NewD←(¯8↓NewA),8/0
     
     ParametersA←⊂(a remrateA deathrateA respondA notA dropoutA moveAtoB NewA)←a,0.025,0.0325,(0.7×0.8),0.2,0,(0.3×0.8),⊂NewA
     ParametersB←⊂(b remrateB deathrateB respondB notB dropoutB moveBtoC NewB)←b,0.025,0.0325,(0.35×0.9×moveAtoB),0,(0.1×moveAtoB),(0.65×0.9×moveAtoB),⊂NewB
     ParametersC←⊂(c remrateC deathrateC respondC notC dropoutC moveCtoD NewC)←c,0.025,0.0325,(0.4×0.8×moveBtoC),0,(0.2×moveBtoC),(0.6×0.8×moveBtoC),⊂NewC
     ParametersD←⊂(d remrateD deathrateD respondD notD dropoutD moveDtoNULL NewD)←d,0.05,0.01,(moveCtoD×1-dropRate),0,(dropRate×moveCtoD),0,⊂NewD
     
     
     (sa sb sc sd rem dec drop not)←(⊃kk),+/¨1↓kk←↓⍉↑years TrialForNYears¨(ParametersA,ParametersB,ParametersC,ParametersD)   ⍝ now I run the projection for 'years' for each drug INDEPENDENT OF EACH OTHER
     
     Rounding(sa+moveAtoB×+/2↑NewA)(sb+moveBtoC×+/3↑NewB)(sc+moveCtoD×+/3↑NewC)sd rem(dec+0.11×drop+not)drop not ⍝ account for the patients who aren't managed (because they have a death rate of 11%)
                                                                                                                      ⍝ and also account for the new paients who were identified in the last 2 or 3 months of the LAST YEAR since they DIDN'T MOVE OUT
     
 }

 Projection←{
     ⍝ Stub function for 2017 APL Problem Solving Competition Healthcare Problem Set Problem 1, Task 1
     ⍝ Your code goes below...
     
      ⍝ The steps, methods and the theory behind my solution is as follows:
      ⍝ If START is the initial number of people taking drug S, where S can be any drug A,B or C
      ⍝ step1: calculate people remaining at the end of each year, that is , REMAINING ← START×(1-0.025)*12 (by subtracting the number of people who went into remission in a year)
      ⍝ step2: calculate TOTAL number of patients going into remission EACH YEAR, that is, REMISSION ← START - REMAINING
      ⍝ step3: calculate the number of patients  being dead at the end of EACH year, that is , DECEASED←0.0325×REMAINING
      ⍝ step4: now recalculate REMAINING at the end of the year as REMAINING ← REMAINING-DEAD
      ⍝ step5: now recursively run the projection again for the value REMAINING
      ⍝ the function at the end gives me the list of required values at the end of each year (1,2,3...)
      ⍝ after running the projection for N years,I first add each year's REMISSION values to get 'rem'
      ⍝ next I add the last Nth Year's REMAINING value with REMISSION to get TOTAL number of survivors
      ⍝ then I sum up all the DECEASED values to get 'dec'
     
     years a b c←⍺,⍵
     
     Yone←{r←⍵-s+d←r-s←(1-0.0325)×r←⍵×(1-0.025)*12 ⋄ (⍺-1)=0:(Rounding(s+r)r d) ⋄ (⍺-1)>0:(Rounding r r d)+(⍺-1)∇ s} ⍝ Dfn to run the Projection for 12 MONTHS
     
     (⊃END),+/¨1↓END←↓⍉↑years Yone¨a b c  ⍝ now I run the projection for each drug for N 'years' and then sum up to get sa, sb, sc, rem and dec values
 }

 Projection2←{
     ⍝ Stub function for 2017 APL Problem Solving Competition Healthcare Problem Set Problem 1, Task 2
     ⍝ Your code goes below...
     
          ⍝ ADVANTAGE OF THIS SOLUTION: able to run the projection for patients taking Drug A, Drug B and Drug C  seperately and INDEPENDENTLY
     
     years a b c←⍺,⍵
     
          ⍝ Now Consider this sentence from the question: "Patients start on drug A and move through B and on to C depending on their response (or lack of it)."
          ⍝ Therefore there are no NEW PATIENTS moving to Drug B for INITIAL 2 months
          ⍝ and similarly there will be NO NEW PATIENTS moving to Drug C for INITIAL 5 months
          ⍝ this is so because it will take time for the NEW PATIENTS to move from Drug A to Drug B and then to Drug C
     
          ⍝ NewA,NewB and NewC are vectors of uniformly distributed (random) number of new occurrences (around 1000 to 3000) every month for drug A,B and C respectively
          ⍝ and the new occurrences values for EVERY MONTH is considered from the END
     NewA←(12×years)Random 1000 3000
     NewB←(¯2↓NewA),2/0  ⍝ the two zeroes indicate that there are no new patients identified for initial 2 months
     NewC←(¯5↓NewA),5/0  ⍝ and similarly no new patients moving to Drug C for initial 5 months
     
          ⍝ remratA = remrateB = remrateC = 0.025 (MONTHLY remission rate expressed in fraction)
          ⍝ deathrateA = deathrateB = deathrateC = 0.0325 (YEARLY death rate  expressed in fraction)
     
          ⍝ Parameters of Drug A are:
          ⍝ respondA = 0.7×0.8= 70% 0f 80% of new patients
          ⍝ notA = 0.2 =  20 % of new patients
          ⍝ dropoutA = 0 (ZERO PERCENTAGE)
          ⍝ moveAtoB = 0.3×0.8= 30% of 80% of new patients (THEY MOVE AFTER 2 MONTHS)
     
          ⍝ Parameters of Drug B are:
          ⍝ respondB = 0.6×0.9×moveAtoB = 0.6×0.9×0.3×0.8 60% of 90 % of 30% of 80% of new patients
          ⍝ notA = 0 i.e ZERO (percentage of new patients who are not given drug B)
          ⍝ dropoutB = 0.1×moveAtoB = 0.1×0.3×0.8 = 10% of 30% of 80% of new patients
          ⍝ moveBtoC = 0.4×0.9×moveAtoB = 0.4×0.9×0.3×0.8 = 40% of 90 % of 30% of 80% of new patients
     
          ⍝ Parameters of Drug C are:
          ⍝ respondC = 0.8×moveBtoC = 0.8×0.4×0.9×moveAtoB = 0.8×0.4×0.9×0.3×0.8 = 80% of 40% of 90% of 30% 0f 80% of new patients
          ⍝ notC = 0 (ZERO PERCENT)
          ⍝ dropoutC = 0.2×moveBtoC = 0.2×0.4×0.9×moveAtoB = 0.2×0.4×0.9×0.3×0.8 = 20% of 40% of 90% of 30% of 80% of new patients
          ⍝ moveCtoNULL = 0 (NONE move from drug C to any other drug, therefore ZERO)
     
     
     ParametersA←⊂(a remrateA deathrateA respondA notA dropoutA moveAtoB NewA)←a,0.025,0.0325,(0.7×0.8),0.2,0,(0.3×0.8),⊂NewA
     ParametersB←⊂(b remrateB deathrateB respondB notB dropoutB moveBtoC NewB)←b,0.025,0.0325,(0.6×0.9×moveAtoB),0,(0.1×moveAtoB),(0.4×0.9×moveAtoB),⊂NewB
     ParametersC←⊂(c remrateC deathrateC respondC notC dropoutC moveCtoNULL NewC)←c,0.025,0.0325,(0.8×moveBtoC),0,(0.2×moveBtoC),0,⊂NewC
     
     (sa sb sc rem dec drop not)←(⊃END),+/¨1↓END←↓⍉↑years TrialForNYears¨(ParametersA,ParametersB,ParametersC) ⍝ now I run the projection for 'years' for each drug INDEPENDENT OF EACH OTHER
     
     Rounding(sa+moveAtoB×+/2↑NewA)(sb+moveBtoC×+/3↑NewB)sc rem dec drop not ⍝ account for the new paients who were identified in the last 2 or 3 months of the LAST YEAR because they DIDN'T MOVE OUT
     
     
 }

 Projection3←{
     ⍝ Stub function for 2017 APL Problem Solving Competition Healthcare Problem Set Problem 1, Task 3
     ⍝ Your code goes below...
     
     years drugs←⍺ ⍵
     ↑years Projection2¨↓drugs  ⍝ Run the function 'Projection2' for each country seperately and independently
 }

 Random←{
     ⍝ generates random number between a range
     ⍝ Left Argument: NumberOfValuesRequired
     ⍝ Right Argument: LowerLimit UpperLimit (both inclusive)
     (?⍺⍴1+--/⍵)+(⌊/⍵)-1
 }

 Rounding←{
      ⍝ Rounding function to round fractional numbers to integers
     ⌈⍵-0.5}

 RunFor12Months←{
      ⍝ this function ACCOUNTS for the NEW PATIENTS identified EACH MONTH
      ⍝ and returns the TOTAL number of people remaining and the TOTAL number of people who went into remission
     (sur remrate respond New)←⍵ ⋄ r←A-remaining←(A←sur+respond×⍺⌷New)×(1-remrate) ⋄ 0=⍺-1:(Rounding remaining r) ⋄ (Rounding 0 r)+(⍺-1)∇((Rounding remaining)remrate respond New)
 }

 TrialForNYears←{
     s remissp deathp resp notp drpp movp New←⍵   ⍝ 's' is initial number of patients, 'deathp' is deathrate, 'resp' is the respond percentage
                                                       ⍝ 'notp' is the percentage of patients who are not being treated, 'drpp' is the dropout percentage
                                                       ⍝ 'movp' is the relative percentage moving from one drug to another
                                                       ⍝ 'New' is the vector of new occurrences every month for that corresponding drug
     
     
     remaining remission←12 RunFor12Months s remissp resp New       ⍝ runs the projection in groups of 12 months (for 1 year) and returns the REMAINING number of patients and patients in REMISIION
     
     remaining-←dec←remaining×deathp   ⍝ recalculate REMAINING value after subtracting the number of deceased patients
     
     notTreated dropped←Rounding+/(notp drpp)∘.×(¯12↑New)  ⍝ calculate the number of people who are not treated and who have dropped out of the therapy
     
     (⍺-1)=0:Rounding(remaining+remission)remission dec dropped notTreated    ⍝ returns the required value
     
     (0(Rounding remission)dec dropped notTreated)+(⍺-1)∇(Rounding remaining)remissp deathp resp notp drpp movp(¯12↓New) ⍝ run the trial for N Years recursively
 }

:EndNamespace 
:Namespace math
(⎕IO ⎕ML ⎕WX)←1 0 3

 Cluster←{
     ⍝ Stub function for 2017 APL Problem Solving CompetitionMaths and Statistics Problem Set Problem 3, Task 3
     ⍝ Your code goes below...
     
     coords←⍵   ⍝ Coordinates
     
     SeedValForAbvTWO←{⍺>0:((⍺-1)∇(⍵,as)),⍨as←upd⌷⍨in←mat⍳d←⌈/mat←(⊂cMean ⍵)∘.dist(upd←coords~⍵) ⋄ ''}   ⍝ Selects 'k-2' initial centroids when the required number of clusters (i.e k) is more than 2
     
     Seeding←{k←⍵⌷⍨⊂(,(2/n)⍴⍳n←⍴⍵)/⍨,EucMat∊⌈/,EucMat←⍵∘.dist ⍵ ⋄ ⍺≤2:⍺↑⌽k ⋄ (⌽k),(⍺-2)SeedValForAbvTWO k}   ⍝ farthest neighbor approach for finding initial centroids using euclidean distance
     
     ClustMem←{(↓MinEucDist)⍳¨⌊/[2]MinEucDist←coords∘.dist ⍵}  ⍝ determines the cluster membership of each coordinates by allocating it to the cluster to which they are closest,
                                                                    ⍝ in terms of Euclidean distance to the cluster mean.
     
     Iteration←{New←ClustMem(2⌷⍉A[(⍋1⌷⍉A←{⍺ ⍵}⌸Old←⍵);])∘.{cMean(⊂⍺)⌷⍵}⊂coords ⋄ (⍺≥0)∨(0≥⍺-1)∨(Old≡New):New ⋄ ∇ New} ⍝ Syntax: MAXIMUM_ITERATIONS Iteration Initial_ClusterMembership_Vector
                                                                                                                           ⍝ In this Dfn 'Iterations', I compare each individual’s distance to its own cluster mean
                                                                                                                           ⍝ and to that of the opposite clusters and if it is smaller compared it's own  cluster mean,
                                                                                                                           ⍝ then it is relocated accordingly. This iterative relocation will continue from this new partition
                                                                                                                           ⍝ until no more relocations occur. However, I have put a LIMIT to the MAXIMUM ITERATIONS that can take place (i.e 50).
     
     50 Iteration ClustMem ⍺ Seeding coords      ⍝ now putting together all the 'Dfns' in one single statement, we can cluster the points and determine the cluster membership
 }

 IQR←{
     ⍝ Stub function for 2017 APL Problem Solving CompetitionMaths and Statistics Problem Set Problem 2, Task 1
     ⍝ Your code goes below...
     
     n←⌊0.5×⍴,⍵                                      ⍝ number to split ⍵ into lower half and upper half
     (median(n×¯1)↑a)-(median n↑a←⍵[⍋⍵])             ⍝ calculates Q3 and Q1, then calculates Q3-Q1
 }

 Outliers←{
     ⍝ Stub function for 2017 APL Problem Solving CompetitionMaths and Statistics Problem Set Problem 2, Task 2
     ⍝ Your code goes below...
     
     Q1 Q3←(median n↑a),(median(¯1×n←⌊0.5×⍴,⍵)↑a←⍵[⍋⍵])                ⍝ Q1: median of lower half
                                                                            ⍝ Q3: median of upper half
     
     IQ←IQR ⍵                                                          ⍝ calculates the InterQuartile Range of ⍵
     
     LIF LOF UIF UOF←(Q1-1.5×IQ)(Q3+1.5×IQ)(Q1-3×IQ)(Q3+3×IQ)          ⍝ LIF: Lower Inner Fence
                                                                            ⍝ LOF: Lower Outer Fence
                                                                            ⍝ UIF: Upper Inner Fence
                                                                            ⍝ UOF: Upper Outer Fence
     
     {(⍵<LIF)∨(⍵>LOF)∧(~⍵<UIF)∧(~⍵>UOF):1 ⋄ (⍵<UIF)∨(⍵>UOF):¯1 ⋄ 0}¨⍵  ⍝  checks each number in ⍵ for possible mild outlier and if not ,then checks for extreme outlier
 }

 SRound←{
     ⍝ Stub function for 2017 APL Problem Solving CompetitionMaths and Statistics Problem Set Problem 1, Task 2
     ⍝ Your code goes below...
     
         ⍝ An interesting solution to an interesting problem
         ⍝ No Loops involved and by simple mathematical tricks we can find the first s-round number greater than or equal to ⍵
     
     RemLeadingZeroes←{(∨\⍵≠'0')/⍵}                ⍝ Dfn to remove leading zeroes in cases such as '0.232' or '0.34' and make it '.232' and '.34'
     NinesCompliment←{{⍵='.':⍵ ⋄ ⊃⍕⍎'9','-',⍵}¨⍵}  ⍝ Dfn to find Nine's Compliment
     AdditiveIdentity←{{⍵='.':⍵ ⋄ '0'}¨⍵}          ⍝ Dfn to find simple additive identity of a numeric vector (Example additive identity of 32.334 is 00.000)
     
     
     ⍺≥SigFig⍕⍵:⍵     ⍝ if ⍵ is already an s-round, then output ⍵
     
     A←RemLeadingZeroes⍕⍵   ⍝ here I remove leading zeroes from ⍵ i.e in cases such as '0.232' or '0.444' or '0.78' and make it '.232' , '.444' , '.78'
     
     ⍎A,'+',('1',⍨¯1↓ID),'+',(t↑ID←AdditiveIdentity A),((t←⍺+'.'∊⍺↑A)↓NinesCompliment A)  ⍝ If you look at this statement from right to left, then following operations takes place:
                                                                                               ⍝ Step1: Find 9's Compliment of  ('number of significant digits in ⍵' - ⍺) digits from the last of ⍵
                                                                                               ⍝ Step2: to the additive identity of ⍵, add 1 to it from least significant digit i.e from the right ( Example: if 245.56 is a number then I get 000.01 from this step)
                                                                                               ⍝ Step3: add the results of Step 1 and Step 2 to get the ANSWER (thats it!)
 }

 SigFig←{
     ⍝ Stub function for 2017 APL Problem Solving CompetitionMaths and Statistics Problem Set Problem 1, Task 1
     ⍝ Your code goes below...
     
     0=⍴,⍵:0  ⍝ if ⍵ is empty, then there exists ZERO significant numbers
     
     ×/⍵∊0:1  ⍝ If ⍵ has neither decimal point, nor non-zero digits, then ONE ZERO, if any is PRESENT, is significant
     
     (+/(⌽∨\(⌽A))∧(∨\(A←A∊'123456789.')))-2×('.'∊A←A,⍕('.'∊A←⍕⍵))  ⍝ counts the number of significant figures according to the specified rules
 }

 cMean←{
     ⍝ Stub function for 2017 APL Problem Solving CompetitionMaths and Statistics Problem Set Problem 3, Task 2
     ⍝ Your code goes below...
     
      ⍝ Calculates the cluster mean of the coordinates represented by ⍵
      ⍝ Here ⎕DIV is set to 1 because I need the result of 0÷0 to be 0 and not 1
     ⎕DIV←1 ⋄ (⍴,⍵)÷⍨+/[1]↑,⍵
 }

 dist←{
     ⍝ Stub function for 2017 APL Problem Solving CompetitionMaths and Statistics Problem Set Problem 3, Task 1
     ⍝ Your code goes below...
     
      ⍝ Calculates the Euclidean Distance between ⍺ and ⍵
     0.5*⍨+/2*⍨⍺-⍵
 }

 median←{
     ⍝ finds the median of a numeric vector
     v←⍵[⍋⍵] ⋄ 0.5×v[⌈0.5×⍴v]+v[⌊1+0.5×⍴v]}

:EndNamespace 
:EndNamespace 
