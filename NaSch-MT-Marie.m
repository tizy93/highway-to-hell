(* ::Package:: *)

(* ::Title:: *)
(*The highway to hell*)


(* ::Subtitle:: *)
(*Menschen machen seltsame Sachen*)


(* ::Text:: *)
(*Staus entwickeln sich aus dem Nichts. *)
(**)
(**)


(*Modul Nagel-Schreckenberg Modell*)
NaSch[nCar_,nCells_,tMax_,vMax_,p_]:=Module[
(*Eingabe Anzahl der Autos nCar, Anzahl der Zellen nCells, Simulationsdauer tMax, 
Maximalgeschwindigkeit vMax und Tr\[ODoubleDot]delwahrscheinlichkeit p mit Funktionsaufruf*)

(*lokale Variablen*)
{xAutos,vAutos,dAutos},

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
(** erzeugt eine zuf\[ADoubleDot]llige (Random) Liste (Position) "xAutos" ohne wiederholungen (Sample) und in aufsteigende Reihen sortiert (Sort)**)
vAutos=RandomInteger[{0,vMax},nCar];
(**zuordnet eine zuf\[ADoubleDot]llige Zahl zu jedes Auto **)

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++, (*Schleife der Runden bis tMax*)

(*Freie Zellen d vor dem Auto bis zum vorderen*)
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,1,nCar-1}]; (*In Schleife, damit es geupdatet wird*)
(**erzeugt der Abstand des Autos mit bedingung dass ein auto kleiner gleich des gr\[ODoubleDot]\[SZ]es xAutos ist, dann ergibt den abstand zwischen zwei Autos **)
(*Arrays fangen bei 1 an; n+1 muss f\[UDoubleDot]r das letzte gleich nCar sein; Abstand des letzten Autos ist bis zum ersten, da Ringstra\[SZ]e*)
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];
(**abstand zur n\[ADoubleDot]chste auto (dar\[UDoubleDot]ber bin ich nicht sicher)????**)
(*Abstand des Autos an Stelle nCar zum ersten wird angeh\[ADoubleDot]ngt;
d immer -1, da freie Zellen vor Auto gemeint sind*)

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,1,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,1,nCar}]; 
(**wird \[CapitalUDoubleDot]berpr\[UDoubleDot]ft, ob der Abstand zwischen ein Auto und die vordere gro\[SZ] genug ist **)

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
vAutos=Table[If[RandomReal[{0,1}]<=p,vAutos[[n]]=Max[vAutos[[n]]-1,0],vAutos[[n]]],{n,1,nCar}]; (*Tr\[ODoubleDot]deln solange noch nicht v=0*)
(**RandomReal erzeugt ein zuf\[ADoubleDot]llige zahl zwischen 0 und 1, falls das unter p=0,3 liegt dan tr\[ODoubleDot]delt das auto sonst nicht **)
(*Falls zuf\[ADoubleDot]llige Zahl nicht im gegebenen Intervall, bleibt v gleich*)

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,1,nCar}];

(*Falls Autos au\[SZ]erhalb Zellen bewegt, wird Bewegung in erster Zelle fortgesetzt, da Ringstra\[SZ]e*)

(*
Print[xAutos];
Print[vAutos];
Print[dAutos];
Print[density];
*)
]
]


NaSch[10,30,5,5,0.3]


(* ::Text:: *)
(*Im Dichteplot wird die Anzahl der Autos N \[UDoubleDot]ber einer Anzahl avCells Zellen berechnet und \[UDoubleDot]ber der Zeit t dargestellt.*)


(*Dichteplot \[UDoubleDot]ber Zeit*)
densityplot[nCar_,nCells_,tMax_,vMax_,p_,avCells_]:=Module[
(*lokale Variablen*)
{xAutos,vAutos,dAutos,regionCars,density},

(*NaSch-Modell*)

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
vAutos=RandomInteger[{0,vMax},nCar]; 

(*Erzeugen einelementige Liste mit Dichte*) 
density=Table[Nothing,{n,1}];

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++, 

(*Freie Zellen d vor dem Auto bis zum vorderen*)
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,1,nCar-1}]; 
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,1,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,1,nCar}]; 

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
vAutos=Table[If[RandomReal[{0,1}]<=p,vAutos[[n]]=Max[vAutos[[n]]-1,0],vAutos[[n]]],{n,1,nCar}]; 

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,1,nCar}];

(*Erstellen Liste mit Autos innerhalb vorgegebener Region von der ersten Zelle bis zur frei w\[ADoubleDot]hlbaren Zelle avCells*)
regionCars=DeleteCases[Table[If[xAutos[[n]]<=avCells,xAutos[[n]],],{n,1,nCar}],Null]; (*Nullen mit DeleteCases gel\[ODoubleDot]scht*)
(*Element zu density-Liste mit Dichte am Zeitpunkt t hinzugef\[UDoubleDot]gt, t entspricht Reihenfolge der Liste*)
AppendTo[density,Length[regionCars]/avCells];
(*Ausgabe xAutos*)
(*
Print[xAutos]; (*Print kostet viel Rechenzeit*)
*)
]
(*Plotten Dichte*)
Delete[density,1]; (*L\[ODoubleDot]schen erstes Nullelement*)
(*Print[density];*)
ListPlot[density]
]



densityplot[7,30,15,5,0.3,10]
(*Table[density[[t]],{t,1,15}]*)


(*Histogramme Geschwindigkeiten und Abstand f\[UDoubleDot]r einen Zeitpunkt*)
vdhisto[nCar_,nCells_,tMax_,vMax_,p_]:=Module[
(*tMax ist betrachteter Zeitpunkt*)

(*lokale Variablen*)
{xAutos,vAutos,dAutos,viAutos,diAutos,i},

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
(** erzeugt eine zuf\[ADoubleDot]llige (Random) Liste (Position) "xAutos" ohne wiederholungen (Sample) und in aufsteigende Reihen sortiert (Sort)**)
vAutos=RandomInteger[{0,vMax},nCar];
(**zuordnet eine zuf\[ADoubleDot]llige Zahl zu jedes Auto **)

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++, (*Schleife der Runden bis tMax*)

(*Freie Zellen d vor dem Auto bis zum vorderen*)
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,1,nCar-1}]; (*In Schleife, damit es geupdatet wird*)
(**erzeugt der Abstand des Autos mit bedingung dass ein auto kleiner gleich des gr\[ODoubleDot]\[SZ]es xAutos ist, dann ergibt den abstand zwischen zwei Autos **)
(*Arrays fangen bei 1 an; n+1 muss f\[UDoubleDot]r das letzte gleich nCar sein; Abstand des letzten Autos ist bis zum ersten, da Ringstra\[SZ]e*)
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];
(*Abstand des Autos an Stelle nCar zum ersten wird angeh\[ADoubleDot]ngt;
d immer -1, da freie Zellen vor Auto gemeint sind*)

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,1,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,1,nCar}]; 
(**wird \[CapitalUDoubleDot]berpr\[UDoubleDot]ft, ob der Abstand zwischen ein Auto und die vordere gro\[SZ] genug ist **)

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
vAutos=Table[If[RandomReal[{0,1}]<=p,vAutos[[n]]=Max[vAutos[[n]]-1,0],vAutos[[n]]],{n,1,nCar}]; (*Tr\[ODoubleDot]deln solange noch nicht v=0*)
(**RandomReal erzeugt ein zuf\[ADoubleDot]llige zahl zwischen 0 und 1, falls das unter p=0,3 liegt dan tr\[ODoubleDot]delt das auto sonst nicht **)
(*Falls zuf\[ADoubleDot]llige Zahl nicht im gegebenen Intervall, bleibt v gleich*)

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,1,nCar}];
(*Falls Autos au\[SZ]erhalb Zellen bewegt, wird Bewegung in erster Zelle fortgesetzt, da Ringstra\[SZ]e*)
]
(*Listen Autos mit Geschwindigkeiten v=0,1,2,3,4,5*)
Clear[viAutos];
viAutos=Table[Select[Table[vAutos[[n]],{n,1,nCar}],#==i &],{i,0,5}];

(*Listen Abst\[ADoubleDot]nde d=0,1,...,nCar*)
Clear[diAutos];
diAutos=Select[Table[Select[Table[dAutos[[n]],{n,1,nCar}],#==i &],{i,0,nCells-nCar-1}],UnsameQ[#, {}] &]; (*Maximaler Abstand ist nCells-nCar-1, falls alle anderen Autos d=0 voneinander*)
(*L\[ODoubleDot]schen der Abst\[ADoubleDot]nde, die nicht vorkommen*)

Histogram[viAutos,{1},AxesLabel->{v,Anzahl Autos mit Indexed[v,"i"]},ColorFunction->"Pastel",ImageSize->Medium]
Histogram[diAutos,{1},AxesLabel->{d,Anzahl Autos mit Indexed[d,"i"]},ColorFunction->"Pastel",ImageSize->Medium]
(*Histogramm z\[ADoubleDot]hlt, wie oft eine Zahl in einer Liste und den Sublisten darin vorkommt*)
]


vdhisto[100,300,15,5,0.3]


(*Velocity-Dependent-Randomization Modell*)
vdrNaSch[nCar_,nCells_,tMax_,vMax_,p_]:=Module[

(*lokale Variablen*)
{xAutos,vAutos,dAutos},

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
(** erzeugt eine zuf\[ADoubleDot]llige (Random) Liste (Position) "xAutos" ohne wiederholungen (Sample) und in aufsteigende Reihen sortiert (Sort)**)
vAutos=RandomInteger[{0,vMax},nCar];
(**zuordnet eine zuf\[ADoubleDot]llige Zahl zu jedes Auto **)

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++, (*Schleife der Runden bis tMax*)

(*Freie Zellen d vor dem Auto bis zum vorderen*)
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,1,nCar-1}];
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,1,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,1,nCar}]; 

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
vAutos=Table[If[vAutos[[n]]==0,If[RandomReal[{0,1}]<=p+0.1,vAutos[[n]],vAutos[[n]]],If[RandomReal[{0,1}]<=p,vAutos[[n]]=vAutos[[n]]-1,vAutos[[n]]]],{n,1,nCar}];
(*Wenn Auto steht ist p um 0.1 erh\[ODoubleDot]ht*)

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,1,nCar}];

(*VDR: beim Anfahren nur mit einer Wahrscheinlichkeit p+q anfahren, hier q=0.1 gew\[ADoubleDot]hlt -> lieber Eingabe?*)
]
]


(*Berechnung mittlere v \[UDoubleDot]ber t, Varianz des mittleren Abstands und des Verkehrsflusses \[UDoubleDot]ber t*)
Meanvarfluss[nCar_,nCells_,tMax_,vMax_,p_,posCell_]:=Module[
(*lokale Variablen*)
{xAutos, vAutos, dAutos, vMittel, dVar,t,dMittel,m,fluss,regionCars},

(*NaSch-Modell*)

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
vAutos=RandomInteger[{0,vMax},nCar]; 

(*Erzeugen einelementige Liste mit vMittel, dVar und fluss*) 
Clear[vMittel];
vMittel = Table [Nothing, {t,1}];
dVar = Table[Nothing, {t,1}];
fluss=Table[Nothing,{n,1}];

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++, 

(*Freie Zellen d vor dem Auto bis zum vorderen*)
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,1,nCar-1}]; 
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,1,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,1,nCar}]; 

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
vAutos=Table[If[RandomReal[{0,1}]<=p,vAutos[[n]]=Max[vAutos[[n]]-1,0],vAutos[[n]]],{n,1,nCar}]; 

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,1,nCar}];

(*mittlere geschwindigkeit*)
AppendTo[vMittel, N[Mean[vAutos],6]];
(*Varianz dAutos*)
AppendTo[dVar, N[Variance[dAutos],6]];
(*Verkehrsfluss*)
regionCars=DeleteCases[Table[If[xAutos[[n]]==posCell,xAutos[[n]],],{n,1,nCar}],Null];
AppendTo[fluss,Length[regionCars]];
]
Print[vMittel];
Print[dVar];
Print[fluss];
ListPlot[vMittel,ImageSize->Medium,ColorFunction->"Rainbow",AxesLabel->{"Zeit t",mittlere Geschwindigkeit OverBar[v]}]
ListPlot[dVar,ImageSize->Medium,ColorFunction->"Rainbow",AxesLabel->{"Zeit t",Varianz des Abstands d}]
ListPlot[fluss,ImageSize->Medium,ColorFunction->"Rainbow",AxesLabel->{"Zeit t","Fluss \[UDoubleDot]ber Zelle"}]
]


Meanvarfluss[30,100,20,5,0.5,4]




(*Fundamentalplot*)
FundamentalD[nCar_,nCells_,tMax_,vMax_,p_,avCells_]:=Module[
(*lokale Variablen*)
{xAutos, vAutos, dAutos, dMittel, fluss, regionCars, cellCars, density, fundamD,mittelFluss,tMittelF,tcellCars,tfluss},

(*NaSch-Modell*)

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
vAutos=RandomInteger[{0,vMax},nCar]; 

(*Erzeugen einelementige Liste mit Dichte, Fluss, dem Mittelwert des Flusses und des zeitlichen Mittel des Flusses*) 
density=Table[Nothing,{n,1}];
fluss=Table[Nothing,{n,1}];
mittelFluss=Table[Nothing,{n,1}];
tfluss=Table[Nothing,{n,1}];

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++, 

(*Freie Zellen d vor dem Auto bis zum vorderen*)
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,1,nCar-1}]; 
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,1,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,1,nCar}]; 

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
vAutos=Table[If[RandomReal[{0,1}]<=p,vAutos[[n]]=Max[vAutos[[n]]-1,0],vAutos[[n]]],{n,1,nCar}]; 

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,1,nCar}];

(*Versuch zeitliches Mittel des Flusses*)
tcellCars=DeleteCases[Table[If[xAutos[[n]]==avCells,xAutos[[n]],],{n,1,nCar}],Null];
AppendTo[tfluss,Length[cellCars]];

(*Verkehrsfluss gemittelt \[UDoubleDot]ber Zellen 1 bis avCells*)
cellCars=Select[DeleteCases[Table[DeleteCases[Table[If[xAutos[[n]]==m,xAutos[[n]],],{n,1,nCar}],Null],{m,1,avCells}],Null],UnsameQ[#,{}]&];
AppendTo[fluss,Length[cellCars]];
AppendTo[mittelFluss, Mean[fluss]];

(*Dichte von Zellen 1 bis avCells*)
regionCars=DeleteCases[Table[If[xAutos[[n]]<=avCells,xAutos[[n]],],{n,1,nCar}],Null]; (*Nullen mit DeleteCases gel\[ODoubleDot]scht*)
AppendTo[density,Length[regionCars]/avCells];

](*tMittelF=Mean[tfluss];
Print[tMittelF];*)
Print[fluss];
Print[mittelFluss];
Print[density];
(*Print[Thread[{density,mittelFluss}]];*)
ListPlot[density,ImageSize->Medium,AxesLabel->Dichte]
ListPlot[mittelFluss,ImageSize->Medium,AxesLabel->Mittelfluss]
ListPlot[Thread[{density,mittelFluss}],ImageSize->Medium,AxesLabel->{Dichte,Mittelfluss}]
]


FundamentalD[30,100,20,10,0.3,10]



