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
{xAutos,vAutos,dAutos,viAutos},

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
(*Listen Autos mit Geschwingigkeiten v=0,1,2,3,4,5*)
viAutos=Table[Select[Table[vAutos[[n]],{n,1,nCar}],vAutos[[m]]==i],{i,0,5},{m,1,nCar}];
Print[viAutos];
]


vdhisto[7,30,15,5,0.3]


(*mittlere geschwindigkeit und varianz (siehe documentation) des mittlere abstand von alle runde i=0 -> i=tMax, dann beide plotten. letzte punnkt : die zusammen plotten und kommentieren)
