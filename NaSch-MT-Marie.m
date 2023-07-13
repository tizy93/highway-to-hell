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
(*erzeugt zuf\[ADoubleDot]llige (Random) Liste xAutos ohne Wiederholungen (Sample) in aufsteigender Reihenfolge (Sort)*)
vAutos=RandomInteger[{0,vMax},nCar];
(*ordnet jedem Auto eine zuf\[ADoubleDot]llige Geschwindigkeit von 0 bis vMax zu*)
(*Einzelne Autos sind gekennzeichnet durch Element-Position in der Liste mit Position xAutos[[n]] und Geschwindigkeit vAutos[[n]]*)

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++, (*Schleife der Runden bis tMax*)

(*Freie Zellen d vor dem Auto bis zum vorderen*)
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,nCar-1}]; (*In Schleife, damit es geupdatet wird*)
(*berechnet freie Zellen zum vorderen Auto normal au\[SZ]er f\[UDoubleDot]r Autos au\[SZ]er dem mit h\[ODoubleDot]chster Positition - da Ringstra\[SZ]e sind die freien Zellen geringer als xAutos[[n+1]]-xAutos[[n]]-1*)
(*Arrays starten mit Element 1; n+1 muss f\[UDoubleDot]r das letzte gleich nCar sein*)
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];
(*Abstand des Autos an letzter Stelle in Liste zum ersten wird angeh\[ADoubleDot]ngt*)

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,nCar}]; 

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
vAutos=Table[If[RandomReal[{0,1}]<=p,vAutos[[n]]=Max[vAutos[[n]]-1,0],vAutos[[n]]],{n,nCar}]; (*Tr\[ODoubleDot]deln solange noch nicht v=0*)
(*Falls zuf\[ADoubleDot]llige Zahl nicht im gegebenen Intervall, bleibt v gleich*)

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,nCar}];
(*Falls Autos au\[SZ]erhalb Zellen bewegt, wird Bewegung in erster Zelle fortgesetzt, da Ringstra\[SZ]e*)
(*Aktualisieren der Positionen nicht anhand Verschieben der Autos innerhalb der Liste, sondern durch Ver\[ADoubleDot]ndern der Eintr\[ADoubleDot]ge in xAutos*)

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
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,nCar-1}]; 
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,nCar}]; 

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
vAutos=Table[If[RandomReal[{0,1}]<=p,vAutos[[n]]=Max[vAutos[[n]]-1,0],vAutos[[n]]],{n,nCar}]; 

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,nCar}];

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
ListPlot[density,ImageSize->Medium,ColorFunction->"Rainbow",Frame->True,FrameLabel->{"Zeit t","Dichte \[Rho]"}]
]



densityplot[7,30,15,5,0.3,10]
(*Table[density[[t]],{t,1,15}]*)


(*Histogramme Geschwindigkeiten und Abstand f\[UDoubleDot]r einen Zeitpunkt*)
vdhisto[nCar_,nCells_,tMax_,vMax_,p_]:=Module[
(*tMax ist betrachteter Zeitpunkt*)

(*lokale Variablen*)
{xAutos,vAutos,dAutos,viAutos,diAutos,i},

(*NaSch-Modell*)

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
vAutos=RandomInteger[{0,vMax},nCar];

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++,

(*Freie Zellen d vor dem Auto bis zum vorderen*)
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,nCar-1}]; 
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,nCar}]; 

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
vAutos=Table[If[RandomReal[{0,1}]<=p,vAutos[[n]]=Max[vAutos[[n]]-1,0],vAutos[[n]]],{n,nCar}]; 

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,nCar}];
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


vdhisto[300,300,15,5,0.3]


(*Berechnung mittlere v \[UDoubleDot]ber t, Varianz des mittleren Abstands und des Verkehrsflusses \[UDoubleDot]ber t*)
Meanvarfluss[nCar_,nCells_,tMax_,vMax_,p_]:=Module[
(*betrachtete Zelle ist letzte Zelle der Stra\[SZ]e*)

(*lokale Variablen*)
{xAutos,vAutos,dAutos,vMittel,dVar,savefluss,savexAutos,m},

(*NaSch-Modell*)

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
vAutos=RandomInteger[{0,vMax},nCar]; 

(*Erzeugen einelementige Liste mit vMittel, dVar und fluss*) 
Clear[vMittel];
vMittel=Table[Nothing,{n,1}];
dVar=Table[Nothing,{n,1}];
savefluss=Table[Nothing,{n,1}];

(*Liste zum Speichern von xAutos aus dem vorherigen Zeitschritt*)
savexAutos=xAutos;
(*Index zum \[CapitalUDoubleDot]berpr\[UDoubleDot]fen der Positionen, startet von der \[UDoubleDot]berpr\[UDoubleDot]ften Zelle nCells*)(*Fluss als Durchfluss von Position nCells zu 1*)
m=nCar;

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++, 

(*Freie Zellen d vor dem Auto bis zum vorderen*)
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,nCar-1}]; 
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,nCar}]; 

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
vAutos=Table[If[RandomReal[{0,1}]<=p,vAutos[[n]]=Max[vAutos[[n]]-1,0],vAutos[[n]]],{n,nCar}]; 

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,nCar}];

(*Verkehrsfluss durch letzte Zelle*)
(*\[CapitalUDoubleDot]berpr\[UDoubleDot]ft, ob Auto mit h\[ODoubleDot]chster Position \[UDoubleDot]ber Stra\[SZ]enende hinaus auf den Anfang zur\[UDoubleDot]ck gefahren ist 
ja: n\[ADoubleDot]chst niedrigeres Auto wird betrachtet + fluss 1 hinzuf\[UDoubleDot]gen, 
nein: Auto wird weiter betrachtet + fluss 0 hinzuf\[UDoubleDot]gen
Index m geht Autos vom letzten Element bis zum ersten durch, danach wieder Start bei letztem*)
If[m==0,m=nCar,m=m];
If[xAutos[[m]]<savexAutos[[m]],
AppendTo[savefluss,1];
m=m-1;,
AppendTo[savefluss,0];
]
Clear[savexAutos];
savexAutos=xAutos;

(*mittlere Geschwindigkeit*)
AppendTo[vMittel, N[Mean[vAutos],6]];
(*Varianz dAutos*)
AppendTo[dVar, N[Variance[dAutos],6]];

];
ResourceFunction["PlotGrid"][{
{ListPlot[vMittel,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"mittlere Geschwindigkeit" OverBar[v]}]},
{ListPlot[dVar,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Varianz des Abstands d"}]},
{ListPlot[savefluss,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,Fluss pro Zeitschritt}]}
},
ImageSize->Large,FrameLabel->{"Zeit t",None}
]

(*Korrelation mittlere Geschwindigkeit und Varianz des Abstands*)
ListPlot[Thread[{dVar,vMittel}],ImageSize->Medium,ColorFunction->"Rainbow",Frame->True,FrameLabel->{"Varianz des Abstands d","mittlere Geschwindigkeit" OverBar[v]}]
]


Meanvarfluss[10,40,50,5,0.15]


(*Fundamentalplot*)
FundamentalD[nCells_,tMax_,vMax_,p_]:=Module[
(*Fluss wird f\[UDoubleDot]r Dichten 0 bis 1 berechnet*)

(*lokale Variablen*)
{xAutos,vAutos,dAutos,nCar,density,addfluss,savexAutos,m,fluss},

(*Erzeugen einelementige Liste mit Dichte, Fluss, und Fluss f\[UDoubleDot]r jede Anzahl an Autos*) 
density=Table[Nothing,{n,1}];
fluss=Table[Nothing,{n,1}];

(*NaSch-Modell*)

(*Schleife f\[UDoubleDot]r ansteigende Dichte/Anzahlen an Autos*)
For[nCar=0,nCar<=nCells,nCar++,

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
vAutos=RandomInteger[{0,vMax},nCar]; 

(*Liste zum Speichern von xAutos aus dem vorherigen Zeitschritt*)
savexAutos=xAutos;
(*Index zum \[CapitalUDoubleDot]berpr\[UDoubleDot]fen der Positionen, startet von der \[UDoubleDot]berpr\[UDoubleDot]ften Zelle nCells*)(*Fluss als Durchfluss von Position nCells zu 1*)
m=nCar;
addfluss=0;

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++, 

(*Freie Zellen d vor dem Auto bis zum vorderen*)
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,nCar-1}]; 
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,nCar}]; 

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
vAutos=Table[If[RandomReal[{0,1}]<=p,vAutos[[n]]=Max[vAutos[[n]]-1,0],vAutos[[n]]],{n,nCar}]; 

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,nCar}];

(*Verkehrsfluss durch letzte Zelle -> Anzahl Autos durch Zelle pro Zeitschritt*)
If[m==0,m=nCar,m=m];
If[xAutos[[m]]<savexAutos[[m]],
addfluss=addfluss+1;
m=m-1;,
addfluss=addfluss;
]
Clear[savexAutos];
savexAutos=xAutos;
]

(*Dichte \[UDoubleDot]ber die gesamte Stra\[SZ]e*)
AppendTo[density,nCar/nCells];

(*Verkehrsfluss f\[UDoubleDot]r Dichte nCar/nCells*);
AppendTo[fluss,addfluss];
]
(*Fehlend: f\[UDoubleDot]r verschiedene p -> im Aufruf selbst*)
(*Fundamentalplot mit addfluss*);
ListPlot[Thread[{density,fluss/tMax}],ImageSize->Medium,Frame->True,FrameLabel->{"Dichte \[Rho]","Zeitliches Mittel des Flusses \[UDoubleDot]ber letzte Zelle"},
PlotStyle->RandomChoice[{Red,Orange,Yellow,LightGreen,LightBlue,Blue,Purple,Pink}]] (*Punkte hell f\[UDoubleDot]r dunklen Hintergrund in sp\[ADoubleDot]terem Notebook*)
]


FundamentalD[100,20,5,0]
(*FundamentalD[100,20,5,0.1]
FundamentalD[100,20,5,0.15]
FundamentalD[100,20,5,0.2]
FundamentalD[100,20,5,0.25]
FundamentalD[100,20,5,0.3]
FundamentalD[100,20,5,0.5]
FundamentalD[100,20,5,0.75]
FundamentalD[100,20,5,1]*)


(*Fundamentalplots des Velocity-Dependent-Randomization Modells*)
vdrNaSch[nCells_,tMax_,vMax_,p_,q_]:=Module[
(*q ist zus\[ADoubleDot]tzliche Wahrscheinlichkeit zum Tr\[ODoubleDot]deln beim Anfahren*)

(*lokale Variablen*)
{xAutos,vAutos,dAutos,nCar,density,addfluss,savexAutos,m,fluss},

(*Erzeugen einelementige Liste mit Dichte, Fluss, und Fluss f\[UDoubleDot]r jede Anzahl an Autos*) 
density=Table[Nothing,{n,1}];
fluss=Table[Nothing,{n,1}];

(*Schleife f\[UDoubleDot]r ansteigende Dichte/Anzahlen an Autos*)
For[nCar=0,nCar<=nCells,nCar++,

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
vAutos=RandomInteger[{0,vMax},nCar];

(*Liste zum Speichern von xAutos aus dem vorherigen Zeitschritt*)
savexAutos=xAutos;
(*Index zum \[CapitalUDoubleDot]berpr\[UDoubleDot]fen der Positionen, startet von der \[UDoubleDot]berpr\[UDoubleDot]ften Zelle nCells*)(*Fluss als Durchfluss von Position nCells zu 1*)
m=nCar;
addfluss=0;

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++, 

(*Freie Zellen d vor dem Auto bis zum vorderen*)
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,nCar-1}];
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,nCar}]; 

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)(*Randomization, q=p0-p, p+q wahrscheinlichkeit dass sie tr\[ODoubleDot]deln falls die vorher v=0 hatten, p=wahrsch. v>0*)
vAutos=Table[If[vAutos[[n]]==0,If[RandomReal[{0,1}]<=p+q,vAutos[[n]],vAutos[[n]]],If[RandomReal[{0,1}]<=p,vAutos[[n]]=vAutos[[n]]-1,vAutos[[n]]]],{n,nCar}];
(*Wenn Auto steht ist p um q erh\[ODoubleDot]ht*)

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,nCar}];

(*VDR: beim Anfahren nur mit einer Wahrscheinlichkeit p+q anfahren -> lieber im Modul festlegen?*)

(*Verkehrsfluss durch letzte Zelle -> Anzahl Autos durch Zelle pro Zeitschritt*)
If[m==0,m=nCar,m=m];
If[xAutos[[m]]<savexAutos[[m]],
addfluss=addfluss+1;
m=m-1;,
addfluss=addfluss;
]
Clear[savexAutos];
savexAutos=xAutos;
]
(*Dichte \[UDoubleDot]ber die gesamte Stra\[SZ]e*)
AppendTo[density,nCar/nCells];

(*Verkehrsfluss f\[UDoubleDot]r Dichte nCar/nCells*);
AppendTo[fluss,addfluss];
]
(*Fehlend: f\[UDoubleDot]r verschiedene p -> im Aufruf selbst*)
(*Fundamentalplot mit addfluss*);
ListPlot[Thread[{density,fluss/tMax}],ImageSize->Medium,Frame->True,FrameLabel->{"Dichte \[Rho]","Zeitliches Mittel des Flusses \[UDoubleDot]ber letzte Zelle"},
PlotStyle->RandomChoice[{Red,Orange,Yellow,LightGreen,LightBlue,Blue,Purple,Pink}]] (*Hell f\[UDoubleDot]r dunklen Hintergrund in sp\[ADoubleDot]terem Notebook*)
]


vdrNaSch[70,20,5,0.15,0.1]


(*Modell zwei Fahrspuren*)
twolanesNaSch[nCells_,tMax_,vMax_,p_]:=Module[
(*nCells ist L\[ADoubleDot]nge beider Spuren, nCar Anzahl aller Autos f\[UDoubleDot]r das Histogramm von v *)
(*F\[UDoubleDot]r Fundamentalplots eine nCells!=0mod5 eingeben, f\[UDoubleDot]r Histogramme nCells=0mod5*)

(*lokale Variablen*)
{xAutos,xAutos1,xAutos2,vAutos1,vAutos2,dAutos1,dAutos2,viAutos,viAutos1,viAutos2,diAutos,diAutos1,diAutos2,nCar,density,density1,density2,
fluss,addfluss,savefluss,savefluss1,savefluss2,savexAutos1,savexAutos2,m1,m2,l1,l2,vMittel,dVar1,dVar2,i},

(*Erzeugen einelementige Liste mit Gesamtdichte und Fluss f\[UDoubleDot]r jede Anzahl an Autos*) 
density=Table[Nothing,{n,1}];
fluss=Table[Nothing,{n,1}];

(*NaSch-Modell*)

(*Schleife f\[UDoubleDot]r ansteigende Dichte/Anzahlen an Autos*)
For[nCar=1,nCar<=nCells,nCar++,

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
(*Erstellen Liste xAutos mit doppelter L\[ADoubleDot]nge als Stra\[SZ]e*)
xAutos=Sort[RandomSample[Range[2 nCells],nCar]];
(*xAutos1=Sort[RandomSample[Range[nCells],nCar]];
xAutos2=Sort[RandomSample[Range[nCells],nCar]];*)
(*Aufteilen Liste in zwei Spuren, 1 rechts, 2 links*)
xAutos1=Select[xAutos,#<=nCells &];
(*Positionen der linken Spur setzen in Bereich von 0 bis nCells*)
If[UnsameQ[xAutos1,{}],xAutos2=Drop[Table[xAutos[[n]]-nCells,{n,nCar}],Length[xAutos1]],xAutos2=xAutos]; (*Problem beim Zugriff auf Liste xAutos -> Table Befehl nicht richtig?*)
(*Geschwindigkeiten f\[UDoubleDot]r alle Autos getrennt auf den Spuren*)
vAutos1=RandomInteger[{0,vMax},Length[xAutos1]];
vAutos2=RandomInteger[{0,vMax},Length[xAutos2]];

(*Listen zum Speichern von der mittleren v, der Varianz von d und der Dichte der jeweiligen Spur \[UDoubleDot]ber t*)
vMittel=Table[Nothing,{n,1}];
dVar1=Table[Nothing,{n,1}];
dVar2=Table[Nothing,{n,1}];
density1=Table[Nothing,{n,1}];
density2=Table[Nothing,{n,1}];

(*Liste zum Speichern von xAutos aus dem vorherigen Zeitschritt*)
savexAutos1=xAutos1;
savexAutos2=xAutos2;
(*Index zum \[CapitalUDoubleDot]berpr\[UDoubleDot]fen der Positionen, startet von der \[UDoubleDot]berpr\[UDoubleDot]ften Zelle nCells*)(*addfluss als Durchfluss von Position nCells zu 1 aufsummiert, savefluss f\[UDoubleDot]r jeden Zeitschritt*)
addfluss=0;
m1=Length[xAutos1];
m2=Length[xAutos2];


savefluss1=Table[Nothing,{n,1}];
savefluss2=Table[Nothing,{n,1}];
l1=Length[xAutos1];
l2=Length[xAutos2];

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++, 
(*Rechte Spur*)
If[Length[xAutos1]>0 && Length[savexAutos1]>0,
  ((*Freie Zellen d vor dem Auto bis zum vorderen*)
  If[Length[xAutos1]>1,
  (dAutos1=Table[If[xAutos1[[n]]<Max[xAutos1],xAutos1[[n+1]]-xAutos1[[n]]-1,nCells-xAutos1[[n]]+Min[xAutos1]-1],{n,Length[xAutos1]-1}];
  AppendTo[dAutos1,If[xAutos1[[nCar]]<Max[xAutos1],xAutos1[[1]]-xAutos1[[Length[xAutos1]]]-1,nCells-xAutos1[[Length[xAutos1]]]+Min[xAutos1]-1]];),
  dAutos1={nCells-1};
  ]; 
  
  (*R1: Beschleunigen, falls vMax noch nicht erreicht*)
  vAutos1=Table[Min[vAutos1[[n]]+1,vMax],{n,Length[xAutos1]}];
  
  (*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
  vAutos1=Table[Min[dAutos1[[n]],vAutos1[[n]]],{n,Length[xAutos1]}];
  
  (*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
  vAutos1=Table[If[RandomReal[{0,1}]<=p,vAutos1[[n]]=Max[vAutos1[[n]]-1,0],vAutos1[[n]]],{n,Length[xAutos1]}]; 
  
  (*R4: Fahren um vAutos Zellen*)
  xAutos1=Table[If[xAutos1[[n]]+vAutos1[[n]]<=nCells,xAutos1[[n]]=xAutos1[[n]]+vAutos1[[n]],xAutos1[[n]]=xAutos1[[n]]+vAutos1[[n]]-nCells],{n,Length[xAutos1]}];
  
  (*Verkehrsfluss durch letzte Zelle -> Anzahl Autos durch Zelle pro Zeitschritt*)
  If[m1==0,m1=Length[xAutos1],m1=m1];
  If[m1>0 && xAutos1[[m1]]<savexAutos1[[m1]],
  (addfluss=addfluss+1;
  m1=m1-1;),
  addfluss=addfluss;
  ];
  Clear[savexAutos1];
  savexAutos1=xAutos1;
  
  (*Gibt mittlere v, Varianz von d, Fluss und Dichte der Spur \[UDoubleDot]ber t f\[UDoubleDot]r 4 Gesamtdichten aus*)
  If[MemberQ[Table[n/5 nCells,{n,4}],nCar],
  
  (*Varianz des Abstands*)
  AppendTo[dVar1,N[Variance[dAutos1],6]];
  
  (*Fluss durch letzte Zelle rechter Spur*)
  If[l1==0,l1=Length[xAutos1],l1=l1];
  If[l1>0 && xAutos1[[l1]]<savexAutos1[[l1]],
  (AppendTo[savefluss1,1];
  l1=l1-1;),
  AppendTo[savefluss1,0];
  ];
  ];
  (*Dichten der Spur \[UDoubleDot]ber t*)
  AppendTo[density1,Length[xAutos1]/nCells];
  ),
  (addfluss=addfluss;
  AppendTo[savefluss1,0];
  AppendTo[density1,0];
  )
  ];
  
  (*Linke Spur*)
  If[Length[xAutos2]>0 && Length[savexAutos2]>0,
  ((*Freie Zellen d vor dem Auto bis zum vorderen*)
  If[Length[xAutos2]>1,
  (dAutos2=Table[If[xAutos2[[n]]<Max[xAutos2],xAutos2[[n+1]]-xAutos2[[n]]-1,nCells-xAutos2[[n]]+Min[xAutos2]-1],{n,Length[xAutos2]-1}]; 
  AppendTo[dAutos2,If[xAutos2[[nCar]]<Max[xAutos2],xAutos2[[1]]-xAutos2[[Length[xAutos2]]]-1,nCells-xAutos2[[Length[xAutos2]]]+Min[xAutos2]-1]];),
  dAutos2={nCells-1};
  ];
  
  (*R1: Beschleunigen, falls vMax noch nicht erreicht*)
  vAutos2=Table[Min[vAutos2[[n]]+1,vMax],{n,Length[xAutos2]}];
  
  (*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
  vAutos2=Table[Min[dAutos2[[n]],vAutos2[[n]]],{n,Length[xAutos2]}];
   
  (*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
  vAutos2=Table[If[RandomReal[{0,1}]<=p,vAutos2[[n]]=Max[vAutos2[[n]]-1,0],vAutos2[[n]]],{n,Length[xAutos2]}];  
  
  (*R4: Fahren um vAutos Zellen*)
  xAutos2=Table[If[xAutos2[[n]]+vAutos2[[n]]<=nCells,xAutos2[[n]]=xAutos2[[n]]+vAutos2[[n]],xAutos2[[n]]=xAutos2[[n]]+vAutos2[[n]]-nCells],{n,Length[xAutos2]}];
  
  (*Verkehrsfluss durch letzte Zelle -> Anzahl Autos durch Zelle pro Zeitschritt*)
  If[m2==0,m2=Length[xAutos2],m2=m2];
  If[m2>0 && xAutos2[[m2]]<savexAutos2[[m2]],
  (addfluss=addfluss+1;
  m2=m2-1;),
  addfluss=addfluss;
  ];
  Clear[savexAutos2];
  savexAutos2=xAutos2;
  
  (*Gibt mittlere v, Varianz von d, Fluss und Dichte der Spur \[UDoubleDot]ber t f\[UDoubleDot]r 4 Gesamtdichten aus*)
  If[MemberQ[Table[n/5 nCells,{n,4}],nCar],
  (*Varianz des Abstands*)
  AppendTo[dVar2,N[Variance[dAutos2],6]]; 
  
  (*Fluss durch letzte Zelle*)
  If[l2==0,l2=Length[xAutos2],l2=l2];
  If[l2>0 && xAutos2[[l2]]<savexAutos2[[l2]],
  (AppendTo[savefluss2,1];
  l2=l2-1;),
  AppendTo[savefluss2,0];
  ];
  ];
 
  (*Dichten der Spur \[UDoubleDot]ber t*)
  AppendTo[density2,Length[xAutos2]/nCells];
  ),
  (addfluss=addfluss;
  AppendTo[savefluss2,0];
  AppendTo[density2,0]
  )
  ];
  
  (*Zusammenz\[ADoubleDot]hlen des Flusses der beiden Spuren*)
  savefluss=Table[savefluss1[[n]]+savefluss2[[n]],{n,tMax}];
  
  (*Mittlere Geschwindigkeit aller Autos*)
  AppendTo[vMittel,N[Mean[Select[Join[vAutos1,vAutos2],UnsameQ[#, {}]&]],6]];
  
  (*Plotten vMittel, dVar und savefluss*)
  ResourceFunction["PlotGrid"][{
  {ListPlot[vMittel,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"mittlere Geschwindigkeit" OverBar[v]}]},
  {ListPlot[dVar1,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Varianz des Abstands d auf rechter Spur"}]},
  {ListPlot[dVar2,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Varianz des Abstands d auf linker Spur"}]},
  {ListPlot[savefluss,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Fluss pro Zeitschritt"}]},
  {ListPlot[density1,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Dichte der rechten Spur"}]},
  {ListPlot[density2,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Dichte der linken Spur"}]}
  },
  ImageSize->Large,FrameLabel->{"Zeit t",None}
  ]; 
  ];

(*Gibt Histogramme von v und d bei tMax f\[UDoubleDot]r 4 Dichten aus*)
(*Keine Histogramme ausgegeben, falls kein Auto auf der Spur*)
If[Length[xAutos1]>0 && MemberQ[Table[n/5 nCells,{n,4}],nCar],
((*Histogramme v und d*)
(*Listen Autos mit Geschwindigkeiten v=0,1,2,3,4,5*)
Clear[viAutos1];
viAutos1=Select[Table[Select[Table[vAutos1[[n]],{n,Length[xAutos1]}],#==i &],{i,0,5}],UnsameQ[#, {}]&];

(*Listen Abst\[ADoubleDot]nde d=0,1,...,nCar*)
Clear[diAutos1];
diAutos1=Select[Table[Select[Table[dAutos1[[n]],{n,Length[xAutos1]}],#==i &],{i,0,nCells-nCar-1}],UnsameQ[#, {}] &];),
(viAutos1={};
diAutos1={};)
];

If[Length[xAutos2]>0 && MemberQ[Table[n/5 nCells,{n,4}],nCar],
((*Histogramme v und d*)
(*Listen Autos mit Geschwindigkeiten v=0,1,2,3,4,5*)
Clear[viAutos2];
viAutos2=Select[Table[Select[Table[vAutos2[[n]],{n,Length[xAutos2]}],#==i &],{i,0,5}],UnsameQ[#, {}]&];

(*Listen Abst\[ADoubleDot]nde d=0,1,...,nCar*)
Clear[diAutos2];
diAutos2=Select[Table[Select[Table[dAutos2[[n]],{n,Length[xAutos2]}],#==i &],{i,0,nCells-nCar-1}],UnsameQ[#, {}] &];),
(viAutos2={};
diAutos2={};)
];

viAutos=Select[Join[viAutos1,viAutos2],UnsameQ[#, {}]&];
diAutos=Select[Join[diAutos1,diAutos2],UnsameQ[#, {}]&];

Histogram[viAutos,{1},AxesLabel->{v,"Anzahl Autos mit" Indexed[v,"i"] "f\[UDoubleDot]r die Dichte" nCar/nCells},ColorFunction->"Pastel",ImageSize->Medium]
Histogram[diAutos,{1},AxesLabel->{d,"Anzahl Autos mit" Indexed[d,"i"] "f\[UDoubleDot]r die Dichte" nCar/nCells},ColorFunction->"Pastel",ImageSize->Medium]

(*Dichte \[UDoubleDot]ber die gesamte Stra\[SZ]e*)
AppendTo[density,nCar/nCells];

(*Verkehrsfluss f\[UDoubleDot]r Dichte nCar/nCells*)
AppendTo[fluss,addfluss];
Clear[savefluss1];
Clear[savefluss2];
];
(*Fehlend: Dichte getrennt f\[UDoubleDot]r Spuren \[UDoubleDot]ber t*)
(*Fundamentalplot mit addfluss*)
(*ListPlot[Thread[{density,fluss/tMax}],ImageSize->Medium,Frame->True,FrameLabel->{"Dichte \[Rho]","Zeitliches Mittel des Flusses \[UDoubleDot]ber letzte Zelle"},
PlotStyle->RandomChoice[{Red,Orange,Yellow,LightGreen,LightBlue,Blue,Purple,Pink}]] (*Punkte hell f\[UDoubleDot]r dunklen Hintergrund in sp\[ADoubleDot]terem Notebook*)*)
]


twolanesNaSch[20,10,5,0.15]
(*list={};
list1=Table[list[[n]],{n,0,0}];
If[list1[[1]]==0,Print[0],Print[1],Print[2]]*)
(*ListDensityPlot f\[UDoubleDot]r Dichteplot der Zellenbesetzung! MovingAverage f\[UDoubleDot]r manuelles Einstellen der zu mittelnden Zellen*)
(*Plot auf Variable a ablegen, dann Print[a]*)

