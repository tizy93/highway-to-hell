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
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,1,nCar-1}]; (*In Schleife, damit es geupdatet wird*)
(*berechnet freie Zellen zum vorderen Auto normal au\[SZ]er f\[UDoubleDot]r Autos au\[SZ]er dem mit h\[ODoubleDot]chster Positition - da Ringstra\[SZ]e sind die freien Zellen geringer als xAutos[[n+1]]-xAutos[[n]]-1*)
(*Arrays starten mit Element 1; n+1 muss f\[UDoubleDot]r das letzte gleich nCar sein*)
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];
(*Abstand des Autos an letzter Stelle in Liste zum ersten wird angeh\[ADoubleDot]ngt*)

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,1,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,1,nCar}]; 

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
vAutos=Table[If[RandomReal[{0,1}]<=p,vAutos[[n]]=Max[vAutos[[n]]-1,0],vAutos[[n]]],{n,1,nCar}]; (*Tr\[ODoubleDot]deln solange noch nicht v=0*)
(*Falls zuf\[ADoubleDot]llige Zahl nicht im gegebenen Intervall, bleibt v gleich*)

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,1,nCar}];
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


(*Berechnung mittlere v \[UDoubleDot]ber t, Varianz des mittleren Abstands und des Verkehrsflusses \[UDoubleDot]ber t*)
Meanvarfluss[nCar_,nCells_,tMax_,vMax_,p_]:=Module[
(*betrachtete Zelle ist letzte Zelle der Stra\[SZ]e*)

(*lokale Variablen*)
{xAutos, vAutos, dAutos, vMittel, dVar, savefluss, savexAutos, m},

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
{xAutos, vAutos, dAutos, nCar, density, addfluss, savexAutos, m, fluss},

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


(*Velocity-Dependent-Randomization Modell mit Fundamentalplots*)
vdrNaSch[nCells_,tMax_,vMax_,p_,q_]:=Module[
(*q ist zus\[ADoubleDot]tzliche Wahrscheinlichkeit zum Tr\[ODoubleDot]deln beim Anfahren*)

(*lokale Variablen*)
{xAutos,vAutos,dAutos, nCar, density, addfluss, savexAutos, m, fluss},

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
dAutos=Table[If[xAutos[[n]]<Max[xAutos],xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+Min[xAutos]-1],{n,1,nCar-1}];
AppendTo[dAutos,If[xAutos[[nCar]]<Max[xAutos],xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+Min[xAutos]-1]];

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,1,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,1,nCar}]; 

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
vAutos=Table[If[vAutos[[n]]==0,If[RandomReal[{0,1}]<=p+q,vAutos[[n]],vAutos[[n]]],If[RandomReal[{0,1}]<=p,vAutos[[n]]=vAutos[[n]]-1,vAutos[[n]]]],{n,1,nCar}];
(*Wenn Auto steht ist p um q erh\[ODoubleDot]ht*)

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,1,nCar}];

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


(* ::Code:: *)
(**)
