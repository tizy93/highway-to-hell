(* ::Package:: *)

(* ::Title:: *)
(*The highway to hell*)


(* ::Subtitle:: *)
(*Menschen machen seltsame Sachen*)


(* ::Text:: *)
(*Staus entwickeln sich aus dem Nichts. *)
(**)
(**)


(* ::Text:: *)
(*Die H\[ADoubleDot]lfte der Deutschen verbringen jedes Jahr im Durchschnitt 40 Stunden im Stau. *)
(**)
(*Oft bilden sich diese Staus aus dem Nichts. Das Nagel-Schreckenberg-Modell simuliert und analysiert den Verkehr unfallfrei und liefert u.A. eine Erkl\[ADoubleDot]rung f\[UDoubleDot]r dieses Ph\[ADoubleDot]nomen. *)
(**)
(*Das NaSch-Modell teilt die Stra\[SZ]e in einzelne Abschnitte, die entweder von einem Auto besetzt oder leer sind. Diese Zellen haben eine L\[ADoubleDot]nge von 7,5 m. Auch die Zeitschritte in Sekunden, oft "Runden" genannt, und Geschwindigkeiten sind diskret, mit Geschwindigkeiten von \[UDoubleDot]blicherweise 0 bis 5. Hierbei ist v=0 ein stehendes Auto, v=1 bedeutet eine Zelle pro Zeitschritt, also 7,5m/s bzw. 27km/h. Somit ist die Maximalgeschwindigkeit v=5 umgerechnet 135km/h. *)
(**)


(* ::Chapter:: *)
(*Nasch-Modell*)


(* ::Text:: *)
(*Es wird das Haupt Modell geschrieben und Geplottet.*)
(*Es besteht aus einer For-Schleife, indem die Schritte f\[UDoubleDot]r jede runde von i=0 bis zum i=tMax entfaltet werden.*)
(*Diese sind die Beschleunigung von jedem Auto um 1. Falls der Abstand von dem n\[ADoubleDot]chsten Auto kleiner als die neue Geschwindigkeit ist, wird abgebremst. Au\[SZ]erdem kann ein Auto mit einer Tr\[ODoubleDot]delwahrscheinlichkeit p noch zus\[ADoubleDot]tzlich abbremsen. Die Autos Fahren in einem ring.*)
(*Diese Model wird wieder in jede Modul geschrieben um den unterschiedlichen \[CapitalUDoubleDot]bungen schreiben und plotten zu k\[ODoubleDot]nnen *)
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
vAutos=RandomInteger[{1,vMax},nCar];
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
(*Vllt fehlend: graphische Darstellung Stra\[SZ]e?*)
];
]


NaSch[100,300,100,5,0.15]


(* ::Chapter:: *)
(*Dichteplot*)


(* ::Text:: *)
(*Der N\[ADoubleDot]chste Modul entspricht den dichteplot. es werden eine bestimmte Anzahl an Zelle beobachtet, sonst w\[UDoubleDot]rde die dichte immer gleich. Die dichte entspricht der Anzahl an Autos die an dem entsprechenden Runde in diesen Intervall Zellen gefahren sind.*)
(**)
(*Im Dichteplot wird die Anzahl der Autos N \[UDoubleDot]ber einer Anzahl avCells Zellen berechnet und \[UDoubleDot]ber der Zeit t dargestellt.*)


(*Dichteplot \[UDoubleDot]ber Zeit*)
densityplot[nCar_,nCells_,tMax_,vMax_,p_,avCells_]:=Module[
(*lokale Variablen*)
{xAutos,vAutos,dAutos,regionCars,density,posxAutos,pos\[UDoubleDot]bert,newpos\[UDoubleDot]bert,dens,densplot,k},

(*NaSch-Modell*)

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
vAutos=RandomInteger[{1,vMax},nCar]; 

(*Erzeugen einelementige Liste mit Dichte und Positionen der Autos*) 
density=Table[Nothing,{n,1}];
pos\[UDoubleDot]bert=Table[Nothing,{n,1}];

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
(*Sortierte Liste mit 1 als besetzten Zelle und 0 als leere Zelle*)
posxAutos=Flatten[Table[If[Select[xAutos,#==n &]=={},0,1],{n,1,nCells}]];
(*Positionen der Autos f\[UDoubleDot]r den DensityPlot abspeichern*)
AppendTo[pos\[UDoubleDot]bert,posxAutos];
];
(*Plotten Dichte*)
dens=ListPlot[density,ImageSize->Medium,ColorFunction->"Rainbow",Frame->True,FrameLabel->{"Zeit t","Dichte \[Rho] \[UDoubleDot]ber ersten "<>ToString[avCells]<>" Zellen"}];
(*Anpassen pos\[UDoubleDot]bert sodass t in positive x-Richtung l\[ADoubleDot]uft statt in positiver y-Richtung*)
newpos\[UDoubleDot]bert=Table[Table[pos\[UDoubleDot]bert[[n,m]],{n,tMax}],{m,nCells}];
densplot=ListDensityPlot[newpos\[UDoubleDot]bert,FrameLabel->{"Zeit t","Zellen der Stra\[SZ]e"},ImageSize->Medium];
Show[densplot]
Show[dens]
]


densityplot[100,300,100,5,0.15,100]



(* ::Text:: *)
(*Diskussion*)


(* ::Chapter:: *)
(*Histogramme*)


(* ::Text:: *)
(*In diesem Modul werden erzeugt die Histogramme der Geschwindigkeit und den Abstand f\[UDoubleDot]r einen Zeitpunkt tMax. *)
(**)


(*Histogramme Geschwindigkeiten und Abstand f\[UDoubleDot]r einen Zeitpunkt*)
vdhisto[nCar_,nCells_,tMax_,vMax_,p_]:=Module[
(*tMax ist betrachteter Zeitpunkt*)

(*lokale Variablen*)
{xAutos,vAutos,dAutos,viAutos,diAutos,i,vhisto,dhisto},

(*NaSch-Modell*)

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
vAutos=RandomInteger[{1,vMax},nCar];

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
];
(*Listen Autos mit Geschwindigkeiten v=0,1,2,3,4,5*)
Clear[viAutos];
viAutos=Table[Select[Table[vAutos[[n]],{n,1,nCar}],#==i &],{i,0,5}];

(*Listen Abst\[ADoubleDot]nde d=0,1,...,nCar*)
Clear[diAutos];
diAutos=Select[Table[Select[Table[dAutos[[n]],{n,1,nCar}],#==i &],{i,0,nCells-nCar-1}],UnsameQ[#, {}] &]; (*Maximaler Abstand ist nCells-nCar-1, falls alle anderen Autos d=0 voneinander*)
(*L\[ODoubleDot]schen der Abst\[ADoubleDot]nde, die nicht vorkommen*)

vhisto=Histogram[viAutos,{1},AxesLabel->{v,Anzahl Autos mit Indexed[v,"i"]},ColorFunction->"Pastel",ImageSize->Medium,PlotLabel->"Histogramm der Geschwindigkeiten"];
dhisto=Histogram[diAutos,{1},AxesLabel->{d,Anzahl Autos mit Indexed[d,"i"]},ColorFunction->"Pastel",ImageSize->Medium,PlotLabel->"Histogramm der Abst\[ADoubleDot]nde"];
(*Histogramm z\[ADoubleDot]hlt, wie oft eine Zahl in einer Liste und den Sublisten darin vorkommt*)
Show[vhisto]
Show[dhisto]
]


vdhisto[100,300,100,5,0.15]


(* ::Text:: *)
(*Aus dem erster Histogramm kann beobachtet werden, dass je gr\[ODoubleDot]\[SZ]er der Abstand zwischen den Autos ist, desto weniger Autos mit diesem Abstand vorhanden sind wegen den Anzahl an Zellen und die Verteilung des Autos.*)
(*Der zweiter Histogramm zeigt dass mehrere Autos die Geschwindigkeit 1 oder 0 besitzen und  weniger sind die Autos mit h\[ODoubleDot]here Geschwindigkeit, weil weniger Autos h\[ODoubleDot]here Abst\[ADoubleDot]nde als 1 oder 0 besitzen.*)
(**)


(* ::Chapter:: *)
(*Meanvarfluss*)


(* ::Text:: *)
(*Hier werden: die mittlere Geschwindigkeit der Autos, *)
(*die Varianz des Abstands zwischen jedes Auto, und den Verkehrsfluss, f\[UDoubleDot]r jede runde berechnet.*)
(*Die Verkehrsfl\[UDoubleDot]sse ist die Anzahl an Auto die durch die letzte Zelle fahren, pro Zeitschritt kann es mit einer Spur also entweder 0 oder 1 sein.*)
(*Anschlie\[SZ]end werden die Grafiken von Geschwindigkeit und die Varianz geplottet und dann die korreliert. Der Fluss wird auch dargestellt.*)
(**)


(*Berechnung mittlere v \[UDoubleDot]ber t, Varianz des mittleren Abstands und des Verkehrsflusses \[UDoubleDot]ber t*)
Meanvarfluss[nCar_,nCells_,tMax_,vMax_,p_]:=Module[
(*betrachtete Zelle ist letzte Zelle der Stra\[SZ]e*)

(*lokale Variablen*)
{xAutos,vAutos,dAutos,vMittel,dVar,savefluss,savexAutos,m},

(*NaSch-Modell*)

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
vAutos=RandomInteger[{1,vMax},nCar]; 

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
];
Clear[savexAutos];
savexAutos=xAutos;

(*mittlere Geschwindigkeit*)
AppendTo[vMittel, N[Mean[vAutos],6]];
(*Varianz dAutos*)
AppendTo[dVar, N[Variance[dAutos],6]];

];
ResourceFunction["PlotGrid"][{
{ListPlot[vMittel,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"mittlere Geschwindigkeit "OverBar[v]}]},
{ListPlot[dVar,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Varianz des Abstands d"}]},
{ListPlot[savefluss,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Fluss pro Zeitschritt"}]}
},
ImageSize->Large,FrameLabel->{"Zeit t",None}
]

(*Korrelation mittlere Geschwindigkeit und Varianz des Abstands*)
ListPlot[Thread[{dVar,vMittel}],ImageSize->Medium,ColorFunction->"Rainbow",Frame->True,FrameLabel->{"Varianz des Abstands d","mittlere Geschwindigkeit " OverBar[v]},
PlotLabel->"Korrelation Varianz des Abstands und mittlere Gewschindigkeit"]
]


Meanvarfluss[100,300,100,5,0.15]


(* ::Text:: *)
(*Die Grafiken von Mittlere Geschwindigkeit, Varianz des Abstands und Der Fluss pro Zeitschritt sind \[UDoubleDot]ber die Zeit dargestellt. Die Zeigen dass die mittlere Geschwindigkeit und die Varianz des abstand haben gr\[ODoubleDot]\[SZ]ere Sprunge. Die Geschwindigkeit variiert zwischen 1 und 2 das bedeutet, dass wann die niedriger werden sich Staus bilden.*)
(*Die Spr\[UDoubleDot]nge von Varianz sind schon h\[ODoubleDot]her als die von der Geschwindigkeit, von 2 bis ungef\[ADoubleDot]hr 10. Die meisten punkte befinden sich um die Varianz herum die in der Mitte zwischen der niedrigste und die h\[ODoubleDot]chste Varianz liegt.*)
(*Die Grafik der Fluss zeigt, wie das nur 1 oder 0 sein kann weil nur eine spur simuliert wird. Da ein Auto nur bis zu einer Zelle vor dem n\[ADoubleDot]chsten fahren kann (R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als d), kann eine Zelle in einer Runde nur von einem Auto durchquert werden.*)
(*Aus der Korrelation der Geschwindigkeit und der Varianz des Abstands ist zu sehen, wie die mittlere Geschwindigkeit linear abnimmt mit der Zunahme von der Varianz des Abstands.*)
(**)


(* ::Chapter:: *)
(*Fundamentalplot*)


(* ::Text:: *)
(*Unter fundamental Plot versteht man die Korrelation zwischen Verkehrsfluss \[UDoubleDot]ber die dichte.*)
(*In diesem Teil wird ein Modul daf\[UDoubleDot]r hergestellt und anschlie\[SZ]end das dargestellt.*)
(*Nachdem wird es f\[UDoubleDot]r unterschiedliche Tr\[ODoubleDot]delwahrscheinlichkeiten p geschrieben und geplottet.*)
(**)


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
vAutos=RandomInteger[{1,vMax},nCar]; 

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
];
Clear[savexAutos];
savexAutos=xAutos;
];

(*Dichte \[UDoubleDot]ber die gesamte Stra\[SZ]e*)
AppendTo[density,nCar/nCells];

(*Verkehrsfluss f\[UDoubleDot]r Dichte nCar/nCells*);
AppendTo[fluss,addfluss];
];
(*Fundamentalplot mit addfluss*);
ListPlot[Thread[{density,fluss/tMax}],ImageSize->Medium,PlotRange->{0,0.9},Frame->True,FrameLabel->{"Dichte \[Rho]","Zeitliches Mittel des Flusses mit p = " <> ToString[p]},
PlotStyle->RandomChoice[{Red,Orange,Yellow,LightGreen,LightBlue,Blue,Purple,Pink}]] (*Punkte hell f\[UDoubleDot]r dunklen Hintergrund in sp\[ADoubleDot]terem Notebook*)
]


FundamentalD[300,100,5,0]
FundamentalD[300,100,5,0.1]
FundamentalD[300,100,5,0.15]
FundamentalD[300,100,5,0.2]
FundamentalD[300,100,5,0.25]
FundamentalD[300,100,5,0.3]
FundamentalD[300,100,5,0.5]
FundamentalD[300,100,5,0.75]
FundamentalD[300,100,5,1]


(* ::Text:: *)
(*Man sieht aus die Fundamentalplots, dass je h\[ODoubleDot]her die Tr\[ODoubleDot]delwarscheinligkeit ist desto h\[ODoubleDot]her ist die Streuung des Flusses und desto geringere ist der Fluss.*)
(*Die Diagramme zeigen wie die H\[ODoubleDot]he der Peaks beim p=0.1 am h\[ODoubleDot]chsten ist, und zwar fast bis 0.7 und dass beim p= 1 viel geringer wird, also bis ungef\[ADoubleDot]hr 0,012 hoch werden. *)
(*Mit eine unterschied von \[CapitalDelta]p=0.05 unterscheid sich den peak von 0.5 und mit \[CapitalDelta]p=0.1 wird das unterschied auch beim ungef\[ADoubleDot]hr 0.1*)
(*Man kann beobachten, dass bis dichte, ungef\[ADoubleDot]hr, 0.125 wird der Fluss linear erh\[ODoubleDot]hen, weil keine Staus noch gibt's und nach diese dichte werden mehrere Staus sich entwickeln und wird damit der Fluss linear abfallen. Der Fluss wird 0 ungef\[ADoubleDot]hr bei dichte=1.*)
(*Eine Ausnahme bildet sich f\[UDoubleDot]r p=1 wo der Fluss fast sofort abf\[ADoubleDot]llt, also ab eine dichte nah an 0.*)
(**)
(**)


(* ::Chapter:: *)
(*VDR*)


(* ::Text:: *)
(*In dieses Modul wird der Nasch- Modell in VDR geschrieben, und zwar mit einer Geschwindigkeit das abh\[ADoubleDot]ngig von der Tr\[ODoubleDot]delwahrscheinlichkeit variiert, also wenn die Geschwindigkeit null ist wird die Tr\[ODoubleDot]delwahrscheinligkeit um q h\[ODoubleDot]her als sonst.*)


(*Fundamentalplots des Velocity-Dependent-Randomization Modells*)
vdrNaSch[nCells_,tMax_,vMax_,q_]:=Module[
(*q ist zus\[ADoubleDot]tzliche Wahrscheinlichkeit zum Tr\[ODoubleDot]deln beim Anfahren*)

(*lokale Variablen*)
{xAutos,vAutos,dAutos,nCar,p,a,density,addfluss,savexAutos,m,fluss,posxAutos,newpos\[UDoubleDot]bert,pos\[UDoubleDot]bert,rhoplot,dichte,viAutos,diAutos,vhisto,dhisto},

(*Leere Liste f\[UDoubleDot]r Plots*)
densplot=Table[Nothing,{n,1}];
histoplot=Table[Nothing,{n,1}];
fundplot=Table[Nothing,{n,1}];

(*Histogramme f\[UDoubleDot]r 3 Dichten*)
rhoplot={60,100,200};

(*Tr\[ODoubleDot]delwahrscheinlichkeiten pi*)
p={0.15,0.3};
a=1;
(*Ausf\[UDoubleDot]hren f\[UDoubleDot]r Tr\[ODoubleDot]delwahrscheinlichkeiten p*)
Do[
((*Erzeugen einelementige Liste mit Dichte, Fluss, und Fluss f\[UDoubleDot]r jede Anzahl an Autos*) 
Clear[density];
density=Table[Nothing,{n,1}];
Clear[fluss];
fluss=Table[Nothing,{n,1}];

(*Schleife f\[UDoubleDot]r ansteigende Dichte/Anzahlen an Autos*)
For[nCar=0,nCar<=nCells,nCar++,

(*Leere Listen f\[UDoubleDot]r ListDensityPlot*)
posxAutos=Table[Nothing,{n,1}];
newpos\[UDoubleDot]bert=Table[Nothing,{n,1}];
pos\[UDoubleDot]bert=Table[Nothing,{n,1}];

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
vAutos=RandomInteger[{1,vMax},nCar];

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
vAutos=Table[If[vAutos[[n]]==0,If[RandomReal[{0,1}]<=p[[a]]+q,vAutos[[n]],vAutos[[n]]],If[RandomReal[{0,1}]<=p[[a]],vAutos[[n]]=vAutos[[n]]-1,vAutos[[n]]]],{n,nCar}];
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
];
Clear[savexAutos];
savexAutos=xAutos;

(*ListDensityPlot*)
If[MemberQ[rhoplot,nCar],
posxAutos=Flatten[Table[If[Select[xAutos,#==n &]=={},0,1],{n,1,nCells}]];
(*Positionen der Autos f\[UDoubleDot]r den DensityPlot abspeichern*)
AppendTo[pos\[UDoubleDot]bert,posxAutos]
];
];
(*Dichte \[UDoubleDot]ber die gesamte Stra\[SZ]e*)
AppendTo[density,nCar/nCells];

(*ListDensityPlot f\[UDoubleDot]r 100 Autos*)
If[nCar==100,
(*Anpassen pos\[UDoubleDot]bert sodass t in positive x-Richtung l\[ADoubleDot]uft statt in positiver y-Richtung*)
newpos\[UDoubleDot]bert=Table[Table[pos\[UDoubleDot]bert[[n,l]],{n,tMax}],{l,nCells}];
Clear[dichte];
dichte=DecimalForm[nCar/nCells,2];
AppendTo[densplot,ListDensityPlot[newpos\[UDoubleDot]bert,FrameLabel->{"Zeit t","Zellen der Stra\[SZ]e"},ImageSize->Medium,PlotLabel->"Dichteplot f\[UDoubleDot]r "<>ToString[nCar]<>" Autos, p = "<>ToString[p[[a]]]<>" und q = "<>ToString[q]]];
];

(*Histogramme f\[UDoubleDot]r 60, 100 und 200 Autos*)
If[MemberQ[rhoplot,nCar],
(*Listen Autos mit Geschwindigkeiten v=0,1,2,3,4,5*)
viAutos=Table[Select[Table[vAutos[[n]],{n,1,nCar}],#==i &],{i,0,5}];

(*Listen Abst\[ADoubleDot]nde d=0,1,...,nCar*)
diAutos=Select[Table[Select[Table[dAutos[[n]],{n,1,nCar}],#==i &],{i,0,nCells-nCar-1}],UnsameQ[#, {}] &]; (*Maximaler Abstand ist nCells-nCar-1, falls alle anderen Autos d=0 voneinander*)
(*L\[ODoubleDot]schen der Abst\[ADoubleDot]nde, die nicht vorkommen*)

AppendTo[histoplot,Histogram[viAutos,{1},AxesLabel->{v,Anzahl Autos mit Indexed[v,"i"]},ColorFunction->"Pastel",PlotRange->{{Automatic,5.5},Automatic},ImageSize->Medium,
PlotLabel->"Histogramm von v f\[UDoubleDot]r "<>ToString[nCar]<>" Autos, p = "<>ToString[p[[a]]]<>" und q = "<>ToString[q]]];
AppendTo[histoplot,Histogram[diAutos,{1},AxesLabel->{d,Anzahl Autos mit Indexed[d,"i"]},ColorFunction->"Pastel",PlotRange->{0,All},ImageSize->Medium,
PlotLabel->"Histogramm von d f\[UDoubleDot]r "<>ToString[nCar]<>" Autos, p = "<>ToString[p[[a]]]<>" und q = "<>ToString[q]]];
(*Histogramm z\[ADoubleDot]hlt, wie oft eine Zahl in einer Liste und den Sublisten darin vorkommt*)
];

(*Verkehrsfluss f\[UDoubleDot]r Dichte nCar/nCells*);
AppendTo[fluss,addfluss]; 
];
(*Fundamentalplot mit addfluss*)
AppendTo[fundplot,ListPlot[Thread[{density,fluss/tMax}],ImageSize->Medium,Frame->True,FrameLabel->{"Dichte \[Rho]","Zeitliches Mittel des Flusses"},PlotLabel->"Fundamentalplot mit p= "<>ToString[p[[a]]]<>" und q = "<>ToString[q],
PlotStyle->RandomChoice[{Red,Orange,Yellow,LightGreen,LightBlue,Blue,Purple,Pink}]]]; (*Hell f\[UDoubleDot]r dunklen Hintergrund in sp\[ADoubleDot]terem Notebook*)
(*Variable a hochz\[ADoubleDot]hlen f\[UDoubleDot]r n\[ADoubleDot]chste p*)
a=a+1;
),2];
Return[{histoplot,densplot,fundplot}]
]


vdrNaSch[300,100,5,0.2]


(* ::Text:: *)
(*Diskussion)*)


(* ::Chapter:: *)
(*Zwei Spuren*)


(* ::Text:: *)
(*Als letzte wird den Model auf zwei Spuren angepasst also wird wieder den nasch Model von Anfang genutzt nicht auf VDR basiert.*)
(**)


(*Modell zwei Fahrspuren*)
twolanesNaSch[nCells_,tMax_,vMax_,p_]:=Module[
(*nCells ist L\[ADoubleDot]nge beider Spuren, nCar Anzahl aller Autos f\[UDoubleDot]r das Histogramm von v *)
(*F\[UDoubleDot]r Fundamentalplots eine nCells!=0mod5 eingeben, f\[UDoubleDot]r Histogramme zus\[ADoubleDot]tzlich nCells=0mod5*)

(*lokale Variablen*)
{xAutos,xAutos1,xAutos2,vAutos1,vAutos2,dAutos1,dAutos2,viAutos,viAutos1,viAutos2,vhisto,diAutos,diAutos1,diAutos2,dhisto,nCar,density,density1,density2,
fluss,addfluss,savefluss,savefluss1,savefluss2,savexAutos1,savexAutos2,savevAutos1,savevAutos2,m1,m2,l1,l2,savem1,savem2,savel1,savel2,
wechselzu1,wechselzu2,wechselvon1,wechselvon2,vMittel,dVar1,dVar2,index1,index2,h,i,j,k,o,r,s},

(*Erzeugen einelementige Liste mit Gesamtdichte und Fluss f\[UDoubleDot]r nCar=0, f\[UDoubleDot]r jedes nCar werden Werte hinzugef\[UDoubleDot]gt*) 
density={0};
fluss={0};

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
If[UnsameQ[xAutos1,{}],xAutos2=Drop[Table[xAutos[[n]]-nCells,{n,nCar}],Length[xAutos1]],xAutos2=xAutos];
(*Geschwindigkeiten f\[UDoubleDot]r alle Autos getrennt auf den Spuren*)
vAutos1=RandomInteger[{1,vMax},Length[xAutos1]];
vAutos2=RandomInteger[{1,vMax},Length[xAutos2]];

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
savem1=m1;
savem2=m2;

savefluss1=Table[Nothing,{n,1}];
savefluss2=Table[Nothing,{n,1}];
l1=Length[xAutos1];
l2=Length[xAutos2];
savel1=l1;
savel2=l2;

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++, 
  (*Freie Zellen d vor dem Auto bis zum vorderen*)
  (*Rechte Spur*)
  If[Length[xAutos1]>1,
  (dAutos1=Table[If[xAutos1[[n]]<Max[xAutos1],xAutos1[[n+1]]-xAutos1[[n]]-1,nCells-xAutos1[[n]]+Min[xAutos1]-1],{n,Length[xAutos1]-1}];
  AppendTo[dAutos1,If[xAutos1[[Length[xAutos1]]]<Max[xAutos1],xAutos1[[1]]-xAutos1[[Length[xAutos1]]]-1,nCells-xAutos1[[Length[xAutos1]]]+Min[xAutos1]-1]];),
  dAutos1={nCells-1};
  ]; 
  (*Linke Spur*)
  If[Length[xAutos2]>1,
  (dAutos2=Table[If[xAutos2[[n]]<Max[xAutos2],xAutos2[[n+1]]-xAutos2[[n]]-1,nCells-xAutos2[[n]]+Min[xAutos2]-1],{n,Length[xAutos2]-1}]; 
  AppendTo[dAutos2,If[xAutos2[[Length[xAutos2]]]<Max[xAutos2],xAutos2[[1]]-xAutos2[[Length[xAutos2]]]-1,nCells-xAutos2[[Length[xAutos2]]]+Min[xAutos2]-1]];),
  dAutos2={nCells-1};
  ];
  
  (*R1: Beschleunigen, falls vMax noch nicht erreicht*)
  (*Rechte Spur*)
  If[Length[xAutos1]>0,
  vAutos1=Table[Min[vAutos1[[n]]+1,vMax],{n,Length[xAutos1]}];
  ];
  savevAutos1=vAutos1;

  (*Linke Spur*)
  If[Length[xAutos2]>0,
  vAutos2=Table[Min[vAutos2[[n]]+1,vMax],{n,Length[xAutos2]}];
  ];
  savevAutos2=vAutos2;
  
  (*R2: Spurwechsel, falls v gr\[ODoubleDot]\[SZ]er als Abstand d und links frei -> bei Wechsel zun\[ADoubleDot]chst Spurwechsel, dann weiter fahren mit v-1 in R3-R5*)
  (*Wechsel rechte Spur zu linker*)
  wechselzu2={};
  wechselvon1={};
  (*Falls Auto auf rechter Spur*)
  If[Length[savexAutos1]>0,
  k=1;
  Do[
  (*Falls v gr\[ODoubleDot]\[SZ]er d und Nachbarzelle links frei, \[CapitalUDoubleDot]berpr\[UDoubleDot]fen Spurwechsel*)
  If[savevAutos1[[k]]>dAutos1[[k]] && Select[savexAutos2,#==savexAutos1[[k]] &]=={},
  (*Falls Auto mit kleinerer Position existiert*)
  (If[UnsameQ[Select[savexAutos2,#<savexAutos1[[k]] &],{}], 
  (*betrachteter Index: Auto mit n\[ADoubleDot]chstkleinerer Position auf linker Spur*)
  (index2=Flatten[Position[savexAutos2,Max[Select[savexAutos2,#<savexAutos1[[k]] &]]]]; (*Position gibt hier Liste in Form von {{x}} aus*)
  (*Falls Spurwechsel ohne Auffahrunfall von hinten und Weiterfahren mit v-1 ohne Anfahren des vorderen Autos m\[ODoubleDot]glich*)
  If[savexAutos2[[index2]]+savevAutos2[[index2]]<savexAutos1[[k]]+savevAutos1[[k]]-1<savexAutos2[[index2]]+dAutos2[[index2]]+1,
  (*Auto von rechter Spur auf Nachbarzelle, v auf v-1 setzen, Indizes f\[UDoubleDot]r Flussberechnung anpassen*)
  ((*Verschiebung r durch hinzugef\[UDoubleDot]gte Autos zu xAutos2*)
  r=Length[Select[wechselzu2,#<=index2+1 &]];
  Insert[xAutos2,savexAutos1[[k]],index2+1+r];
  Insert[vAutos2,savevAutos1[[k]]-1,index2+1+r];
  If[index2+1+r<=m2,m2=m2+1];
  If[index2+1+r<=l2,l2=l2+1];
  (*Positionen hinzugef\[UDoubleDot]gter Elemente in xAutos2 und vAutos2*)
  AppendTo[wechselzu2,index2+1];
  (*Verschiebung s durch entfernte Autos aus xAutos1*)
  s=Length[Select[wechselvon1,#<=k &]];
  Delete[xAutos1,k-s];
  Delete[vAutos1,k-s];
  If[k-s<=m1,m1=m1-1];
  If[k-s<=l1,l1=l1-1];
  (*Positionen herausgenommener Elemente aus xAutos1 und vAutos1*)
  AppendTo[wechselvon1,k];),
  (*Ohne n\[ODoubleDot]tige L\[UDoubleDot]cke links kein Wechsel, Betrachten n\[ADoubleDot]chstes Auto*)
  k=k+1;
  ];),
  (*Auto xAutos1[[k]] ist Auto mit kleinster Position, \[CapitalUDoubleDot]berpr\[UDoubleDot]fen anderes Ende, da Ringstra\[SZ]e*)
  (*Falls linke Spur nicht leer*)
  (If[Length[savexAutos2]>0,
  (*betrachteter Index: Auto mit gr\[ODoubleDot]\[SZ]ter Position, also erstes am anderen Ende*)
  (index2=Flatten[Position[savexAutos2,Max[savexAutos2]]];
  (*Falls Spurwechsel ohne Auffahrunfall von hinten und Weiterfahren mit v-1 ohne Anfahren des vorderen Autos m\[ODoubleDot]glich*)
  If[savexAutos2[[index2]]+savevAutos2[[index2]]-nCells<savexAutos1[[k]]+savevAutos1[[k]]-1<savexAutos2[[index2]]+dAutos2[[index2]]+1-nCells,
  (*Auto von rechter Spur auf Nachbarzelle, v auf v-1 setzen, Indizes f\[UDoubleDot]r Flussberechnung anpassen*)
  ((*Verschiebung r durch hinzugef\[UDoubleDot]gte Autos zu xAutos2*)
  r=Length[Select[wechselzu2,#<=index2+1 &]];
  Insert[xAutos2,savexAutos1[[k]],index2+1+r];
  Insert[vAutos2,savevAutos1[[k]]-1,index2+1+r];
  If[index2+1+r<=m2,m2=m2+1];
  If[index2+1+r<=l2,l2=l2+1];
  (*Positionen hinzugef\[UDoubleDot]gter Elemente in xAutos2 und vAutos2*)
  AppendTo[wechselzu2,index2+1];
  (*Verschiebung s durch entfernte Autos aus xAutos1*)
  s=Length[Select[wechselvon1,#<=k &]];
  Delete[xAutos1,k-s];
  Delete[vAutos1,k-s];
  If[k-s<=m1,m1=m1-1];
  If[k-s<=l1,l1=l1-1];
  (*Positionen k entfernter Elemente aus xAutos1 und vAutos1, verwendet zum Vergleich mit savexAutos1 und savevAutos1,
  deshalb nur k*)
  AppendTo[wechselvon1,k];),
  (*Kein Platz, Betrachten n\[ADoubleDot]chstes Auto*)
  k=k+1;
  ];),
  (*Falls linke Spur leer: Spurwechsel, Anpassung v, Indizes f\[UDoubleDot]r Fluss*)
  (Insert[xAutos2,savexAutos1[[k]],1];
  Insert[vAutos2,savevAutos1[[k]]-1,1];
  If[1<=m2,m2=m2+1];
  If[1<=l2,l2=l2+1];
  (*Positionen hinzugef\[UDoubleDot]gter Elemente in xAutos2 und vAutos2*)
  AppendTo[wechselzu2,1];
  (*Verschiebung s durch entfernte Autos aus xAutos1*)
  s=Length[Select[wechselvon1,#<=k &]];
  Delete[xAutos1,k-s];
  Delete[vAutos1,k-s];
  If[k-s<=m1,m1=m1-1];
  If[k-s<=l1,l1=l1-1];
  (*Positionen entfernter Elemente aus xAutos1 und vAutos1*)
  AppendTo[wechselvon1,k];)
  ];)
  ];),
  (*v nicht gr\[ODoubleDot]\[SZ]er d, Betrachten n\[ADoubleDot]chstes Auto*)
  k=k+1;
  ];,Length[savexAutos1]];
  ];
 
  (*Wechsel linke Spur zu rechter*) 
  wechselvon2={};
  wechselzu1={};
  (*Falls Auto auf linker Spur*)
  If[Length[savexAutos2]>0,
  h=1;
  Do[
  (*Falls rechte Nachbarzelle leer*)
  If[Select[savexAutos1,#==savexAutos2[[h]] &]=={},
  (*Falls Auto mit kleinerer Position existiert*)
  (If[UnsameQ[Select[savexAutos1,#<savexAutos2[[h]] &],{}],
  (*Betrachteter Index: n\[ADoubleDot]chstes Auto mit kleinerer Position*)
  index1=Flatten[Position[savexAutos1,Max[Select[savexAutos1,#<savexAutos2[[h]] &]]]];
  (*Falls Spurwechsel ohne Auffahrunfall von hinten und Weiterfahren mit v-1 ohne Anfahren des vorderen Autos m\[ODoubleDot]glich*)
  (If[savexAutos1[[index1]]+savevAutos1[[index1]]<savexAutos2[[h]]+savevAutos2[[h]]-1<savexAutos1[[index1]]+dAutos1[[index1]]+1,
  ((*Verschiebung o von index1 durch hinzugef\[UDoubleDot]gte Autos zu xAutos1 minus entfernte Autos aus xAutos1*)
  o=Length[Select[wechselzu1,#<=index1+1 &]]-Length[Select[wechselvon1,#<=index1+1 &]];
  Insert[xAutos1,savexAutos2[[h]],index1+1+o];
  Insert[vAutos1,savevAutos2[[h]]-1,index1+1+o];
  If[index1+1+o<=m1,m1=m1+1];
  If[index1+1+o<=l1,l1=l1+1];
  (*Positionen hinzugef\[UDoubleDot]gter Elemente in xAutos1 und vAutos1*)
  AppendTo[wechselzu1,index1+1];
  (*Verschiebung j von h durch hinzugef\[UDoubleDot]gte Autos zu xAutos2 minus entfernte Autos aus xAutos2*)
  j=Length[Select[wechselzu2,#<=h &]]-Length[Select[wechselvon2,#<=h &]];
  Delete[xAutos2,h+j];
  Delete[vAutos2,h+j]; 
  If[h+j<=m2,m2=m2-1];
  If[h+j<=l2,l2=l2-1];
  AppendTo[wechselvon2,h];),
  h=h+1;
  ];),
  (*Auto savexAutos1[[h]] ist Auto mit kleinster Position, \[CapitalUDoubleDot]berpr\[UDoubleDot]fen anderes Ende*)
  ((*Falls rechte Spur nicht leer*)
  If[Length[savexAutos1]>0,
  ((*Betrachteter Index: Auto mit h\[ODoubleDot]chster Position, also anderes Ende*)
  index1=Flatten[Position[savexAutos1,Max[savexAutos1]]];
  (*Falls Spurwechsel ohne Auffahrunfall von hinten und Weiterfahren mit v-1 ohne Anfahren des vorderen Autos m\[ODoubleDot]glich*)
  If[savexAutos1[[index1]]+savevAutos1[[index1]]-nCells<savexAutos2[[h]]+savevAutos2[[h]]-1<savexAutos1[[index1]]+dAutos1[[index1]]+1-nCells,
  (*Auto von rechter Spur auf Nachbarzelle, v auf v-1 setzen, Indizes f\[UDoubleDot]r Flussberechnung anpassen*)
  ((*Verschiebung o von index1 durch hinzugef\[UDoubleDot]gte Autos zu xAutos1 minus entfernte Autos aus xAutos1*)
  o=Length[Select[wechselzu1,#<=index1+1 &]]-Length[Select[wechselvon1,#<=index1+1 &]];
  Insert[xAutos1,savexAutos2[[h]],index1+1+o];
  Insert[vAutos1,savevAutos2[[h]]-1,index1+1+o];
  If[index1+1+o<=m1,m1=m1+1];
  If[index1+1+o<=l1,l1=l1+1];
  (*Positionen hinzugef\[UDoubleDot]gter Elemente in xAutos2 und vAutos2*)
  AppendTo[wechselzu1,index1+1];
  (*Verschiebung j von h durch hinzugef\[UDoubleDot]gte Autos zu xAutos2 minus entfernte Autos aus xAutos2*)
  j=Length[Select[wechselzu2,#<=h &]]-Length[Select[wechselvon2,#<=h &]];
  Delete[xAutos2,h+j];
  Delete[vAutos2,h+j];
  If[h+j<=m2,m2=m2-1];
  If[h+j<=l2,l2=l2-1];
  (*Positionen herausgenommener Elemente aus xAutos2 und vAutos2*)
  AppendTo[wechselvon2,h];),
  (*Rechte Spur nicht frei, Betrachten n\[ADoubleDot]chstes Auto*)
  h=h+1;
  ];),
  (*Falls rechte Spur leer: Spurwechsel, Anpassung v, Indizes f\[UDoubleDot]r Fluss*)
  (Insert[xAutos1,savexAutos2[[h]],1];
  Insert[vAutos1,savevAutos2[[h]]-1,1];
  If[1<=m1,m1=m1+1];
  If[1<=l1,l1=l1+1];
  (*Positionen hinzugef\[UDoubleDot]gter Elemente in xAutos1 und vAutos1*)
  AppendTo[wechselzu1,1];
  (*Verschiebung o von h durch hinzugef\[UDoubleDot]gte Autos zu xAutos2 minus entfernte Autos aus xAutos2*)
  o=Length[Select[wechselzu2,#<=h &]]-Length[Select[wechselvon2,#<=h &]];
  Delete[xAutos2,h+o];
  Delete[vAutos2,h+o];
  If[h+o<=m2,m2=m2-1];
  If[h+o<=l2,l2=l2-1];
  (*Positionen entfernter Elemente aus xAutos2 und vAutos2*)
  AppendTo[wechselvon2,h];)
  ];)
  ];),
  (*Nachbarzelle nicht frei*)
  h=h+1;
  ];,Length[savexAutos2]];
  ];
  
  (*Nach Spurwechsel freie Zellen d vor dem Auto bis zum vorderen*)
  (*Rechte Spur*)
  Clear[dAutos1];
  If[Length[xAutos1]>1,
  (dAutos1=Table[If[xAutos1[[n]]<Max[xAutos1],xAutos1[[n+1]]-xAutos1[[n]]-1,nCells-xAutos1[[n]]+Min[xAutos1]-1],{n,Length[xAutos1]-1}];
  AppendTo[dAutos1,If[xAutos1[[Length[xAutos1]]]<Max[xAutos1],xAutos1[[1]]-xAutos1[[Length[xAutos1]]]-1,nCells-xAutos1[[Length[xAutos1]]]+Min[xAutos1]-1]];),
  dAutos1={nCells-1};
  ]; 
  (*Linke Spur*)
  Clear[dAutos2];
  If[Length[xAutos2]>1,
  (dAutos2=Table[If[xAutos2[[n]]<Max[xAutos2],xAutos2[[n+1]]-xAutos2[[n]]-1,nCells-xAutos2[[n]]+Min[xAutos2]-1],{n,Length[xAutos2]-1}]; 
  AppendTo[dAutos2,If[xAutos2[[Length[xAutos2]]]<Max[xAutos2],xAutos2[[1]]-xAutos2[[Length[xAutos2]]]-1,nCells-xAutos2[[Length[xAutos2]]]+Min[xAutos2]-1]];),
  dAutos2={nCells-1};
  ];
   
  (*R3: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
  (*Rechte Spur*)
  If[Length[xAutos1]>0,
  vAutos1=Table[Min[dAutos1[[n]],vAutos1[[n]]],{n,Length[xAutos1]}];
  ];
  (*Linke Spur*)
  If[Length[xAutos2]>0,
  vAutos2=Table[Min[dAutos2[[n]],vAutos2[[n]]],{n,Length[xAutos2]}];
  ];
  
  (*R4: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
  (*Rechte Spur*)
  If[Length[xAutos1]>0,
  vAutos1=Table[If[RandomReal[{0,1}]<=p,vAutos1[[n]]=Max[vAutos1[[n]]-1,0],vAutos1[[n]]],{n,Length[xAutos1]}]; 
  ];
  (*Linke Spur*)
  If[Length[xAutos2]>0,
  vAutos2=Table[If[RandomReal[{0,1}]<=p,vAutos2[[n]]=Max[vAutos2[[n]]-1,0],vAutos2[[n]]],{n,Length[xAutos2]}];
  ];
  
  (*R5: Fahren um vAutos Zellen*)
  (*Rechte Spur*)
  If[Length[xAutos1]>0,
  xAutos1=Table[If[xAutos1[[n]]+vAutos1[[n]]<=nCells,xAutos1[[n]]=xAutos1[[n]]+vAutos1[[n]],xAutos1[[n]]=xAutos1[[n]]+vAutos1[[n]]-nCells],{n,Length[xAutos1]}];,
  (AppendTo[density1,0];
  AppendTo[savefluss1,0];)
  ];
  (*Linke Spur*)
  If[Length[xAutos2]>0,
  xAutos2=Table[If[xAutos2[[n]]+vAutos2[[n]]<=nCells,xAutos2[[n]]=xAutos2[[n]]+vAutos2[[n]],xAutos2[[n]]=xAutos2[[n]]+vAutos2[[n]]-nCells],{n,Length[xAutos2]}];,
  (AppendTo[density2,0];
  AppendTo[savefluss2,0];)
  ];
  
  (*Verkehrsfluss durch letzte Zelle -> Anzahl Autos durch Zelle pro Zeitschritt*)
  (*Rechte Spur*)
  If[m1<=0,m1=Length[xAutos1],m1=m1]; (*Fluss muss auch in Spurwechsel True mit If[Max[xAutosi]+vAutosi[[Flatten[Position[Max[xAutosi]]]]]>nCells, addfluss=addfluss+1; m1=m1-1;], gleich f\[UDoubleDot]r savefluss*)
  If[savem1<=0,savem1=Length[savexAutos1],savem1=savem1];
  If[Length[savexAutos1]>0,
  If[m1>0 && xAutos1[[m1]]<savexAutos1[[savem1]], (*Fehlend: Index f\[UDoubleDot]r savexAutos muss separate Variable sein, separat hoch gez\[ADoubleDot]hlt wenn Fluss*)
  (addfluss=addfluss+1;
  m1=m1-1;
  savem1=savem1-1;),
  addfluss=addfluss;
  ];
  ];
  (*Linke Spur*)
  If[m2<=0,m2=Length[xAutos2],m2=m2];
  If[savem2<=0,savem2=Length[savexAutos2],savem2=savem2];
  If[Length[savexAutos2]>0,
  If[m2>0 && xAutos2[[m2]]<savexAutos2[[savem2]],
  (addfluss=addfluss+1;
  m2=m2-1;
  savem2=savem2-1;),
  addfluss=addfluss;
  ];
  ];
  (*Gibt mittlere v, Varianz von d, Fluss und Dichte der Spur \[UDoubleDot]ber t f\[UDoubleDot]r 4 Gesamtdichten aus*)
  If[MemberQ[Table[n/5 nCells,{n,4}],nCar],
  (*Varianz des Abstands*)
  (*Rechte Spur*)
  If[Length[xAutos1]>1,
  AppendTo[dVar1,N[Variance[dAutos1],6]];,
  AppendTo[dVar1,0];
  ];
  (*Linke Spur*)
  If[Length[xAutos2]>1,
  AppendTo[dVar2,N[Variance[dAutos2],6]];,
  AppendTo[dVar2,0];
  ];
  (*Fluss durch letzte Zelle der jeweiligen Spur*)
  (*Rechte Spur*)
  If[l1<=0,l1=Length[xAutos1],l1=l1];
  If[savel1<=0,savel1=Length[savexAutos1],savel1=savel1];
  If[Length[savexAutos1]>0,
  If[l1>0 && xAutos1[[l1]]<savexAutos1[[savel1]],
  (AppendTo[savefluss1,1];
  l1=l1-1;
  savel1=savel1-1;),
  AppendTo[savefluss1,0];
  ];
  ];
  (*Linke Spur*)
  If[l2<=0,l2=Length[xAutos2],l2=l2];
  If[savel2<=0,savel2=Length[savexAutos2],savel2=savel2];
  If[Length[savexAutos2]>0,
  If[l2>0 && xAutos2[[l2]]<savexAutos2[[savel2]],
  (AppendTo[savefluss2,1];
  l2=l2-1;
  savel2=savel2-1;),
  AppendTo[savefluss2,0];
  ];
  ];
  ];
  Clear[savexAutos1]; 
  savexAutos1=xAutos1;
  Clear[savevAutos1];
  savevAutos1=vAutos1;
  Clear[savexAutos2];
  savexAutos2=xAutos2;
  Clear[savevAutos2];
  savevAutos2=vAutos2;
  Clear[savem1];
  savem1=m1;
  Clear[savem2];
  savem2=m2;
  Clear[savel1];
  savel1=l1;
  Clear[savel2];
  savel2=l2;
  
  (*Dichten der Spur \[UDoubleDot]ber t*)
  (*Rechte Spur*)
  AppendTo[density1,Length[xAutos1]/nCells];
  (*Linke Spur*)
  AppendTo[density2,Length[xAutos2]/nCells];
  
  (*Mittlere Geschwindigkeit aller Autos*)
  AppendTo[vMittel,N[Mean[Select[Join[vAutos1,vAutos2],UnsameQ[#, {}]&]],6]]; 
  ];
  If[MemberQ[Table[n/5 nCells,{n,4}],nCar],
  (*Plotten vMittel, dVar und Dichte der jeweiligen Spur f\[UDoubleDot]r 4 Dichten*)
  ResourceFunction["PlotGrid"][{
  {ListPlot[vMittel,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"mittlere Geschwindigkeit" OverBar[v]}]},
  {ListPlot[dVar1,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Varianz des Abstands d auf rechter Spur"}]},
  {ListPlot[dVar2,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Varianz des Abstands d auf linker Spur"}]},
  {ListPlot[density1,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Dichte der rechten Spur"}]},
  {ListPlot[density2,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Dichte der linken Spur"}]}
  },
  ImageSize->Large,FrameLabel->{"Zeit t",None}
  ];
 (*Zusammenz\[ADoubleDot]hlen des Flusses der beiden Spuren f\[UDoubleDot]r 4 Dichten*)
 savefluss=Table[savefluss1[[n]]+savefluss2[[n]],{n,tMax+1}];
 ListPlot[savefluss,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{"Zeit t","Fluss pro Zeitschritt"}]
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

If[MemberQ[Table[n/5 nCells,{n,4}],nCar],
viAutos=Select[Join[viAutos1,viAutos2],UnsameQ[#, {}]&];
diAutos=Select[Join[diAutos1,diAutos2],UnsameQ[#, {}]&];
vhisto=Histogram[Flatten[viAutos],{1},AxesLabel->{v,"Anzahl Autos mit" Indexed[v,"i"] "f\[UDoubleDot]r die Dichte"},ColorFunction->"Pastel",ImageSize->Medium];
dhisto=Histogram[Flatten[diAutos],{1},AxesLabel->{d,"Anzahl Autos mit" Indexed[d,"i"] "f\[UDoubleDot]r die Dichte"},ColorFunction->"Pastel",ImageSize->Medium];
Print[Show[vhisto]];
Print[Show[dhisto]];
]

(*Dichte \[UDoubleDot]ber die gesamte Stra\[SZ]e*)
AppendTo[density,nCar/nCells];

(*Verkehrsfluss f\[UDoubleDot]r Dichte nCar/nCells*)
AppendTo[fluss,addfluss];
Clear[savefluss1];
Clear[savefluss2];
Clear[density1];
Clear[density2]; (*lieber noch mal einelementige Liste draus machen? Sonst Probleme mit AppendTo?*)
]
(*Fehlend: Dichte getrennt f\[UDoubleDot]r Spuren \[UDoubleDot]ber t*)
(*Fundamentalplot mit addfluss*)
(*ListPlot[Thread[{density,fluss/tMax}],ImageSize->Medium,Frame->True,FrameLabel->{"Dichte \[Rho]","Zeitliches Mittel des Flusses \[UDoubleDot]ber letzte Zelle"},
PlotStyle->RandomChoice[{Red,Orange,Yellow,LightGreen,LightBlue,Blue,Purple,Pink}]] (*Punkte hell f\[UDoubleDot]r dunklen Hintergrund in sp\[ADoubleDot]terem Notebook*)*)
]


twolanesNaSch[300,100,5,0.15]



a=1;
a=2;
a


(* ::Text:: *)
(*(Diskussion)*)


(* ::Chapter:: *)
(*Weiterf\[UDoubleDot]hrende Frage*)


(* ::Text:: *)
(*Dieses Modul zeigt wie sich eine Schlange, aufgrund einer Ampel und einen Blitzer, bilden kann. *)
(**)
