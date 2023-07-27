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
(*F\[UDoubleDot]r eine bessere Lesbarkeit unseres Projekts wird ein Download des ben\[ODoubleDot]tigten Fonts \[UDoubleDot]ber den Link https://www.dafontfree.co/downloads/ac-dc/ empfohlen.*)
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
{xAutos,vAutos,dAutos,minAuto,maxAuto,xnasch,vnasch,dnasch},

(*Listen f\[UDoubleDot]r x, v und d f\[UDoubleDot]r Berechnungen au\[SZ]erhalb des Moduls*)
xnasch={};
vnasch={};
dnasch={};

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
AppendTo[xnasch,xAutos];
(*erzeugt zuf\[ADoubleDot]llige (Random) Liste xAutos ohne Wiederholungen (Sample) in aufsteigender Reihenfolge (Sort)*)
vAutos=RandomInteger[{1,vMax},nCar];
AppendTo[vnasch,vAutos];
(*ordnet jedem Auto eine zuf\[ADoubleDot]llige Geschwindigkeit von 0 bis vMax zu*)
(*Einzelne Autos sind gekennzeichnet durch Element-Position in der Liste mit Position xAutos[[n]] und Geschwindigkeit vAutos[[n]]*)

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=1,i<tMax,i++, (*Schleife der Runden bis tMax, Anfangsposition au\[SZ]erhalb Schleife ist t=1, deshalb bis t=tMax-1, sodass Anzahl Runden = tMax*)

(*Oft verwendete Variablen*)
minAuto=Min[xAutos];
maxAuto=Max[xAutos];

If[Length[xAutos]>1,
((*Freie Zellen d vor dem Auto bis zum vorderen*)
dAutos=Table[If[xAutos[[n]]<maxAuto,xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+minAuto-1],{n,nCar-1}]; (*In Schleife, damit es geupdatet wird*)
(*berechnet freie Zellen zum vorderen Auto normal au\[SZ]er f\[UDoubleDot]r Autos au\[SZ]er dem mit h\[ODoubleDot]chster Positition - da Ringstra\[SZ]e sind die freien Zellen geringer als xAutos[[n+1]]-xAutos[[n]]-1*)
(*Arrays starten mit Element 1; n+1 muss f\[UDoubleDot]r das letzte gleich nCar sein*)
AppendTo[dAutos,If[xAutos[[nCar]]<maxAuto,xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+minAuto-1]];
(*Abstand des Autos an letzter Stelle in Liste zum ersten wird angeh\[ADoubleDot]ngt*)),
dAutos={nCells-1};
];

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

(*Abspeichern in Variablen*)
AppendTo[xnasch,xAutos];
AppendTo[vnasch,vAutos];
(*Letzter Durchlauf der Schleife speichert vorletzten Abstand ab, deshalb au\[SZ]erhalb Schleife letztes Element abspeichern*)
AppendTo[dnasch,dAutos];
];
(*Erneute Berechnung f\[UDoubleDot]r Endposition*)
minAuto=Min[xAutos];
maxAuto=Max[xAutos];

(*Berechnen Abst\[ADoubleDot]nde im letzten Zeitschritt*)
dAutos=Table[If[xAutos[[n]]<maxAuto,xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+minAuto-1],{n,nCar-1}];
AppendTo[dAutos,If[xAutos[[nCar]]<maxAuto,xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+minAuto-1]];

(*Abspeichern in dnasch*)
AppendTo[dnasch,dAutos];

Return[{xnasch,vnasch,dnasch}]
]


nasch100=NaSch[100,300,100,5,0.15];


(*Grafische Darstellung Stra\[SZ]e*)
(*F\[UDoubleDot]r NaSch-Modell ohne VDR q=0*)
densityplot[Modell_,nCar_,p_,q_]:=Module[
{nCells,tMax,vMax,strasse,newstrasse,listdensityplot},

(*Variablen aus vorherigem NaSch-Aufruf*)
nCells=300;
tMax=100;
vMax=5;

(*Daten aus Berechnungen nasch oder vdr*)
nasch=Which[Modell=="NaSch",Which[!p==0.15,NaSch[nCar,nCells,tMax,vMax,p][[1]],p==0.15 && !nCar==100,NaSch[nCar,nCells,tMax,vMax,p][[1]],p==0.15 && nCar==100,nasch100],
Modell=="vdrNaSch",vdrNaSch[nCar,nCells,tMax,vMax,p,q][[1]]];

(*Sortierte Liste der Stra\[SZ]e mit leeren Zellen als 0 und besetzt als 1*)
strasse=Table[Table[If[Select[nasch[[m]],#==n &]=={},0,1],{n,1,nCells}],{m,tMax}];

(*Anpassen strasse sodass t in positive x-Richtung l\[ADoubleDot]uft statt in positiver y-Richtung*)
newstrasse=Table[Table[strasse[[n,m]],{n,tMax}],{m,nCells}];

(*ListDensityPlot*)
listdensityplot=ListDensityPlot[newstrasse,FrameLabel->{"Zeit t","Zellen der Stra\[SZ]e"},ImageSize->Medium,PlotLegends->Automatic,
PlotLabel->ToString[Modell]<>": Grafische Darstellung der Stra\[SZ]e mit "<>ToString[nCar]<>" Autos und p="<>ToString[p],ColorFunction->"SolarColors"];
Return[listdensityplot]
]


listdensityplot=densityplot[nasch];
Show[listdensityplot]


(*Nach Gebrauch f\[UDoubleDot]r RAM-Speicher Variablen clearen wenn nicht weiter ben\[ODoubleDot]tigt*)
Clear[listdensityplot];


(* ::Chapter:: *)
(*Dichteplot*)


(* ::Text:: *)
(*Der N\[ADoubleDot]chste Modul entspricht den dichteplot. es werden eine bestimmte Anzahl an Zelle beobachtet, sonst w\[UDoubleDot]rde die dichte immer gleich. Die dichte entspricht der Anzahl an Autos die an dem entsprechenden Runde in diesen Intervall Zellen gefahren sind.*)
(**)
(*Im Dichteplot wird die Anzahl der Autos N \[UDoubleDot]ber einer Anzahl avCells Zellen berechnet und \[UDoubleDot]ber der Zeit t dargestellt.*)


(*Dichteplot \[UDoubleDot]ber Zeit*)
(*F\[UDoubleDot]r NaSch-Modell ohne VDR q=0*)
dichteplot[Modell_,nCar_,avCells_,p_,q_]:=Module[
(*lokale Variablen*)
{nCells,tMax,vMax,nasch,avstrasse,newavstrasse,tavstrasse,anzahl,laengen,iter,t,cell,listdichteplot},

(*Variablen aus vorherigem NaSch-Aufruf*)
nCells=300;
tMax=100;
vMax=5;

(*Fehlermeldung wenn avCells negativ oder kein Teiler von nCells*)
If[avCells>0 && Divisible[nCells,avCells],
((*Liste Anzahl Autos innerhalb Intervall avCells und das pro Zeitschritt*)
laengen={};

(*Daten aus Berechnungen nasch oder vdr*)
nasch=Which[Modell=="NaSch",Which[!p==0.15,NaSch[nCar,nCells,tMax,vMax,p][[1]],p==0.15 && !nCar==100,NaSch[nCar,nCells,tMax,vMax,p][[1]],p==0.15 && nCar==100,nasch100],
Modell=="vdrNaSch",vdrNaSch[nCar,nCells,tMax,vMax,p,q][[1]]];

(*Anzahl der Intervalle avCells in nCells=iter, Anzahl Autos innerhalb Intervall=anzahl*)
iter=0;t=1;anzahl=0;

(*For-Schleife f\[UDoubleDot]r einzelne Intervalle mit L\[ADoubleDot]nge avCells*)
For[cell=1+iter,cell<=avCells+iter,cell++,

(*Falls Auto auf Zelle, Anzahl erh\[ODoubleDot]hen*)
If[Select[nasch[[t]],#==cell &]!={},anzahl=anzahl+1,anzahl=anzahl];

(*Falls letzte Zelle des Intervalls betrachtet, Anzahl abgespeichert, wieder auf 0*)
If[Divisible[cell,avCells],
AppendTo[laengen,anzahl];
anzahl=0;
(*Falls letzte Subliste in xnasch betrachtet, n\[ADoubleDot]chsten Intervall betrachten ab erster Subliste mit t=0*)
If[t==tMax,
(iter=iter+avCells;
If[iter>nCells-avCells,Break[]];
t=1;),
(*Sonst: n\[ADoubleDot]chste Subliste betrachten mit gleichem Intervall*)
t=t+1;];

(*Betrachten erste Zelle im Intervall f\[UDoubleDot]r n\[ADoubleDot]chsten Durchgang*)
cell=1+iter;
];
];
(*Ergebnis ist Liste mit Anzahlen der Autos von Zelle 0 bis avCells f\[UDoubleDot]r t=1,2,3,..., dann von Zelle avCells+1 bis 2 avCells usw.*)
(*Aufteilen laengen in Sublisten f\[UDoubleDot]r die Intervalle*)
laengen=Partition[laengen,tMax];

listdichteplot=ListDensityPlot[laengen,FrameLabel->{"Zeit t","Intervalle"},ImageSize->Medium,
PlotLabel->ToString[Modell]<>": Dichteplot \[UDoubleDot]ber Zeit t mit Intervalll\[ADoubleDot]nge "<>ToString[avCells],PlotLegends->Automatic];
Return[listdichteplot]),
(*Fehlermeldungen, falls avCells unpassend*)
Which[avCells<=0,Print["Fehler! Die eingegebene Zahl muss gr\[ODoubleDot]\[SZ]er als Null sein."],
!Divisible[nCells,avCells],Print["Fehler! Die eingegebene Zahl muss ein Teiler von nCells=300 sein."]];
];
]


listdichteplot=dichteplot[nasch,10];
Show[listdichteplot]


Clear[listdichteplot];


(* ::Text:: *)
(*Das Dichteplot stellt die Stra\[SZ]e \[UDoubleDot]ber die Zeit dar. Auf der y-Achse sind die Zellen der Stra\[SZ]e mit besetzten Zellen als helle Punkte und leere als dunkelblaue Punkte. Die Entwicklung \[UDoubleDot]ber die Zeit ist in positiver x-Richtung aufgetragen. Es l\[ADoubleDot]sst sich beobachten, dass eine Ansammlung von Autos bei t=0 in der Grafik in positiver x-Richtung nach unten wandert, indem Autos aus den vorderen Zellen der Ansammlung losfahren und die hinteren sich stauen bzw neue Autos auf den Stau auffahren.*)


(* ::Chapter:: *)
(*Histogramme*)


(* ::Text:: *)
(*In diesem Modul werden erzeugt die Histogramme der Geschwindigkeit und den Abstand f\[UDoubleDot]r einen Zeitpunkt tMax. *)
(**)


(*Daten NaSch-Modell f\[UDoubleDot]r nCar=60,100,200*)
histonasch={NaSch[60,300,100,5,0.15],nasch100,NaSch[200,300,100,5,0.15]};


(*Histogramme Geschwindigkeiten und Abstand f\[UDoubleDot]r einen Zeitpunkt*)
(*Falls Modell NaSch, q=0*)
vdhisto[Modell_,carlist_,tlist_,p_,q_]:=Module[
(*tlist sind Zeitpunkte, f\[UDoubleDot]r die Histogramme zu bestimmen sind; auch einzelnen Zeitpunkt als Liste eingeben*)

(*lokale Variablen*)
{nCells,nCar,tMax,vMax,anzahlt,anzahlcars,zeit,viAutos,diAutos,i,vhisto,dhisto,histoplot,nasch},

(*Variablen aus vorherigem NaSch-Aufruf*)
nCells=300;
tMax=100;
vMax=5;

(*Listen der Plots*)
histoplot={};
anzahlt=Length[tlist];
anzahlcars=Length[carlist];

(*F\[UDoubleDot]r betrachtete Anzahlen Autos*)
For[k=1,k<=anzahlcars,k++,

(*Anzahl Autos*)
nCar=carlist[[k]];

(*Daten aus Berechnungen nasch oder vdr*)
nasch=Which[Modell=="NaSch",Which[p==0.15,Which[!MemberQ[{60,100,200},nCar],NaSch[nCar,nCells,tMax,vMax,p][[1]],nCar==60,histonasch[[1]],nCar==100,histonasch[[2]],nCar==200,histonasch[[3]]],
!p==0.15,NaSch[nCar,nCells,tMax,vMax,p]],Modell=="vdrNaSch",vdrNaSch[nCar,nCells,tMax,vMax,p,q]];

For[j=1,j<=anzahlt,j++,
(*Betrachtete Zeit*)
zeit=tlist[[j]];
(*Listen Autos mit Geschwindigkeiten v=0,1,2,3,4,5*)
Clear[viAutos];
viAutos=Table[Select[Table[nasch[[2,zeit,n]],{n,1,nCar}],#==i &],{i,0,5}];

(*Listen Abst\[ADoubleDot]nde d=0,1,...,nCar*)
Clear[diAutos];
diAutos=Select[Table[Select[Table[nasch[[3,zeit,n]],{n,1,nCar}],#==i &],{i,0,Max[nasch[[3,zeit]]]}],UnsameQ[#, {}] &]; 
(*L\[ODoubleDot]schen der Abst\[ADoubleDot]nde, die nicht vorkommen*)

AppendTo[histoplot,Histogram[viAutos,{1},AxesLabel->{v,Anzahl Autos mit Indexed[v,"i"]},ColorFunction->"Pastel",PlotRange->{{Automatic,5.5},Automatic},ImageSize->Medium,
PlotLabel->ToString[Modell]<>": Histogramm von v mit "<>ToString[nCar]<>" Autos, t="<>ToString[zeit]<>" und p="<>ToString[p]]];
AppendTo[histoplot,Histogram[diAutos,{1},AxesLabel->{d,Anzahl Autos mit Indexed[d,"i"]},ColorFunction->"Pastel",PlotRange->{0,All},ImageSize->Medium,
PlotLabel->ToString[Modell]<>": Histogramm von d mit "<>ToString[nCar]<>" Autos, t="<>ToString[zeit]<>" und p="<>ToString[p]]];
(*Histogramm z\[ADoubleDot]hlt, wie oft eine Zahl in einer Liste und den Sublisten darin vorkommt*)
];];
Return[histoplot]
]


histoplot=vdhisto["NaSch",{60,100,200},{50,100},0.15];
GraphicsGrid[Table[{histoplot[[i]],histoplot[[i+1]],histoplot[[i+2]],histoplot[[i+3]]},{i,3}],ImageSize->Full]


Clear[histoplot];


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
(*Falls Modell NaSch, q=0*)
Meanvarfluss[Modell_,nCar_,p_,q_]:=Module[
(*betrachtete Zelle f\[UDoubleDot]r den Fluss ist die letzte Zelle der Stra\[SZ]e*)

(*lokale Variablen*)
{nCells,tMax,vMax,vMittel,dVar,nasch,savefluss,savexAutos,m,meanvarflussplot,meanvarkorrplot},

(*Variablen aus vorherigem NaSch-Aufruf*)
nCells=300;
tMax=100;
vMax=5;

(*Liste f\[UDoubleDot]r Fluss pro Zeitschritt*)
savefluss={};
dVar={};
vMittel={};

(*Daten aus Berechnungen nasch oder vdr*)
nasch=Which[Modell=="NaSch",Which[p==0.15,Which[!MemberQ[{60,100,200},nCar],NaSch[nCar,nCells,tMax,vMax,p][[1]],nCar==60,histonasch[[1]],nCar==100,histonasch[[2]],nCar==200,histonasch[[3]]],
!p==0.15,NaSch[nCar,nCells,tMax,vMax,p][[1]]],Modell=="vdrNaSch",vdrNaSch[nCar,nCells,tMax,vMax,p,q][[1]]];

(*Erstes Auto, das die letzte Zelle passiert, ist das mit h\[ODoubleDot]chster Position*)
m=nCar;

For[t=1,t<=tMax,t++,
(*Verkehrsfluss durch letzte Zelle*)
(*\[CapitalUDoubleDot]berpr\[UDoubleDot]ft, ob Auto mit h\[ODoubleDot]chster Position \[UDoubleDot]ber Stra\[SZ]enende hinaus auf den Anfang zur\[UDoubleDot]ck gefahren ist 
ja: n\[ADoubleDot]chst niedrigeres Auto wird betrachtet + fluss 1 hinzuf\[UDoubleDot]gen, 
nein: Auto wird weiter betrachtet + fluss 0 hinzuf\[UDoubleDot]gen
Index m geht Autos vom letzten Element bis zum ersten durch, danach wieder Start bei letztem*)
If[t<tMax,
If[m==0,m=nCar,m=m];
If[nasch[[1,t+1,m]]<nasch[[1,t,m]],
AppendTo[savefluss,1];
m=m-1;,
AppendTo[savefluss,0];
];];

(*mittlere Geschwindigkeit*)
AppendTo[vMittel, N[Mean[nasch[[2,t]]],6]];
(*Varianz dAutos*)
AppendTo[dVar, N[Variance[nasch[[3,t]]],6]];
];

meanvarflussplot=ResourceFunction["PlotGrid"][{
{ListPlot[vMittel,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"mittlere Geschwindigkeit "OverBar[v]}]},
{ListPlot[dVar,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Varianz des Abstands d"}]},
{ListPlot[savefluss,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Fluss pro Zeitschritt"}]}
},
ImageSize->Large,FrameLabel->{"Zeit t",None}
];

(*Korrelation mittlere Geschwindigkeit und Varianz des Abstands*)
meanvarkorrplot=ListPlot[Thread[{dVar,vMittel}],ImageSize->Medium,ColorFunction->"Rainbow",Frame->True,FrameLabel->{"Varianz des Abstands d","mittlere Geschwindigkeit " OverBar[v]},
PlotLabel->ToString[Modell]<>": Korrelation Varianz des Abstands und mittlere Gewschindigkeit"];
Return[{meanvarflussplot,meanvarkorrplot}]
]


meanvarfluss=Meanvarfluss[nasch];
Show[meanvarfluss[[1]]]
Show[meanvarfluss[[2]]]


Clear[meanvarfluss];


(* ::Text:: *)
(*Die Grafiken von Mittlere Geschwindigkeit, Varianz des Abstands und Der Fluss pro Zeitschritt sind \[UDoubleDot]ber die Zeit dargestellt. Die Zeigen dass die mittlere Geschwindigkeit und die Varianz des abstand haben gr\[ODoubleDot]\[SZ]ere Sprunge. Die Geschwindigkeit variiert zwischen 1 und 2 das bedeutet, dass wann die niedriger werden sich Staus bilden.*)
(*Die Spr\[UDoubleDot]nge von Varianz sind schon h\[ODoubleDot]her als die von der Geschwindigkeit, von 2 bis ungef\[ADoubleDot]hr 10. Die meisten punkte befinden sich um die Varianz herum die in der Mitte zwischen der niedrigste und die h\[ODoubleDot]chste Varianz liegt.*)
(*Die Grafik der Fluss zeigt, wie das nur 1 oder 0 sein kann weil nur eine spur simuliert wird. Da ein Auto nur bis zu einer Zelle vor dem n\[ADoubleDot]chsten fahren kann (R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als d), kann eine Zelle in einer Runde nur von einem Auto durchquert werden.*)
(*Aus der Korrelation der Geschwindigkeit und der Varianz des Abstands ist zu sehen, wie die mittlere Geschwindigkeit linear abnimmt mit der Zunahme von der Varianz des Abstands.*)
(**)


(* ::Chapter:: *)
(*Fundamentalpunkt*)


(* ::Text:: *)
(*Unter fundamental Plot versteht man die Korrelation zwischen Verkehrsfluss \[UDoubleDot]ber die dichte.*)
(*In diesem Teil wird ein Modul daf\[UDoubleDot]r hergestellt und anschlie\[SZ]end das dargestellt.*)
(*Nachdem wird es f\[UDoubleDot]r unterschiedliche Tr\[ODoubleDot]delwahrscheinlichkeiten p geschrieben und geplottet.*)
(**)


(*Daten f\[UDoubleDot]r Fundamentalplots aus histonasch*)
fundnasch60=histonasch[[1,1]];
fundnasch100=histonasch[[2,1]];
fundnasch200=histonasch[[3,1]];


(*Fundamentalplot*)
(*Eingabe: Falls Modell NaSch q=0*)
FundamentalD[Modell_,p_,q_]:=Module[
(*Fluss wird f\[UDoubleDot]r Dichten 0 bis 1 berechnet*)

(*lokale Variablen*)
{nCells,nCar,tMax,vMax,anzahlp,density,addfluss,savexAutos,m,fluss,funddata,fundplot,nasch,tnasch,nexttnasch},

(*Variablen aus vorherigem NaSch-Aufruf*)
nCells=300;
tMax=100;
vMax=5;

(*Erstes Element in Subliste ist Dichte, zweites Fluss*)
funddata={{0,0}};

(*Schleife f\[UDoubleDot]r ansteigende Dichte/Anzahlen an Autos*)
For[nCar=1,nCar<nCells,nCar++,

(*Berechnen NaSch-Modell, VDR-Modell oder Abrufen berechnete Daten aus histonasch*)
nasch=Which[Modell=="NaSch",Which[p==0.15,Which[!MemberQ[{60,100,200},nCar],NaSch[nCar,nCells,tMax,vMax,p][[1]],nCar==60,fundnasch60,nCar==100,fundnasch100,nCar==200,fundnasch200],
!p==0.15,NaSch[nCar,nCells,tMax,vMax,p][[1]]],Modell=="vdrNaSch",vdrNaSch[nCar,nCells,tMax,vMax,p,q][[1]]];

(*Index zum \[CapitalUDoubleDot]berpr\[UDoubleDot]fen der Positionen, startet von der \[UDoubleDot]berpr\[UDoubleDot]ften Zelle nCells*)(*Fluss als Durchfluss von Position nCells zu 1*)
m=nCar;
addfluss=0;

(*Schleife \[UDoubleDot]ber Zeit f\[UDoubleDot]r Berechnung des Flusses*)
For[t=1,t<=tMax,t++,

(*Verkehrsfluss durch letzte Zelle -> Anzahl Autos durch Zelle pro Zeitschritt*)
If[t<tMax,

(*Abspeichern zu betrachtende Listen*)
tnasch=nasch[[t]];
nexttnasch=nasch[[t+1]];

If[m==0,m=nCar,m=m];
(*erster Iterator: nCar-Sublisten, Verschiebung k (nCells-1), da Listen f\[UDoubleDot]r andere p angeh\[ADoubleDot]ngt; zweiter Iterator: xnasch abrufen; 
dritter Iterator: Zeitschritt, vierter Iterator: Auto an Position m in Liste*)
If[nexttnasch[[m]]<tnasch[[m]],
addfluss=addfluss+1;
m=m-1;
];];
];
Clear[nasch];
Clear[tnasch];
Clear[nexttnasch];

(*Dichte und Verkehrsfluss*)
AppendTo[funddata,{nCar/nCells,addfluss}];
];

AppendTo[funddata,{1,0}];

(*Gesamtfluss durch Zeit teilen*)
funddata=Table[funddata[[n,2]]/tMax,{n,Length[funddata]}];

(*Fundamentalplot mit density und addfluss*)
fundplot=ListPlot[funddata,ImageSize->Medium,PlotRange->{Automatic,{0,0.9}},Frame->True,FrameLabel->{"Dichte \[Rho]","Zeitliches Mittel des Flusses"},
PlotStyle->RandomChoice[{Red,Orange,Yellow,LightGreen,LightBlue,Blue,Purple,Pink}],PlotLabel->ToString[Modell]<>": Fundamentalplot mit p="<>ToString[p]]; (*Punkte hell f\[UDoubleDot]r dunklen Hintergrund in sp\[ADoubleDot]terem Notebook*)

Return[fundplot]
]


fundplot={};
AppendTo[fundplot,FundamentalD["NaSch",0]];(*;{0,0.1,0.15,0.2,0.25,0.3,0.5,0.75,1}*)
AppendTo[fundplot,FundamentalD["NaSch",0.1]];
AppendTo[fundplot,FundamentalD["NaSch",0.15]];
AppendTo[fundplot,FundamentalD["NaSch",0.2]];
AppendTo[fundplot,FundamentalD["NaSch",0.25]];
AppendTo[fundplot,FundamentalD["NaSch",0.3]];
AppendTo[fundplot,FundamentalD["NaSch",0.5]];
AppendTo[fundplot,FundamentalD["NaSch",0.75]];
AppendTo[fundplot,FundamentalD["NaSch",1]];

GraphicsGrid[Table[fundplot[[n]],{m,1,9,3},{n,m,m+2,1}],ImageSize->Full]


Clear[fundplot];
Clear[fundnasch60];
Clear[fundnasch100];
Clear[fundnasch200];


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
vdrNaSch[nCar_,nCells_,tMax_,vMax_,p_,q_]:=Module[
(*q ist zus\[ADoubleDot]tzliche Wahrscheinlichkeit zum Tr\[ODoubleDot]deln beim Anfahren*)

(*lokale Variablen*)
{xAutos,vAutos,dAutos,xvdr,vvdr,dvdr,minAuto,maxAuto},

(*Erzeugen Listen f\[UDoubleDot]r x, v und d f\[UDoubleDot]r jeden Zeitschritt*)
xvdr={};
vvdr={};
dvdr={};

(*Autos haben Position x und Geschwindigkeit v zum vorderen Auto*)
xAutos=Sort[RandomSample[Range[nCells],nCar]];
vAutos=RandomInteger[{1,vMax},nCar];

(*Abspeichern Anfangsaufstellung*)
AppendTo[xvdr,xAutos];
AppendTo[vvdr,vAutos];

(*Verkehrsregeln aus NaSch-Modell implementieren*)
For[i=0,i<=tMax,i++, 

(*Oft verwendete Variablen*)
minAuto=Min[xAutos];
maxAuto=Max[xAutos];

(*Freie Zellen d vor dem Auto bis zum vorderen*)
dAutos=Table[If[xAutos[[n]]<maxAuto,xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+minAuto-1],{n,nCar-1}];
AppendTo[dAutos,If[xAutos[[nCar]]<maxAuto,xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+minAuto-1]];

(*R1: Beschleunigen, falls vMax noch nicht erreicht*)
vAutos=Table[Min[vAutos[[n]]+1,vMax],{n,nCar}];

(*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
vAutos=Table[Min[dAutos[[n]],vAutos[[n]]],{n,nCar}]; 

(*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)(*Randomization, q=p0-p, p+q wahrscheinlichkeit dass sie tr\[ODoubleDot]deln falls die vorher v=0 hatten, p=wahrsch. v>0*)
vAutos=Table[If[vAutos[[n]]==0,If[RandomReal[{0,1}]<=p+q,vAutos[[n]],vAutos[[n]]],If[RandomReal[{0,1}]<=p,vAutos[[n]]=vAutos[[n]]-1,vAutos[[n]]]],{n,nCar}];
(*Wenn Auto steht ist p um q erh\[ODoubleDot]ht*)

(*R4: Fahren um vAutos Zellen*)
xAutos=Table[If[xAutos[[n]]+vAutos[[n]]<=nCells,xAutos[[n]]=xAutos[[n]]+vAutos[[n]],xAutos[[n]]=xAutos[[n]]+vAutos[[n]]-nCells],{n,nCar}];

(*Abspeichern in globale Variablen*)
AppendTo[xvdr,xAutos];
AppendTo[vvdr,vAutos];
(*Letzter Durchlauf der Schleife speichert vorletzten Abstand ab, deshalb au\[SZ]erhalb Schleife letztes Element abspeichern*)
AppendTo[dvdr,dAutos];
];
(*Erneute Berechnung f\[UDoubleDot]r Endposition*)
minAuto=Min[xAutos];
maxAuto=Max[xAutos];

(*Berechnen Abst\[ADoubleDot]nde im letzten Zeitschritt*)
dAutos=Table[If[xAutos[[n]]<maxAuto,xAutos[[n+1]]-xAutos[[n]]-1,nCells-xAutos[[n]]+minAuto-1],{n,nCar-1}];
AppendTo[dAutos,If[xAutos[[nCar]]<maxAuto,xAutos[[1]]-xAutos[[nCar]]-1,nCells-xAutos[[nCar]]+minAuto-1]];

(*Abspeichern in dvdr*)
AppendTo[dvdr,dAutos];

Return[{xvdr,vvdr,dvdr}]
]


(*Plots zum Untersuchen des VDR-Modells*)
vdrplots={};
AppendTo[vdrplots,vdhisto["vdrNaSch",{60,100},{100},0.15,0.2]];
AppendTo[vdrplots,vdhisto["vdrNaSch",{60,100},{100},0.15,0.4]];
AppendTo[vdrplots,dichteplot["vdrNaSch",100,10,0.15,0.2]];
AppendTo[vdrplots,dichteplot["vdrNaSch",100,10,0.15,0.4]];
AppendTo[vdrplots,FundamentalD["vdrNaSch",0.15,0.2]];
AppendTo[vdrplots,FundamentalD["vdrNaSch",0.3,0.2]];


vdrplots=Flatten[vdrplots];
GraphicsGrid[Table[vdrplots[[n]],{m,1,5,4},{n,m,m+3}],ImageSize->Full]
GraphicsGrid[Table[vdrplots[[n]],{m,9,11,2},{n,m,m+1}],ImageSize->Full]


Clear[vdrplots];


(* ::Text:: *)
(*Aus den ersten zwei Histogrammen kann gefolgert werden, dass aufgrund von der geringeren Anzahl an Autos (60) \[UDoubleDot]ber der Anzahl an Zellen (300) die Erh\[ODoubleDot]hung der Tr\[ODoubleDot]delwahrscheinlichkeit q nicht h\[ADoubleDot]ufig vorkommt, weil der Platz zwischen zwei Autos h\[ADoubleDot]ufig h\[ODoubleDot]her ist als dessen Geschwindigkeit. *)
(*Das passiert weniger bei einer h\[ODoubleDot]heren Anzahl an Fahrzeugen (z.B. 100 \[UDoubleDot]ber 300), weil die Abst\[ADoubleDot]nde geringer sind. Das verursacht eine Erh\[ODoubleDot]hung der Tr\[ODoubleDot]delwahrscheinlichkeit um q, weshalb sich bilden Staus bilden. Eine sehr geringere Menge an Autos haben einen gr\[ODoubleDot]\[SZ]eren Abstand.*)
(*Bei 200 Autos ist letzteres nicht mehr erkennbar, die Abst\[ADoubleDot]nde sind sehr gering.*)
(*Aus den n\[ADoubleDot]chsten Histogrammen, mit p erh\[ODoubleDot]ht zu 0.3, l\[ADoubleDot]sst sich beobachten, dass aufgrund der h\[ODoubleDot]heren Tr\[ODoubleDot]delwahrscheinlichkeit viele Autos eine geringere Geschwindigkeit haben im Vergleich zu p=0.15. *)
(*Aus dem Dichteplot kann beobachtet werden, dass mit einer Wahrscheinlichkeit von p=0.15 die lokale Dichte der Autos mehrmals h\[ODoubleDot]her ist, es bilden sich gr\[ODoubleDot]\[SZ]ere Staus. Mit einer Wahrscheinlichkeit p=0.3 ist die lokale Dichte h\[ADoubleDot]ufig h\[ODoubleDot]her auf der ganzen Stra\[SZ]e, aber die Staus werden k\[UDoubleDot]rzer.*)
(*Die Fundamentalplots sehen wie die f\[UDoubleDot]r 0.15 und 0.3 aus der Fundamentalplots teil (s. oben) aus. Also wird die zus\[ADoubleDot]tzliche q nicht in der Grafik viel auswirken.*)


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
{xAutos,xAutos1,xAutos2,vAutos1,vAutos2,dAutos1,dAutos2,viAutos,viAutos1,viAutos2,diAutos,diAutos1,diAutos2,nCar,density,density1,density2,
fluss,addfluss,savefluss,savefluss1,savefluss2,savexAutos1,savexAutos2,savevAutos1,savevAutos2,m1,m2,l1,l2,savem1,savem2,savel1,savel2,
vMittel,dVar1,dVar2,index1,index2,nachindex1,nachindex2,h,i,j,k,o,r,s,rhoplot,strecke1,strecke2,fahrt1,fahrt2,
vdvardensplot,flussplot,fundplot,histoplot,laengesx1,laengesx2,laengex1,laengex2},

(*Erzeugen einelementige Liste mit Gesamtdichte und Fluss f\[UDoubleDot]r nCar=0, f\[UDoubleDot]r jedes nCar werden Werte hinzugef\[UDoubleDot]gt*) 
density={0};
fluss={0};

(*Erzeugen Listen f\[UDoubleDot]r Plots*)
histoplot=Table[Nothing,{n,1}];
vdvardensplot=Table[Nothing,{n,1}];
flussplot=Table[Nothing,{n,1}];

(*Liste Anzahlen Autos, f\[UDoubleDot]r die Histogramme, dVar, vMittel und der Fluss geplottet werden*)
rhoplot={60,100,200};
Clear[nachindex1];

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
xAutos2=Which[xAutos1!={},Drop[Table[xAutos[[n]]-nCells,{n,nCar}],Length[xAutos1]],xAutos1=={},Table[xAutos[[n]]-nCells,{n,nCar}]];
Print["Nach Erstellen: xAutos1=",xAutos1,", xAutos2=",xAutos2];
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

(*Oft verwendete Variablen*)
laengex1=Length[xAutos1];
laengex2=Length[xAutos2];
laengesx1=Length[savexAutos1];
laengesx2=Length[savexAutos2];

  (*Freie Zellen d vor dem Auto bis zum vorderen*)
  (*Rechte Spur*)
  If[laengex1>1,
  (dAutos1=Table[If[xAutos1[[n]]<Max[xAutos1],xAutos1[[n+1]]-xAutos1[[n]]-1,nCells-xAutos1[[n]]+Min[xAutos1]-1],{n,laengex1-1}];
  AppendTo[dAutos1,If[xAutos1[[laengex1]]<Max[xAutos1],xAutos1[[1]]-xAutos1[[laengex1]]-1,nCells-xAutos1[[laengex1]]+xAutos1[[1]]-1]];),
  dAutos1={nCells-1};
  ]; 
  (*Linke Spur*)
  If[laengex2>1,
  (dAutos2=Table[If[xAutos2[[n]]<Max[xAutos2],xAutos2[[n+1]]-xAutos2[[n]]-1,nCells-xAutos2[[n]]+Min[xAutos2]-1],{n,laengex2-1}]; 
  AppendTo[dAutos2,If[xAutos2[[laengex2]]<Max[xAutos2],xAutos2[[1]]-xAutos2[[laengex2]]-1,nCells-xAutos2[[laengex2]]+xAutos2[[1]]-1]];),
  dAutos2={nCells-1};
  ];
  
  (*R1: Beschleunigen, falls vMax noch nicht erreicht*)
  (*Rechte Spur*)
  If[laengex1>0,
  vAutos1=Table[Min[vAutos1[[n]]+1,vMax],{n,laengex1}];
  ];
  savevAutos1=vAutos1;

  (*Linke Spur*)
  If[laengex2>0,
  vAutos2=Table[Min[vAutos2[[n]]+1,vMax],{n,laengex2}];
  ];
  savevAutos2=vAutos2;
  Print["Vor Spurenwechsel: vAutos1=",vAutos1,", vAutos2=",vAutos2,", nCar=",nCar,", xAutos1=",xAutos1,", xAutos2=",xAutos2];
  
  (*R2: Spurwechsel, falls v gr\[ODoubleDot]\[SZ]er als Abstand d und links frei -> bei Wechsel zun\[ADoubleDot]chst Spurwechsel, dann weiter fahren mit v-1 in R3-R5*)
  
  (*Wechsel rechte Spur zu linker*)
  (*Falls Auto auf rechter Spur*)
  If[laengesx1>0,
  k=1;
  Do[
  (*Falls v gr\[ODoubleDot]\[SZ]er d und Nachbarzelle links frei, \[CapitalUDoubleDot]berpr\[UDoubleDot]fen Spurwechsel*)
  (*Falls Auto mit kleinerer Position existiert*)
  If[savevAutos1[[k]]>dAutos1[[k]] && Select[savexAutos2,#==savexAutos1[[k]] &]=={} && UnsameQ[Select[savexAutos2,#<savexAutos1[[k]] &],{}],
  ((*betrachteter Index: Auto mit n\[ADoubleDot]chstkleinerer Position auf linker Spur*)
  index2=Lookup[PositionIndex[savexAutos2],Max[Select[savexAutos2,#<savexAutos1[[k]] &]]][[1]]; 
  (*Strecke des ankommenden Autos links mit Abbremsen um 1*)
  strecke2=Max[savexAutos2[[index2]]+savevAutos2[[index2]]-1,savexAutos2[[index2]]];
  (*Weiterfahrt nach Wechsel*)
  fahrt1=Max[savexAutos1[[k]]+savevAutos1[[k]]-1,savexAutos1[[k]]]; (*Max suchen unn\[ODoubleDot]tig, v muss gr\[ODoubleDot]\[SZ]er d, also mindestens 1 sein*)
  (*Index des n\[ADoubleDot]chsten Autos*)
  nachindex2=Which[index2<laengesx2,index2+1,index2>=laengesx2,1];
  (*Strecke des ankommenden Autos mit m\[ODoubleDot]glichem Ausbremsen um 1 muss kleiner als eigene Position nach Wechsel und Weiterfahrt mit v-1,
  letztere mit Ausgebremst werden um 1 muss kleiner als Position des vorderen Autos mit Weiterfahrt*)
  If[strecke2<fahrt1 && Max[fahrt1-1,savexAutos1[[k]]]<savexAutos2[[nachindex2]]+savevAutos2[[nachindex2]], 
  (*Auto von rechter Spur auf Nachbarzelle, v auf v-1 setzen, Indizes f\[UDoubleDot]r Flussberechnung anpassen*)
  (*Verschiebung r durch hinzugef\[UDoubleDot]gte Autos zu xAutos2*)
  Clear[r];
  r=Lookup[PositionIndex[xAutos2],savexAutos2[[index2]]][[1]];
  xAutos2=Insert[xAutos2,savexAutos1[[k]],r+1];
  vAutos2=Insert[vAutos2,savevAutos1[[k]]-1,r+1];
  m2=Which[r+1<=m2,m2+1,r+1>m2,m2];
  l2=Which[r+1<=l2,l2+1,r+1>l2,l2];
  (*Verschiebung s durch entfernte Autos aus xAutos1*)
  Clear[s];
  s=Lookup[PositionIndex[xAutos1],savexAutos1[[k]]][[1]];
  xAutos1=Delete[xAutos1,s];
  vAutos1=Delete[vAutos1,s];
  m1=Which[s<=m1,m1-1,s>m1,m1];
  l1=Which[s<=l1,l1-1,s>l1,l1];
  Print["Zeile 715: In Schritt k=",k,"savexAutos1=",savexAutos1,", savexAutos2=",savexAutos2,", xAutos1=",xAutos1,", xAutos2=",xAutos2,", index2=",index2,", r=",r,", dAutos1=",dAutos1,", savevAutos1=",savevAutos1];
  ];),
  ((*Auto xAutos1[[k]] ist Auto mit kleinster Position, \[CapitalUDoubleDot]berpr\[UDoubleDot]fen anderes Ende, da Ringstra\[SZ]e*)
  (*Falls linke Spur nicht leer*)
  If[laengesx2>0,
  ((*betrachteter Index: Auto mit gr\[ODoubleDot]\[SZ]ter Position, also erstes am anderen Ende*)
  index2=Lookup[PositionIndex[savexAutos2],Max[savexAutos2]][[1]];
  (*Strecke des ankommenden Autos mit Abbremsen um 1*)
  strecke2=Max[savexAutos2[[index2]]+savevAutos2[[index2]]-1-nCells,savexAutos2[[index2]]-nCells];
  (*Weiterfahrt nach Wechsel*)
  fahrt1=Max[savexAutos1[[k]]+savevAutos1[[k]]-1,savexAutos1[[k]]];
  (*Index des n\[ADoubleDot]chsten Autos*)
  nachindex2=Which[index2<laengesx2,index2+1,index2>=laengesx2,1]; 
  (*Strecke des ankommenden Autos mit m\[ODoubleDot]glichem Ausbremsen um 1 muss kleiner als eigene Position nach Wechsel und Weiterfahrt mit v-1,
  letztere muss mit m\[ODoubleDot]glichem Abbremsen um 1 kleiner als Position des vorderen Autos mit Weiterfahrt*)
  If[strecke2<fahrt1 && Max[fahrt1-1,savexAutos1[[k]]]<savexAutos2[[nachindex2]]+savevAutos2[[nachindex2]]-nCells,
  (*Auto von rechter Spur auf Nachbarzelle, v auf v-1 setzen, Indizes f\[UDoubleDot]r Flussberechnung anpassen*)
  (*Verschiebung r durch hinzugef\[UDoubleDot]gte Autos zu xAutos2*)
  Clear[r];
  r=Lookup[PositionIndex[xAutos2],savexAutos2[[index2]]][[1]];
  xAutos2=Insert[xAutos2,savexAutos1[[k]],r+1];
  vAutos2=Insert[vAutos2,savevAutos1[[k]]-1,r+1];
  m2=Which[r+1<=m2,m2+1,r+1>m2,m2];
  l2=Which[r+1<=l2,l2+1,r+1>l2,l2];
  (*Verschiebung s durch entfernte Autos aus xAutos1*)
  Clear[s];
  s=Lookup[PositionIndex[xAutos1],savexAutos1[[k]]][[1]];
  xAutos1=Delete[xAutos1,s];
  vAutos1=Delete[vAutos1,s];
  m1=Which[s<=m1,m1-1,s>m1,m1];
  l1=Which[s<=l1,l1-1,s>l1,l1];
  Print["Zeile 754: In Schritt k=",k,"savexAutos1=",savexAutos1,", savexAutos2=",savexAutos2,", xAutos1=",xAutos1,", xAutos2=",xAutos2,", index2=",index2,", r=",r,", dAutos1=",dAutos1,", savevAutos1=",savevAutos1];
  ];),
  (*Falls linke Spur leer: Spurwechsel, Anpassung v, Indizes f\[UDoubleDot]r Fluss*)
  ((*Durch Spurwechsel neue L\[ADoubleDot]nge von xAutos2*)
  Clear[laengex2];
  laengex2=Length[xAutos2];
  index2=Which[laengex2>0,
  Which[Select[xAutos2,#<savexAutos1[[k]] &]!={},Lookup[PositionIndex[xAutos2],Max[Select[xAutos2,#<savexAutos1[[k]] &]]][[1]],Select[xAutos2,#<savexAutos1[[k]] &]=={},0],
  laengex2==0,0];
  xAutos2=Insert[xAutos2,savexAutos1[[k]],index2+1];
  vAutos2=Insert[vAutos2,savevAutos1[[k]]-1,index2+1];
  m2=Which[index2+1<=m2,m2+1,index2+1>m2,m2];
  l2=Which[index2+1<=l2,l2+1,index2+1>l2,l2];
  (*Verschiebung s durch entfernte Autos aus xAutos1*)
  Clear[s];
  s=Lookup[PositionIndex[xAutos1],savexAutos1[[k]]][[1]];
  xAutos1=Delete[xAutos1,s];
  vAutos1=Delete[vAutos1,s];
  m1=Which[s<=m1,m1-1,s>m1,m1];
  l1=Which[s<=l1,l1-1,s>l1,l1];
  Print["Zeile 780: In Schritt k=",k,"savexAutos1=",savexAutos1,", savexAutos2=",savexAutos2,", xAutos1=",xAutos1,", xAutos2=",xAutos2,", index2=",index2,", s=",s,", dAutos1=",dAutos1,", savevAutos1=",savevAutos1];
  )
  ];)
  ];
  (*N\[ADoubleDot]chstes Auto \[CapitalUDoubleDot]berpr\[UDoubleDot]fen*)
  k=k+1;,laengesx1];
  ];
 
  (*Wechsel linke Spur zu rechter*) 
  (*Falls Auto auf linker Spur*)
  If[laengesx2>0,
  h=1;
  Do[
  (*Falls rechte Nachbarzelle leer*)
  (*Falls Auto mit kleinerer Position existiert*)
  If[Select[savexAutos1,#==savexAutos2[[h]] &]=={} && UnsameQ[Select[savexAutos1,#<savexAutos2[[h]] &],{}], 
  ((*Betrachteter Index: n\[ADoubleDot]chstes Auto mit kleinerer Position*)
  index1=Lookup[PositionIndex[savexAutos1],Max[Select[savexAutos1,#<savexAutos2[[h]] &]]][[1]];
  Print["index1=",index1,", Position des n\[ADoubleDot]chstkleineren Elements =",Position[savexAutos1,Max[Select[savexAutos1,#<savexAutos2[[h]] &]]]];
  (*Strecke des ankommenden Autos rechts mit Abbremsen um 1*)
  strecke1=Max[savexAutos1[[index1]]+savevAutos1[[index1]]-1,savexAutos1[[index1]]];
  (*Weiterfahrt nach Wechsel*)
  fahrt2=Max[savexAutos2[[h]]+savevAutos2[[h]]-1,savexAutos2[[h]]];
  (*Index des n\[ADoubleDot]chsten Autos*)
  nachindex1=Which[index1<laengesx1,index1+1,index1>=laengesx1,1];
  (*Strecke des ankommenden Autos mit m\[ODoubleDot]glichem Ausbremsen um 1 muss kleiner als eigene Position nach Wechsel und Weiterfahrt mit v-1,
  Weiterfahrt mit Abbremsen um 1 oder 2 muss kleiner als Position des vorderen Autos mit Weiterfahrt*)
  If[strecke1<fahrt2 && Max[fahrt2-1,savexAutos2[[h]]]<savexAutos1[[nachindex1]]+savevAutos1[[nachindex1]] (*|| Max[fahrt2-2,savexAutos2[[h]]]<savexAutos1[[nachindex1]]+savevAutos1[[nachindex1]]*),
  (*Verschiebung o von index1 durch hinzugef\[UDoubleDot]gte Autos zu xAutos1 minus entfernte Autos aus xAutos1*)
  Clear[o];
  o=Lookup[PositionIndex[xAutos1],savexAutos1[[index1]]][[1]];
  xAutos1=Insert[xAutos1,savexAutos2[[h]],o+1];
  vAutos1=Insert[vAutos1,savevAutos2[[h]]-1,o+1];
  m1=Which[o+1<=m1,m1+1,o+1>m1,m1];
  l1=Which[o+1<=l1,l1+1,o+1>l1,l1];
  (*Verschiebung j von h durch hinzugef\[UDoubleDot]gte Autos zu xAutos2 minus entfernte Autos aus xAutos2*)
  Clear[j];
  j=Lookup[PositionIndex[xAutos2],savexAutos2[[h]]][[1]];
  xAutos2=Delete[xAutos2,j];
  vAutos2=Delete[vAutos2,j]; 
  m2=Which[j<=m2,m2-1,j>m2,m2];
  l2=Which[j<=l2,l2-1,j>l2,l2];
  Print["Zeile 822: In Schritt h=",h,"savexAutos1=",savexAutos1,", savexAutos2=",savexAutos2,", xAutos1=",xAutos1,", xAutos2=",xAutos2,", index1=",index1,", o=",o,", dAutos1=",dAutos1,", savevAutos1=",savevAutos1];
  ];),
  (*Auto savexAutos1[[h]] ist Auto mit kleinster Position, \[CapitalUDoubleDot]berpr\[UDoubleDot]fen anderes Ende*)
  ((*Falls rechte Spur nicht leer*)
  If[laengesx1>0,
  ((*Betrachteter Index: Auto mit h\[ODoubleDot]chster Position, also anderes Ende*)
  index1=Lookup[PositionIndex[savexAutos1],Max[savexAutos1]][[1]];
  (*Strecke des ankommenden Autos rechts mit Abbremsen um 1*)
  strecke1=Max[savexAutos1[[index1]]+savevAutos1[[index1]]-1-nCells,savexAutos1[[index1]]-nCells];
  (*Weiterfahrt nach Wechsel*)
  fahrt2=Max[savexAutos2[[h]]+savevAutos2[[h]]-1,savexAutos2[[h]]];
  (*Index des n\[ADoubleDot]chsten Autos*)
  nachindex1=Which[index1<laengesx1,index1+1,index1>=laengesx1,1];
  (*Falls Spurwechsel ohne Auffahrunfall von hinten und Weiterfahren mit v-1 ohne Anfahren des vorderen Autos m\[ODoubleDot]glich*)
  If[strecke1-nCells<fahrt2 
  && Max[fahrt2-1,savexAutos2[[h]]]<savexAutos1[[nachindex1]]+savevAutos1[[nachindex1]]-nCells (*|| Max[fahrt2-2,savexAutos2[[h]]]<savexAutos1[[nachindex1]]+savevAutos1[[nachindex1]]-nCells*),
  (*Auto von rechter Spur auf Nachbarzelle, v auf v-1 setzen, Indizes f\[UDoubleDot]r Flussberechnung anpassen*)
  (*Verschiebung o von index1 durch hinzugef\[UDoubleDot]gte Autos zu xAutos1 minus entfernte Autos aus xAutos1*)
  Clear[o];
  o=Lookup[PositionIndex[xAutos1],savexAutos1[[index1]]][[1]];
  xAutos1=Insert[xAutos1,savexAutos2[[h]],o+1];
  vAutos1=Insert[vAutos1,savevAutos2[[h]]-1,o+1];
  m1=Which[o+1<=m1,m1+1,o+1>m1,m1];
  l1=Which[o+1<=l1,l1+1,o+1>l1,l1];
  (*Verschiebung j von h durch hinzugef\[UDoubleDot]gte Autos zu xAutos2 minus entfernte Autos aus xAutos2*)
  Clear[j];
  j=Lookup[PositionIndex[xAutos2],savexAutos2[[h]]][[1]];
  xAutos2=Delete[xAutos2,j];
  vAutos2=Delete[vAutos2,j];
  m2=Which[j<=m2,m2-1,j>m2,m2];
  l2=Which[j<=l2,l2-1,j>l2,l2];
  Print["Zeile 855: In Schritt h=",h,"savexAutos1=",savexAutos1,", savexAutos2=",savexAutos2,", xAutos1=",xAutos1,", xAutos2=",xAutos2,", index1=",index1,", o=",o,", dAutos1=",dAutos1,", savevAutos1=",savevAutos1];
  ];),
  (*Falls rechte Spur zun\[ADoubleDot]chst leer: Spurwechsel, Anpassung v, Indizes f\[UDoubleDot]r Fluss*)
  ((*L\[ADoubleDot]nge xAutos1 nach m\[ODoubleDot]glichen Spurwechseln*)
  Clear[laengex1];
  laengex1=Length[xAutos1];
  index1=Which[laengex1>0,
  Which[Select[xAutos1,#<savexAutos2[[h]] &]!={},Lookup[PositionIndex[xAutos1],Max[Select[xAutos1,#<savexAutos2[[h]] &]]][[1]],Select[xAutos1,#<savexAutos2[[h]] &]=={},0],
  laengex1==0,0];
  xAutos1=Insert[xAutos1,savexAutos2[[h]],index1+1];
  vAutos1=Insert[vAutos1,savevAutos2[[h]]-1,index1+1];
  m1=Which[index1+1<=m1,m1+1,index1+1>m1,m1];
  l1=Which[index1+1<=l1,l1+1,index1+1>l1,l1];
  (*Verschiebung o von h durch hinzugef\[UDoubleDot]gte Autos zu xAutos2 minus entfernte Autos aus xAutos2*)
  Clear[j];
  j=Lookup[PositionIndex[xAutos2],savexAutos2[[h]]][[1]];
  xAutos2=Delete[xAutos2,j];
  vAutos2=Delete[vAutos2,j];
  m2=Which[j<=m2,m2-1,j>m2,m2];
  l2=Which[j<=l2,l2-1,j>l2,l2];
  Print["Zeile 877: In Schritt h=",h,", savexAutos1=",savexAutos1,", savexAutos2=",savexAutos2,", xAutos1=",xAutos1,", xAutos2=",xAutos2,", index1=",index1,", j=",j,", dAutos1=",dAutos1,", savevAutos1=",savevAutos1];
  )
  ];)
  ];
  (*Betrachten n\[ADoubleDot]chstes Auto in savexAutos*)
  h=h+1;,laengesx2];
  ];
  Clear[laengex1];
  Clear[laengex2];
  laengex1=Length[xAutos1];
  laengex2=Length[xAutos2];
  
  (*Nach Spurwechsel freie Zellen d vor dem Auto bis zum vorderen*)
  (*Rechte Spur*)
  Clear[dAutos1];
  If[laengex1>1,
  (dAutos1=Table[If[xAutos1[[n]]<Max[xAutos1],xAutos1[[n+1]]-xAutos1[[n]]-1,nCells-xAutos1[[n]]+Min[xAutos1]-1],{n,laengex1-1}];
  AppendTo[dAutos1,If[xAutos1[[laengex1]]<Max[xAutos1],xAutos1[[1]]-xAutos1[[laengex1]]-1,nCells-xAutos1[[laengex1]]+xAutos1[[1]]-1]];),
  dAutos1={nCells-1};
  ]; 
  (*Linke Spur*)
  Clear[dAutos2];
  If[laengex2>1,
  (dAutos2=Table[If[xAutos2[[n]]<Max[xAutos2],xAutos2[[n+1]]-xAutos2[[n]]-1,nCells-xAutos2[[n]]+Min[xAutos2]-1],{n,laengex2-1}]; 
  AppendTo[dAutos2,If[xAutos2[[laengex2]]<Max[xAutos2],xAutos2[[1]]-xAutos2[[laengex2]]-1,nCells-xAutos2[[laengex2]]+xAutos2[[1]]-1]];),
  dAutos2={nCells-1};
  ];
   
  (*R3: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
  (*Rechte Spur*)
  If[laengex1>0,
  vAutos1=Table[Min[dAutos1[[n]],vAutos1[[n]]],{n,laengex1}];
  ];
  (*Linke Spur*)
  If[laengex2>0,
  vAutos2=Table[Min[dAutos2[[n]],vAutos2[[n]]],{n,laengex2}];
  ];
  
  (*R4: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
  (*Rechte Spur*)
  If[laengex1>0,
  vAutos1=Table[If[RandomReal[{0,1}]<=p,vAutos1[[n]]=Max[vAutos1[[n]]-1,0],vAutos1[[n]]],{n,laengex1}]; 
  ];
  (*Linke Spur*)
  If[laengex2>0,
  vAutos2=Table[If[RandomReal[{0,1}]<=p,vAutos2[[n]]=Max[vAutos2[[n]]-1,0],vAutos2[[n]]],{n,laengex2}];
  ];
  Print["Vor R5: xAutos1=",xAutos1,", xAutos2=",xAutos2,", vAutos1=",vAutos1,", vAutos2=",vAutos2];
  
  (*R5: Fahren um vAutos Zellen*)
  (*Rechte Spur*)
  If[laengex1>0,
  xAutos1=Table[If[xAutos1[[n]]+vAutos1[[n]]<=nCells,xAutos1[[n]]=xAutos1[[n]]+vAutos1[[n]],xAutos1[[n]]=xAutos1[[n]]+vAutos1[[n]]-nCells],{n,laengex1}];,
  (AppendTo[density1,0];
  AppendTo[savefluss1,0];)
  ];
  (*Linke Spur*)
  If[laengex2>0,
  xAutos2=Table[If[xAutos2[[n]]+vAutos2[[n]]<=nCells,xAutos2[[n]]=xAutos2[[n]]+vAutos2[[n]],xAutos2[[n]]=xAutos2[[n]]+vAutos2[[n]]-nCells],{n,laengex2}];,
  (AppendTo[density2,0];
  AppendTo[savefluss2,0];)
  ];
  Print["Nach R5: xAutos1=",xAutos1,", xAutos2=",xAutos2];
  (*Verkehrsfluss durch letzte Zelle -> Anzahl Autos durch Zelle pro Zeitschritt*)
  (*Rechte Spur*)
  If[m1<=0,m1=laengex1,m1=m1]; (*Fluss muss auch in Spurwechsel True mit If[Max[xAutosi]+vAutosi[[Flatten[Position[Max[xAutosi]]]]]>nCells, addfluss=addfluss+1; m1=m1-1;], gleich f\[UDoubleDot]r savefluss*)
  If[savem1<=0,savem1=laengesx1,savem1=savem1];
  If[laengesx1>0,
  If[m1>0 && xAutos1[[m1]]<savexAutos1[[savem1]], (*Fehlend: Index f\[UDoubleDot]r savexAutos muss separate Variable sein, separat hoch gez\[ADoubleDot]hlt wenn Fluss*)
  (addfluss=addfluss+1;
  m1=m1-1;
  savem1=savem1-1;),
  addfluss=addfluss;
  ];
  ];
  (*Linke Spur*)
  If[m2<=0,m2=laengex2,m2=m2];
  If[savem2<=0,savem2=laengesx2,savem2=savem2];
  If[laengesx2>0,
  If[m2>0 && xAutos2[[m2]]<savexAutos2[[savem2]],
  (addfluss=addfluss+1;
  m2=m2-1;
  savem2=savem2-1;),
  addfluss=addfluss;
  ];
  ];
  (*Gibt mittlere v, Varianz von d, Fluss und Dichte der Spur \[UDoubleDot]ber t f\[UDoubleDot]r 3 Gesamtdichten aus*)
  If[MemberQ[rhoplot,nCar],
  (*Varianz des Abstands*)
  (*Rechte Spur*)
  If[laengex1>1,
  AppendTo[dVar1,N[Variance[dAutos1],6]];,
  AppendTo[dVar1,0];
  ];
  (*Linke Spur*)
  If[laengex2>1,
  AppendTo[dVar2,N[Variance[dAutos2],6]];,
  AppendTo[dVar2,0];
  ];
  (*Fluss durch letzte Zelle der jeweiligen Spur*)
  (*Rechte Spur*)
  If[l1<=0,l1=laengex1,l1=l1];
  If[savel1<=0,savel1=laengesx1,savel1=savel1];
  If[Length[savexAutos1]>0,
  If[l1>0 && xAutos1[[l1]]<savexAutos1[[savel1]],
  (AppendTo[savefluss1,1];
  l1=l1-1;
  savel1=savel1-1;),
  AppendTo[savefluss1,0];
  ];
  ];
  (*Linke Spur*)
  If[l2<=0,l2=laengex2,l2=l2];
  If[savel2<=0,savel2=laengesx2,savel2=savel2];
  If[laengesx2>0,
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
  AppendTo[density1,laengex1/nCells];
  (*Linke Spur*)
  AppendTo[density2,laengex2/nCells];
  
  (*Mittlere Geschwindigkeit aller Autos*)
  AppendTo[vMittel,N[Mean[Select[Join[vAutos1,vAutos2],UnsameQ[#, {}]&]],6]]; 
  ];
  If[MemberQ[rhoplot,nCar],
  (*Plotten vMittel, dVar und Dichte der jeweiligen Spur f\[UDoubleDot]r 3 Dichten*)
  AppendTo[vdvardensplot,ResourceFunction["PlotGrid"][{
  {ListPlot[vMittel,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"mittlere Geschwindigkeit" OverBar[v]}]},
  {ListPlot[dVar1,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Varianz von d, rechte Spur"}]},
  {ListPlot[dVar2,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Varianz von d, linke Spur"}]},
  {ListPlot[density1,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Dichte \[Rho], rechte Spur"}]},
  {ListPlot[density2,ImageSize->Automatic,ColorFunction->"Rainbow",Frame->True,FrameLabel->{None,"Dichte \[Rho], linke Spur"}]}
  },
  ImageSize->Large,FrameLabel->{"Zeit t",None},PlotLabel->"Plots f\[UDoubleDot]r "<>ToString[nCar]<>" Autos"
  ]];
 (*Zusammenz\[ADoubleDot]hlen des Flusses der beiden Spuren f\[UDoubleDot]r 3 Dichten
 savefluss=Table[savefluss1[[n]]+savefluss2[[n]],{n,tMax+1}];
 AppendTo[flussplot,ListPlot[savefluss,ImageSize->Medium,ColorFunction->"Rainbow",Frame->True,FrameLabel->{"Zeit t","Fluss"},PlotLabel->"Fluss pro Zeitschritt f\[UDoubleDot]r "<>ToString[nCar]<>" Autos"]];*)
 ];
 
(*Gibt Histogramme von v und d bei tMax f\[UDoubleDot]r 3 Dichten aus*)
(*Keine Histogramme ausgegeben, falls kein Auto auf der Spur*)
If[laengex1>0 && MemberQ[rhoplot,nCar],
((*Histogramme v und d*)
(*Listen Autos mit Geschwindigkeiten v=0,1,2,3,4,5*)
Clear[viAutos1];
viAutos1=Select[Table[Select[Table[vAutos1[[n]],{n,laengex1}],#==i &],{i,0,5}],UnsameQ[#, {}]&];

(*Listen Abst\[ADoubleDot]nde d=0,1,...,nCar*)
Clear[diAutos1];
diAutos1=Select[Table[Select[Table[dAutos1[[n]],{n,laengex1}],#==i &],{i,0,Max[dAutos1]}],UnsameQ[#, {}] &];),
(viAutos1={};
diAutos1={};)
];

If[laengex2>0 && MemberQ[rhoplot,nCar],
((*Histogramme v und d*)
(*Listen Autos mit Geschwindigkeiten v=0,1,2,3,4,5*)
Clear[viAutos2];
viAutos2=Select[Table[Select[Table[vAutos2[[n]],{n,laengex2}],#==i &],{i,0,5}],UnsameQ[#, {}]&];

(*Listen Abst\[ADoubleDot]nde d=0,1,...,nCar*)
Clear[diAutos2];
diAutos2=Select[Table[Select[Table[dAutos2[[n]],{n,laengex2}],#==i &],{i,0,Max[dAutos2]}],UnsameQ[#, {}] &];),
(viAutos2={};
diAutos2={};)
];

If[MemberQ[rhoplot,nCar],
viAutos=Select[Join[viAutos1,viAutos2],UnsameQ[#, {}]&];
diAutos=Select[Join[diAutos1,diAutos2],UnsameQ[#, {}]&];
AppendTo[histoplot,Histogram[Flatten[viAutos],{1},AxesLabel->{v,"Anzahl Autos mit" Indexed[v,"i"]},PlotRange->{{Automatic,5.5},Automatic},
Ticks->{Range[0,5,1],Automatic},PlotLabel->"Histogramm von v f\[UDoubleDot]r "<>ToString[nCar]<>" Autos",ColorFunction->"Pastel",ImageSize->Medium]];
AppendTo[histoplot,Histogram[Flatten[diAutos],{1},AxesLabel->{d,"Anzahl Autos mit" Indexed[d,"i"]},PlotRange->{0,All},
Ticks->{Range[0,Max[diAutos],1],Automatic},PlotLabel->"Histogramm von d f\[UDoubleDot]r "<>ToString[nCar]<>" Autos",ColorFunction->"Pastel",ImageSize->Medium]];
];

(*Dichte \[UDoubleDot]ber die gesamte Stra\[SZ]e*)
AppendTo[density,nCar/nCells];

(*Verkehrsfluss f\[UDoubleDot]r Dichte nCar/nCells*)
AppendTo[fluss,addfluss];
Clear[savefluss1];
Clear[savefluss2];
Clear[density1];
Clear[density2]; (*lieber noch mal einelementige Liste draus machen? Sonst Probleme mit AppendTo?*)
];
(*Fundamentalplot mit addfluss*)
fundplot=ListPlot[Thread[{density,fluss/tMax}],ImageSize->Medium,Frame->True,FrameLabel->{"Dichte \[Rho]","Zeitliches Mittel des Flusses \[UDoubleDot]ber letzte Zelle"},
PlotLabel->"Fundamentalplot f\[UDoubleDot]r p = "<>ToString[p],PlotStyle->RandomChoice[{Red,Orange,Yellow,LightGreen,LightBlue,Blue,Purple,Pink}]]; (*Punkte hell f\[UDoubleDot]r dunklen Hintergrund in sp\[ADoubleDot]terem Notebook*)

(*twolanesplots=GraphicsGrid[{vdvardensplot,flussplot,fundplot,histoplot}];
(*Ausgabe Plots*)
Return[twolanesplots]*)
]


list={{1,2},{3,4}};
list[[1,1]]


twolanesNaSch[20,30,5,0.15];
(*Show[twolanesplots]*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*(Diskussion)*)


(* ::Chapter:: *)
(*Weiterf\[UDoubleDot]hrende Frage*)


(* ::Text:: *)
(*Dieses Modul zeigt wie sich eine Schlange, aufgrund einer Ampel und einen Blitzer, bilden kann. *)
(**)
