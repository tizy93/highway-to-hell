(* ::Package:: *)

(* ::Code:: *)
(**)


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



(*Modul Nagel-Schreckenberg Modell*)
Meanvarfluss[nCar_, nCells_, tMax_, vMax_, p_,posCell_,avCells_] :=
    Module[
      (*Eingabe Anzahl der Autos nCar, Anzahl der Zellen nCells, Simulationsdauer
     tMax, 
Maximalgeschwindigkeit vMax und Tr\[ODoubleDot]delwahrscheinlichkeit p mit Funktionsaufruf
    *)(*lokale Variablen*){xAutos, vAutos, dAutos, vMittel, dVar,t,dMittel,m,fluss,cellCars,regionCars,density}
        ,
        (*Autos haben Position x und Geschwindigkeit v zum vorderen Auto
            *)
        xAutos = Sort[RandomSample[Range[nCells], nCar]];
        (** erzeugt eine zuf\[ADoubleDot]llige (Random) Liste (Position) "xAutos"
             ohne wiederholungen (Sample) und in aufsteigende Reihen sortiert (Sort
            )**)
        vAutos = RandomInteger[{0, vMax}, nCar];
        (**zuordnet eine zuf\[ADoubleDot]llige Zahl zu jedes Auto **)
        (*Mittelgeschwindigkeit*)
        Clear[vMittel];
         vMittel = Table [Nothing, {t,1}];
        (*dMittel= Table[Nothing , {m,1}];*)
        (*varianz dAuto*)
        dVar = Table[Nothing, {t,1}];
        (*Verkehrsfluss*)
        fluss=Table[Nothing,{n,1}];
        
        (*Verkehrsregeln aus NaSch-Modell implementieren*)
        For[i = 0, i <= tMax, i++,(*Schleife der Runden bis tMax*)(*Freie
             Zellen d vor dem Auto bis zum vorderen*)
             dAutos =Table[If[xAutos[[n]] < Max[xAutos],xAutos[[n + 1]] - xAutos[[n]] - 1,nCells - xAutos[[n]] + Min[xAutos] - 1],{n, 1, nCar - 1}];(*In Schleife, damit es geupdatet wird*)
            (**erzeugt der Abstand des Autos mit bedingung dass ein auto kleiner gleich des gr\[ODoubleDot]\[SZ]es xAutos ist, dann ergibt den abstand zwischen zwei Autos **)
            (*Arrays fangen bei 1 an; n+1 muss f\[UDoubleDot]r das letzte gleich nCar sein; Abstand des letzten Autos ist bis zum ersten, da Ringstra\[SZ]e*)
            AppendTo[ dAutos, If[xAutos[[nCar]] < Max[xAutos], xAutos[[1]] - xAutos[[nCar]] - 1,nCells - xAutos[[nCar]] + Min[xAutos] - 1]];
            
         (*Abstand des Autos an Stelle nCar zum ersten wird angeh\[ADoubleDot]ngt; d immer -1, da freie Zellen vor Auto gemeint sind*)
            (*R1: Beschleunigen, falls vMax noch nicht erreicht*)
            vAutos = Table[Min[vAutos[[n]] + 1, vMax], {n, 1, nCar}];
                
            (*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
            vAutos = Table[Min[dAutos[[n]], vAutos[[n]]], {n, 1, nCar}];
            (**wird \[CapitalUDoubleDot]berpr\[UDoubleDot]ft, ob der Abstand zwischen ein Auto und die vordere gro\[SZ] genug ist **)
            (*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
            vAutos = Table[If[RandomReal[{0, 1}] <= p, vAutos[[n]] = Max[vAutos[[n]] - 1, 0], vAutos[[n]]],{n, 1, nCar}]; (*Tr\[ODoubleDot]deln solange noch nicht v=0*)
            (**RandomReal erzeugt ein zuf\[ADoubleDot]llige zahl zwischen 0 und 1
                , falls das unter p=0,3 liegt dan tr\[ODoubleDot]delt das auto sonst nicht **)
            (*Falls zuf\[ADoubleDot]llige Zahl nicht im gegebenen Intervall, bleibt
                 v gleich*)
            (*R4: Fahren um vAutos Zellen*)
            xAutos =
                Table[If[xAutos[[n]] + vAutos[[n]] <= nCells,xAutos[[n]] = xAutos[[n]] + vAutos[[n]],xAutos[[n]] = xAutos[[n]] + vAutos[[n]] - nCells],{n, 1, nCar}
                ];
            (*Falls Autos au\[SZ]erhalb Zellen bewegt, wird Bewegung in erster
                 Zelle fortgesetzt, da Ringstra\[SZ]e*)
            (*Print[xAutos];
            Print[dAutos];
            Print[vAutos];*)
                    
        (*mittlere geschwindigkeit*)
        AppendTo[vMittel, N[Mean[vAutos],6]];
        (*AppendTo[dMittel, N[Mean[dAutos],6]];*)
        (*varianz dAuto*)
        AppendTo[dVar, N[Variance[dAutos],6]];
        (*Verkehrsfluss*)
        cellCars=DeleteCases[Table[If[xAutos[[n]]==posCell,xAutos[[n]],],{n,1,nCar}],Null];
        AppendTo[fluss,Length[cellCars]];
        (*Erstellen Liste mit Autos innerhalb vorgegebener Region von der ersten Zelle bis zur frei w\[ADoubleDot]hlbaren Zelle avCells*)
        regionCars=DeleteCases[Table[If[xAutos[[n]]<=avCells,xAutos[[n]],],{n,1,nCar}],Null]; (*Nullen mit DeleteCases gel\[ODoubleDot]scht*)
        (*Element zu density-Liste mit Dichte am Zeitpunkt t hinzugef\[UDoubleDot]gt, t entspricht Reihenfolge der Liste*)
        AppendTo[density,Length[regionCars]/avCells];
        
        
        ]
        Print[vMittel];
        (*dVar= N[Variance[dMittel],7];*)
        Print[dVar];
        (*Print[dMittel];*)
        Print[fluss];
        ListPlot[fluss,ImageSize->Medium]
        
     ]
     
   (*value={1,2,2,3,4,5,6}
   Mittel=Mean [value]
   var=Variance[value]*)
    (*mittlere geschwindigkeit*)


Meanvarfluss[30,100,20,10,0.5,4]





(*Modul Nagel-Schreckenberg Modell*)
FundamentalD[nCar_, nCells_, tMax_, vMax_, p_,avCells_] :=
    Module[
      (*Eingabe Anzahl der Autos nCar, Anzahl der Zellen nCells, Simulationsdauer
     tMax, 
Maximalgeschwindigkeit vMax und Tr\[ODoubleDot]delwahrscheinlichkeit p mit Funktionsaufruf
    *)(*lokale Variablen*){xAutos, vAutos, dAutos, dMittel, fluss, regionCars, cellCars, density, fundamD,mittelFluss}
        ,
        (*Autos haben Position x und Geschwindigkeit v zum vorderen Auto
            *)
        xAutos = Sort[RandomSample[Range[nCells], nCar]];
        (** erzeugt eine zuf\[ADoubleDot]llige (Random) Liste (Position) "xAutos"
             ohne wiederholungen (Sample) und in aufsteigende Reihen sortiert (Sort
            )**)
        vAutos = RandomInteger[{0, vMax}, nCar];
        (**zuordnet eine zuf\[ADoubleDot]llige Zahl zu jedes Auto **)
        (*Mittelgeschwindigkeit*)
        
        (*Erzeugen einelementige Liste mit Dichte*) 
         density=Table[Nothing,{n,1}];
        (*Verkehrsfluss*)
        fluss=Table[Nothing,{n,1}];
        (*mittelwert der fluss*)
        mittelFluss=Table[Nothing,{n,1}];
                
        (*Verkehrsregeln aus NaSch-Modell implementieren*)
        For[i = 0, i <= tMax, i++,(*Schleife der Runden bis tMax*)(*Freie
             Zellen d vor dem Auto bis zum vorderen*)
             dAutos =Table[If[xAutos[[n]] < Max[xAutos],xAutos[[n + 1]] - xAutos[[n]] - 1,nCells - xAutos[[n]] + Min[xAutos] - 1],{n, 1, nCar - 1}];(*In Schleife, damit es geupdatet wird*)
            (**erzeugt der Abstand des Autos mit bedingung dass ein auto kleiner gleich des gr\[ODoubleDot]\[SZ]es xAutos ist, dann ergibt den abstand zwischen zwei Autos **)
            (*Arrays fangen bei 1 an; n+1 muss f\[UDoubleDot]r das letzte gleich nCar sein; Abstand des letzten Autos ist bis zum ersten, da Ringstra\[SZ]e*)
            AppendTo[ dAutos, If[xAutos[[nCar]] < Max[xAutos], xAutos[[1]] - xAutos[[nCar]] - 1,nCells - xAutos[[nCar]] + Min[xAutos] - 1]];
            
         (*Abstand des Autos an Stelle nCar zum ersten wird angeh\[ADoubleDot]ngt; d immer -1, da freie Zellen vor Auto gemeint sind*)
            (*R1: Beschleunigen, falls vMax noch nicht erreicht*)
            vAutos = Table[Min[vAutos[[n]] + 1, vMax], {n, 1, nCar}];
                
            (*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
            vAutos = Table[Min[dAutos[[n]], vAutos[[n]]], {n, 1, nCar}];
            (**wird \[CapitalUDoubleDot]berpr\[UDoubleDot]ft, ob der Abstand zwischen ein Auto und die vordere gro\[SZ] genug ist **)
            (*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
            vAutos = Table[If[RandomReal[{0, 1}] <= p, vAutos[[n]] = Max[vAutos[[n]] - 1, 0], vAutos[[n]]],{n, 1, nCar}]; (*Tr\[ODoubleDot]deln solange noch nicht v=0*)
            (**RandomReal erzeugt ein zuf\[ADoubleDot]llige zahl zwischen 0 und 1
                , falls das unter p=0,3 liegt dan tr\[ODoubleDot]delt das auto sonst nicht **)
            (*Falls zuf\[ADoubleDot]llige Zahl nicht im gegebenen Intervall, bleibt
                 v gleich*)
            (*R4: Fahren um vAutos Zellen*)
            xAutos =
                Table[If[xAutos[[n]] + vAutos[[n]] <= nCells,xAutos[[n]] = xAutos[[n]] + vAutos[[n]],xAutos[[n]] = xAutos[[n]] + vAutos[[n]] - nCells],{n, 1, nCar}
                ];
            (*Falls Autos au\[SZ]erhalb Zellen bewegt, wird Bewegung in erster
                 Zelle fortgesetzt, da Ringstra\[SZ]e*)
            (*Print[xAutos];
            Print[dAutos];
            Print[vAutos];*)
            
        (*Verkehrsfluss*)
        cellCars=Select[DeleteCases[Table[DeleteCases[Table[If[xAutos[[n]]==m,xAutos[[n]],],{n,1,nCar}],Null],{m,1,avCells}],Null],UnsameQ[#,{}]&];
        AppendTo[fluss,Length[cellCars]];
        AppendTo[mittelFluss, Mean[fluss]];
        regionCars=DeleteCases[Table[If[xAutos[[n]]<=avCells,xAutos[[n]],],{n,1,nCar}],Null]; (*Nullen mit DeleteCases gel\[ODoubleDot]scht*)
        (*Element zu density-Liste mit Dichte am Zeitpunkt t hinzugef\[UDoubleDot]gt, t entspricht Reihenfolge der Liste*)
        AppendTo[density,Length[regionCars]/avCells];
        (*Print[cellCars];*)
        
        ]
       
        (*Print[fluss];*)
        Print[mittelFluss];
        Print[density];
        ListPlot[density,ImageSize->Medium,AxesLabel->Dichte]
        ListPlot[mittelFluss,ImageSize->Medium,AxesLabel->Mittelfluss]
        ListPlot[Partition[Join[density,mittelFluss],2],ImageSize->Medium,AxesLabel->{Dichte,Mittelfluss}]
       Print[Partition[Join[density,mittelFluss],2]];
     ]



FundamentalD[30,100,20,10,0.3,30]



