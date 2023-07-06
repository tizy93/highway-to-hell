(* ::Package:: *)

(*Modul Nagel-Schreckenberg Modell*)

NaSch[nCar_, nCells_, tMax_, vMax_, p_] :=
    Module[
      (*Eingabe Anzahl der Autos nCar, Anzahl der Zellen nCells, Simulationsdauer
     tMax, 
Maximalgeschwindigkeit vMax und Tr\[ODoubleDot]delwahrscheinlichkeit p mit Funktionsaufruf
    *)(*lokale Variablen*){xAutos, vAutos, dAutos}
        ,
        (*Autos haben Position x und Geschwindigkeit v zum vorderen Auto
            *)
        xAutos = Sort[RandomSample[Range[nCells], nCar]];
        (** erzeugt eine zuf\[ADoubleDot]llige (Random) Liste (Position) "xAutos"
             ohne wiederholungen (Sample) und in aufsteigende Reihen sortiert (Sort
            )**)
        vAutos = RandomInteger[{0, vMax}, nCar];
        (**zuordnet eine zuf\[ADoubleDot]llige Zahl zu jedes Auto **)
        (*Verkehrsregeln aus NaSch-Modell implementieren*)
        For[i = 0, i <= tMax, i++,(*Schleife der Runden bis tMax*)(*Freie
             Zellen d vor dem Auto bis zum vorderen*)dAutos =
                Table[
                    If[xAutos[[n]] < Max[xAutos],
                        xAutos[[n + 1]] - xAutos[[n]] - 1
                        ,
                        nCells - xAutos[[n]] + Min[xAutos] - 1
                    ]
                    ,
                    {n, 1, nCar - 1}
                ];(*In Schleife, damit es geupdatet wird*)
            (**erzeugt der Abstand des Autos mit bedingung dass ein auto
                 kleiner gleich des gr\[ODoubleDot]\[SZ]es xAutos ist, dann ergibt den abstand zwischen
                 zwei Autos **)
            (*Arrays fangen bei 1 an; n+1 muss f\[UDoubleDot]r das letzte gleich 
                nCar sein; Abstand des letzten Autos ist bis zum ersten, da Ringstra\[SZ]e
                *)
            AppendTo[
                dAutos
                ,
                If[xAutos[[nCar]] < Max[xAutos],
                    xAutos[[1]] - xAutos[[nCar]] - 1
                    ,
                    nCells - xAutos[[nCar]] + Min[xAutos] - 1
                ]
            ];
            (**abstand zur n\[ADoubleDot]chste auto (dar\[UDoubleDot]ber bin ich nicht sicher
                )????**)
         (*Abstand des Autos an Stelle nCar zum ersten wird angeh\[ADoubleDot]ngt
    ;
d immer -1, da freie Zellen vor Auto gemeint sind*)
            (*R1: Beschleunigen, falls vMax noch nicht erreicht*)
            vAutos = Table[Min[vAutos[[n]] + 1, vMax], {n, 1, nCar}];
                
            (*R2: Abbremsen, falls v gr\[ODoubleDot]\[SZ]er als Abstand d*)
            vAutos = Table[Min[dAutos[[n]], vAutos[[n]]], {n, 1, nCar
                }];
            (**wird \[CapitalUDoubleDot]berpr\[UDoubleDot]ft, ob der Abstand zwischen ein Auto und die
                 vordere gro\[SZ] genug ist **)
            (*R3: Tr\[ODoubleDot]deln mit Wahrscheinlichkeit p*)
            vAutos =
                Table[
                    If[RandomReal[{0, 1}] <= p,
                        vAutos[[n]] = Max[vAutos[[n]] - 1, 0]
                        ,
                        vAutos[[n]]
                    ]
                    ,
                    {n, 1, nCar}
                ]; (*Tr\[ODoubleDot]deln solange noch nicht v=0*)
            (**RandomReal erzeugt ein zuf\[ADoubleDot]llige zahl zwischen 0 und 1
                , falls das unter p=0,3 liegt dan tr\[ODoubleDot]delt das auto sonst nicht **)
            (*Falls zuf\[ADoubleDot]llige Zahl nicht im gegebenen Intervall, bleibt
                 v gleich*)
            (*R4: Fahren um vAutos Zellen*)
            xAutos =
                Table[
                    If[xAutos[[n]] + vAutos[[n]] <= nCells,
                        xAutos[[n]] = xAutos[[n]] + vAutos[[n]]
                        ,
                        xAutos[[n]] = xAutos[[n]] + vAutos[[n]] - nCells
                            
                    ]
                    ,
                    {n, 1, nCar}
                ];
            (*Falls Autos au\[SZ]erhalb Zellen bewegt, wird Bewegung in erster
                 Zelle fortgesetzt, da Ringstra\[SZ]e*)
            (*Print[xAutos];*)
           
            Print[vAutos]
           (* Print[dAutos];*)
        ]
    ]
    


NaSch[10,30,5,5,0.3]


