(* ::Package:: *)
BeginPackage["Utilities`Run`Workarounds`"]


Begin["`Private`"]


Unprotect@Run

Update@Run

$TrapRun=StringMatchQ[$SystemID,___~~"Windows"~~___]

Run[args__]/;$TrapRun:=
	Block[{$TrapRun=False},
		Run["\""<>
			StringJoin@
				BoxForm`Intercalate[
					ToString[#,InputForm]&/@{args},
					" "
					]<>
			"\""
			]
		]

Update@Run

Protect@Run

Update@Run


End[]


EndPackage[]


(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)