(* ::Package:: *)
BeginPackage["Utilities`FilterOptions`Extensions`",
	{"Utilities`FilterOptions`"}]


Begin["`Private`"]


(*these rule functions return a flat sequence of unique rules in input order*)
(*if you don't like the abuse of the FilterOptions function name, just consider
the lhs union to be a type of filtering (filtering of duplicates)*)


ruleUnionNoCheck[]=Sequence[]

ruleUnionNoCheck[rules__]:=
	Module[{encounteredLhses=Alternatives[],ruleParser},
		ruleParser[rule:_[lhs_,_]]:=
			If[MatchQ[lhs,encounteredLhses],
				Identity[Sequence][],
				AppendTo[encounteredLhses,lhs];rule
				];
			ruleParser/@Unevaluated@rules
		]


Unprotect@FilterOptions

Update@FilterOptions

With[{dValues=DownValues@FilterOptions},
	If[Cases[
			dValues,
			symb_Symbol/;AtomQ@Unevaluated@symb&&
				Context@Unevaluated@symb===Context[],
			{0,Infinity}
			]==={},
		oldDownValues=dValues;
		DownValues@FilterOptions=.
		]
	]

FilterOptions[(opts:(Rule|RuleDelayed)[_,_]...)?OptionQ]:=ruleUnionNoCheck[opts]

FilterOptions[opts__?OptionQ]:=ruleUnionNoCheck@@Flatten@{opts}

FilterOptions[symb_Symbol/;AtomQ@symb,opts__List?OptionQ]:=
	ruleUnionNoCheck[FilterOptions[symb,##]&@@Flatten@{opts}]

If[ValueQ@oldDownValues,
	DownValues@FilterOptions=Join[DownValues@FilterOptions,oldDownValues];
	Clear@oldDownValues]

Update@FilterOptions

Protect@FilterOptions

Update@FilterOptions


End[]


EndPackage[]


(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)