(* ::Package:: *)
BeginPackage["XML`Workarounds`"]


Begin["`Private`"]


(*
load the System`Convert`XMLDump`* functions by using XML`MathML`BoxesToMathML
*)
XML`MathML`BoxesToMathML["\[Beta]"]


(*Prevent default namespace assignments from appearing if they are identical
to any of the prefixed namespace assignments. This change makes it possible to
keep namespace prefixes intact during imports and exports, which is important
when doing xslt (version 1.0) based on prefixes.*)
PrependTo[
	DownValues@
		System`Convert`XMLDump`includeQNameInAttributes,
	HoldPattern[
		System`Convert`XMLDump`includeQNameInAttributes[
			{Evaluate[System`Convert`XMLDump`$XMLNamesNS],"xmlns"}|"xmlns"->ns_
			]/;MemberQ[System`Convert`XMLDump`$namespacePrefixes[[All,1]],
					ns]
		]:>Sequence[]
	]


End[]


EndPackage[]
(*BeginPackage["XML`Workarounds`",{"Utilities`FilterOptions`"}]*)
(*context handling*)
(*
If[!NameQ[#<>"*"],Get@#]&/@{"Utilities`BadArgumentHandling`"}

old$ContextPath=$ContextPath
$ContextPath=Flatten@{$ContextPath,"Utilities`BadArgumentHandling`"}
*)
(*String preprocessing to remove xmlns="..." + ImportNamespacePrefixedXMLString
overcomes a bug preventing Mathematica from generating namespace prefixed
MathML. The old version of this function, with a non randomized container tag,
was called rawXML.*)
(*I found out the bug can be avoided by specifying "SymbolicXML" instead of
"XML" as the Import type. Also, a modification to includeQNameInAttributes is
able to completely prevent generation of the xmlns="..." statements anyway.*)
(*
ImportNamespacePrefixedXMLString[xmlString__String,opts___?OptionQ]:=
	With[{tagPost=ToString@SequenceForm[
		FromCharacterCode@Random[Integer,{97,122}],"llama",
		Sequence@@Table[Random[Integer,{0,9}],{5}],"bait>"]
		},
		Sequence@@
			ImportString["<"<>tagPost<>xmlString<>"</"<>tagPost,
				"XML",
				FilterOptions[ImportString,Sequence@@Flatten@{opts}]
				][[2,3]]
		]

GeneralDownValue@ImportNamesspacePrefixedXMLString;
*)
(*
$ContextPath=old$ContextPath
Remove@old$ContextPath
*)


(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)