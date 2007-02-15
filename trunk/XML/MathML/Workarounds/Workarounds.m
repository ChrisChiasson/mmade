(* ::Package:: *)

BeginPackage["XML`MathML`Workarounds`"]


BoxesToMathMLBoxes::"usage"=
	"BoxesToMathMLBoxes[boxes] creates a modified form of boxes that is more \
easily handled by XML`MathML`BoxesToMathML and other functions."


Begin["`Private`"]

(*context handling*)
If[!NameQ[#<>"*"],Get@#]&/@{"Utilities`BadArgumentHandling`"}

old$ContextPath=$ContextPath
$ContextPath=Flatten@{$ContextPath,"XML`","XML`MathML`",
	"Utilities`BadArgumentHandling`"}


(*Boolean opposite of current value of ShowStringCharacters from the front end*)
dontShowStringCharacters:=!ShowStringCharacters/.
	AbsoluteOptions[$FrontEnd,ShowStringCharacters]


BoxesToMathMLBoxes[boxes_]/;dontShowStringCharacters&&
	MemberQ[Unevaluated@boxes,InterpretationBox,{0,Infinity},Heads->True]:=
	With[{intBoxesPos=
			Position[Unevaluated@boxes,
				InterpretationBox[_,
					(number:_Real|_Integer)/;AtomQ@Unevaluated@number,
					___
					]
				]
			},
		(*reference: MMA 5.2 help on ReplacePart, specifically ReplaceAt*)
		With[{intFirstBoxes=
			Extract[Unevaluated@boxes,
				Append[#,1]&/@intBoxesPos,
				HoldComplete
				]},
			With[{intReplacedBoxes=intFirstBoxes/.str_String:>
					With[{result=StringReplace[str,"\""->""]},
						result/;AtomQ@Unevaluated@str&&SyntaxQ@str
						]
					},
				ReplacePart[Unevaluated@boxes,
					intReplacedBoxes,
					intBoxesPos,
					Thread[{Range@Length[intBoxesPos],1}]
					]
				]
			]
		]

BoxesToMathMLBoxes[boxes_]=boxes

GeneralDownValue@BoxesToMathMLBoxes;


$ContextPath=old$ContextPath
Remove@old$ContextPath


End[]


EndPackage[]
(*patterns*)
(*containsMsPatternObject=(_List|_String)?(!FreeQ[#,"ms"]&);
containsMtextPatternObject=(_List|_String)?(!FreeQ[#,"mtext"]&);
containsMoPatternObject=(_List|_String)?(!FreeQ[#,"mo"]&);*)

(*questionable old workarounds

$mspaceWidth="5pt";

reformatMtext[(*\[COMPATIBILITYNoBreak]*)
	XMLElement[mtextHead:containsMtextPatternObject,{attributes___},{"\:f3a2"}]
	]=(*XMLElement[mtextHead,{attributes},{""}]*)Sequence[]

reformatMtext[
	XMLElement[
		mtextHead:containsMtextPatternObject,{attributes___},{str_String}
		]
	]/;AtomQ[Unevaluated[str]]&&StringLength[str]>=1&&
		StringMatchQ[StringTake[str,-1],Whitespace]:=
	Sequence[
		reformatMtext[XMLElement[mtextHead,{attributes},{StringDrop[str,-1]}]](*,
		XMLElement[
			mtextHead/."mtext"->"mspace",
			{"width"->$mspaceWidth,attributes},
			{}
			]*)
		];

reformatMtext[
	XMLElement[
		mtextHead:containsMtextPatternObject,{attributes___},{str_String}
		]
	]/;AtomQ[Unevaluated[str]]&&StringLength[str]>=1&&
		StringMatchQ[StringTake[str,1],Whitespace]:=
	Sequence[
		(*XMLElement[
			mtextHead/."mtext"->"mspace",
			{"width"->$mspaceWidth,attributes},
			{}
			],*)
		reformatMtext[XMLElement[mtextHead,{attributes},{StringDrop[str,1]}]]
		];

reformatMtext[element:XMLElement[containsMtextPatternObject,{___},{___String}]]=
	element;

GeneralDownValue@reformatMtext;

reformatMs[
	XMLElement[msHead:containsMsPatternObject,{attributes___},{str_String}]/;
		AtomQ[Unevaluated[str]]&&(!ShowStringCharacters/.
			AbsoluteOptions[$FrontEnd,ShowStringCharacters])
	]:=
	Module[{midStr,strMod=StringReplace[str,{constantStringReplacements}]},
		If[StringMatchQ[str,StringExpression["\\\"",midStr__,"\\\""]],
			XMLElement[msHead/."ms"->"ms",
				{attributes},
				{StringTake[strMod,{3,-3}]}
				],
			XMLElement[msHead/."ms"->"mtext",{attributes},{strMod}]
			]
		];

(*do not remove the condition from this function evaluation it is part of a 
two by two decision matrix - you will need to look at all four conditions
to be sure you can remove it*)
reformatMs[XMLElement[msHead:containsMsPatternObject,{attributes___},{}]/;
		(!ShowStringCharacters/.
			AbsoluteOptions[$FrontEnd,ShowStringCharacters])]:=
		XMLElement[msHead/."ms"->"mtext",{attributes},{}];

reformatMs[elem:XMLElement[containsMsPatternObject,__]]:=elem;

GeneralDownValue@reformatMs;

end of questionable old workarounds*)


(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)