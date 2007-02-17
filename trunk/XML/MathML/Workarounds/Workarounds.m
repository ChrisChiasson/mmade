(* ::Package:: *)

BeginPackage["XML`MathML`Workarounds`"]


Begin["`Private`"]

(*context handling*)
If[!NameQ[#<>"*"],Get@#]&/@{"Utilities`BadArgumentHandling`"}

old$ContextPath=$ContextPath
$ContextPath=Flatten@{$ContextPath,"XML`","XML`MathML`",
	"Utilities`BadArgumentHandling`"}


(*
load the System`Convert`MathMLDump`* functions by using
XML`MathML`BoxesToMathML once
*)
BoxesToMathML["\[Beta]"];


(*take care of missing invisible times for ExpressionToMathML[0.2*a]*)
System`Convert`MathMLDump`operandQ[token_String]/;
	AtomQ@Unevaluated@token&&SyntaxQ@token&&
		ToExpression[token,InputForm,NumberQ]=True


(*take care of annotations appearing even after telling them not to as in
XML`MathML`ExpressionToSymbolicMathML[
	NumberForm[5],
	"Annotations"->{},
	"Formats"->{"PresentationMathML"},
	"IncludeMarkupAnnotations"->False,
	"MathAttributes"->{}
	]
*)
DownValues@System`Convert`MathMLDump`BoxesToSMML=
	With[{oDv=DownValues@System`Convert`MathMLDump`BoxesToSMML},
		Insert[oDv,
			HoldPattern[
				System`Convert`MathMLDump`BoxesToSMML[
					TagBox[boxes_,
						"AnnotationsTagWrapper"[
							TagBox[_,"MathMLContentTag",___]
							],
						___
						]
					]
				]/;!MemberQ[System`Convert`MathMLDump`formats,"ContentMathML"]:>
				System`Convert`MathMLDump`BoxesToSMML@boxes,
			Position[oDv,"AnnotationsTagWrapper",Infinity,1][[1,1]]
			]
		];


(*Boolean opposite of current value of ShowStringCharacters from the front end*)
If[$Notebooks,
	dontShowStringCharacters:=!ShowStringCharacters/.
		AbsoluteOptions[$FrontEnd,ShowStringCharacters],
	dontShowStringCharacters=True
	]


(*If the front end isn't showing string characters, then the MathML box
processing should ignore those string characters.*)
boxesToMathMLBoxes::"usage"=
	"BoxesToMathMLBoxes[boxes] creates a modified form of boxes that is more \
easily handled by XML`MathML`BoxesToMathML and other functions."

boxesToMathMLBoxes[boxes_]/;dontShowStringCharacters&&
	MemberQ[Unevaluated@boxes,InterpretationBox,{0,Infinity},Heads->True]:=
	With[{intBoxesPos=Append[#,1]&/@
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
				intBoxesPos,
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

boxesToMathMLBoxes[boxes_]=boxes

GeneralDownValue@boxesToMathMLBoxes;


(*
take care of strings having extra quotation marks within InterpretationBox's
first boxes argument, for example:
XML`MathML`ExpressionToMathML@NumberForm@5
*)
System`Convert`MathMLDump`BoxesToSMMLPreProcess[
	System`Convert`MathMLDump`data_
	]:=(MakeExpression["System`Hold",TraditionalForm];
		MakeExpression[TagBox["System`Hold",MatrixForm]];
		MakeBoxes[InverseFunction[Hold]];
		boxesToMathMLBoxes@System`Convert`MathMLDump`data/.
			StyleBox[
				RowBox[{(pieces__)?System`Convert`MathMLDump`richTextPieceQ}]
				]->
				System`Convert`MathMLDump`RichText[pieces]
			)


(*a good example ExpressionToMathML call that showcases fixes from this file:
XML`MathML`ExpressionToMathML[NumberForm[5.3``0.7*a],
	"Formats"->{"PresentationMathML"},"IncludeMarkupAnnotations"->False]
*)


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