(* ::Package:: *)

BeginPackage["XML`MathML`Workarounds`",{"XML`Workarounds`"}]


$CommonMathMLCompatibility::"usage"="$CommonMathMLCompatibility is a variable \
that, if \
True, causes the XML`MathML` functions to change MathML markup so that it is \
more compatible with other programs. Specifically, some character replacements \
are done so that the programs can display the equations properly."


$FirefoxMathMLCompatibility::"usage"="$FirefoxMathMLCompatibility is a \
variable that, if \
True, causes the XML`MathML` functions to change MathML markup so that it is \
more compatible with the Firefox web browser. Specifically, some character \
replacements are done so that Firefox can display the equations properly."


$SVGMathMathMLCompatibility::"usage"="$SVGMathMathMLCompatibility is a \
variable that, if \
True, causes the XML`MathML` functions to change MathML markup so that it is \
more compatible with the program SVGMath. At the time of this writing, that \
means <mo>(</mo>stuff<mo>)</mo> is marked up as <mfenced>stuff</mfenced>"<>
(*
" and <mtext> elements containing only \[NoBreak] or \[InvisibleSpace] are \
removed"<>
*)
". This is useful for sending MathML to SVGMath because SVGMath tends to draw \
parenthesis a bit low if the markup preceeding the parenthesis has a \
subscript"<>
(*
" SVGMath also has problems drawing those aforementioned <mtext> elements"<>
*)
". Some character replacements are also made."


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


(*take care of nobreak spacing handling
the built in BoxesToSSML rule to avoid this situation around matrices doesn't
"see past" TagBox:
XML`MathML`ExpressionToMathML[MatrixForm[{a,b}],
	"Formats"->{"PresentationMathML"},"IncludeMarkupAnnotations"->False]
*)
System`Convert`MathMLDump`BoxesToSMML[char:"\[NoBreak]"]:=
	XMLElement["mspace",
		{System`Convert`MathMLDump`baseCharacterToLinebreak[char]},
		{}
		]


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


(*take care of lack of font size unit on mathsize attribute, as in:
XML`MathML`ExpressionToMathML@StyleForm[5,FontSize->11]
*)
DownValues@System`Convert`MathMLDump`convertOptionToAttribute=
	DeleteCases[DownValues@System`Convert`MathMLDump`convertOptionToAttribute,
		_?(!FreeQ[#,FontSize]&)
		];
System`Convert`MathMLDump`convertOptionToAttribute[_,FontSize,val_?NumberQ]:=
	"mathsize"->ToString@SequenceForm[val,"pt"]


(*a good example ExpressionToMathML call that showcases fixes from this file:
XML`MathML`ExpressionToMathML[StyleForm[NumberForm[5.3``0.7*a],FontSize->11],
	"Formats"->{"PresentationMathML"},"IncludeMarkupAnnotations"->False]
*)


(*handle parenthesis being drawn too low for contents when the head of an
expression has a subscript*)
(*remove extra mspace elements resulting from nobreak characters just inside
parenthesis*)
(*replace characters SVGMath can't handle*)
sVGMathCompatibilityFunction[xml_]:=
	xml//.
		{XMLElement[
			containerElement_,
			containerAttributes_,
			{pre___,
				XMLElement[
					"mo",
					moAttributes_,
					{"("}
					],
				spaces:XMLElement["mspace",__]...,
				mid:XMLElement[Except["mspace"],__]..,
				spaces___,
				XMLElement[
					"mo",
					moAttributes_,
					{")"}
					],
				post___
				}
			]:>XMLElement[
					containerElement,
					containerAttributes,
					{pre,
						XMLElement["mfenced",moAttributes,{mid}],
						post
						}
					],
			XMLElement["semanitcs",_,{bodyElements___}]->bodyElements,
			XMLElement["annotation-xml",
				{___,"encoding"->"MathML-Content",___},_]->Sequence[](*,
			XMLElement[
				"mtext",
				_,
				{"\[NoBreak]"|"\[InvisibleSpace]"}
				]->
				Sequence[]*),
			str_String/;StringQ@Unevaluated@str:>
				StringReplace[str,{"\[LongEqual]"->"="(*,
						"\[InvisibleSpace]"->"",
						"\[LeftBracketingBar]"|"\[RightBracketingBar]"|
							"\[VerticalSeparator]"->"|"
						*)}]
			}


(*replace characters Firefox can't handle*)
firefoxCompatibilityFunction[xml_]:=
	xml/.
		{str_String/;StringQ@Unevaluated@str:>
				StringReplace[str,
					{"\[LongEqual]"->"\:ff1d"(*FULL WIDTH EQUALS SIGN*),
						"\[Piecewise]"->"{"(*,
						"\[InvisibleApplication]"->"",
						"\[Cross]"->"\[Times]",
						"\[Equal]"->"=",
						"\[Rule]"->"\[RightArrow]"
						"\[InvisibleSpace]"->"\:200b"
						*)}]
			}


PrependTo[DownValues@System`Convert`MathMLDump`BoxesToSMMLPostProcess,
	g:System`Convert`MathMLDump`BoxesToSMMLPostProcess[__]/;
		TrueQ[#1]:>
		Block[{#1=False},
			ReleaseHold@MapAt[#2,Hold[g],{1,1}]
			]
	]&@@@{{$CommonMathMLCompatibility,commonCompatibilityFunction},
		{$FirefoxMathMLCompatibility,firefoxCompatibilityFunction},
		{$SVGMathMathMLCompatibility,sVGMathCompatibilityFunction}
		}


$ContextPath=old$ContextPath
Remove@old$ContextPath


End[]


EndPackage[]


(*patterns
containsMtextPatternObject=(_List|_String)?(!FreeQ[#,"mtext"]&);
containsMoPatternObject=(_List|_String)?(!FreeQ[#,"mo"]&);
containsMsPatternObject=(_List|_String)?(!FreeQ[#,"ms"]&);
*)
(*questionable old workarounds (for reference in writing new functionality)

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

formatNumberFormMathMLNumber[
	str_String/;AtomQ[Unevaluated[str]]&&SyntaxQ[str]
	]:=
	Module[
		{number},
		(*this should be a regular ToBoxes call, not toBoxes*)
		ToBoxes@number/;
			validateGiveNumber[ToExpression[str,InputForm,HoldComplete],number]
		];

formatNumberFormMathMLNumber[str_String]:=str;

GeneralDownValue@formatNumberFormMathMLNumber;

validateGiveNumber[HoldComplete[str_String],number_Symbol]/;
	AtomQ[Unevaluated[str]]&&SyntaxQ[str]:=
	validateGiveNumberKernel[ToExpression[str,InputForm,HoldComplete],number];

validateGiveNumber[_,number_Symbol]=False;

GeneralDownValue@validateGiveNumber;

validateGiveNumberKernel[HoldComplete[numberVal:_Real|_Integer],number_Symbol]:=
	(number=numberVal;True);

validateGiveNumberKernel[_,number_Symbol]=False;

GeneralDownValue@validateGiveNumberKernel;

formatNumberFormMathMLInterpretationBox[boxes_,number_?NumberQ,otherArgs___]:=
	Module[{str},boxes/.str_String:>formatNumberFormMathMLNumber[str]];

GeneralDownValue@formatNumberFormMathMLInterpretationBox;

formatNumberFormMathMLBoxes[boxes_]:=
	Module[{intBoxes,number,otherArgs,result},
		boxes/.InterpretationBox[intBoxes_,number_?NumberQ,otherArgs___]:>
			Block[
				{InterpretationBox},
				formatNumberFormMathMLInterpretationBox[
					intBoxes,
					number,
					otherArgs
					]/;True
				]
		];

GeneralDownValue@formatNumberFormMathMLBoxes;

expressionToSymbolicMathML[expr_,boxes_,opts:optionsOrNullPseudoPatternObject]:=
	Module[{melement,aHead,pre,mid,post,body},
		sVGMathCompatibility[rawXML@
			StringReplace[
				BoxesToMathML[
					formatNumberFormMathMLBoxes[boxes],
					"ElementFormatting"->None,
					FilterOptions[
						BoxesToMathML,
						Sequence@@(ConversionOptions/.{opts}),
						opts
						]
					],
				StringExpression[
					Whitespace,
					"xmlns=",
					quoteCharStringPatternObject,
					mathMlNameSpace,
					quoteCharStringPatternObject]->"",
				1]/.melement:XMLElement[containsMsPatternObject,___]:>
						reformatMs[melement]/.
(*indent adjusted*)		melement:XMLElement[containsMtextPatternObject,___]:>
								reformatMtext[melement],
			opts]/.XMLElement[
					aHead_,
					{pre___,"mathsize"->mid_String/;DigitQ@mid,post___},
					body_]:>XMLElement[aHead,{pre,"mathsize"->mid<>"pt",post},
						body]
		];

end of questionable old workarounds*)


(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)