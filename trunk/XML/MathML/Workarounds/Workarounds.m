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


(*exploreDVs and matchingDVs: code for looking at DownValues
practical calls:
exploreDVs[Names["System`Convert`MathMLDump`*"],symb_Symbol/;AtomQ@Unevaluated@
	symb:>With[{result=ToString@Unevaluated@symb},result/;True],str_String/;
		AtomQ@Unevaluated@str&&StringMatchQ[str,___~~"ShowContents"~~___,
			IgnoreCase->True]]
matchingDVs[Unevaluated@
	System`Convert`MathMLDump`BoxesToSMML[TagBox[StyleBox["V",
		FontVariations->{"StrikeThrough"->True},FontFamily->"Times New Roman",
		FontSize->14],"MathMLPresentationTag",AutoDelete->True]]]
*)
exploreDVs[nameSet:{__String},selectionTransform:_List|_Rule|_RuleDelayed,
	selectionPattern_]:=
	Select[ToExpression[#,InputForm,DownValues],
		!FreeQ[#/.selectionTransform,selectionPattern]&
		]&/@Select[nameSet,
				!FreeQ[ToExpression[#,InputForm,DownValues]/.selectionTransform,
					selectionPattern]&
				]

matchingDVs[symb_Symbol,call_]:=
	Select[DownValues@Unevaluated@symb,MatchQ[Unevaluated@call,#[[1]]]&]

matchingDVs[call_]:=matchingDVs[Head@Unevaluated@call,Unevaluated@call]


(*insert DownValues*)
Attributes@insertDownValueByFirstPositionOf={HoldFirst}

insertDownValueByFirstPositionOf[symb_Symbol/;AtomQ@Unevaluated@symb,
	downValue_,pattern_,offset:_:0]:=
	DownValues@Unevaluated@symb=
		With[{oDv=DownValues@Unevaluated@symb},
			Insert[oDv,Unevaluated@downValue,
				Position[oDv,Unevaluated@pattern,Infinity,1][[1,1]]+offset]
			]


(*_at least_ one of the FontVariations can be supported: "StrikeThrough"
reference:
XML`MathML`ExpressionToMathML[StyleForm[V,FontVariations->{"StrikeThrough"->
	True},FontFamily->"Times New Roman",FontSize->14,ShowContents->False]]
*)
System`Convert`MathMLDump`optionOfBoxIsLiftableQ[FontVariations,StyleBox]=False

insertDownValueByFirstPositionOf[System`Convert`MathMLDump`BoxesToSMML,
	HoldPattern[System`Convert`MathMLDump`BoxesToSMML[
		StyleBox[content_,l___,FontVariations->{___,"StrikeThrough"->True,___},
			r___]]
		]:>XMLElement["menclose",{"notation"->"horizontalstrike"},
			{System`Convert`MathMLDump`BoxesToSMML[StyleBox[content,l,r]]}
			],
	ShowContents,
	1
	]


(*take care of nobreak spacing handling
the built in BoxesToSSML rule to avoid this situation around matrices doesn't
"see past" TagBox:
XML`MathML`ExpressionToMathML[MatrixForm[{a,b}],
	"Formats"->{"PresentationMathML"},"IncludeMarkupAnnotations"->False]
in MMA 5, this needs to be inserted before the DownValue for mtext
*)
insertDownValueByFirstPositionOf[System`Convert`MathMLDump`BoxesToSMML,
	HoldPattern[
		System`Convert`MathMLDump`BoxesToSMML[char:"\[NoBreak]"]
		]:>XMLElement["mspace",
			{System`Convert`MathMLDump`baseCharacterToLinebreak[char]},
			{}
			],
	XMLElement["mtext",{},{System`Convert`MathMLDump`token}]
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
insertDownValueByFirstPositionOf[System`Convert`MathMLDump`BoxesToSMML,
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
	"AnnotationsTagWrapper"
	]


(*Boolean opposite of current value of ShowStringCharacters from the front end*)
If[$Notebooks,
	dontShowStringCharacters:=!ShowStringCharacters/.
		AbsoluteOptions[$FrontEnd,ShowStringCharacters],
	dontShowStringCharacters=True
	]


(*take care of strings so that they display as they would in the front end
XML`MathML`ExpressionToSymbolicMathML["a",
	"IncludeMarkupAnnotations"->False,"Formats"->{"PresentationMathML"}
	]//FullForm

XML`MathML`ExpressionToSymbolicMathML[Method->"\"Unimodal\"",
	"IncludeMarkupAnnotations"->False,"Formats"->{"PresentationMathML"}
	]//FullForm
*)

insertDownValueByFirstPositionOf[System`Convert`MathMLDump`BoxesToSMML,
	HoldPattern[
		System`Convert`MathMLDump`BoxesToSMML[
			str_String/;dontShowStringCharacters&&
				StringMatchQ[str,"\"*\""]
			]
		]:>XMLElement["mtext",
			{},
			{System`Convert`CommonDump`UnescapeLinearSyntax@
				StringTake[str,{2,-2}]
				}
			],
	"\"*\""
	]


(*assumes a regular string*)
leadingAndTrailingWhitespaceHandler[str_String]:=
	With[{result=Sequence@@StringCases[str,wlhs:Whitespace|""~~sign:"-"|""~~
		mid:(__~~Except@WhitespaceCharacter)~~wrhs:Whitespace|"":>
			RowBox[Flatten@
				{Characters@wlhs,Characters@sign,mid,Characters@wrhs}
				]
		]},
		result/;Length@Unevaluated@result>=1&&Length@First@result>=2]

leadingAndTrailingWhitespaceHandler[str_String]=str;

(*
leadingAndTrailingWhitespaceHandler[boxes_]:=boxes/.str_String/;AtomQ@
	Unevaluated@str&&SyntaxQ@str:>leadingAndTrailingWhitespaceHandler@str
*)
	
GeneralDownValue@leadingAndTrailingWhitespaceHandler


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
			With[{intReplacedBoxes=intFirstBoxes/.str_String/;AtomQ@Unevaluated@
						str&&SyntaxQ@str:>
					With[{result=leadingAndTrailingWhitespaceHandler@
							StringReplace[str,"\""->""]},result/;True]
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

GeneralDownValue@boxesToMathMLBoxes


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


(*take care of NumberForm numbers (when using two arguments to NumberForm)
since this is difficult to demonstrate from the standard (non-workaround)
operation of ExpressionToMathML, I show a case from BoxesToMathML
XML`MathML`BoxesToMathML["0.065420","Formats"->{"PresentationMathML"},
	"IncludeMarkupAnnotations"->False]
note the trailing zero
*)
System`Convert`MathMLDump`toStandardizedStringNumber[s_String?DigitQ]=.;
System`Convert`MathMLDump`toStandardizedStringNumber[s_String]=s;


(*handle single space characters that aren't properly handled by the catch-all
_String to mtext (default) definition as in:
XML`MathML`BoxesToMathML[" "]
whitespace is collapsed to nothing by compliant presentation renderers
*)
System`Convert`MathMLDump`BoxesToSMML[" "]=
	XMLElement["mspace",
		{"width"->ToString[System`Convert`MathMLDump`choppedNumber[
						System`Convert`MathMLDump`convertUnits[
							"mediummathspace","em",-200,200
							]
						],
					InputForm]<>"em"
			},
		{}
		](*experimental en space*)
	(*XMLElement["mtext",{},List@FromCharacterCode[8194]]*)


(*a good example ExpressionToMathML call that showcases fixes from this file:
XML`MathML`ExpressionToMathML[StyleForm[NumberForm[5.3``0.7*a],FontSize->11],
	"Formats"->{"PresentationMathML"},"IncludeMarkupAnnotations"->False]
*)


(*
add strikethrough characters if the target system does not support
menclose notation->"horizontalstrike"
*)
(*I don't think I need this top level XMLObject rule.*)
horizontalStrikeThroughCharacters[
	start:XMLObject["Document"][_,_XMLElement,_]]:=
	MapAt[horizontalStrikeThroughCharacters,start,{2}]

horizontalStrikeThroughCharacters[
	XMLElement["menclose",attributes_List/;
			MemberQ[attributes,"notation"->str_String/;
				StringMatchQ[str,___~~"horizontalstrike"~~___]
				],
		body_]]:=
	Block[{doHorizontalStrike=True},
		XMLElement["menclose",attributes,
			horizontalStrikeThroughCharacters/@body]
			]

horizontalStrikeThroughCharacters[XMLElement["menclose",attributes_,body_]]:=
	Block[{doHorizontalStrike=False},
		XMLElement["menclose",attributes,
			horizontalStrikeThroughCharacters/@body]
			]

horizontalStrikeThroughCharacters[XMLElement[element_,attributes_,body_]]:=
	XMLElement[element,attributes,horizontalStrikeThroughCharacters/@body]

(*strikethrough combining diacritical mark*)
horizontalStrikeThroughCharacters[str_String]/;doHorizontalStrike:=
	StringReplace[str,char_->char~~"\:0336"]

horizontalStrikeThroughCharacters[str_String]=str

horizontalStrikeThroughCharacters[other_]=other


(*handle parenthesis being drawn too low for contents when the head of an
expression has a subscript*)
(*remove extra mspace elements resulting from nobreak characters just inside
parenthesis*)
(*replace characters SVGMath can't handle*)
sVGMathCompatibilityFunction[xml_]:=
	horizontalStrikeThroughCharacters/@ReplaceRepeated[xml,
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
			}]


(*replace characters Firefox can't handle*)
firefoxCompatibilityFunction[xml_]:=
	horizontalStrikeThroughCharacters/@(xml/.
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
			})


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
		reformatMtext[XMLElement[mtextHead,{attributes},{StringDrop[str,-1]}]],
		XMLElement[
			mtextHead/."mtext"->"mspace",
			{"width"->$mspaceWidth,attributes},
			{}
			]
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