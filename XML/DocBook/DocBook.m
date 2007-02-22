(* ::Package:: *)
BeginPackage["XML`DocBook`",
	{"Utilities`FilterOptions`","Utilities`FilterOptions`Extensions`",
		"XML`MathML`Workarounds`","Utilities`GhostscriptPDFExport`"}];

$ExportWidth::usage="$ExportWidth specifies the width at which to line \
wrap exported expressions.";

$MultipleGraphicsExportTypes::usage="Alternative file types for which \
Mathematica can perform a direct export to an animation. Other file type \
choices export only one frame (part), given by the value of \
GraphicsListPart, in a list of graphics objects.";

$PrintResolution::usage="$PrintResolution gives the default resolution \
used to export graphics to raster formats destined for display in print.";

$ScreenResolution::usage="$ScreenResolution is the default resolution \
used to export graphics to raster formats destined for display on screen. You \
should set this in your kernel's init.m file via \
XML`DocBook`$ScreenResolution=dpi, where dpi is the dpi of your screen.";

$ToBoxesFunction::usage="$ToBoxesFunction[expr,opts] converts and expression \
to boxes using StyleForm nested inside ToBoxes. It allows you to control \
the output box type and the text form via its options.";

AllowMathPhrase::usage="This is an option with a boolean value (True|False) \
that controls wether the particular export will use <mathphrase> to represent \
the subclass of expressions that can be represented by pure DocBook markup.";

AlternateSizePacketMethod::usage="This option for the DocBook*Equation \
commands makes calculation of the baseline and bounding box of a particular \
piece of typeset mathematics take Magnification into account after the call to \
the front end - instead of before. Sometimes, this can result in a better \
baseline shift value."

BoldHeadings::usage="BoldHeadings is an option for DocBookTable that applies \
StyleForm[#,FontWeight->\"Bold\"]& to the entries in the heading before they \
are passed off to DocBookInlineEquation. This helps \"immutable\" output such \
as pictures and vector renditions of mathematics and fonts match the default \
styling the DocBook stylesheets would normally apply to such an entry if it \
were plain text.";

Caption::usage="This is an option for the DocBook* table, figure, and equation \
functions that accepts a string, XMLElement or XMLChain as a caption.";

CellOptions::usage="CellOptions is a list of options provided to the \
Cell expression that will be exported by one of the DocBook*Equation \
commands.";

CharacterReplacements::usage="This option for the XMLDocument accepts as its \
right hand side a list of rules to be used in a StringReplace on all strings in
the xmlchain argument.";

CommonMathMLCompatibility::usage="This is an option for the DocBook*Equation \
functions that will set $CommonMathMLCompatibility=True at appropriate times \
during \
output generation. For more information, see the usage message for \
$CommonMathMLCompatibility.";

DataAttributes::usage="These attributes are applied to the element inside the \
<*object> element. This is usually an <imagedata> or <phrase> element.";

Declarations::usage="Declarations contains the rules for the pseudo attributes \
of the xml declaration at the beginning of the output XML document.";

GraphicsListPart::usage="This is an option for DocBook*Figure and \
DocBookInlineMediaMediaObject with a right hand value that indicates the Part \
of a list \
of graphics objects to be exported in case Export is incapable of creating an \
animated version of the plot in the format given by FileType. Plots for which \
Export can create an animated \
version are given by $MultipleGraphicsExportTypes.";

DocBookEquation::usage="DocBookEquation[\"id\",\"title\",expr,opts]";

DocBookEquationSequence::usage="DocBookEquationSequence[expr1,expr2...] may be \
fed into DocBookEquation as the expression argument to create an equation \
element that has multiple mediaobject or mathphrase children.";

DocBookFigure::usage="DocBookFigure[\"id\",\"title\",\"alt text\",graphics,\
opts]";

DocBookInformalFigure::usage="DocBookInformalFigure[\"id\",\"alt text\",\
graphics,opts]";

DocBookInformalEquation::usage="DocBookInformalEquation[\"id\",expr,opts]";

DocBookInformalTable::usage="DocBookInformalTable[\"id\",\"description\",\
table,opts]";

DocBookInlineEquation::usage="DocBookInlineEquation[\"id\",expr,opts]";

DocBookInlineEquationOptions::usage="DocBookInlineEquationOptions is an option \
for DocBookTable that allows one to specify the options fed to \
DocBookInlineEquation."

DocBookInlineMediaObject::usage="DocBookInlineMediaObject[\"id\",\
\"description\",graphics,opts]";

DocBookTable::usage="DocBookTable[\"id\",\"title\",\"alt text\",table,opts]";

ExportDelayed::usage="This is an inert version of Export. When one is ready \
to actually perform the Export, Replace ExportDelayed with Export. These \
ExportDelayed objects are handled differently by this package depending \
on whether they are \"XML\" or non-XML types. The type is given in the third \
argument. See ToXML and XMLChain.";

Exports::usage="Exports is an option for DocBookEquation and DocBookFigure \
that gives a nested list of option lists for the functions that are called \
in the process of making an equation or figure element.";

ExportsOption::rlnf=
    "An ObjectAttributes rule with an entry for the role `1` was not found in \
the list of option lists. Therefore, the rule(s) `2` can't be added to the \
exports for role `1`.";

ExportsOption::usage="ExportsOption[optionListList,role,newOptions] gives \
the optionListList with one of its entries having new rules corresponding to \
newOptions. The entry that is replaced is the one that has a rule for \
ObjectAttributes that includes \"role\"->role. ExportsOption[symb,role,\
newOptions] does the same thing, but gets optionListList from the right hand \
side of the Exports option given by Options[symb,Exports]. Both of these \
call forms are useful for modifying the Exports option of the \
DocBook*Equation/Figure/Table functions. A call might be as follows: \
DocBookFigure[...,Exports->ExportsOption[DocBookFigure,\"fo\",ExportType->\"PNG\
\"]";

ExportType::usage="This is the type of output that will be generated for the \
expression being exported. It must be one of the types handled by Export.";

FirefoxMathMLCompatibility::usage="This is an option for the DocBook*Equation \
functions that will set $FirefoxMathMLCompatibility=True at appropriate times \
during \
output generation. For more information, see the usage message for \
$FirefoxMathMLCompatibility.";

InlineMediaObjectElement::usage"InlineMediaObjectElement is the type of \
element used to contain inlineequation media.";

MediaObjectElement::usage="MediaObjectElement is the type of element used \
to contain equation and informalequation media.";

NotebookOptions::usage="NotebookOptions is a list of options provided to the \
Notebook expression that will be exported by one of the DocBook*Equation \
commands.";

ObjectAttributes::usage="These attributes are applied to the <*object> element \
inside the <mediaobject> element.";

ObjectContainer::usage="ObjectContainer is an option for the DocBook*Equation \
functions that allows one to specify the type of media object container for \
the generated equations (inline or regular block level.";

PrependDirectory::usage="PrependDirectory is an option for XMLDocument that \
allows prepension of a directory name to each id in an XML chain. This option \
provides an easy method to set the output directory of the files in the chain. \
Set the option to the path string that you would like to append to the ids."; 

SetIdAttribute::usage="An boolean option for the DocBook* functions that \
states whether or not to set the xml:id attribute on the generated element.";

SVGMathMathMLCompatibility::usage="This is an option for the DocBook*Equation \
functions that will set $SVGMathMathMLCompatibility=True at appropriate times \
during \
output generation. For more information, see the usage message for \
$SVGMathMathMLCompatibility.";

TextOptions::usage="A sub option of the Exports option. It affects downstream"<>
	"ToBoxes and StyleBox calls. Well, some of them. It typically contains"<>
	"the sub options TextStyle and FormatType.";

TitleAbbrev::usage="An options for the DocBook* functions that accepts a \
right hand value like the title argument. It is usually shorter than the \
title.";

ToBoxesFunction::usage="This pure function is applied to the expression to be \
exported to obtain its box form. Within the package, the right hand side of \
rule becomes the head of a function that accepts the expression to be exported \
and the Sequenced right hand side of TextOptions.";

ToXML::usage="This function sequences the XML out of an XMLChain and Sows \
all the rest of the ExportDelayed types. See ExportDelayed and XMLChain.";

WriteDimensions::usage="This is an option for graphic Exports of the DocBook*\
Equation/Figure functions that (currently) causes the dimensions of the export \
to be extracted from an EPS ((if ReplaceBoundingBox is false or if the export \
is a type of Graphics) and the export role isn't html) or \
GetBoundingBoxSizePacket otherwise. Also, if the function is able to use \
GetBoundingBoxSizePacket, it writes a processing instruction into the XML that \
can be for baseline adjustment.";

XMLChain::usage="XMLChain[XMLElement[..]] An XMLChain is a function that will \
Sow the XML in its \
argument just before Reaping the XML and non-XML tags (that are handled \
by this package). Xml chain is also the term used to describe a list of \
ExportDelayed functions, because that is what XMLChain generates. \
Similarly, there are other functions that generate these ExportDelayed \
lists, and they are called XMLChain generating functions. See \
ExportDelayed and ToXML.";

XMLDocument::badprep="A bad path of `1` was given as the PrependDirectory \
option to XMLDocument. The prepend will be skipped.";

XMLDocument::usage="XMLDocument[\"file\",xmlchain,opts] produces a list of \
ExportDelayed objects with an \"XML\" export that has appropriate \
serialization options and character replacements for DocBook. xmlchain is a \
list of ExportDelayed objects of the kind produced by XMLChain, \
DocBookEquation, DocBookTable, etc.";

Begin["`Private`"];

(*context handling*)
If[!NameQ[#<>"*"],Get@#]&/@{"Utilities`BadArgumentHandling`"}

old$ContextPath=$ContextPath
$ContextPath=Flatten@{$ContextPath,"XML`","XML`MathML`",
	"Utilities`BadArgumentHandling`"}


(*the BoxesToMathML call on the greek character is needed to define
System`ConvertersDump`fullPathNameExport, System`Convert`XMLDump`$XMLNamesNS,
and System`Convert`MathMLDump`$MathMLNS*)

BoxesToMathML["\[Beta]"];


$MultipleGraphicsExportTypes=Alternatives@"GIF";

vectorGraphicsTypes="EPS"|"PDF"|"SVG";

(*patterns*)

superScriptAndSubscriptPatternObject=SuperscriptBox|SubscriptBox;

boxesPatternObject=Alternatives@@
	ToExpression/@Cases[Names["System`*"],x_/;StringMatchQ[x,___~~"Box"]];

notBoxExpressionPatternObject=
	Except[
		Union@
			Flatten[
				BlankSequence/@boxesPatternObject|
					boxesPatternObject|_String|List|__List
				]
		];

nonRowBoxesPatternObject=Module[{x},DeleteCases[boxesPatternObject,RowBox]];

nonRowSuperscriptOrSubscriptBoxesPatternObject=DeleteCases[
	nonRowBoxesPatternObject,
	superScriptAndSubscriptPatternObject
	];

stripableBoxesPatternObject=
	Alternatives[
		InterpretationBox,
		TagBox,
		StyleBox,
		FormBox
		];

nonRowSupSubOrStripableBoxes=
	DeleteCases[
		nonRowSuperscriptOrSubscriptBoxesPatternObject,
		stripableBoxesPatternObject
		];

unwantedBeneathScriptBoxes=
	DeleteCases[
		nonRowBoxesPatternObject,
		stripableBoxesPatternObject
		];

rowBoxOrStringPatternObject=(_RowBox|_String)..;

ruleHeadPatternObject=Rule|RuleDelayed;

allSewingTags=xmlSewingTag|otherSewingTag;

xmlFileType="XML";

nothing=""|None|Null;

stringOrNothingPatternObject=_String|nothing;

booleanPatternObject=True|False;

equationElementNameStringsPatternObject="equation"|"informalequation"|
	"inlineequation";

figureElementNameStringsPatternObject="figure"|"informalfigure";

tableElementNameStringsPatternObject="table"|"informaltable";

xmlElementPseudoPatternObject=(_XMLElement|XMLObject[_String][__])(*?(Function[
	SymbolicXMLQ[#,True]])*);

xmlPseudoPatternObject:=_String|xmlElementPseudoPatternObject;

xmlOrNothingPseudoPatternObject=xmlPseudoPatternObject|nothing;

sequenceXmlPseudoPatternObject=xmlPseudoPatternObject..;

sequenceXmlOrNothingPseudoPatternObject=xmlOrNothingPseudoPatternObject..;

multipleXmlOrNothingPseudoPatternObject=
	{sequenceXmlOrNothingPseudoPatternObject};

sequenceNullXmlPseudoPatternObject=xmlPseudoPatternObject...;

multipleNullXmlPseudoPatternObject={sequenceNullXmlPseudoPatternObject};

optionPseudoPatternObject=(ruleHeadPatternObject[_,_])?OptionQ;

optionsOrNullPseudoPatternObject=optionPseudoPatternObject...;

(*the subpattern that should be repeated for tablePseudoPatternObject is rather
difficult to name - so I just left it as an expression*)

tablePseudoPatternObject={{__},{__}..}?MatrixQ;

exportDelayedPseudoPatternObject=ExportDelayed[_String,_,_String,
	optionsOrNullPseudoPatternObject];

exportXmlDelayedPseudoPatternObject=ExportDelayed[stringOrNothingPatternObject,
	_,xmlFileType,optionsOrNullPseudoPatternObject];

(*An XmlChain is defined as a List of ExportDelayed statments containing one
ExportDelayed of the xmlFileType.*)

exportXmlChainPseudoPatternObject={exportDelayedPseudoPatternObject...,
	exportXmlDelayedPseudoPatternObject,exportDelayedPseudoPatternObject...};

xmlOrExportXmlChainPseudoPatternObject=xmlPseudoPatternObject|
	exportXmlChainPseudoPatternObject;

xmlOrExportXmlChainOrNothingPseudoPatternObject=
	xmlOrExportXmlChainPseudoPatternObject|nothing;

graphicsPatternObject=(Graphics|Graphics3D|SurfaceGraphics|ContourGraphics|
	DensityGraphics)[__];

multipleGraphicsPatternObject={graphicsPatternObject..};

graphicsOrMultipleGraphicsPatternObject=graphicsPatternObject|
	multipleGraphicsPatternObject;

xmlNameSpace="http://www.w3.org/XML/1998/namespace";

quoteCharStringPatternObject="\""|"'";

xmlIdAttribute={xmlNameSpace,"id"};

docBookEquationVersion="5.0-extension MathML-2.0 SVG-1.1";

docBookEquationVersionAttributeRule="version"->docBookEquationVersion;

docBookNameSpace="http://docbook.org/ns/docbook";

xmlnsNameSpaceAttribute={System`Convert`XMLDump`$XMLNamesNS,"xmlns"};

docBookNameSpaceAttributeRule=xmlnsNameSpaceAttribute->docBookNameSpace;

symbolicMLConversionOptions=ConversionOptions->{"ElementFormatting"->None};

(*i think:*)
(*Mathematica is incapable of generating fully correct ContentMathML*)
(*it also messes up the export of NumberForm[xpr] in annotations*)

mathMLConversionOptions=Sequence["Formats"->{"PresentationMathML"},
	"NamespacePrefixes"->{System`Convert`MathMLDump`$MathMLNS->"mml"},
	"IncludeMarkupAnnotations"->False];

xmlAttributePatternObject=ruleHeadPatternObject[_String,_String];

multipleXmlAttributePatternObject={xmlAttributePatternObject..};

sequenceNullXmlAttributePatternObject=xmlAttributePatternObject...;

multipleNullXmlAttributePatternObject={sequenceNullXmlAttributePatternObject};

heldExpressionPatternObject=Blank/@Alternatives[Hold,HoldComplete];

exportsPatternObject={{optionsOrNullPseudoPatternObject}..};

dontReFormatFormsPatternObject=Blank/@Alternatives[TraditionalForm];

(*page size*)
If[!ValueQ[$ExportWidth],$ExportWidth=450];

(*dpi*)
If[!ValueQ[$PrintResolution],$PrintResolution=300];
If[!ValueQ[$ScreenResolution],$ScreenResolution=96];
pdfScaleAttribute="scale":>ToString@N[$ScreenResolution/$PrintResolution*100];

(*functions*)


(*option switching by role*)

ExportsOption[
	oldExportsRhs:{__?OptionQ},
	role_String,newExportsSubOptions__?OptionQ
	]:=
	With[
		{rolePatternPositions=
			Position[oldExportsRhs,
				ruleHeadPatternObject[
					ObjectAttributes,
					{___,ruleHeadPatternObject["role",role],___}
					]
				],
			opts=Flatten@{newExportsSubOptions}
			},
		With[{exportPosition=First@First@rolePatternPositions},
			ReplacePart[oldExportsRhs,
				ruleFlatUnion[newExportsSubOptions,
					Extract[oldExportsRhs,exportPosition]
					],
				exportPosition
				]
			]/;If[rolePatternPositions=!={},
				True,Message[ExportsOption::rlnf,role,opts]
				]
		];

ExportsOption[
	symb_Symbol,role_String,newExportsSubOptions__?OptionQ
	]:=
	With[
		{result=Check[ExportsOption[Exports/.Options[symb,Exports],
			role,newExportsSubOptions],$Failed]},
		result/;result=!=$Failed
		];

GeneralDownValue[ExportsOption];

(*expression to string conversion*)

removeRowBoxes[expr_]:=Module[{args},expr//.RowBox[{args__}]:>Sequence[args]];

GeneralDownValue@removeRowBoxes;

docBookSuperscript[expr:rowBoxOrStringPatternObject]:=
	RowBox[{
		{expr}[[1]],
		Function[
			XMLElement["superscript",{},{StringJoin[##]}]]@@
				removeRowBoxes@Rest@{expr}
		}];

GeneralDownValue@docBookSuperscript;

docBookSubscript[expr:rowBoxOrStringPatternObject]:=
	RowBox[{
		{expr}[[1]],
		Function[
			XMLElement["subscript",{},{StringJoin[##]}]]@@
				removeRowBoxes@Rest@{expr}
		}];

GeneralDownValue@docBookSubscript;

stringFormattableQ[boxes_]:=
	Module[{subXpr,sewingTag},
		And[
			FreeQ[boxes,nonRowSupSubOrStripableBoxes],
			Sequence@@Flatten@Reap[
				boxes/.(superScriptAndSubscriptPatternObject)[subxpr__]:>
					Sow[FreeQ[{subxpr},unwantedBeneathScriptBoxes],sewingTag],
				sewingTag
				][[2]]
			]
		];
	
GeneralDownValue@stringFormattableQ;

constantStringReplacements=Sequence["\\n"->"\n"];

formatString[str_String/;AtomQ[Unevaluated[str]]&&
	(!ShowStringCharacters/.AbsoluteOptions[$FrontEnd,ShowStringCharacters])]:=
	StringReplace[str,{constantStringReplacements,"\\\""->"\"","\""->""}];

formatString[str_String]:=StringReplace[str,{constantStringReplacements}];

GeneralDownValue@formatString;

removeUnwantedBoxes[boxes_]:=
	Module[{unwantedBoxExpr,symb},
		boxes//.
			unwantedBoxExpr:nonRowSuperscriptOrSubscriptBoxesPatternObject[__]:>
				unwantedBoxExpr[[1]]
		];

GeneralDownValue@removeUnwantedBoxes;

unStringableBoxesQ[boxes_/;FreeQ[boxes,notBoxExpressionPatternObject]]:=False;

(*This should always give False. If it doesn't, either the toString routine
is broken, or a MakeBoxes definition created something that isn't a box or a
string*)

unStringableBoxesQ[boxes_]:=True;

GeneralDownValue@unStringableBoxesQ;

toBoxes[expr_,opts___?OptionQ]:=
	(ToBoxesFunction/.{opts})[
		expr,
		Sequence@@Rule@@@(TextOptions/.{opts})
		];

GeneralDownValue@toBoxes;

toString::"usb"="Can't convert `1` into a string.";

toStringKernel[expr_,boxes_,opts:optionsOrNullPseudoPatternObject]:=
	Module[{strippedBoxes},
		strippedBoxes=removeUnwantedBoxes[boxes]/.str_String:>formatString@str/.
			optionPseudoPatternObject->Sequence[];
		If[unStringableBoxesQ[strippedBoxes],
			Message[toString::"usb",expr];Abort[]
			];
		Block[
			{SuperscriptBox=docBookSuperscript,SubscriptBox=docBookSubscript},
			Sequence@@Flatten[{strippedBoxes/.RowBox->List}]
			]
		];

toString[expr_,boxes_/;stringFormattableQ[boxes],
	opts:optionsOrNullPseudoPatternObject]:=
	toStringKernel[expr,boxes,opts];

toString[expr_,boxes_,opts:optionsOrNullPseudoPatternObject]:=
	ToString[
		expr,
		Sequence@@Rule@@@{FilterOptions[ToString,TextOptions/.{opts},opts]}
		];

GeneralDownValue@toString;


(*fullPathNameExport requires a path and a file type from $ExportFormats*)

fullPathNameExport=System`ConvertersDump`fullPathNameExport;

idLast[id_String]:=Last@fromFileName@fullPathNameExport[id,"XML"];

GeneralDownValue@idLast;

(*xmlIdAttributeRule*)

Options@xmlIdAttributeRule={SetIdAttribute->True};

xmlIdAttributeRule[id_String,opts:optionsOrNullPseudoPatternObject]:=If[
	SetIdAttribute/.{opts}/.Options@xmlIdAttributeRule,xmlIdAttribute->idLast@
		id,Unevaluated[Sequence[]]];

GeneralDownValue@xmlIdAttributeRule;

fileRefAttribute[id_String]:="fileref"->idLast@id;

GeneralDownValue@fileRefAttribute;

noAttributeXmlElement[element_String,content:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=XMLElement[element,{},{content}];

GeneralDownValue@noAttributeXmlElement;

processDescriptionPart[descriptionPart_String,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["phrase",descriptionPart,opts];

processDescriptionPart[descriptionPart:xmlElementPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=descriptionPart;

GeneralDownValue@processDescriptionPart;

textObjectElement[attributes:multipleNullXmlAttributePatternObject,
	xml:multipleXmlOrNothingPseudoPatternObject]:=
	XMLElement["textobject",attributes,xml];

textObjectElement[alt:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=noAttributeXmlElement["textobject",alt,
	opts];

GeneralDownValue@textObjectElement;

mathPhraseElement[phrase:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["mathphrase",phrase,opts];

GeneralDownValue@mathPhraseElement;

phraseElement[attributes:multipleNullXmlAttributePatternObject,
	xml:multipleXmlOrNothingPseudoPatternObject]:=
	XMLElement["phrase",attributes,xml];

phraseElement[phrase:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["phrase",phrase,opts];

GeneralDownValue@phraseElement;

inlineEquationElement[attributes:multipleNullXmlAttributePatternObject,
	xml:multipleXmlOrNothingPseudoPatternObject]:=
	XMLElement["inlineequation",attributes,xml];

GeneralDownValue@inlineEquationElement;

altElement[alt:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["alt",alt,opts];

GeneralDownValue@altElement;

(*captionElement*)

(*the DocBook caption does not accept raw strings, so convert them to <para>s*)

processCaptionPart[captionPart_String,opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["para",captionPart,opts];

processCaptionPart[captionPart:xmlElementPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=captionPart;

GeneralDownValue@processCaptionPart;

captionElement[caption:exportXmlChainPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=captionElement[ToXML[caption,opts]];

captionElement[caption:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=Module[{case},noAttributeXmlElement[
		"caption",Sequence@@processCaptionPart/@{caption},opts]];

captionElement[nothing,opts:optionsOrNullPseudoPatternObject]:=Sequence[];

GeneralDownValue@captionElement;

(*titleElements*)

titleElement[title:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=noAttributeXmlElement["title",title,
		opts];

titleElement[nothing,opts:optionsOrNullPseudoPatternObject]:=Sequence[];

GeneralDownValue@titleElement;

titleabbrevElement[titleabbrev:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=noAttributeXmlElement["titleabbrev",
	titleabbrev,opts];

titleabbrevElement[nothing,opts:optionsOrNullPseudoPatternObject]:=Sequence[];

GeneralDownValue@titleabbrevElement;

(*needs a switch to something more formal - perhaps it will be made when 
I move other functions to have titleabbrev ability*)

titleElements[True,
	title:xmlOrExportXmlChainPseudoPatternObject,
	titleAbbrev:xmlOrExportXmlChainPseudoPatternObject|Automatic,
	opts:optionsOrNullPseudoPatternObject]:=
	Sequence[titleElement[ToXML@title,opts],
		If[titleAbbrev===Automatic,
			Unevaluated[Sequence[]],
			titleabbrevElement[ToXML@titleAbbrev,opts]]];

titleabbrevElement[titleabbrev:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=noAttributeXmlElement["titleabbrev",
	titleabbrev,opts];

titleElements[False,___]=Sequence[];

GeneralDownValue@titleElements;

(*imageobject*)

imageDataElement[xml:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=noAttributeXmlElement["imagedata",xml,
	opts];

GeneralDownValue@imageDataElement;

(*the xmlid attribute rule was commented out because the attribute is
unneeded and because it causes id collisions if an inline equation is used more
than once, even with SetIdAttribute->False - if this functionality is needed,
then a way to propagate DocBook* command options for SetIdAttribute to
imageObjectElement must be created*)

imageObjectElement[id_String,expr_,boxes_,"Text",idExtension_String,
	imageObjectAttributes:multipleNullXmlAttributePatternObject,
	imageDataAttributes:multipleNullXmlAttributePatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	textObjectElement[
		{Sequence@@imageObjectAttributes
			(*,xmlIdAttributeRule[id<>idExtension,opts]*)},
		{phraseElement[
			imageDataAttributes,
			{inlineEquationElement[
				{},
				{mathPhraseElement[
					toString[
						expr,
						boxes,
						Sequence@@
							Replace[
								{opts},
								ruleHeadPatternObject[FormatType,_]->
									FormatType->InputForm,
								{3}
								]
						]
					]}
				]}
			]}
		];

imageObjectElement[id_String,expr_,boxes_,"MathML",idExtension_String,
	imageObjectAttributes:multipleNullXmlAttributePatternObject,_List,
	opts:optionsOrNullPseudoPatternObject]:=
	XMLElement["imageobject",
		{Sequence@@imageObjectAttributes
			(*,xmlIdAttributeRule[id<>idExtension,opts]*)},
		{imageDataElement@
			Block[{$SVGMathMathMLCompatibility=
						SVGMathMathMLCompatibility/.{opts},
					$FirefoxMathMLCompatibility=
						FirefoxMathMLCompatibility/.{opts},
					$CommonCompatibility=
						CommonCompatibility/.{opts}},
				ImportString[
					BoxesToMathML[boxes,
						FilterOptions[BoxesToMathML,
							Sequence@@(ConversionOptions/.{opts}),
							opts
							]
						],
					"SymbolicXML"
					]
				]
			}
		];

fileExtension[filetype_String]:=ToLowerCase@StringReplace[filetype,
	{"EPSTIFF"->"eps"},
	IgnoreCase->True];

GeneralDownValue@fileExtension;

imageObjectElement[
	id_String,
	graphics:graphicsOrMultipleGraphicsPatternObject,
	filetype_String,
	idExtension_String,
	imageObjectAttributes:multipleNullXmlAttributePatternObject,
	imageDataAttributes:multipleNullXmlAttributePatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	With[{fileName=StringJoin[id,idExtension,".",fileExtension@filetype],
		graphicsListPart=GraphicsListPart/.{opts}},
		Sow[
			ExportDelayed[
				fileName,
				graphics[[
					If[Head@graphics===List&&
						!StringMatchQ[
							filetype,
							$MultipleGraphicsExportTypes,
							IgnoreCase->True
							],
						If[Element[graphicsListPart,Integers],
							graphicsListPart,
							1],
						All]
					]],
				filetype,
				FilterOptions[Export,opts]
				],
			otherSewingTag];
		XMLElement["imageobject",{Sequence@@
			imageObjectAttributes(*,xmlIdAttributeRule[id<>idExtension,opts]*)},
			{XMLElement["imagedata",{Sequence@@imageDataAttributes,
				fileRefAttribute[fileName]},{}]}]];


Options@getBoundingBoxSizePacket={AlternateSizePacketMethod->False};

getBoundingBoxSizePacket[expr:(_Notebook|_Cell),
	opts:optionsOrNullPseudoPatternObject]/;
		(AlternateSizePacketMethod/.{opts}/.Options@getBoundingBoxSizePacket):=
	FrontEndExecute[GetBoundingBoxSizePacket[expr]]/Magnification^2/.
		AbsoluteOptions[$FrontEnd,Magnification]

getBoundingBoxSizePacket[expr:(_Notebook|_Cell),
	opts:optionsOrNullPseudoPatternObject]:=GetBoundingBoxSizePacket[expr]

GeneralDownValue@getBoundingBoxSizePacket

(*
insertMagnificationThenGetBoundingBoxSizePacket[expr_]:=
	FrontEndExecute[GetBoundingBoxSizePacket[Append[expr,Magnification->1]]];

GeneralDownValue@insertMagnificationThenGetBoundingBoxSizePacket;
*)


(*
prepareBaslineAdjustment transforms the numeric basline height into a string,
adding a zero after the decimal point for compatibility with other programs
*)
prepareBaselineAdjustment[baseToBottom_?NumberQ,idExtension_String]:=
	prepareBaselineAdjustment[ToString[-baseToBottom],idExtension];

prepareBaselineAdjustment[negativeBaseToBottom_String,"html"|"xhtml"]/;
	!DigitQ[StringTake[negativeBaseToBottom,-1]]:=
	negativeBaseToBottom<>"0";

prepareBaselineAdjustment[negativeBaseToBottom_String,_]:=negativeBaseToBottom;

GeneralDownValue@prepareBaselineAdjustment;


callFePdfCellToNotebook[cellExpr_Cell,notebookExpr_Notebook]:=
	Module[{$trapCallFrontEnd=True},
		Unprotect@MathLink`CallFrontEnd;
		MathLink`CallFrontEnd[
			ExportPacket[cellExpr,"PDF",exportPacketOtherArgs___],
			callFrontEndOtherArgs___]/;$trapCallFrontEnd:=
			Block[{$trapCallFrontEnd=False},
				MathLink`CallFrontEnd[
					ExportPacket[notebookExpr,"PDF",exportPacketOtherArgs],
					callFrontEndOtherArgs
					]
				];
		Protect@MathLink`CallFrontEnd
		]

GeneralDownValue@callFePdfCellToNotebook


(*I need to take into account that my callFePdfCellToNotebook
workaround does not work on getBoundingBoxSizePacket*)
imageObjectElement[
	id_String,
	expr_,
	boxes_,
	filetype_String,
	idExtension_String,
	imageObjectAttributes:multipleNullXmlAttributePatternObject,
	imageDataAttributes:multipleNullXmlAttributePatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[
		{contentHeight,contentWidth,baseToBottom,notebook,cell,
			fileName=StringJoin[id,idExtension,".",fileExtension@filetype],
			writeDimensions=TrueQ[WriteDimensions/.{opts}],
			vectorGraphicsType=
				StringMatchQ[filetype,
					vectorGraphicsTypes,
					IgnoreCase->True
					]
			},
		notebook=
			Notebook[
				{cell=Cell[
					StripBoxes[boxes],
					Sequence@@Rule@@@(CellOptions/.{opts})
					]},
				Sequence@@Rule@@@(NotebookOptions/.{opts})
				];
		(*points are the units after these conversions*)
		If[writeDimensions,{contentWidth,contentHeight,baseToBottom}=
			First@getBoundingBoxSizePacket[notebook,opts]];
(*old (more capable) code for obtaining the dimensions of typeset math*)
(*
		If[writeDimensions,
			{contentWidth,contentHeight,baseToBottom}=
				If[vectorGraphicsType||MatchQ[expr,graphicsPatternObject],
					epsBounds[
						notebook,
						ReleaseHold[Hold[opts]/.
							ruleHeadPatternObject[
								"IncludeSpecialFonts",_]->
									"IncludeSpecialFonts"->False
							]
						],
					First@getBoundingBoxSizePacket[notebook,opts]
					]
			];
*)
		If[$VersionNumber>=6&&ToUpperCase@filetype==="PDF",
			callFePdfCellToNotebook[cell,notebook];
			notebook=cell
			];
		Sow[ExportDelayed[
				fileName,
				notebook,
				filetype,
				FilterOptions[Export,opts]
				],
			otherSewingTag
			];
		XMLElement["imageobject",
			{Sequence@@imageObjectAttributes
				(*,xmlIdAttributeRule[id<>idExtension,opts]*)
				},
			{XMLElement["imagedata",
				{Sequence@@imageDataAttributes,
					fileRefAttribute[fileName],
					If[writeDimensions&&vectorGraphicsType,
						Identity[Sequence][
							"contentwidth"->ToString@contentWidth<>"pt",
							"contentdepth"->ToString@contentHeight<>"pt"
							],
						Identity[Sequence][]
						]
					},
				{If[writeDimensions,
					XMLObject["ProcessingInstruction"][
						"db"<>(idExtension/."xhtml"->"html"),
						"alignment-adjust=\""<>
(*indent adjusted*)		prepareBaselineAdjustment[baseToBottom,idExtension]<>
								"pt\""
						],
					Identity[Sequence][]
					]}
				]}
			]
		];

GeneralDownValue@imageObjectElement;


InlineMediaObjectElement[imageObjects:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["inlinemediaobject",imageObjects,opts];

GeneralDownValue@InlineMediaObjectElement;

MediaObjectElement[imageObjects:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["mediaobject",imageObjects,opts];

GeneralDownValue@MediaObjectElement;

(*ToXML*)

(*This ToXML function may have a Sowing side effect.*)

ToXML[xmlChain:exportXmlChainPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=Module[{xml=Sequence@@Cases[xmlChain,
		exportXmlDelayedPseudoPatternObject][[All,2]]},Sow[#,otherSewingTag]&/@
		DeleteCases[xmlChain,exportXmlDelayedPseudoPatternObject];xml];

ToXML[xml:xmlPseudoPatternObject,opts:optionsOrNullPseudoPatternObject]:=xml;

ToXML[nothing,opts:optionsOrNullPseudoPatternObject]:=Sequence[];

GeneralDownValue@ToXML;

(*XMLChain*)

XMLChain[id:stringOrNothingPatternObject,
	xmlexpr:xmlElementPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Flatten@Reap[Sow[ExportDelayed[id,xmlexpr,xmlFileType,
		FilterOptions[Export,opts]],xmlSewingTag],allSewingTags][[2]];

(*consider removing this definition below -
it can cause problems with user input
when XMLChain is used unnecessarily*)

XMLChain[xmlexpr:xmlElementPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	XMLChain[None,xmlexpr,opts];

GeneralDownValue@XMLChain;

Attributes@XMLChain={HoldAll};

(*xmlDeclaration*)

xmlDeclaration[declarations__?OptionQ]:=XMLObject["Declaration"][declarations];

xmlDeclaration[]=Sequence[];

GeneralDownValue@xmlDeclaration;

(*XMLDocument*)

Options@XMLDocument=
	{Declarations->
		{"Version"->"1.0",
			"Encoding"->"US-ASCII"(*"UTF-8"*)
			},
	PrependDirectory->False,
	symbolicMLConversionOptions,
	CharacterReplacements->{}
	};

XMLDocument[file_String,
	xmlChain:xmlOrExportXmlChainPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[
		{characterReplacements,
			declarations,
			options=Sequence@@Join[{opts},Options@XMLDocument],
			prependDirectory,
			string,
			unPrependedResult
			},
		characterReplacements=CharacterReplacements/.{options};
		declarations=Declarations/.{options};
		unPrependedResult=
			Flatten@
				Reap[
					Sow[
						ExportDelayed[
							file,
							XMLObject["Document"][
								{xmlDeclaration[
									Sequence@@
										If[
											MatchQ[
												declarations,
												nothing],
											{},
											declarations
											]
										]
									},
								ToXML@xmlChain,
								{}
								],
							xmlFileType,
							FilterOptions[Export,options]
							]/.string_String:>
								StringReplace[
									string,
									characterReplacements],
						xmlSewingTag],
					allSewingTags][[2]];
		prependDirectory=PrependDirectory/.{options};
		If[
			MatchQ[
				prependDirectory,
				Except[nothing|False]
				],
			If[
				MatchQ[
					prependDirectory,
					_String
					],
				MapAt[
					prependDirectory<>#&,
					#,
					1]&/@unPrependedResult,
				Message[
					XMLDocument::badprep,
					prependDirectory
					];
				unPrependedResult
				],
			unPrependedResult
			]
		];

GeneralDownValue@XMLDocument;

(*equations*)


Options@$ToBoxesFunction={FormatType:>$FormatType,TextStyle:>$TextStyle};

$ToBoxesFunction[expr_,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[
		{options=Sequence[opts,Sequence@@Options@$ToBoxesFunction],
			styleForm=StyleForm
			},
		ToBoxes[
			styleForm[NumberForm@expr,
				Sequence@@(TextStyle/.{options})
				],
			FormatType/.{options}
			]
		];

GeneralDownValue@$ToBoxesFunction;

$boxExportOptions=
	{ToBoxesFunction->$ToBoxesFunction,
		TextOptions->Options@$ToBoxesFunction};

$cellExportOptions={
	If[$VersionNumber>=6,ShowStringCharacters->False,Identity[Sequence][]],
	Background->None,
	CellFrameMargins->{{0,0},{0,0}},
	CellMargins->{{0,0},{0,0}},
	ShowCellBracket->False
	};

$notebookExportOptions={WindowWidth->Infinity,Magnification->1};

$mathMlXhtmlExpressionExportOptions={
	AllowMathPhrase->False,
	ConversionOptions->{mathMLConversionOptions},
	DataAttributes->{},
	ExportType->"MathML",
	ObjectAttributes->{"role"->"xhtml"},
	Sequence@@$boxExportOptions
	};

$pngHtmlExpressionExportOptions={
	DataAttributes->{},
	ExportType->"PNG",
	WriteDimensions->True,
	ImageResolution:>$ScreenResolution,
	ObjectAttributes->{"role"->"html"},
	Sequence@@$boxExportOptions
	};

$epsPdfExpressionExportOptions={
	ConversionOptions->{"IncludeSpecialFonts"->True},
	DataAttributes->{(*pdfScaleAttribute*)},
	ExportType->"PDF",
	(*ImageResolution:>$PrintResolution,*)
	ObjectAttributes->{"role"->"fo"},
	Sequence@@$boxExportOptions
	};

$mathMlPdfExpressionExportOptions={
	ConversionOptions->{mathMLConversionOptions},
	DataAttributes->{},
	ExportType->"MathML",
	ObjectAttributes->{"role"->"fo"},
	Sequence@@$boxExportOptions
	};

$textAllAlternateExpressionExportOptions={
	AllowMathPhrase->True,
	DataAttributes->{},
	ExportType->"Text",
	ObjectAttributes->{},
	Sequence@@$boxExportOptions
	};

$docBookEquationGeneralAdditionalExportOptions=
	{CellOptions->Append[$cellExportOptions,PageWidth:>$ExportWidth],
		NotebookOptions->$notebookExportOptions};

Options@docBookEquationGeneral={
	Attributes->{docBookNameSpaceAttributeRule,
		docBookEquationVersionAttributeRule},
	Caption->None,
	Exports->
		{$mathMlXhtmlExpressionExportOptions,
			Flatten@{$pngHtmlExpressionExportOptions,
				$docBookEquationGeneralAdditionalExportOptions,
				AllowMathPhrase->True
				},
			Flatten@{(*$mathMlPdfExpressionExportOptions*)
				$epsPdfExpressionExportOptions,
				$docBookEquationGeneralAdditionalExportOptions,
				AllowMathPhrase->False
				},
			$textAllAlternateExpressionExportOptions
			},
	ObjectContainer->MediaObjectElement,
	SetIdAttribute->True,
	TitleAbbrev->Automatic
	};

Options@DocBookEquation:=Options@docBookEquationGeneral;

Options@DocBookInformalEquation:=Options@docBookEquationGeneral;

$docBookInlineEquationAdditionalExportOptions=
	{CellOptions->$cellExportOptions,
		NotebookOptions->$notebookExportOptions
		};

Options@DocBookInlineEquation:=
	DeleteCases[
		Options@docBookEquationGeneral,
		Rule[Caption,_]
		];

SetOptions[DocBookInlineEquation,
	ObjectContainer->InlineMediaObjectElement,
	Exports->
		{$mathMlXhtmlExpressionExportOptions,
			Flatten@{$pngHtmlExpressionExportOptions,
				AllowMathPhrase->True,
				$docBookInlineEquationAdditionalExportOptions
				},
			Flatten@{$mathMlPdfExpressionExportOptions,
				SVGMathMathMLCompatibility->True,AllowMathPhrase->False
				(*$epsPdfExpressionExportOptions,AllowMathPhrase->False,
				ReplaceBoundingBox->True,WriteDimensions->True,
				UseMinimumWidthDimension->False,
				UseMinimumHeightDimension->False,
				$docBookInlineEquationAdditionalExportOptions*)
				},
			$textAllAlternateExpressionExportOptions	
			}
	];

$docBookFigureGeneralAdditionalOptions={AllowMathPhrase->False};

Options@docBookFigureGeneral={
	Attributes->
		{docBookNameSpaceAttributeRule,
			docBookEquationVersionAttributeRule},
	Caption->None,
	Exports->
		{
			Fold[
				Append,
				$pngHtmlExpressionExportOptions,
				$docBookFigureGeneralAdditionalOptions
				],
			Fold[
				Append,
				$epsPdfExpressionExportOptions,
				$docBookFigureGeneralAdditionalOptions
				]
			},
	GraphicsListPart->-1,
	ObjectContainer->MediaObjectElement,
	SetIdAttribute->True,
	TitleAbbrev->Automatic
	};

Options@DocBookFigure=Options@docBookFigureGeneral;

Options@DocBookInformalFigure=Options@docBookFigureGeneral;

Options@DocBookInlineMediaObject=DeleteCases[Options@docBookFigureGeneral,
	Rule[Caption,_]];

(*option maintencance needed*)
Options@docBookTableGeneral={
	Attributes->{docBookNameSpaceAttributeRule,
	docBookEquationVersionAttributeRule},
	TitleAbbrev->Automatic,
	BoldHeadings->True,
	Caption->None,
	DocBookInlineEquationOptions->
		{Attributes->{},
			SetIdAttribute->False,
			Rule[Exports,
				Exports/.
					(Options@
						DocBookInlineEquation/.
							ruleHeadPatternObject[
(*spacing altered*)	dimOpt:UseMinimumHeightDimension|UseMinimumWidthDimension,
								_
								]->dimOpt->False(*disabled for now*))
				]
			}
	};

Options@DocBookTable=Options@docBookTableGeneral;

Options@DocBookInformalTable=Options@docBookTableGeneral;


docBookEquationGeneralKernel[id_String,expressions_DocBookEquationSequence,
	options:optionsOrNullPseudoPatternObject]:=
	Sequence@@MapIndexed[
		docBookEquationGeneralKernel[id<>"_"<>ToString[#2[[1]]],#,options]&,
			expressions];

exportObjectListKernel[id_String,expr_,boxes_,
	opts:optionsOrNullPseudoPatternObject]/;
		(AllowMathPhrase/.{opts})===True&&stringFormattableQ[boxes]:=
	textObjectElement[
		ObjectAttributes/.{opts},
		{phraseElement[
			DataAttributes/.{opts},
			{inlineEquationElement[
				{},
				{mathPhraseElement[toStringKernel[expr,boxes,opts]]}
				]}
			]}
		];

exportObjectListKernel[id_String,expr_,boxes_,
	opts:optionsOrNullPseudoPatternObject]:=
	imageObjectElement[id,
		expr,
		boxes,
		ExportType/.{opts},
		"role"/.(ObjectAttributes/.{opts}),
		ObjectAttributes/.{opts},
		DataAttributes/.{opts},
		opts
		];

GeneralDownValue@exportObjectListKernel;

exportObjectList[
	id_String,
	expr_,
	exports:exportsPatternObject,
	opts:optionsOrNullPseudoPatternObject
	]:=
	exportObjectListKernel[id,expr,toBoxes[expr,Sequence@@#],Sequence@@#,
		opts]&/@exports;

docBookEquationGeneralKernel[id_String,expr_,
	options:optionsOrNullPseudoPatternObject]:=
	(ObjectContainer/.{options})[
		Sequence@@exportObjectList[id,expr,Exports/.{options},
			Sequence@@DeleteCases[{options},_[Exports,_]]]
		];

docBookEquationGeneral[id_String,
	equationTag:equationElementNameStringsPatternObject,
	hasTitle:booleanPatternObject,
	title:xmlOrExportXmlChainOrNothingPseudoPatternObject,
	expr_,
	caption:xmlOrExportXmlChainOrNothingPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[{options=Sequence[opts,Sequence@@Options@docBookEquationGeneral]},
		Flatten@Reap[Sow[ExportDelayed[id,
			XMLElement[equationTag,
				{Sequence@@(Attributes/.{options}),
					xmlIdAttributeRule[id,options]
					},
				{titleElements[hasTitle,title,TitleAbbrev/.{options}],
					docBookEquationGeneralKernel[id,expr,options],
					captionElement@caption
					}
				],
			xmlFileType,FilterOptions[Export,options]],xmlSewingTag],
			allSewingTags][[2]]
		];

GeneralDownValue@docBookEquationGeneral;

DocBookEquation[id_String,title:
	xmlOrExportXmlChainPseudoPatternObject,expr_,opts:
	optionsOrNullPseudoPatternObject]:=Module[{options=Sequence[opts,Sequence@@
		Options@DocBookEquation]},docBookEquationGeneral[id,"equation",True,
			title,expr,Caption/.{options},options]];

GeneralDownValue@DocBookEquation;

DocBookInformalEquation[id_String,expr_,opts:optionsOrNullPseudoPatternObject]:=
	Module[{options=Sequence[opts,Sequence@@Options@
		DocBookInformalEquation]},docBookEquationGeneral[id,"informalequation",
		False,None,expr,Caption/.{options},options]];

GeneralDownValue@DocBookInformalEquation;

DocBookInlineEquation[id_String,expr_,opts:optionsOrNullPseudoPatternObject]:=
	docBookEquationGeneral[id,"inlineequation",False,None,expr,None,Sequence[
		opts,Sequence@@Options@DocBookInlineEquation]];

GeneralDownValue@DocBookInlineEquation;

(*graphics*)


exportGraphicsObjectList[
	id_String,
	graphics:graphicsOrMultipleGraphicsPatternObject,
	exports:exportsPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Function[
		imageObjectElement[
			id,
			graphics,
			ExportType/.#,
			"role"/.(ObjectAttributes/.#),
			ObjectAttributes/.#,
			DataAttributes/.#,
			Sequence@@#,
			opts
			]		
		]/@exports;

docBookFigureGeneralKernel[
	id_String,
	description:xmlPseudoPatternObject,
	graphics:graphicsOrMultipleGraphicsPatternObject,
	options:optionsOrNullPseudoPatternObject
	]:=
	(ObjectContainer/.{options})[
		Sequence@@exportGraphicsObjectList[id,graphics,Exports/.{options},
			Sequence@@DeleteCases[{options},_[Exports,_]]],
		textObjectElement@processDescriptionPart@description
		];

docBookFigureGeneral[
	id_String,
	figureTag:figureElementNameStringsPatternObject,
	hasTitle:booleanPatternObject,
	title:xmlOrExportXmlChainOrNothingPseudoPatternObject,
	description:xmlPseudoPatternObject,
	graphics:graphicsOrMultipleGraphicsPatternObject,
	caption:xmlOrExportXmlChainOrNothingPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject
	]:=
	Module[{options=Sequence[opts,Sequence@@Options@docBookFigureGeneral]},
		Flatten@
			Reap[
				Sow[
					ExportDelayed[
						id,
						XMLElement[
							figureTag,
							{
								Sequence@@(Attributes/.{options}),
								xmlIdAttributeRule[id,options]
								},
							{
								titleElements[
									hasTitle,
									title,
									TitleAbbrev/.{options}
									],
								docBookFigureGeneralKernel[
									id,
									description,
									graphics,
									options
									],
								captionElement@caption
								}
							],
						xmlFileType,
						FilterOptions[Export,options]
						],
					xmlSewingTag
					],
				allSewingTags][[2]]
		];

GeneralDownValue@docBookFigureGeneral;

DocBookFigure[id_String,title:xmlOrExportXmlChainPseudoPatternObject,
	description:xmlPseudoPatternObject,
	graphics:graphicsOrMultipleGraphicsPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[{options=Sequence[opts,Sequence@@Options@
		DocBookFigure]},docBookFigureGeneral[id,"figure",True,title,description,
		graphics,Caption/.{options},options]];

GeneralDownValue@DocBookFigure;

DocBookInformalFigure[id_String,description:xmlPseudoPatternObject,graphics:
	graphicsOrMultipleGraphicsPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=Module[{options=Sequence[opts,Sequence@@
		Options@DocBookInformalFigure]},docBookFigureGeneral[id,
		"informalfigure",False,None,description,graphics,Caption/.{options},
			options]];

GeneralDownValue@DocBookInformalFigure;

DocBookInlineMediaObject[
	id_String,
	description:xmlPseudoPatternObject,
	graphics:graphicsOrMultipleGraphicsPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Flatten@
		Reap[
			Sow[ExportDelayed[
				id,
				XMLElement[
					"inlinemediaobject",
					{
						Apply[
							Sequence,
							Attributes/.{opts}/.Options@DocBookInlineMediaObject
							],
						xmlIdAttributeRule[id,opts]
						},
					{
						Sequence@@
							Function[
								imageObjectElement[id,graphics,##,opts]
								]@@@
									ReplaceAll[
										Exports/.{opts},
										Options@DocBookInlineMediaObject
										],
						textObjectElement@processDescriptionPart@description
						}
					],
				xmlFileType,
				FilterOptions[Export,opts]
				],
			xmlSewingTag
			],
		allSewingTags
		][[2]];

GeneralDownValue@DocBookInlineMediaObject;

(*tables*)

tGroupElement[attributes:multipleNullXmlAttributePatternObject,
	xml:multipleXmlOrNothingPseudoPatternObject]:=
	XMLElement["tgroup",attributes,xml];

GeneralDownValue@tGroupElement;

tHeadElement[headRow:xmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["thead",headRow,opts];

GeneralDownValue@tHeadElement;

tBodyElement[bodyRows:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["tbody",bodyRows,opts];

GeneralDownValue@tBodyElement;

rowElement[entries:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["row",entries,opts];

GeneralDownValue@rowElement;

entryElement[entryContent:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["entry",entryContent,opts];

GeneralDownValue@rowElement;

tableEntryToXML[id_String,arg_SequenceForm,position:{__Integer},
	opts___?OptionQ
	]:=
	Sequence@@MapIndexed[tableEntryToXML[id,#,Join[position,#2],opts]&,arg];

tableEntryToXML[id_String,"",__]:="";

tableEntryToXML[id_String,arg_String/;StringQ@arg,_,opts___?OptionQ]:=
	Sequence@@StringSplit[arg,{"\n"->XMLObject["ProcessingInstruction"]["lb"]}];

tableEntryToXML[id_String,arg_,position:{1,__Integer},
	opts___?OptionQ/;(BoldHeadings/.{opts})
	]:=
	tableEntryToXML[id,StyleForm[arg,FontWeight->"Bold"],position,
		Sequence@@
			DeleteCases[{opts},ruleHeadPatternObject[BoldHeadings,__]]
		];

tableEntryToXML[id_String,arg_,position:{__Integer},opts___?OptionQ]:=
	ToXML[
		DocBookInlineEquation[id<>StringJoin@@("_"<>ToString[#1]&)/@position,
			arg,Sequence@@(DocBookInlineEquationOptions/.{opts})
			]
		];

GeneralDownValue@tableEntryToXML;

docBookTableGeneral[id_String,
	tableTag:tableElementNameStringsPatternObject,
	hasTitle:booleanPatternObject,
	title:xmlOrExportXmlChainOrNothingPseudoPatternObject,
	description:xmlPseudoPatternObject,
	tablexpr:tablePseudoPatternObject,
	caption:xmlOrExportXmlChainOrNothingPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[
		{options=Sequence[opts,Sequence@@Options@docBookTableGeneral],
			boldHeadings},
		Flatten@Reap[Sow[ExportDelayed[id,XMLElement[tableTag,
			{Sequence@@(Attributes/.{options}),
				xmlIdAttributeRule[id,options]
				},
			boldHeadings=If[(BoldHeadings/.{options})===True,True,False];
			{titleElements[hasTitle,title,TitleAbbrev/.{options}],
				textObjectElement@processDescriptionPart@description,
				Apply[
					tGroupElement[
						{"cols"->ToString@Dimensions[tablexpr][[2]]},
						{tHeadElement[#1],tBodyElement[##2]}
						]&,
					rowElement@@@
						MapIndexed[
							entryElement[
								tableEntryToXML[id,##,options],
								options
								]&,
							tablexpr,
							{2}
							]
					],
				captionElement@caption
				}
			],xmlFileType,FilterOptions[Export,options]],xmlSewingTag],
				allSewingTags][[2]]
		];

GeneralDownValue@docBookTableGeneral;

DocBookTable[id_String,title:xmlOrExportXmlChainOrNothingPseudoPatternObject,
	description:xmlPseudoPatternObject,
	tablexpr:tablePseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[{options=Sequence[opts,Sequence@@Options@DocBookTable]},
		docBookTableGeneral[id,"table",True,title,description,tablexpr,
			Caption/.{options},options]
		];

GeneralDownValue@DocBookTable;

DocBookInformalTable[id_String,description:xmlPseudoPatternObject,
	tablexpr:tablePseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[{options=Sequence[opts,Sequence@@Options@DocBookTable]},
		docBookTableGeneral[id,"informaltable",False,None,description,tablexpr,
			Caption/.{options},options]
		];

GeneralDownValue@DocBookInformalTable;


$ContextPath=old$ContextPath
Remove@old$ContextPath


End[];


EndPackage[];


(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)