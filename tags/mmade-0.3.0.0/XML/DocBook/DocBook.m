(* ::Package:: *)

BeginPackage["XML`DocBook`",{"Utilities`FilterOptions`"}];

$ExportWidth::usage="$ExportWidth specifies the width at which to line \
wrap exported expressions.";

$MultipleGraphicsExportTypes::usage="Alternative file types for which \
Mathematica can perform a direct export to an animation. Other file type \
choices export only one frame (part), given by the value of \
GraphicsListPart, in a list of graphics objects.";

$PrintResolution::usage="$PrintResolution gives the default resolution \
used to export graphics to raster formats destubed fir display in print.";

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

SVGMathCompatibility::usage="This is an option for the DocBook*Equation \
functions \
that makes MathML format exports convert <mo>(</mo>...<mo>)</mo> markup to \
<mfenced>...</mfenced> and removes <mtext> elements that containt \[NoBreak] \
or \[InvisibleSpace]. It is useful for sending MathML to SVGMath because \
SVGMath tends to draw parenthesis a bit low if the markup preceeding the \
parenthesis has a subscript. SVGMath also has problems drawing those \
aforementioned <mtext> elements.";

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

DocBookString::usage="DocBookString[str1,str2,...] displays as str1<>str2. \
it is useful for avoiding quotation marks around exported strings.";

DownValueParts::usage="DownValueParts is an option for PickBadArguments that \
gives the appropriate Part argument to select the proper DownValue of the \
function call to be debugged.";

ExportDelayed::usage="This is an inert version of Export. When one is ready \
to actually perform the Export, Replace ExportDelayed with Export. These \
ExportDelayed objects are handled differently by this package depending \
on whether they are \"XML\" or non-XML types. The type is given in the third \
argument. See ToXML and XMLChain.";

ExportDryRun::chtype=StringReplace[Export::chtype,"Export"->"ExportDryRun"];

ExportDryRun::usage="ExportDryRun[\"file\",expr,\"format\"] pretends to export \
expr to \"file\" in \"format\" by returning {\"file\",ExportString[expr,\
\"format\"]}.";

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
DocBookFigure[...,Exports->ExportsOption[DocBookFigure,\"fo\",ExportType->\"PNG\"]";

ExportType::usage="This is the type of output that will be generated for the \
expression being exported. It must be one of the types handled by Export.";

FileBaseName::usage="FileBaseName[\"fileName\"] returns the name of the file "<>
	"after the last path separator.";

FromRelativePath::usage="FromRelativePath[\"relativeFileName\"] returns the "<>
	"full path of the file if it exists under $Path";

General::badargs="Bad arguments were supplied to `1`. The call was as follows: \
`2`";

InlineMediaObjectElement::usage"InlineMediaObjectElement is the type of \
element used to contain inlineequation media.";

InputFileBaseName::usage="InputFileBaseName[] gives the base file name of "<>
	"$Input. This is useful for copying the source file to an export "<>
	"directory";

InputDirectoryName::usage="InputDirectoryName[] will search the path in an "<>
	"attempt to find the presently executing .m file.";

InputFileName::usage="InputFileName[] will search $Path in an attempt to "<>
	"find the presently executing $Input file. This does not work fo "<>
	"interactive notebooks.";

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

Overwrite::usage="Overwrite is an option for CopyFile that allows it to "<>
	"safely overwrite the destination file";

PickBadArguments::usage="PickBadArguments[heldFunctionCall,opts] \
heldFunctionCall must have Head Hold that contains an expression which does \
not match any DownValues for the Symbol immediately within Hold.";

PrependDirectory::usage="PrependDirectory is an option for XMLDocument that \
allows prepension of a directory name to each id in an XML chain. This option \
provides an easy method to set the output directory of the files in the chain. \
Set the option to the path string that you would like to append to the ids."; 

ReplaceBoundingBox::usage"This is an option for graphic Exports of the DocBook*\
Equation functions that, under the conditions mentioned in the usage message \
for WriteDimensions, where GetBoundingBoxSizePacket is used, rewrites the \
bounding box with the information from GetBoundingBoxSizePacket.";

SetIdAttribute::usage="An boolean option for the DocBook* functions that \
states whether or not to set the xml:id attribute on the generated element.";

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

UseMinimumHeightDimension::usage="This is an option for graphic Exports of the \
DocBook*Equation functions that will cause the bounding box rewriting routine \
to use the minimum height for the exported graphic at the expense of an \
incorrect baseline shift. It is useful as a sub-option of DocBookTable because \
DocBookTable uses DocBookInlineEquation for the contents of its cells, which \
should be as tightly wrapped to the contents as possible (because it currently \
can't be made very tight at all and this eliminates the most white space). It \
also causes other \"random\" problems with things dropping out of place.";

UseMinimumWidthDimension::usage="This option is the same as \
UseMinimumHeightDimension, but is for the width.";

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
by this package). An XMLChain is also the name given to a list of \
ExportDelayed functions, because that is what this function generates. \
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

$ContextPath=Fold[Insert[##,2]&,$ContextPath,Reverse@{"XML`MathML`","XML`"}];

$MultipleGraphicsExportTypes=Alternatives@"GIF";

vectorGraphicsTypes="EPS"|"PDF"|"SVG";

(*patterns*)
containsMsPatternObject=_?(!FreeQ[#,"ms"]&);
containsMtextPatternObject=_?(!FreeQ[#,"mtext"]&);
containsMoPatternObject=_?(!FreeQ[#,"mo"]&);

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

ruleOrRuleDelayedPatternObject=Rule|RuleDelayed;

allSewingTags=xmlSewingTag|otherSewingTag;

xmlFileType="XML";

nothing=""|None|Null;

stringOrNothingPatternObject=_String|nothing;

booleanPatternObject=True|False;

equationElementNameStringsPatternObject="equation"|"informalequation"|
	"inlineequation";

figureElementNameStringsPatternObject="figure"|"informalfigure";

tableElementNameStringsPatternObject="table"|"informaltable";

xmlElementPseudoPatternObject=_?(Function[SymbolicXMLQ[#,True]]);

xmlPseudoPatternObject:=_String|xmlElementPseudoPatternObject;

xmlOrNothingPseudoPatternObject=xmlPseudoPatternObject|nothing;

sequenceXmlPseudoPatternObject=xmlPseudoPatternObject..;

sequenceXmlOrNothingPseudoPatternObject=xmlOrNothingPseudoPatternObject..;

multipleXmlOrNothingPseudoPatternObject=
	{sequenceXmlOrNothingPseudoPatternObject};

sequenceNullXmlPseudoPatternObject=xmlPseudoPatternObject...;

multipleNullXmlPseudoPatternObject={sequenceNullXmlPseudoPatternObject};

optionPseudoPatternObject=((Rule|RuleDelayed)[_,_])?OptionQ;

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

mathMlNameSpace="http://www.w3.org/1998/Math/MathML";

quoteCharStringPatternObject="\""|"'";

xmlIdAttribute={xmlNameSpace,"id"};

docBookEquationVersion="5.0-extension MathML-2.0 SVG-1.1";

docBookEquationVersionAttributeRule="version"->docBookEquationVersion;

docBookNameSpace="http://docbook.org/ns/docbook";

xmlnsNameSpace="http://www.w3.org/2000/xmlns/";

xmlnsNameSpaceAttribute={xmlnsNameSpace,"xmlns"};

docBookNameSpaceAttributeRule=xmlnsNameSpaceAttribute->docBookNameSpace;

symbolicMLConversionOptions=ConversionOptions->{"ElementFormatting"->None};

(*i think:*)
(*Mathematica is incapable of generating fully correct ContentMathML*)
(*it also messes up the export of NumberForm[xpr] in annotations*)

mathMLConversionOptions=Sequence["Formats"->{"PresentationMathML"},
	"NamespacePrefixes"->{mathMlNameSpace->"mml"},
	"IncludeMarkupAnnotations"->False];

ruleHeadPatternObject=Rule|RuleDelayed;

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
If[!ValueQ[$ScreenResolution],$ScreenResolution=86];
pdfScaleAttribute="scale":>ToString@N[$ScreenResolution/$PrintResolution*100];

(*functions*)

(*argument debugging*)

defineBadArgs[symbol_Symbol]:=Module[{args},symbol[args__]:=(Message[
	General::badargs,symbol,HoldForm[symbol[args]]];Abort[])];

defineDebugArgs[symbol_Symbol]:=Module[{args,debugString,debugSymbol,result,
	rules},rules={debugString->"debug`"<>SymbolName[symbol]};
	AppendTo[rules,debugSymbol->ToExpression[debugString/.rules]];
	result=ReleaseHold[Hold[symbol[args__]:=Dialog[DialogProlog:>Print[
		debugString],DialogSymbols:>{debugSymbol=Hold[symbol[args]]}]]/.rules];
	If[MatchQ[{result},{$Failed}],Abort[],result]];

(*PickBadArguments*)

Options@PickBadArguments={DownValueParts->Sequence[1,1],PadExpression->0};

PickBadArguments[heldFunctionCall_Hold,opts___?OptionQ]:=
	Module[
		{argumentLists,downValueParts,functionHead=heldFunctionCall[[1,0]],
			options=Sequence[opts,Sequence@@Options@PickBadArguments],
			padLength},
		downValueParts=DownValueParts/.{options};
		argumentLists=ReleaseHold[
			{heldFunctionCall,
				Part[Block[{DownValues},DownValues[functionHead]],
					downValueParts
					]
				}/.functionHead->List
			];
		padLength=Max[Length/@argumentLists];
		MapThread[
			If[MatchQ[##],Unevaluated[Sequence[]],{##}]&,
			PadRight[#,padLength,PadExpression/.{options}]&/@argumentLists]
		];

defineBadArgs@PickBadArguments;

(*rule handling*)

ruleFlatUnion[opts__?OptionQ]:=
	With[{rules=Flatten@{opts}},
		Module[{encounteredLhses=Alternatives[],lhs,rule,ruleParser},
			ruleParser[rule:ruleOrRuleDelayedPatternObject[lhs_,_]]:=
				If[MatchQ[lhs,encounteredLhses],
					Identity[Sequence][],
					AppendTo[encounteredLhses,lhs];rule
					];
				ruleParser/@rules
			]
		];

defineBadArgs[ruleFlatUnion];

(*option switching by role*)

ExportsOption[
	oldExportsRhs:{__?OptionQ},
	role_String,newExportsSubOptions__?OptionQ
	]:=
	With[
		{rolePatternPositions=
			Position[oldExportsRhs,
				ruleOrRuleDelayedPatternObject[
					ObjectAttributes,
					{___,ruleOrRuleDelayedPatternObject["role",role],___}
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

defineBadArgs[ExportsOption];

Unprotect[CopyFile];

Update[CopyFile];

Options@CopyFile={Overwrite->True};

CopyFile[srcFile_String,
	destFile_String,
	Overwrite->True]:=
	Module[{tmpFile},
		Switch[FileType[destFile],
			None,
			CopyFile[srcFile,destFile],
			File,
			tmpFile=Close[OpenTemporary[]];
				DeleteFile[tmpFile];
				CopyFile[srcFile,tmpFile];
				DeleteFile[destFile];
				CopyFile[tmpFile,destFile];
				DeleteFile[tmpFile],
			Directory,
			Abort[]
			]
		];

Protect[CopyFile];

Update[CopyFile];

MakeBoxes[DocBookString[strs__String],_]:=StringJoin[strs];

(*expression to string conversion*)

removeRowBoxes[expr_]:=Module[{args},expr//.RowBox[{args__}]:>Sequence[args]];

defineBadArgs@removeRowBoxes;

docBookSuperscript[expr:rowBoxOrStringPatternObject]:=
	RowBox[{
		{expr}[[1]],
		Function[
			XMLElement["superscript",{},{StringJoin[##]}]]@@
				removeRowBoxes@Rest@{expr}
		}];

defineBadArgs@docBookSuperscript;

docBookSubscript[expr:rowBoxOrStringPatternObject]:=
	RowBox[{
		{expr}[[1]],
		Function[
			XMLElement["subscript",{},{StringJoin[##]}]]@@
				removeRowBoxes@Rest@{expr}
		}];

defineBadArgs@docBookSubscript;

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
	
defineBadArgs@stringFormattableQ;

constantStringReplacements=Sequence["\\n"->"\n"];

formatString[str_String/;AtomQ[Unevaluated[str]]&&
	(!ShowStringCharacters/.AbsoluteOptions[$FrontEnd,ShowStringCharacters])]:=
	StringReplace[str,{constantStringReplacements,"\\\""->"\"","\""->""}];

formatString[str_String]:=StringReplace[str,{constantStringReplacements}];

defineBadArgs@formatString;

removeUnwantedBoxes[boxes_]:=
	Module[{unwantedBoxExpr,symb},
		boxes//.
			unwantedBoxExpr:nonRowSuperscriptOrSubscriptBoxesPatternObject[__]:>
				unwantedBoxExpr[[1]]
		];

defineBadArgs@removeUnwantedBoxes;

unStringableBoxesQ[boxes_/;FreeQ[boxes,notBoxExpressionPatternObject]]:=False;

(*This should always give False. If it doesn't, either the toString routine
is broken, or a MakeBoxes definition that created something that isn't
a box or a string*)

unStringableBoxesQ[boxes_]:=True;

defineBadArgs@unStringableBoxesQ;

toBoxes[expr_,opts___?OptionQ]:=
	(ToBoxesFunction/.{opts})[
		expr,
		Sequence@@Rule@@@(TextOptions/.{opts})
		];

defineBadArgs@toBoxes;

toString::"usb"="Can't convert `1` into a string.";

toStringKernel[expr_,boxes_,opts:optionsOrNullPseudoPatternObject]:=
	Module[{strippedBoxes},
		strippedBoxes=removeUnwantedBoxes[boxes]/.
			str_String:>formatString@str;
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

defineBadArgs@toString;

(*the BoxesToMathML call on the greek character is needed to define
System`Convert`MathMLDump`BoxesToSMML,
System`Convert`XMLDump`generateNumericEntityFromCharacterCode and
System`ConvertersDump`fullPathNameExport*)

BoxesToMathML["\[Beta]"];

(*escapeStringXML should convert non ASCII character codes to SGML numeric
entities*)

escapeStringXML[strxpr_String]:=Apply[StringJoin,If[Or[33<=#<=127,#==10],
	FromCharacterCode[#],
	System`Convert`XMLDump`generateNumericEntityFromCharacterCode@#]&/@
		ToCharacterCode[strxpr]]

escapeStrings[expr_]:=Module[{string},expr/.string_String:>
	escapeStringXML[string]];

(*fromFileName is given in the function reference under ToFileName*)

fromFileName[path_String]:=Module[{dir,file},(dir=Most[#];file=#[[-1]])&@
	StringSplit[path,$PathnameSeparator|"/",All];
	If[Length[dir]>0&&dir[[1]]=="",dir[[1]]=$PathnameSeparator];
	If[Length[dir]==1,dir=dir[[1]]];If[file=="",{dir},{dir,file}]];

(*fullPathNameExport requires a path and a file type from $ExportFormats*)

fullPathNameExport=System`ConvertersDump`fullPathNameExport;

FromRelativePath[relativeFileName_String]:=
	Check[
		If[
			relativeFileName==="",
			"",
			First[
				(Pick[#,FileType/@#,File]&)[Prepend[
					(#<>$PathnameSeparator<>relativeFileName&)/@
						$Path,relativeFileName]]
				]
			],
		$Failed
		];

defineBadArgs@FromRelativePath;

InputFileName[]:=FromRelativePath[$Input];

defineBadArgs@InputFileName;

FileBaseName[fileName_String]:=fromFileName[fileName][[2]];

defineBadArgs@FileBaseName;

InputFileBaseName[]:=FileBaseName@InputFileName[];

defineBadArgs@InputFileBaseName;

InputDirectoryName[]:=DirectoryName[InputFileName[]];

defineBadArgs@InputDirectoryName;

idLast[id_String]:=Last@fromFileName@fullPathNameExport[id,"XML"];

defineBadArgs@idLast;

(*xmlIdAttributeRule*)

Options@xmlIdAttributeRule={SetIdAttribute->True};

xmlIdAttributeRule[id_String,opts:optionsOrNullPseudoPatternObject]:=If[
	SetIdAttribute/.{opts}/.Options@xmlIdAttributeRule,xmlIdAttribute->idLast@
		id,Unevaluated[Sequence[]]];

defineBadArgs@xmlIdAttributeRule;

fileRefAttribute[id_String]:="fileref"->idLast@id;

defineBadArgs@fileRefAttribute;

noAttributeXmlElement[element_String,content:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=XMLElement[element,{},{content}];

defineBadArgs@noAttributeXmlElement;

processDescriptionPart[descriptionPart_String,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["phrase",descriptionPart,opts];

processDescriptionPart[descriptionPart:xmlElementPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=descriptionPart;

defineBadArgs@processDescriptionPart;

textObjectElement[attributes:multipleNullXmlAttributePatternObject,
	xml:multipleXmlOrNothingPseudoPatternObject]:=
	XMLElement["textobject",attributes,xml];

textObjectElement[alt:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=noAttributeXmlElement["textobject",alt,
	opts];

defineBadArgs@textObjectElement;

mathPhraseElement[phrase:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["mathphrase",phrase,opts];

defineBadArgs@mathPhraseElement;

phraseElement[attributes:multipleNullXmlAttributePatternObject,
	xml:multipleXmlOrNothingPseudoPatternObject]:=
	XMLElement["phrase",attributes,xml];

phraseElement[phrase:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["phrase",phrase,opts];

defineBadArgs@phraseElement;

inlineEquationElement[attributes:multipleNullXmlAttributePatternObject,
	xml:multipleXmlOrNothingPseudoPatternObject]:=
	XMLElement["inlineequation",attributes,xml];

defineBadArgs@inlineEquationElement;

altElement[alt:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["alt",alt,opts];

defineBadArgs@altElement;

(*captionElement*)

(*the DocBook caption does not accept raw strings, so convert them to <para>s*)

processCaptionPart[captionPart_String,opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["para",captionPart,opts];

processCaptionPart[captionPart:xmlElementPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=captionPart;

defineBadArgs@processCaptionPart;

captionElement[caption:exportXmlChainPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=captionElement[ToXML[caption,opts]];

captionElement[caption:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=Module[{case},noAttributeXmlElement[
		"caption",Sequence@@processCaptionPart/@{caption},opts]];

captionElement[nothing,opts:optionsOrNullPseudoPatternObject]:=Sequence[];

defineBadArgs@captionElement;

(*titleElements*)

titleElement[title:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=noAttributeXmlElement["title",title,
		opts];

titleElement[nothing,opts:optionsOrNullPseudoPatternObject]:=Sequence[];

defineBadArgs@titleElement;

titleabbrevElement[titleabbrev:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=noAttributeXmlElement["titleabbrev",
	titleabbrev,opts];

titleabbrevElement[nothing,opts:optionsOrNullPseudoPatternObject]:=Sequence[];

defineBadArgs@titleabbrevElement;

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

defineBadArgs@titleElements;

(*imageobject*)

$mspaceWidth="0.5em";

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
		XMLElement[
			mtextHead/."mtext"->"mspace",
			{"width"->$mspaceWidth,attributes},
			{}
			],
		reformatMtext[XMLElement[mtextHead,{attributes},{StringDrop[str,1]}]]
		];

reformatMtext[element:XMLElement[containsMtextPatternObject,{___},{___String}]]=
	element;

defineBadArgs@reformatMtext;

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

defineBadArgs@reformatMs;

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

defineBadArgs@formatNumberFormMathMLNumber;

validateGiveNumber[HoldComplete[str_String],number_Symbol]/;
	AtomQ[Unevaluated[str]]&&SyntaxQ[str]:=
	validateGiveNumberKernel[ToExpression[str,InputForm,HoldComplete],number];

validateGiveNumber[_,number_Symbol]=False;

defineBadArgs@validateGiveNumber;

validateGiveNumberKernel[HoldComplete[numberVal:_Real|_Integer],number_Symbol]:=
	(number=numberVal;True);

validateGiveNumberKernel[_,number_Symbol]=False;

defineBadArgs@validateGiveNumberKernel;

formatNumberFormMathMLInterpretationBox[boxes_,number_?NumberQ,otherArgs___]:=
	Module[{str},boxes/.str_String:>formatNumberFormMathMLNumber[str]];

defineBadArgs@formatNumberFormMathMLInterpretationBox;

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

defineBadArgs@formatNumberFormMathMLBoxes;

(*the rawXML + expressionToSymbolicMathML trick overcomes a bug preventing
Mathematica from generating namespace prefixed MathML*)

rawXML[mathMl_String,opts:optionsOrNullPseudoPatternObject]:=
	Sequence@@
		ImportString[
			"<llamabait>"<>mathMl<>"</llamabait>",
			xmlFileType,
			ConversionOptions->{"NormalizeWhitespace"->False},
			FilterOptions[ImportString,opts]
			][[2,3]];

defineBadArgs@rawXML;

sVGMathCompatibility[
	xml:xmlElementPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject
	]/;If[(SVGMathCompatibility/.{opts})===True,True,False]:=
	Module[
		{containerElement,containerAttributes,moHead,moAttributes,pre,mid,post},
		xml//.
			{XMLElement[
				containerElement_,
				containerAttributes_,
				{pre___,
					XMLElement[
						moHead:containsMoPatternObject,
						moAttributes_,
						{"("}
						],
					mid__,
					XMLElement[
						moHead_,
						moAttributes_,
						{")"}
						],
					post___
					}
				]:>
					With[{mfenced=moHead/."mo"->"mfenced"},
						XMLElement[
							containerElement,
							containerAttributes,
							{pre,
								XMLElement[mfenced,moAttributes,{mid}],
								post
								}
							]
						],
				XMLElement[
					containsMtextPatternObject,
					_,
					{"\[NoBreak]"|"\[InvisibleSpace]"}
					]->
					Sequence[]
				}
		];

sVGMathCompatibility[xml:xmlElementPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject
	]:=xml;

defineBadArgs@sVGMathCompatibility;

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
					body_]:>
						XMLElement[aHead,{pre,"mathsize"->mid<>"pt",post},body]
		];

defineBadArgs@expressionToSymbolicMathML;

imageDataElement[xml:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=noAttributeXmlElement["imagedata",xml,
	opts];

defineBadArgs@imageDataElement;

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
	opts:optionsOrNullPseudoPatternObject]:=XMLElement["imageobject",{Sequence@@
		imageObjectAttributes(*,xmlIdAttributeRule[id<>idExtension,opts]*)},
		{imageDataElement@expressionToSymbolicMathML[expr,boxes,opts]}];

fileExtension[filetype_String]:=ToLowerCase@StringReplace[filetype,
	{"EPSTIFF"->"eps"},
	IgnoreCase->True];

defineBadArgs@fileExtension;

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

(*this next definition masks a problem where Magnification->1 added to a
Notebook sometimes gives an incorrect baseline*)

getBoundingBoxSizePacket[
	expr_Notebook,
	filetype_String/;
		!StringMatchQ[filetype,vectorGraphicsTypes,IgnoreCase->True]
	]:=
	(getBoundingBoxSizePacketThenAdjustForMagnification[expr]+
		getBoundingBoxSizePacket[expr])/2;

getBoundingBoxSizePacket[expr_Notebook]:=
	FrontEndExecute[GetBoundingBoxSizePacket[Append[expr,Magnification->1]]];

getBoundingBoxSizePacket[expr_]:=
	getBoundingBoxSizePacketThenAdjustForMagnification[expr];

defineBadArgs@getBoundingBoxSizePacket;

getBoundingBoxSizePacketThenAdjustForMagnification[expr_]:=
	FrontEndExecute[GetBoundingBoxSizePacket[expr]]/Magnification^2/.
		AbsoluteOptions[$FrontEnd,Magnification];

defineBadArgs@getBoundingBoxSizePacketThenAdjustForMagnification;

(*returns adjustments and comment end position*)
epsSystem[expr_,opts___?OptionQ]:=
	Module[{commentEndPos,headerList,epsList,llx,lly,urx,ury,width,yUp,yDown,
		height,newLlx,newLly,newUrx,newUry,rht,orgWidth,orgHeight,
		replaceBoundingBox=If[(ReplaceBoundingBox/.{opts})===True,True,False],
		useMinimumWidthDimension=
			If[(UseMinimumWidthDimension/.{opts})===True,True,False],
		useMinimumHeightDimension=
			If[(UseMinimumHeightDimension/.{opts})===True,True,False]
		},
		epsList=
			ImportString[
				ExportString[
					expr,
					"EPS",
					FilterOptions[ExportString,opts]
					],
				"Lines"];
		commentEndPos=First@First@Position[epsList,"%%EndComments"];
		headerList=Take[epsList,{1,commentEndPos+1}];
		{llx,lly,urx,ury}=ToExpression/@Flatten@StringCases[
					headerList,
					StringExpression[
						"%%HiResBoundingBox: ",
						llx__," ",lly__," ",
						urx__," ",ury__]->
							{llx,lly,urx,ury}
						];
		If[replaceBoundingBox&&!MatchQ[expr,graphicsPatternObject],
			{{width,yUp,yDown}}=getBoundingBoxSizePacket[expr];
			height=yUp+yDown;
			orgWidth=urx-llx;
			If[useMinimumWidthDimension&&width>orgWidth,width=orgWidth];
			orgHeight=ury-lly;
			If[useMinimumHeightDimension&&height>orgHeight,
				{yUp,yDown,height}={yUp,yDown,height}*orgHeight/height];
			{newLlx,newUrx,newLly,newUry}=Sequence@@@
				{Mean[{llx,urx}]+{-1,1}width/2,Mean[{lly,ury}]+{-1,1}height/2};
			headerList=
				StringReplace[
					headerList,
					{"%%BoundingBox:"~~__->
						ToString[SequenceForm@@
							BoxForm`Intercalate[
								{"%%BoundingBox:",
									Floor@newLlx,Floor@newLly,
									Ceiling@newUrx,Ceiling@newUry},
								" "
								]
							],
						"%%HiResBoundingBox:"~~__->
							ToString[SequenceForm@@
								BoxForm`Intercalate[
									{"%%HiResBoundingBox:",
										newLlx,newLly,newUrx,newUry},
									" "
									]
								],
						(ToString@SequenceForm[lly," ",ury]~~" trans"~~rht__):>
  							ToString@SequenceForm[newLly," ",newUry]~~
  								" trans"~~rht
  						}
  					],
  			width=urx-llx;
  			height=ury-lly;
  			yDown=0;
			{newLlx,newLly,newUrx,newUry}={llx,lly,urx,ury}
			];
		{{newLlx,newLly,newUrx,newUry},
			{width,height,yDown},
			commentEndPos,
			Join[headerList,Take[epsList,{commentEndPos+2,-1}]]}
		];

defineBadArgs@epsSystem;

epsList[expr_,opts___?OptionQ]:=
	Module[{epsList,commentEndPos,llx,lly,urx,ury,width,height,yDown},
		{{llx,lly,urx,ury},{width,height,yDown},commentEndPos,epsList}=
			epsSystem[expr,opts];
		Fold[Insert[#1,#2,commentEndPos+1]&,
			epsList,
			ToString/@{SequenceForm[llx," neg ",lly," neg translate"],
				SequenceForm[
					"<</PageSize [",width," ",height,"]>>setpagedevice"
					]
				}
			]
		];

defineBadArgs@epsList;

epsBounds[expr_,opts___?OptionQ]:=
	ToExpression/@epsSystem[expr,opts][[2]];

defineBadArgs@epsBounds;

prepareBaselineAdjustment[baseToBottom_?NumberQ,idExtension_String]:=
	prepareBaselineAdjustment[ToString[-baseToBottom],idExtension];

prepareBaselineAdjustment[negativeBaseToBottom_String,"html"|"xhtml"]/;
	!DigitQ[StringTake[negativeBaseToBottom,-1]]:=
	negativeBaseToBottom<>"0";

prepareBaselineAdjustment[negativeBaseToBottom_String,_]:=negativeBaseToBottom;

defineBadArgs@prepareBaselineAdjustment;

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
		{contentHeight,contentWidth,baseToBottom,notebook,
			fileName=StringJoin[id,idExtension,".",fileExtension@filetype],
			writeDimensions=If[(WriteDimensions/.{opts})===True,True,False],
			vectorGraphicsType=
				StringMatchQ[filetype,
					vectorGraphicsTypes,
					IgnoreCase->True
					]
			},
		notebook=
			Notebook[
				{Cell[
					StripBoxes[boxes],
					Sequence@@Rule@@@(CellOptions/.{opts})	
					]},
				Sequence@@Rule@@@(NotebookOptions/.{opts})
				];
		(*points are the units after these conversions*)
		If[writeDimensions,
			{contentWidth,contentHeight,baseToBottom}=
				If[vectorGraphicsType||MatchQ[expr,graphicsPatternObject],
					epsBounds[
						notebook,
						ReleaseHold[Hold[opts]/.
							ruleOrRuleDelayedPatternObject[
								"IncludeSpecialFonts",_]->
									"IncludeSpecialFonts"->False
							]
						],
					First@getBoundingBoxSizePacket[notebook,filetype]
					]
			];
		Sow[
			ExportDelayed[
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

defineBadArgs@imageObjectElement;

InlineMediaObjectElement[imageObjects:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["inlinemediaobject",imageObjects,opts];

defineBadArgs@InlineMediaObjectElement;

MediaObjectElement[imageObjects:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["mediaobject",imageObjects,opts];

defineBadArgs@MediaObjectElement;

(*ToXML*)

(*This ToXML function may have a Sowing side effect.*)

ToXML[xmlChain:exportXmlChainPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=Module[{xml=Sequence@@Cases[xmlChain,
		exportXmlDelayedPseudoPatternObject][[All,2]]},Sow[#,otherSewingTag]&/@
		DeleteCases[xmlChain,exportXmlDelayedPseudoPatternObject];xml];

ToXML[xml:xmlPseudoPatternObject,opts:optionsOrNullPseudoPatternObject]:=xml;

ToXML[nothing,opts:optionsOrNullPseudoPatternObject]:=Sequence[];

defineBadArgs@ToXML;

(*XMLChain*)

XMLChain[id:stringOrNothingPatternObject,
	xmlexpr:xmlElementPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Flatten@Reap[Sow[ExportDelayed[id,xmlexpr,xmlFileType,
		FilterOptions[Export,opts]],xmlSewingTag],allSewingTags][[2]];

XMLChain[xmlexpr:xmlElementPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	XMLChain[None,xmlexpr,opts];

defineBadArgs@XMLChain;

Attributes@XMLChain={HoldAll};

(*xmlDeclaration*)

xmlDeclaration[declarations__?OptionQ]:=XMLObject["Declaration"][declarations];

xmlDeclaration[]=Sequence[];

defineBadArgs@xmlDeclaration;

(*XMLDocument*)

Options@XMLDocument=
	{Declarations->
		{"Version"->"1.0",
			"Encoding"->"US-ASCII"(*"UTF-8"*)
			},
	PrependDirectory->False,
	symbolicMLConversionOptions,
	CharacterReplacements->{
	"\[LongEqual]"->"="(*"\:ff1d"*)(*FULL WIDTH EQUALS SIGN can't be used due 
	to FOP and XEP incompatability*),"\[Piecewise]"->"{",
	"\[InvisibleApplication]"->""(*Firefox workaround*),"\[Cross]"->"\[Times]",
	"\[Equal]"->"=","\[Rule]"->"\[RightArrow]",
	"\[InvisibleSpace]"->(*"\:200b"*)"",
	"\[LeftBracketingBar]"|"\[RightBracketingBar]"|"\[VerticalSeparator]"->"|"}
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

defineBadArgs@XMLDocument;

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

defineBadArgs@$ToBoxesFunction;

$boxExportOptions=
	{ToBoxesFunction->$ToBoxesFunction,
		TextOptions->Options@$ToBoxesFunction};

$cellExportOptions={
	Background->None,
	CellFrameMargins->{{0,0},{0,0}},
	CellMargins->{{0,0},{0,0}},
	ShowCellBracket->False
	};

$notebookExportOptions={WindowWidth->Infinity};

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

defineBadArgs@exportObjectListKernel;

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

defineBadArgs@docBookEquationGeneral;

Options@DocBookEquation:=Options@docBookEquationGeneral;

DocBookEquation[id_String,title:
	xmlOrExportXmlChainPseudoPatternObject,expr_,opts:
	optionsOrNullPseudoPatternObject]:=Module[{options=Sequence[opts,Sequence@@
		Options@DocBookEquation]},docBookEquationGeneral[id,"equation",True,
			title,expr,Caption/.{options},options]];

defineBadArgs@DocBookEquation;

Options@DocBookInformalEquation:=Options@docBookEquationGeneral;

DocBookInformalEquation[id_String,expr_,opts:optionsOrNullPseudoPatternObject]:=
	Module[{options=Sequence[opts,Sequence@@Options@
		DocBookInformalEquation]},docBookEquationGeneral[id,"informalequation",
		False,None,expr,Caption/.{options},options]];

defineBadArgs@DocBookInformalEquation;

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
				SVGMathCompatibility->True,AllowMathPhrase->False
				(*$epsPdfExpressionExportOptions,AllowMathPhrase->False,
				ReplaceBoundingBox->True,WriteDimensions->True,
				UseMinimumWidthDimension->False,
				UseMinimumHeightDimension->False,
				$docBookInlineEquationAdditionalExportOptions*)
				},
			$textAllAlternateExpressionExportOptions	
			}
	];

DocBookInlineEquation[id_String,expr_,opts:optionsOrNullPseudoPatternObject]:=
	docBookEquationGeneral[id,"inlineequation",False,None,expr,None,Sequence[
		opts,Sequence@@Options@DocBookInlineEquation]];

defineBadArgs@DocBookInlineEquation;

(*graphics*)

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

defineBadArgs@docBookFigureGeneral;

Options@DocBookFigure=Options@docBookFigureGeneral;

DocBookFigure[id_String,title:xmlOrExportXmlChainPseudoPatternObject,
	description:xmlPseudoPatternObject,
	graphics:graphicsOrMultipleGraphicsPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[{options=Sequence[opts,Sequence@@Options@
		DocBookFigure]},docBookFigureGeneral[id,"figure",True,title,description,
		graphics,Caption/.{options},options]];

defineBadArgs@DocBookFigure;

Options@DocBookInformalFigure=Options@docBookFigureGeneral;

DocBookInformalFigure[id_String,description:xmlPseudoPatternObject,graphics:
	graphicsOrMultipleGraphicsPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=Module[{options=Sequence[opts,Sequence@@
		Options@DocBookInformalFigure]},docBookFigureGeneral[id,
		"informalfigure",False,None,description,graphics,Caption/.{options},
			options]];

defineBadArgs@DocBookInformalFigure;

Options@DocBookInlineMediaObject=DeleteCases[Options@docBookFigureGeneral,
	Rule[Caption,_]];

(*option maintencance needed*)

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

defineBadArgs@DocBookInlineMediaObject;

(*tables*)

tGroupElement[attributes:multipleNullXmlAttributePatternObject,
	xml:multipleXmlOrNothingPseudoPatternObject]:=
	XMLElement["tgroup",attributes,xml];

defineBadArgs@tGroupElement;

tHeadElement[headRow:xmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["thead",headRow,opts];

defineBadArgs@tHeadElement;

tBodyElement[bodyRows:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["tbody",bodyRows,opts];

defineBadArgs@tBodyElement;

rowElement[entries:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["row",entries,opts];

defineBadArgs@rowElement;

entryElement[entryContent:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["entry",entryContent,opts];

defineBadArgs@rowElement;

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
							ruleOrRuleDelayedPatternObject[
(*spacing altered*)	dimOpt:UseMinimumHeightDimension|UseMinimumWidthDimension,
								_
								]->dimOpt->False(*disabled for now*))
				]
			}
	};

(*change the table code so that strings don't pass through inlineequation*)

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
		Flatten@Reap[Sow[ExportDelayed[id,XMLElement["table",
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
								Which[
(*Handle as separate case in case of chnage*)
									MatchQ[#,""],
									"",
									(*MatchQ[#,_String],
									#,*)
									True,
									ToXML@DocBookInlineEquation[
										id<>StringJoin@@
											Function["_"<>ToString[#]]/@#2,
										If[(First@#2)===1&&boldHeadings,
											StyleForm[#1,FontWeight->"Bold"],
											#1
											],
										Sequence@@
											(DocBookInlineEquationOptions
												/.{options}
												)
										]
									],
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

defineBadArgs@docBookTableGeneral;

Options@DocBookTable=Options@docBookTableGeneral;

DocBookTable[id_String,title:xmlOrExportXmlChainOrNothingPseudoPatternObject,
	description:xmlPseudoPatternObject,
	tablexpr:tablePseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[{options=Sequence[opts,Sequence@@Options@DocBookTable]},
		docBookTableGeneral[id,"table",True,title,description,tablexpr,
			Caption/.{options},options]
		];

defineBadArgs@DocBookTable;

Options@DocBookInformalTable=Options@docBookTableGeneral;

DocBookInformalTable[id_String,description:xmlPseudoPatternObject,
	tablexpr:tablePseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[{options=Sequence[opts,Sequence@@Options@DocBookTable]},
		docBookTableGeneral[id,"informaltable",False,None,description,tablexpr,
			Caption/.{options},options]
		];

defineBadArgs@DocBookInformalTable;

(*ExportDryRun*)

Options@ExportDryRun=Options@ExportString;

ExportDryRun[file_String,expr_,type_String,opts:
	optionsOrNullPseudoPatternObject]:={file,ExportString[expr,type,opts]};

(*file_String will trigger the ExportDryRun::chtype message in more cases than
it should - such as when file is a stream and not a file name*)

ExportDryRun[file_,expr_,type_String,opts:optionsOrNullPseudoPatternObject]:=
	{Message[Export::chtype,file];file,ExportString[expr,type,opts]};

defineBadArgs@ExportDryRun;

(*adding options to export -- this is not the "overload" mention in 
the PDF export message*)

Unprotect[Export];
Update/@{Export};
Options@Export=
	Flatten@{Options@Export,
		UseMinimumWidthDimension->False,
		UseMinimumHeightDimension->False,
		ReplaceBoundingBox->False}
Protect[Export];
Update/@{Export};

(*executable stuff*)
quote="\""<>#<>"\""&;

If[$SystemID==="Windows",
	run=Run@quote@StringJoin@BoxForm`Intercalate[{##}," "]&,
	run=Run
	];

(*ghostscript*)

If[!ValueQ@OverloadExport,OverloadExport=True];

If[OverloadExport,
Ghostscript`Executable::notfound="The XML`DocBook` package can't find \
Ghostscript. The package needs Ghostscript to overload the Export function for \
PDFs. Please set Ghostscript`Executable equal to the path of your copy of \
Ghostscript. On Windows machines, this is likely to be the path to \
gswin32c.exe. On Linux machines, this is likely to be the path to gs. You may \
freely download a copy of Ghostscript from http://www.cs.wisc.edu/~ghost/. If \
you don't want to see this message every time you load the XML`DocBook` \
package, put the definition of Ghostscript`Executable in your kernel's init.m \
file or set the ghostscript property in one of your MMADE Ant configuration \
files. If you do not want XML`DocBook` to overload Export, set XML`DocBook`\
Private`OverloadExport=False and restart the kernel.";

If[!ValueQ@Ghostscript`Executable,
	Ghostscript`Executable=AntProperty["ghostscript"]];

If[!StringQ[Ghostscript`Executable],
	Ghostscript`Executable=
		Switch[$SystemID,
			"Windows",
			"C:\\Program Files\\gs\\gs8.54\\bin\\gswin32c.exe",
			"Linux",
			"/usr/bin/gs"
			]
	];

If[!StringQ@Ghostscript`Executable||FileType@Ghostscript`Executable===None,
  Message[Ghostscript`Executable::notfound]
  ];

Unprotect[System`ConvertersDump`exportFormatQ,Message,Export];
Update/@{System`ConvertersDump`exportFormatQ,Message,Export};

System`ConvertersDump`exportFormatQ["PDF"]=False;

Message[Export::format,_]=Sequence[];

Export[pdfFile_String,expr_,"PDF",opts___?OptionQ]/;
	StringQ@Ghostscript`Executable&&FileType@Ghostscript`Executable===File:=
	Module[{args,epsFile,stem},
		epsFile=StringReplace[pdfFile,stem__~~".pdf"->stem~~".eps"];
		Export[epsFile,epsList[expr,opts],"Lines"];
		If[0===
			run[quote@Ghostscript`Executable,
				"-dCompatibilityLevel=1.4","-q","-dSAFER","-dNOPAUSE","-dBATCH",
				"-sDEVICE=pdfwrite","-sOutputFile="<>pdfFile,"-c",
				".setpdfwrite","-f",fullPathNameExport[epsFile,"EPS"]
				],
			pdfFile,
			$Failed
			]
		];

Export[pdfFile_String,xpr_,"PDF",opts___?OptionQ]:=
	Message[Ghostscript`Executable::notfound];

Protect[System`ConvertersDump`exportFormatQ,Message,Export];
Update/@{System`ConvertersDump`exportFormatQ,Message,Export};
]

If[!ValueQ@FontTools`Executable,
	FontTools`Executable=
		Switch[
			$SystemID,
			"Windows",
			"C:\\Program Files\\TTX\\ttx.exe",
			"Linux",
			"/usr/bin/ttx"
			]
		];

If[!ValueQ@CreateUnicodeFonts,CreateUnicodeFonts=False];

If[!ValueQ@unicodeFontsDir,
	unicodeFontsDir=ToFileName[{InputDirectoryName[],"Fonts"}]
	];

If[FileType[unicodeFontsDir]=!=Directory,CreateDirectory[unicodeFontsDir]];

unicodeFontFiles=FileNames["*.ttf",unicodeFontsDir];

If[CreateUnicodeFonts&&Length@unicodeFontFiles<20,
If[FileType[FontTools`Executable]=!=File,
	Message[FontTools`Executable::notfound];Abort[]
	];

originalTtfDirectory=
	StringJoin[
		BoxForm`Intercalate[
			{$TopDirectory,"SystemFiles","Fonts","TrueType"},
			$PathnameSeparator
			]
		];

If[FileType[originalTtfDirectory]===None,
	General::ttfnf="The Mathematica true type fonts were not found.";
	Message[General::ttfnf];
	Abort[]
	];

originalTtfFontFiles=FileNames["*.ttf",originalTtfDirectory];

ttxRun=run@@
	Flatten[{
		quote@FontTools`Executable,
		"-d",
		quote@StringTake[unicodeFontsDir,{1,-2}],
		quote/@originalTtfFontFiles
		}];

If[ttxRun=!=0,
	General::fcuttx="XML`DocBook` failed to create the Unicode True Type XML "<>
		"versions of the Mathematica fonts.";
	Message[General::fcuttx];
	Abort[]
	];

unicodeXMLFontFiles=FileNames["*.ttx",unicodeFontsDir];

toUnicode[XMLObject["Document"][pre_,body_,post_]]:=
	XMLObject["Document"][
		pre/.("Encoding"->_)->("Encoding"->"US-ASCII"),
		toUnicode[body],
		post
		];

toUnicode[XMLElement["ttFont",attributes_,body_]]:=
	Block[{names},
		setFontNames/@body;XMLElement["ttFont",attributes,toUnicode/@body]
		];

setFontNames[XMLElement["name",attributes_,body_]]:=setFontNames/@body;

setFontNames[XMLElement["namerecord",attributes_?OptionQ/;
	(!FreeQ[attributes,#]&)/@And["platformID","platEncID"],nameData:{_String}]
	]:=
	Module[
		{name=Hold[names["platformID","platEncID"]]/.attributes,
		value,
		normNameData=
			StringJoin[
				BoxForm`Intercalate[Flatten[StringSplit/@nameData]," "]
				]
			},
		value=ReleaseHold@name;
		If[
			Function[name=name/.Hold->Unevaluated;#][
				Map[Hold,value,{0}]=!=name
				],
			ReleaseHold@Hold[Set][name,{value,normNameData}],
			ReleaseHold@Hold[Set][name,name=normNameData]
			]
		];

toUnicode[XMLElement["cmap",attributes_,body_]]:=
	XMLElement["cmap",attributes,toUnicode/@body];

Needs["Statistics`DescriptiveStatistics`"];

toUnicode[
	XMLElement[
		cmapFormat_/;StringMatchQ[cmapFormat,"cmap_format_"~~__],
		attributes_?OptionQ/;(!FreeQ[attributes,#]&)/@
			And["platformID","platEncID"],body_
			]
		]:=
	Block[{name=Flatten[names["platformID","platEncID"]/.attributes][[2]]},
		XMLElement[cmapFormat,attributes,toUnicode/@body]
		];

toUnicode[XMLElement["map",attributes_,body_]]:=
	XMLElement["map",toUnicode[attributes],body];

weirdFontName=FromCharacterCode[{19809,29800,13133,28526,28416}];

decimalUnicodeCharacterNumber[charNum_?NumberQ,name_String]:=
	Module[{asciiChar,candidateUnicode,char,entity,modName},
		modName=
			StringReplace[name,
				{"Mono"|"-Bold"->"",weirdFontName->"Mathematica3"}
				];
		char=FromCharacterCode[charNum,modName];
		entity=System`Convert`XMLDump`determineEntityExportFunction[
			{char},"US-ASCII"][char];
		asciiChar=FromCharacterCode[charNum,"ASCII"];
		If[asciiChar===char||StringLength[entity]<4,
			charNum,
			Block[{candidateUnicode=StringTake[entity,{3,-2}]},
				If[DigitQ[candidateUnicode],
					ToExpression@candidateUnicode,
					charNum					
					]
				]
			]
		];

defineDebugArgs@decimalUnicodeCharacterNumber;

hexUnicodeCharacterNumberString[charNum_?NumberQ,name_String]:=StringJoin@
	Prepend[
		StringSplit[
			Check[
				Cases[
					ToBoxes[
						BaseForm[decimalUnicodeCharacterNumber[charNum,name],16]
						],
					SubscriptBox[__],
					{0,Infinity}
					][[1,1]],
				tempNum={charNum,name};Abort[]
				],
			"\""
			],
		"0x"
		];

defineDebugArgs@hexUnicodeCharacterNumberString;

toUnicode[attributes_?OptionQ/;!FreeQ[attributes,"code"]]:=
	Flatten@{
		"code"->
			hexUnicodeCharacterNumberString[
				ToExpression[StringReplace["code"/.attributes,"0x"->"16^^"]],
				name
				],
		DeleteCases[attributes,"code"->_]
		};

toUnicode[arg_]=arg;

defineDebugArgs@toUnicode;

fontXMLPortOptions=ConversionOptions->
	{"NormalizeWhitespace"->False,
		"IncludeEmbeddedObjects"->True,
		"PreserveCDATASections"->True
		};

Export[#,
	toUnicode@
		Import[
			#,
			"SymbolicXML",
			fontXMLPortOptions
			],
	"XML"
	]&/@unicodeXMLFontFiles;

ttxRun=run@@
	Flatten[{
		quote@FontTools`Executable,
		quote/@unicodeXMLFontFiles
		}];

If[ttxRun=!=0,
	General::fcuttf="XML`DocBook` failed to create the Unicode True Type "<>
		"versions of the Mathematica fonts.";
	Message[General::fcuttf];
	Abort[]
	];

]

End[];
EndPackage[];
(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)