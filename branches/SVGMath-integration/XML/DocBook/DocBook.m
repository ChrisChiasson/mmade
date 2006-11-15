(* ::Package:: *)

BeginPackage["XML`DocBook`",{"Utilities`FilterOptions`"}];

$ExportWidth::usage="$ExportWidth specifies the width at which to line \
wrap exported expressions.";

$PrintResolution::usage="$PrintResolution gives the default resolution \
used to export graphics to raster formats destubed fir display in print.";

$ScreenResolution::usage="$ScreenResolution is the default resolution \
used to export graphics to raster formats destined for display on screen.";

$ToBoxesFunction::usage="$ToBoxesFunction[expr,opts] converts and expression \
to boxes using StyleForm nested inside ToBoxes. It allows you to control \
the output box type and the text form via its options.";

AllowMathPhrase::usage="This is an option with a boolean value (True|False) \
that controls wether the particular export will use <mathphrase> to represent \
the subclass of expressions that can be represented by pure DocBook markup.";

Caption::usage="This is an option for the DocBook* table, figure, and equation \
functions that accepts a string, XMLElement or XMLChain as a caption.";

CellOptions::usage="CellOptions is a list of options provided to the \
Cell expression that will be exported by one of the DocBook*Equation \
commands.";

CharacterReplacements::usage="This option for the XMLDocument accepts as its \
right hand side a list of rules to be used in a StringReplace on all strings in
the xmlchain argument.";

DataAttributes::usage="These attributes are applied to the element inside the \
<*object> element. This is usually an <imagedata> or <phrase> element.";

Declarations::usage="Declarations contains the rules for the pseudo attributes \
of the xml declaration at the beginning of the output XML document.";

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

DocBookInlineMediaObject::usage="DocBookInlineMediaObject[\"id\",\
\"description\",graphics,opts]";

DocBookTable::usage="DocBookTable[\"id\",\"title\",\"description\",table,opts]";

DocBookString::usage="DocBookString[str1,str2,...] displays as str1<>str2. \
it is useful for avoiding quotation marks around exported strings.";

DownValueParts::usage="DownValueParts is an option for PickBadArguments that \
gives the appropriate Part argument to select the proper DownValue of the \
function call to be debugged.";

ExportDelayed::usage="This is an intert version of Export. When one is ready \
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

XMLChain::usage="An XMLChain is a function that will Sow the XML in its \
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

(*patterns*)
containsMsPatternObject=_?(!FreeQ[#,"ms"]&);
containsMtextPatternObject=_?(!FreeQ[#,"mtext"]&);

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

tablePseudoPatternObject={{__}..};

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

expressionToSymbolicMathML[expr_,boxes_,opts:optionsOrNullPseudoPatternObject]:=
	Module[{melement},rawXML@
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
						melement:XMLElement[containsMtextPatternObject,___]:>
							reformatMtext[melement]
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
	Module[{fileName=StringJoin[id,idExtension,".",fileExtension@filetype]},
		Sow[
			ExportDelayed[
				fileName,
				graphics,
				filetype,
				FilterOptions[Export,opts]
				],
			otherSewingTag];
		XMLElement["imageobject",{Sequence@@
			imageObjectAttributes(*,xmlIdAttributeRule[id<>idExtension,opts]*)},
			{XMLElement["imagedata",{Sequence@@imageDataAttributes,
				fileRefAttribute[fileName]},{}]}]];

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
		{verticalAdjustment,
			notebook,
			fileName=StringJoin[id,idExtension,".",fileExtension@filetype]
			},
		notebook=
			Notebook[
				{Cell[
					BoxData[boxes],
					Sequence@@Rule@@@(CellOptions/.{opts})	
					]},
				Sequence@@Rule@@@(NotebookOptions/.{opts})
				];
		(*verticalAdjustment=-FrontEndExecute[
			System`GetBoundingBoxSizePacket[notebook]
			][[1,3]];*)
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
					fileRefAttribute[fileName]
					},
				{(*XMLObject["ProcessingInstruction"][
					"dbfo",
					"alignment-adjust=\""<>ToString[verticalAdjustment]<>"\""
					]*)}
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
	xmlexpr:heldExpressionPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Flatten@Reap[Sow[ExportDelayed[id,ReleaseHold@xmlexpr,xmlFileType,
		FilterOptions[Export,opts]],xmlSewingTag],allSewingTags][[2]];

XMLChain[xmlexpr:heldExpressionPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	XMLChain[None,xmlexpr,opts];

defineBadArgs@XMLChain;

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
	"\[InvisibleSpace]"->"\:200b",
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

$cellExportOptions={
	Background->None,
	CellFrameMargins->{{0,0},{0,0}},
	CellMargins->{{0,0},{0,0}},
	ShowCellBracket->False
	};

$notebookExportOptions={WindowWidth->Infinity};

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
	ImageResolution:>$ScreenResolution,
	ObjectAttributes->{"role"->"html"}
	};

$epsPdfExpressionExportOptions={
	ConversionOptions->{"IncludeSpecialFonts"->True},
	DataAttributes->{(*pdfScaleAttribute*)},
	ExportType->"EPS",
	(*ImageResolution:>$PrintResolution,*)
	ObjectAttributes->{"role"->"fo"}
	};

$mathMlPdfExpressionExportOptions={
	AllowMathPhrase->False,
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
	{Sequence@@$boxExportOptions,
		CellOptions->Append[$cellExportOptions,PageWidth:>$ExportWidth],
		NotebookOptions->$notebookExportOptions
		};

Options@docBookEquationGeneral={
	Attributes->{docBookNameSpaceAttributeRule,
		docBookEquationVersionAttributeRule},
	Caption->None,
	Exports->
		{$mathMlXhtmlExpressionExportOptions,
			Fold[
				Append,
				$pngHtmlExpressionExportOptions,
				Append[
					$docBookEquationGeneralAdditionalExportOptions,
					AllowMathPhrase->True
					]
				],
			Fold[
				Append,
				$epsPdfExpressionExportOptions,
				Append[
					$docBookEquationGeneralAdditionalExportOptions,
					AllowMathPhrase->False
					]
				],
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

exportObjectList[id_String,expr_,exports:exportsPatternObject]:=
	exportObjectListKernel[id,expr,toBoxes[expr,Sequence@@#],Sequence@@#]&/@
		exports;

docBookEquationGeneralKernel[id_String,expr_,
	options:optionsOrNullPseudoPatternObject]:=
	(ObjectContainer/.{options})[
		Sequence@@exportObjectList[id,expr,Exports/.{options}]
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
	{Sequence@@$boxExportOptions,
		AllowMathPhrase->True,
		CellOptions->$cellExportOptions,
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
			Fold[
				Append,
				$pngHtmlExpressionExportOptions,
				$docBookInlineEquationAdditionalExportOptions
				],
			(*$mathMlPdfExpressionExportOptions*)
			Fold[
				Append,
				$epsPdfExpressionExportOptions,
				$docBookInlineEquationAdditionalExportOptions
				],
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
	ObjectContainer->MediaObjectElement,
	SetIdAttribute->True,
	TitleAbbrev->Automatic
	};

exportGraphicsObjectList[
	id_String,
	graphics:graphicsOrMultipleGraphicsPatternObject,
	exports:exportsPatternObject]:=
	Function[
		imageObjectElement[
			id,
			graphics,
			ExportType/.#,
			"role"/.(ObjectAttributes/.#),
			ObjectAttributes/.#,
			DataAttributes/.#,
			Sequence@@#
			]		
		]/@exports;

docBookFigureGeneralKernel[
	id_String,
	description:xmlPseudoPatternObject,
	graphics:graphicsOrMultipleGraphicsPatternObject,
	options:optionsOrNullPseudoPatternObject
	]:=
	(ObjectContainer/.{options})[
		Sequence@@exportGraphicsObjectList[id,graphics,Exports/.{options}],
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
	DocBookInlineEquationOptions->{Attributes->{},SetIdAttribute->False}
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
		{options=Sequence[opts,Sequence@@Options@docBookTableGeneral]},
		Flatten@Reap[Sow[ExportDelayed[id,XMLElement["table",
			{Sequence@@(Attributes/.{options}),
				xmlIdAttributeRule[id,options]
				},
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
								ToXML@DocBookInlineEquation[
									id<>StringJoin@@
										Function["_"<>ToString[#]]/@#2,
									#,
									Sequence@@
										(DocBookInlineEquationOptions
											/.{options}
											)
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

(*file_ will trigger the ExportDryRun::chtype message in more cases than it
 should - such as when file is a stram and not a file name*)

ExportDryRun[file_,expr_,type_String,opts:optionsOrNullPseudoPatternObject]:=
	{Message[Export::chtype,file];file,ExportString[expr,type,opts]};

defineBadArgs@ExportDryRun;

End[];
EndPackage[];
(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)
