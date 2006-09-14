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

Caption::usage="";

CharacterReplacements::usage="";

DataAttributes::usage="These attributes are applied to the element inside the \
<*object> element. This is usually an <imagedata> or <phrase> element.";

Declarations::usage="Declarations contains the rules for the pseudo attributes \
of the xml declaration at the beginning of the output XML document.";

DocBookEquation::usage="DocBookEquation[\"id\",\"title\",expr,opts]";

DocBookEquationSequence::usage="DocBookEquationSequence[expr1,expr2...] may be \
fed into DocBookEquation as the expression argument to create an equation \
element that has multiple mediaobject or mathphrase children."

DocBookFigure::usage="DocBookFigure[\"id\",\"title\",\"alt text\",graphics,\
opts]";

DocBookInformalFigure::usage="";

DocBookInformalEquation::usage="DocBookInformalEquation[\"id\",expr,opts]";;

DocBookInformalTable::usage="DocBookInformalTable[\"id\",\"description\",table,\
opts]";

DocBookInlineEquation::usage="DocBookInlineEquation[\"id\",expr,opts]";

DocBookInlineMediaObject::usage="";

DocBookTable::usage="DocBookTable[\"id\",\"title\",\"description\",table,opts]";

DownValueArguments::usage="";

ExportDelayed::usage="";

ExportDryRun::chtype=StringReplace[Export::chtype,"Export"->"ExportDryRun"];

ExportDryRun::usage="ExportDryRun[\"file\",expr,\"format\"] pretends to export \
expr to \"file\" in \"format\" by returning {\"file\",ExportString[expr,\
\"format\"]}.";

Exports::usage="Exports is an option for DocBookEquation and DocBookFigure \
that gives a nested list of option lists for the functions that are called \
in the process of making an equation or figure element.";

ExportType::usage="This is the type of output that will be generated for the \
expression being exported. It must be one of the types handled by Export"

General::badargs="Bad arguments were supplied to `1`. The call was as follows: \
`2`";

InlineMediaObject::usage="";

InputFileBaseName::usage="InputFileBaseName[] gives the base file name of "<>
	"$Input. This is useful for copying the source file to an export directory";

InputDirectoryName::usage="InputDirectoryName[] will search the path in an "<>
	"attempt to find the presently executing .m file";

InputFileName::usage="InputFileName[] will search $Path in an attempt to "<>
	"find the presently executing $Input file. This does not work fo "<>
	"interactive notebooks.";

MediaObject::usage="";

ObjectAttributes::usage="These attributes are applied to the <*object> element \
inside the <mediaobject> element.";

ObjectContainer::usage="";

Overwrite::usage="Overwrite is an option for CopyFile that allows it to "<>
	"safely overwrite the destination file";

PickBadArguments::usage="";

PrependDirectory::usage="PrependDirectory is an option for XMLDocument that \
allows prepension of a directory name to each id in an XML chain. This option \
provides an easy method to set the output directory of the files in the chain. \
Set the option to the path string that you would like to append to the ids."; 

SetIdAttribute::usage="";

TitleAbbrev::usage="";

ToBoxesFunction::usage="This pure function is applied to the expression to be \
exported to obtain its box form.";

ToXML::usage="";

XMLChain::usage="";

XMLDocument::badprep="A bad path of `1` was given as the PrependDirectory \
option to XMLDocument. The prepend will be skipped."

XMLDocument::usage="";

Begin["`Private`"];

$ContextPath=Fold[Insert[##,2]&,$ContextPath,Reverse@{"XML`MathML`","XML`"}];

(*patterns*)
superScriptAndSubscriptPatternObject=SuperscriptBox|SubscriptBox;

boxesPatternObject=Alternatives@@
	ToExpression/@Cases[Names["System`*"],x_/;StringMatchQ[x,___~~"Box"]];

nonRowBoxesPatternObject=Module[{x},DeleteCases[boxesPatternObject,RowBox]];

nonRowSuperscriptOrSubscriptBoxesPatternObject=DeleteCases[
	nonRowBoxesPatternObject,
	superScriptAndSubscriptPatternObject
	];

stripableBoxesPatternObject=Alternatives[TagBox];

stringFormattablePseudoPatternObject=_?stringFormattableQ;

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

optionPseudoPatternObject=((Rule|RuleDelayed)[_,_])?OptionQ

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

mathMlNameSpace="http://www.w3.org/1998/Math/MathML"

quoteCharStringPatternObject="\""|"'";

xmlIdAttribute={xmlNameSpace,"id"};

docBookEquationVersion="5.0-extension MathML-2.0 SVG-1.1";

docBookEquationVersionAttributeRule="version"->docBookEquationVersion;

docBookNameSpace="http://docbook.org/ns/docbook";

xmlnsNameSpace="http://www.w3.org/2000/xmlns/";

xmlnsNameSpaceAttribute={xmlnsNameSpace,"xmlns"};

docBookNameSpaceAttributeRule=xmlnsNameSpaceAttribute->docBookNameSpace;

symbolicMLConversionOptions=ConversionOptions->{"ElementFormatting"->None};

mathMLConversionOptions=Sequence["Formats"->{"PresentationMathML",
	"ContentMathML"},"NamespacePrefixes"->{mathMlNameSpace->"mml"}];

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

Options@PickBadArguments={DownValueParts->Sequence[1, 1],PadExpression->0}

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

defineDebugArgs@PickBadArguments;

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

(*expression to string conversion*)

docBookSuperscript[expr__String]:=RowBox[{{expr}[[1]],
	Sequence@@Function[XMLElement["superscript",{},{#}]]/@Rest@{expr}}];

defineDebugArgs@docBookSuperscript;

docBookSubscript[expr__String]:=RowBox[{{expr}[[1]],
	Sequence@@Function[XMLElement["subscript",{},{#}]]/@Rest@{expr}}];

defineDebugArgs@docBookSubscript;

Options@toString=Options@ToString;
SetOptions[toString,FormatType->InputForm];

toString[string__String,
	opts:optionsOrNullPseudoPatternObject]:=
	StringJoin[string];

stringFormattableQ[expr_]:=Module[
	{subXpr,sewingTag},
	And[
		FreeQ[ToBoxes[expr],
			DeleteCases[
				nonRowSuperscriptOrSubscriptBoxesPatternObject,
				stripableBoxesPatternObject
				]],
		Sequence@@Flatten@Reap[
			ToBoxes[expr]/.(superScriptAndSubscriptPatternObject)[subxpr__]:>
				Sow[FreeQ[{subxpr},nonRowBoxesPatternObject],sewingTag],
			sewingTag
			][[2]]
		]
	];

defineDebugArgs@stringFormattableQ;

toString[expr:stringFormattablePseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[
		{boxExpr,unwantedBoxExpr},
		boxExpr=ToBoxes[expr,
			FormatType/.{opts}/.Options@toString/.InputForm->StandardForm]//.
			unwantedBoxExpr:nonRowSuperscriptOrSubscriptBoxesPatternObject[__]:>
			unwantedBoxExpr[[1]];
		Block[
			{SuperscriptBox=docBookSuperscript,SubscriptBox=docBookSubscript},
			Sequence@@Flatten[{boxExpr/.RowBox->List}]
			]
		];
	
toString[expr_,opts:optionsOrNullPseudoPatternObject]:=
	ToString[expr,opts,Sequence@@Options@toString];

defineDebugArgs@toString;

(*the BoxesToMathML call on the greek character is needed to define
System`Convert`XMLDump`generateNumericEntityFromCharacterCode and
System`ConvertersDump`fullPathNameExport*)

BoxesToMathML["\[Beta]"];

(*escapeStringXML converts non ASCII character codes to SGML numeric
entities, AFAIK*)

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

InputFileName[]:=
	If[
		$Input=="",
		"",
		First[
			(Pick[#,FileType/@#,File]&)[
				(#<>$PathnameSeparator<>$Input&)/@
					$Path]
			]
		];

defineDebugArgs@InputFileName;

InputFileBaseName[]:=fromFileName[InputFileName[]][[2]];

defineDebugArgs@InputFileBaseName;

InputDirectoryName[]:=DirectoryName[InputFileName[]];

defineDebugArgs@InputDirectoryName;

idLast[id_String]:=Last@fromFileName@fullPathNameExport[id,"XML"];

defineDebugArgs@idLast;

(*xmlIdAttributeRule*)

Options@xmlIdAttributeRule={SetIdAttribute->True};

xmlIdAttributeRule[id_String,opts:optionsOrNullPseudoPatternObject]:=If[
	SetIdAttribute/.{opts}/.Options@xmlIdAttributeRule,xmlIdAttribute->idLast@
		id,Unevaluated[Sequence[]]];

defineDebugArgs@xmlIdAttributeRule;

fileRefAttribute[id_String]:="fileref"->idLast@id;

defineDebugArgs@fileRefAttribute;

noAttributeXmlElement[element_String,content:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=XMLElement[element,{},{content}];

defineDebugArgs@noAttributeXmlElement;

textObjectElement[attributes:multipleNullXmlAttributePatternObject,
	xml:multipleXmlOrNothingPseudoPatternObject]:=
	XMLElement["textobject",attributes,xml];

textObjectElement[alt:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=noAttributeXmlElement["textobject",alt,
	opts];

defineDebugArgs@textObjectElement;

mathPhraseElement[phrase:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["mathphrase",phrase,opts];

defineDebugArgs@mathPhraseElement;

phraseElement[attributes:multipleNullXmlAttributePatternObject,
	xml:multipleXmlOrNothingPseudoPatternObject]:=
	XMLElement["phrase",attributes,xml];

phraseElement[phrase:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["phrase",phrase,opts];

defineDebugArgs@phraseElement;

inlineEquationElement[attributes:multipleNullXmlAttributePatternObject,
	xml:multipleXmlOrNothingPseudoPatternObject]:=
	XMLElement["inlineequation",attributes,xml];

defineDebugArgs@inlineEquationElement;

altElement[alt:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["alt",alt,opts];

defineDebugArgs@altElement;

(*captionElement*)

(*the DocBook caption does not accept raw strings, so convert them to <para>s*)

processCaptionPart[captionPart_String,opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["para",captionPart,opts];

processCaptionPart[captionPart:xmlElementPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=captionPart;

captionElement[caption:exportXmlChainPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=captionElement[ToXML[caption,opts]];

captionElement[caption:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=Module[{case},noAttributeXmlElement[
		"caption",Sequence@@processCaptionPart/@{caption},opts]];

captionElement[nothing,opts:optionsOrNullPseudoPatternObject]:=Sequence[];

defineDebugArgs@captionElement;

(*titleElements*)

titleElement[title:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=noAttributeXmlElement["title",title,
		opts];

titleElement[nothing,opts:optionsOrNullPseudoPatternObject]:=Sequence[];

defineDebugArgs@titleElement;

titleabbrevElement[titleabbrev:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=noAttributeXmlElement["titleabbrev",
	titleabbrev,opts];

titleabbrevElement[nothing,opts:optionsOrNullPseudoPatternObject]:=Sequence[];

defineDebugArgs@titleabbrevElement;

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

defineDebugArgs@titleElements;

(*imageobject*)

(*the rawXML + expressionToSymbolicMathML trick overcomes a bug preventing
Mathematica from generating namespace prefixed MathML*)

rawXML[mathMl_String,opts:optionsOrNullPseudoPatternObject]:=Sequence@@
	ImportString["<llamabait>"<>mathMl<>"</llamabait>",xmlFileType,
		FilterOptions[ExportString,opts]][[2,3]];

expressionToSymbolicMathML[expr_,opts:optionsOrNullPseudoPatternObject]:=rawXML@
	StringReplace[ExpressionToMathML[expr,opts],StringExpression[Whitespace,
		"xmlns=",quoteCharStringPatternObject,mathMlNameSpace,
		quoteCharStringPatternObject]->"",1];

defineDebugArgs@expressionToSymbolicMathML;

imageDataElement[xml:sequenceXmlPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=noAttributeXmlElement["imagedata",xml,
	opts];

defineDebugArgs@imageDataElement;

(*the xmlid attribute rule was commented out because the attribute is
unneeded and because it causes id collisions if an inline equation is used more
than once, even with SetIdAttribute->False - if this functionality is needed,
then a way to propagate DocBook* command options for SetIdAttribute to
imageObjectElement must be created*)

imageObjectElement[id_String,expr_,"MathML",idExtension_String,
	imageObjectAttributes:multipleNullXmlAttributePatternObject,_List,
	opts:optionsOrNullPseudoPatternObject]:=XMLElement["imageobject",{Sequence@@
		imageObjectAttributes(*,xmlIdAttributeRule[id<>idExtension,opts]*)},
		{imageDataElement@expressionToSymbolicMathML[expr,FilterOptions[
			ExpressionToMathML,Sequence@@(ConversionOptions/.{opts}),opts]]}];

fileExtension[filetype_String]:=ToLowerCase@StringReplace[filetype,
	{"EPSTIFF"->"eps"},
	IgnoreCase->True]

defineDebugArgs@fileExtension;

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
	filetype_String,
	idExtension_String,
	imageObjectAttributes:multipleNullXmlAttributePatternObject,
	imageDataAttributes:multipleNullXmlAttributePatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[{fileName=StringJoin[id,idExtension,".",fileExtension@filetype]},
		Sow[
			ExportDelayed[
				fileName,
				Notebook[
					{Cell[
						BoxData[
							(ToBoxesFunction/.{opts})[
								expr,
								Sequence@@Rule@@@(NonAltTextOptions/.{opts})
								]
							],
						Sequence@@Rule@@@(CellOptions/.{opts})	
						]},
					Sequence@@Rule@@@(NotebookOptions/.{opts})
					],
				filetype,
				FilterOptions[Export,opts]
				],
			otherSewingTag
			];
		XMLElement["imageobject",{Sequence@@
			imageObjectAttributes(*,xmlIdAttributeRule[id<>idExtension,opts]*)},
			{XMLElement["imagedata",{Sequence@@imageDataAttributes,
				fileRefAttribute[fileName]},{}]}]];

defineDebugArgs@imageObjectElement;

InlineMediaObjectElement[imageObjects:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["inlinemediaobject",imageObjects,opts];

defineDebugArgs@InlineMediaObjectElement;

MediaObjectElement[imageObjects:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["mediaobject",imageObjects,opts];

defineDebugArgs@MediaObjectElement;

(*ToXML*)

(*This ToXML function may have a Sowing side effect.*)

ToXML[xmlChain:exportXmlChainPseudoPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=Module[{xml=Sequence@@Cases[xmlChain,
		exportXmlDelayedPseudoPatternObject][[All,2]]},Sow[#,otherSewingTag]&/@
		DeleteCases[xmlChain,exportXmlDelayedPseudoPatternObject];xml];

ToXML[xml:xmlPseudoPatternObject,opts:optionsOrNullPseudoPatternObject]:=xml;

ToXML[nothing,opts:optionsOrNullPseudoPatternObject]:=Sequence[];

defineDebugArgs@ToXML;

(*XMLChain*)

XMLChain[id:stringOrNothingPatternObject,
	xmlexpr:heldExpressionPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Flatten@Reap[Sow[ExportDelayed[id,ReleaseHold@xmlexpr,xmlFileType,
		FilterOptions[Export,opts]],xmlSewingTag],allSewingTags][[2]];

XMLChain[xmlexpr:heldExpressionPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	XMLChain[None,xmlexpr,opts];

defineDebugArgs@XMLChain;

(*xmlDeclaration*)

xmlDeclaration[declarations__?OptionQ]:=XMLObject["Declaration"][declarations];

xmlDeclaration[]=Sequence[];

defineDebugArgs@xmlDeclaration;

(*XMLDocument*)

Options@XMLDocument={Declarations->{"Version"->"1.0","Encoding"->"UTF-8"},
	PrependDirectory->False,symbolicMLConversionOptions,CharacterReplacements->{
	"\[LongEqual]"->"="(*"\:ff1d"*)(*FULL WIDTH EQUALS SIGN can't be used due 
	to FOP and XEP incompatability*),"\[Piecewise]"->"{",
	"\[InvisibleApplication]"->""(*Firefox workaround*),"\[Cross]"->"\:00D7",
	"\[Equal]"->"="}};

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

defineDebugArgs@XMLDocument;

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
			styleForm[expr,
				Sequence@@(TextStyle/.{options})
				],
			FormatType/.{options}
			]
		];

$boxExportOptions=
	{ToBoxesFunction->$ToBoxesFunction,
		NonAltTextOptions->Options@$ToBoxesFunction};

$mathMlXhtmlExpressionExportOptions={
	AllowMathPhrase->False,
	ConversionOptions->{mathMLConversionOptions},
	DataAttributes->{},
	ExportType->"MathML",
	ObjectAttributes->{"role"->"xhtml"}
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
	}

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
				]
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

(*exports is a list of option lists, therefore, # in the Function represents a
replacement list*)

exportObjectList[id_String,expr_,
	exports:exportsPatternObject]:=
	Function[
		If[
			And[
				MatchQ[expr,
					stringFormattablePseudoPatternObject
					],
				(AllowMathPhrase/.#)===True
				],
			textObjectElement[
				ObjectAttributes/.#,
				{phraseElement[
					DataAttributes/.#,
					{inlineEquationElement[
						{},
						{mathPhraseElement[
							toString[
								expr,
								Sequence@@(NonAltTextOptions/.#)
								]
							]}
						]}
					]}
				],
			imageObjectElement[id,
				expr,
				ExportType/.#,
				"role"/.(ObjectAttributes/.#),
				ObjectAttributes/.#,
				DataAttributes/.#,
				Sequence@@#
				]
			]]/@exports;

docBookEquationGeneralKernel[id_String,expr_,
	options:optionsOrNullPseudoPatternObject]:=
	Module[{exprString=toString[expr]},
		(ObjectContainer/.{options})[
			Sequence@@exportObjectList[id,expr,Exports/.{options}],
			textObjectElement@phraseElement@exprString
			]
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

defineDebugArgs@docBookEquationGeneral;

Options@DocBookEquation:=Options@docBookEquationGeneral;

DocBookEquation[id_String,title:
	xmlOrExportXmlChainPseudoPatternObject,expr_,opts:
	optionsOrNullPseudoPatternObject]:=Module[{options=Sequence[opts,Sequence@@
		Options@DocBookEquation]},docBookEquationGeneral[id,"equation",True,
			title,expr,Caption/.{options},options]];

defineDebugArgs@DocBookEquation;

Options@DocBookInformalEquation:=Options@docBookEquationGeneral;

DocBookInformalEquation[id_String,expr_,opts:optionsOrNullPseudoPatternObject]:=
	Module[{options=Sequence[opts,Sequence@@Options@
		DocBookInformalEquation]},docBookEquationGeneral[id,"informalequation",
		False,None,expr,Caption/.{options},options]];

defineDebugArgs@DocBookInformalEquation;

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
			Fold[
				Append,
				$epsPdfExpressionExportOptions,
				$docBookInlineEquationAdditionalExportOptions
				]
			}
(*		{$mathMlXhtmlExpressionExportOptions,
			Fold[
				Append,
				$pngHtmlExpressionExportOptions,
				Append[
					$docBookInlineEquationAdditionalExportOptions,
					AllowMathPhrase->True
					]
				],
			Fold[
				Append,
				$epsPdfExpressionExportOptions,
				Append[
					$docBookInlineEquationAdditionalExportOptions,
					AllowMathPhrase->False
					]
				]
			}
*)	];

DocBookInlineEquation[id_String,expr_,opts:optionsOrNullPseudoPatternObject]:=
	docBookEquationGeneral[id,"inlineequation",False,None,expr,None,Sequence[
		opts,Sequence@@Options@DocBookInlineEquation]];

defineDebugArgs@DocBookInlineEquation;

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
	description_String,
	graphics:graphicsOrMultipleGraphicsPatternObject,
	options:optionsOrNullPseudoPatternObject
	]:=
	(ObjectContainer/.{options})[
		Sequence@@exportGraphicsObjectList[id,graphics,Exports/.{options}],
		textObjectElement@phraseElement@description
		];

docBookFigureGeneral[
	id_String,
	figureTag:figureElementNameStringsPatternObject,
	hasTitle:booleanPatternObject,
	title:xmlOrExportXmlChainOrNothingPseudoPatternObject,
	description_String,
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

defineDebugArgs@docBookFigureGeneral;

Options@DocBookFigure=Options@docBookFigureGeneral;

DocBookFigure[id_String,title:xmlOrExportXmlChainPseudoPatternObject,
	description_String,graphics:graphicsOrMultipleGraphicsPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[{options=Sequence[opts,Sequence@@Options@
		DocBookFigure]},docBookFigureGeneral[id,"figure",True,title,description,
		graphics,Caption/.{options},options]];

defineDebugArgs@DocBookFigure;

Options@DocBookInformalFigure=Options@docBookFigureGeneral;

DocBookInformalFigure[id_String,description_String,graphics:
	graphicsOrMultipleGraphicsPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=Module[{options=Sequence[opts,Sequence@@
		Options@DocBookInformalFigure]},docBookFigureGeneral[id,
		"informalfigure",False,None,description,graphics,Caption/.{options},
			options]];

defineDebugArgs@DocBookInformalFigure;

Options@DocBookInlineMediaObject=DeleteCases[Options@docBookFigureGeneral,
	Rule[Caption,_]];

(*option maintencance needed*)

DocBookInlineMediaObject[id_String,description_String,graphics:
	graphicsOrMultipleGraphicsPatternObject,opts:
	optionsOrNullPseudoPatternObject]:=Flatten@Reap[Sow[ExportDelayed[id,
	XMLElement["inlinemediaobject",{Apply[Sequence,Attributes/.{opts}/.Options@
		DocBookInlineMediaObject],xmlIdAttributeRule[id,opts]},{Sequence@@
		Function[imageObjectElement[id,graphics,##,opts]]@@@ReplaceAll[Exports/.
			{opts},Options@DocBookInlineMediaObject],textObjectElement@
			phraseElement@description}],xmlFileType,FilterOptions[Export,opts]],
		xmlSewingTag],allSewingTags][[2]];

defineDebugArgs@DocBookInlineMediaObject;

(*tables*)

tGroupElement[attributes:multipleNullXmlAttributePatternObject,
	xml:multipleXmlOrNothingPseudoPatternObject]:=
	XMLElement["tgroup",attributes,xml];

defineDebugArgs@tGroupElement;

tHeadElement[headRow:xmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["thead",headRow,opts];

defineDebugArgs@tHeadElement;

tBodyElement[bodyRows:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["tbody",bodyRows,opts];

defineDebugArgs@tBodyElement;

rowElement[entries:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["row",entries,opts];

defineDebugArgs@rowElement;

entryElement[entryContent:sequenceXmlPseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	noAttributeXmlElement["entry",entryContent,opts];

defineDebugArgs@rowElement;

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
	description_String,
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
				Apply[
					tGroupElement[
						{"cols"->ToString@Dimensions[tablexpr][[2]]},
						{tHeadElement[#1],tBodyElement[##2]}
						]&,
					rowElement@@@
						MapIndexed[
							entryElement[
								Which[
									MatchQ[#,nothing],
									"",
									MatchQ[#,_String],
									#,
									True,
									ToXML@DocBookInlineEquation[
										id<>StringJoin@@
											Function["_"<>ToString[#]]/@#2,
										#,
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

defineDebugArgs@docBookTableGeneral;

Options@DocBookTable=Options@docBookTableGeneral

DocBookTable[id_String,title_String,description_String,
	tablexpr:tablePseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[{options=Sequence[opts,Sequence@@Options@DocBookTable]},
		docBookTableGeneral[id,"table",True,title,description,tablexpr,
			Caption/.{options},options]
		];

defineDebugArgs@DocBookTable;

Options@DocBookInformalTable=Options@docBookTableGeneral

DocBookInformalTable[id_String,description_String,
	tablexpr:tablePseudoPatternObject,
	opts:optionsOrNullPseudoPatternObject]:=
	Module[{options=Sequence[opts,Sequence@@Options@DocBookTable]},
		docBookTableGeneral[id,"informaltable",False,None,description,tablexpr,
			Caption/.{options},options]
		];

defineDebugArgs@DocBookInformalTable;

(*ExportDryRun*)

Options@ExportDryRun=Options@ExportString;

ExportDryRun[file_String,expr_,type_String,opts:
	optionsOrNullPseudoPatternObject]:={file,ExportString[expr,type,opts]};

(*file_ will trigger the ExportDryRun::chtype message in more cases than it
 should - such as when file is a stram and not a file name*)

ExportDryRun[file_,expr_,type_String,opts:optionsOrNullPseudoPatternObject]:=
	{Message[Export::chtype,file];file,ExportString[expr,type,opts]};

defineDebugArgs@ExportDryRun;

End[];
EndPackage[];