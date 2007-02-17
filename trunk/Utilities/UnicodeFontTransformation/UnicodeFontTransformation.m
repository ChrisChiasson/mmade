(* ::Package:: *)
BeginPackage["Utilities`UnicodeFontTransformation`"]


(*
This package contains the Mathematica font editing code the was in DocBook.m.
The code uses a program called TTX to transform Mathematica fonts into XML
files. Subsequently, Unicode points for every glyph in each file are calculated,
creating a new cmap. The last step is to edit the rest of the font data
to match the new cmap, but I didn't do this because I made SVGMath to work with
other Unicode enabled fonts. I keep this code in MMADE because I wrote it in one
day and I like it. Note that it isn't being maintained.
*)


Begin["`Private`"]


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


End[]


EndPackage[]


(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)