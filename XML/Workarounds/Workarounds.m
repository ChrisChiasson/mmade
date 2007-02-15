(* ::Package:: *)
BeginPackage["XML`Workarounds`"]


Begin["`Private`"]

rawXML[mathMl_String,opts:optionsOrNullPseudoPatternObject]:=
	Sequence@@
		ImportString[
			"<llamabait>"<>mathMl<>"</llamabait>",
			xmlFileType,
			ConversionOptions->{"NormalizeWhitespace"->False},
			FilterOptions[ImportString,opts]
			][[2,3]];

GeneralDownValue@rawXML;


End[]


EndPackage[]