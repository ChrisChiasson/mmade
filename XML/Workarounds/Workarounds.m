(* ::Package:: *)
BeginPackage["XML`Workarounds`"]


Begin["`Private`"]

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

GeneralDownValue@rawXML;


End[]


EndPackage[]