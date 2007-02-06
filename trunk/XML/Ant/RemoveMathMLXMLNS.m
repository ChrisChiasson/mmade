(*this file is called from the SVGMath related task of build.xml*)
removeMathMLXMLNSFiles=
	Ant["Project"]@
		getReference["removemathmlxmlnsfiles"]@
			list[]

Export[#,
	ReplacePart[
		Import[#,"SymbolicXML",
			ConversionOptions->{"NormalizeWhitespace"->False}
			]/.
			HoldPattern[{"http://www.w3.org/2000/xmlns/","xmlns"}->
				"http://www.w3.org/1998/Math/MathML"
				]->Sequence[],
		"Encoding"->"US-ASCII"(*"UTF-8"*),
		{1,1,-1}
		],
	"XML",
	ConversionOptions->{"ElementFormatting"->None}
	]&/@removeMathMLXMLNSFiles
(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)