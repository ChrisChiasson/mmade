BeginPackage["Utilities`GhostscriptPDFExport`",
	{"Utilities`Run`Workarounds`","Utilities`FilterOptions`"}]


Begin["`Private`"]


(*the BoxesToMathML call on the greek character is needed to define
System`ConvertersDump`fullPathNameExport, and
System`ConvertersDump`exportFormatQ*)
XML`MathML`BoxesToMathML["\[Beta]"]


If[!ValueQ@OverloadExport,OverloadExport=True]


If[OverloadExport&&$VersionNumber<6,
Ghostscript`Executable::"notfound"="The Utilities`GhostscriptPDFExport` \
package can't find \
Ghostscript. The package needs Ghostscript to overload the Export function for \
PDFs. Please set Ghostscript`Executable equal to the path of your copy of \
Ghostscript. On Windows machines, this is likely to be the path to \
gswin32c.exe. On Linux machines, this is likely to be the path to gs. You may \
freely download a copy of Ghostscript from http://www.cs.wisc.edu/~ghost/. If \
you don't want to see this message on every load of the \
Utilities`GhostscriptPDFExport` \
package, put the definition of Ghostscript`Executable in your kernel's init.m \
file or set the ghostscript property in one of your MMADE Ant configuration \
files. If you do not want Utilities`GhostscriptPDFExport` to overload Export, \
set Utilities`GhostscriptPDFExport`Private`OverloadExport=False and restart \
the kernel.";


quote="\""<>#<>"\""&;


If[!ValueQ@Ghostscript`Executable,
	Ghostscript`Executable=AntProperty["ghostscript"]];


If[!StringQ[Ghostscript`Executable],
	Ghostscript`Executable=
		Which[StringMatchQ[$SystemID,___~~"Windows"~~___],
			"C:\\Program Files\\gs\\gs8.54\\bin\\gswin32c.exe",
			StringMatchQ[$SystemID,___~~"Linux"~~___],
			"/usr/bin/gs"
			]
	];


If[!StringQ@Ghostscript`Executable||FileType@Ghostscript`Executable===None,
  Message[Ghostscript`Executable::notfound]
  ];


Unprotect[System`ConvertersDump`exportFormatQ,Message,Export];
Update/@{System`ConvertersDump`exportFormatQ,Message,Export};


System`ConvertersDump`exportFormatQ["PDF"]=False;


Message[Export::"format","PDF"]=Sequence[];


Export::"gsrunf"="The Run to call Ghostscript failed with an exit status of `1`\
.";


Export[pdfPathFragment_String,expr_,"PDF",opts___?OptionQ]/;
	StringQ@Ghostscript`Executable&&FileType@Ghostscript`Executable===File:=
	Module[{pdfFile,commentEndPos,epsFile,epsList,exitStatus,llx,lly,urx,ury},
		Check[
			epsList=ImportString[
				ExportString[expr,"EPS",FilterOptions[ExportString,opts]],
				"Lines"];
			commentEndPos=First@First@Position[epsList,"%%EndComments"];
			(*{{llx, lly},{urx, ury}}=MathLink`CallFrontEnd[
				ExportPacket[expr,"PostScript",Verbose->False]
				][[2]];*)
			{llx,lly,urx,ury}=ToExpression/@Flatten@StringCases[
				Take[epsList,{1,commentEndPos}],
				StringExpression[
					"%%HiResBoundingBox: ",
					llx__," ",lly__," ",
					urx__," ",ury__]->
						{llx,lly,urx,ury}
					];
			epsList=Insert[epsList,
				Unevaluated[
					ToString@SequenceForm["<</PageSize [",urx-llx," ",ury-lly,
						"]>>setpagedevice"
						],
					ToString@SequenceForm[llx," neg ",lly," neg translate"]
					],
				commentEndPos+1
				];
			pdfFile=System`ConvertersDump`fullPathNameExport[pdfPathFragment,
				"PDF"];
			epsFile=StringReplace[pdfFile,stem__~~".pdf"->stem~~".eps",
				IgnoreCase->True];
			Export[epsFile,epsList,"Lines"];
			exitStatus=Run[Ghostscript`Executable,"-dCompatibilityLevel=1.4",
				"-q","-dSAFER","-dNOPAUSE","-dBATCH","-sDEVICE=pdfwrite",
				"-sOutputFile="<>pdfFile,"-c",".setpdfwrite","-f",epsFile
				];
			If[0===exitStatus,
				pdfFile,
				Message[Export::"gsrunf",exitStatus]
				],
			$Failed
			]
		];


Export[pdfFile_String,xpr_,"PDF",opts___?OptionQ]:=
	Message[Ghostscript`Executable::"notfound"];


Protect[System`ConvertersDump`exportFormatQ,Message,Export];
Update/@{System`ConvertersDump`exportFormatQ,Message,Export};


XML`DocBook`ExportDryRun[file_String,expr_,"PDF",opts___]:=
	{file,ExportString[expr,"EPS",opts]};

XML`DocBook`ExportDryRun[file_,expr_,"PDF",opts___]:=
	{Message[Export::"chtype",file];file,ExportString[expr,"EPS",opts]};


]


End[]


EndPackage[]



(*old eps handling code (which has more capabilities)*)
(*
epsSystem[expr_,opts___?OptionQ]:=
	Module[{commentEndPos,headerList,epsList,llx,lly,urx,ury,width,yUp,yDown,
		height,newLlx,newLly,newUrx,newUry,rht,orgWidth,orgHeight},
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
			{{width,yUp,yDown}}=getBoundingBoxSizePacket[expr,opts];
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
*)
(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)