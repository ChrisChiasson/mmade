BeginPackage["Utilities`GhostscriptPDFExport`",{"Utilities`Run`Workarounds`"}]


Begin["`Private`"]


(*the BoxesToMathML call on the greek character is needed to define
System`ConvertersDump`fullPathNameExport, and
System`ConvertersDump`exportFormatQ*)
XML`MathML`BoxesToMathML["\[Beta]"]


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


Export[pdfFile_String,expr_,"PDF",opts___?OptionQ]/;
	StringQ@Ghostscript`Executable&&FileType@Ghostscript`Executable===File:=
	Module[{args,epsFile,stem},
		epsFile=StringReplace[pdfFile,
			stem__~~".pdf"->stem~~".eps",IgnoreCase->True];
		Export[epsFile,epsList[expr,opts],"Lines"];
		If[0===
			Run[quote@Ghostscript`Executable,
				"-dCompatibilityLevel=1.4","-q","-dSAFER","-dNOPAUSE","-dBATCH",
				"-sDEVICE=pdfwrite","-sOutputFile="<>quote@pdfFile,"-c",
				".setpdfwrite","-f",quote@
					System`ConvertersDump`fullPathNameExport[epsFile,"EPS"]
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


End[];


EndPackage[];


(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)