(* ::Package:: *)
BeginPackage["Utilities`FileHandling`"]


FileBaseName::usage="FileBaseName[\"fileName\"] returns the name of the file "<>
	"after the last path separator.";


FromRelativePath::usage="FromRelativePath[\"relativeFileName\"] returns the "<>
	"full path of the file if it exists under $Path";

InputDirectoryName::usage="InputDirectoryName[] will search the path in an "<>
	"attempt to find the presently executing .m file.";


InputFileBaseName::usage="InputFileBaseName[] gives the base file name of "<>
	"$Input. This is useful for copying the source file to an export "<>
	"directory";


InputFileName::usage="InputFileName[] will search $Path in an attempt to "<>
	"find the presently executing $Input file. This does not work fo "<>
	"interactive notebooks.";


Overwrite::usage="Overwrite is an option for CopyFile that allows it to "<>
	"safely overwrite the destination file";


Begin["`Private`"]


If[!NameQ[#<>"*"],Get@#]&/@{"Utilities`BadArgumentHandling`"}

old$ContextPath=$ContextPath
$ContextPath=Flatten@{$ContextPath,"XML`","XML`MathML`",
	"Utilities`BadArgumentHandling`"}


toFileName[str_String]=str;

toFileName[file_FrontEnd`FileName]:=ToFileName@file;


Unprotect[CopyFile];

Update[CopyFile];

Options@CopyFile={Overwrite->True};

(*I haven't decided how to make any of my file functions that use
	the front end work when the front end and the kernel are using
	different file systems.*)

CopyFile[src:(_String|_FrontEnd`FileName),
	dest:(_String|_FrontEnd`FileName),
	Overwrite->True]:=
	With[{srcFile=toFileName@src,
			destFile=toFileName@dest},
		Switch[FileType[destFile],
			None,
			CopyFile[srcFile,destFile],
			File,
			With[{tmpFile=Close[OpenTemporary[]]},
				DeleteFile[tmpFile];
				CopyFile[srcFile,tmpFile];
				DeleteFile[destFile];
				CopyFile[tmpFile,destFile];
				DeleteFile[tmpFile]
				],
			Directory,
			Abort[]
			]
		];

Protect[CopyFile];

Update[CopyFile];


(*fromFileName is given in the function reference under ToFileName*)

fromFileName[arg_FrontEnd`FileName]:=List@@arg[[{1,2}]]/.""->Sequence[];

fromFileName[path_String]:=Module[{dir,file},(dir=Most[#];file=#[[-1]])&@
	StringSplit[path,$PathnameSeparator|"/",All];
	If[Length[dir]>0&&dir[[1]]=="",dir[[1]]=$PathnameSeparator];
	If[Length[dir]==1,dir=dir[[1]]];If[file=="",{dir},{dir,file}]];


FromRelativePath[relativeFileName_String]:=
	Check[
		If[relativeFileName==="",
			"FileName"/.
				NotebookInformation[EvaluationNotebook[]],
			First[
				(Pick[#,FileType/@#,File]&)[Prepend[
					(#<>$PathnameSeparator<>relativeFileName&)/@
						$Path,relativeFileName]]
				]
			],
		$Failed
		];

GeneralDownValue@FromRelativePath;


InputFileName[]:=FromRelativePath[$Input];

GeneralDownValue@InputFileName;


FileBaseName[arg_FrontEnd`FileName]:=arg[[2]];

FileBaseName[fileName_String]:=fromFileName[fileName][[2]];

GeneralDownValue@FileBaseName;


InputFileBaseName[]:=FileBaseName@InputFileName[];

GeneralDownValue@InputFileBaseName;


InputDirectoryName[]:=ToFileName@@Most@fromFileName@InputFileName[];

GeneralDownValue@InputDirectoryName;


$ContextPath=old$ContextPath
Remove@old$ContextPath


End[]


EndPackage[]


(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)