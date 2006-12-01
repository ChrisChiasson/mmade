(*this file is described in the exec-math task of build.xml*)
PrependTo[$Path,
	AntProperty["mmade_xml_parent_dir"]];

$Path=Join[Ant["Project"]@getReference["mpath"]@list[],$Path];

$DisplayFunction=Identity;

(*log messages*)

$MessagePrePrint=Function[AntLog[##]];

Unprotect[Print];
Update[Print];
Clear[Print];
Print[stuff__]:=AntLog[SequenceForm[stuff]];
Protect[Print];
Update[Print];

(*check that the m files exist and then execute them
in a notebook or non-notebook environment (user chooses)*)

$MMADEBuildDirectory=AntProperty["build"];

If[FileType[$MMADEBuildDirectory]===Directory,
	Null,
	AntFail[
		StringJoin["The MMADE build directory, ",
			build,
			", has FileType ",
			ToString@$MMADEBuildDirectory
			"."
			]
		]
	]

Module[{failedmFiles,
	mFiles=Ant["Project"]@getReference["mfiles"]@list[],
	stringTrueQ=StringMatchQ[ToString@#,"True",IgnoreCase->True]&},
	failedmFiles=Pick[#,FileType/@#,None|Directory]&[mFiles];
	If[failedmFiles=!={},
		AntFail["The following mfile(s) that you wanted me "<>
			"to execute do(es) not exist: "<>
			ToString@failedmFiles]];
	If[stringTrueQ@AntProperty["usexvnc"],
		SetOptions[Developer`InstallFrontEnd,
			Developer`LaunchFlags->
				Join[{"-display :1"},
					Developer`LaunchFlags
						/.Options@
							Developer`InstallFrontEnd
					]
			]
		];
	If[stringTrueQ@AntProperty["get-mfiles"],
		If[stringTrueQ@AntProperty["usefrontend"],
			Developer`SetSystemOptions[LegacyFrontEnd->False];
				Developer`UseFrontEnd[Get[#]]&/@mFiles,
			Get/@mFiles
			]
		]
	]
(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)