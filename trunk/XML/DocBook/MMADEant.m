(*this file is described in the exec-math task of build.xml*)
PrependTo[$Path,
	AntProperty["env.MMADE_XML_PARENT_DIR"]];

AntLog[$Path];

<<"XML`DocBook`";

$Path=Join[Ant["Project"]@getReference["mpath"]@list[],$Path];

$DisplayFunction=Identity;

Module[{failedmFiles,
	mFiles=Ant["Project"]@getReference["mfiles"]@list[],
	stringTrueQ=StringMatchQ[ToString@#,"True",IgnoreCase->True]&},
	failedmFiles=Pick[#,FileType/@#,None|Directory]&[mFiles];
	If[failedmFiles=!={},
		"The following mfile(s) that you wanted me"<>
			"to execute do(es) not exist: "<>
			ToString@failedmFiles];
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
	If[stringTrueQ@AntProperty["usefrontend"],
		Get/@mFiles,
		Developer`UseFrontEnd[Get[#]]&/@mFiles
		]
	]
