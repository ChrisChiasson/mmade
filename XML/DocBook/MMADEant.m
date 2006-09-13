(*this file is described in the exec-math task of build.xml*)
Prepend[$Path,
	AntProperty["env.MMADE_XML_PARENT_DIR"]];

<<XML`DocBook`;

$Path=Join[Ant["Project"]@getReference["mpath"]@list[],$Path];

$DisplayFunction=Identity;

Module[{
	initFiles=Ant["Project"]@getReference["initfiles"]@list[],
	stringTrueQ=StringMatchQ[ToString@#,"True",IgnoreCase->True]&},
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
		Get/@initFiles,
		Developer`UseFrontEnd[Get[#]]&/@initFiles
		]
	]