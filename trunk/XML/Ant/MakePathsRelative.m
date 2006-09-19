(*this file is described in the exec-math task of build.xml*)
makePathsRelativeFiles=
	Ant["Project"]@
		getReference["makepathsrelativefiles"]@
			list[];

pretext="src=\"";

build=StringJoin[
	Flatten[
		{#,"/"}&/@
			StringSplit[
				AntProperty["build"],
				$PathnameSeparator|"/"
				]
		]
	];

replacePath[string_String]:=
	StringReplace[string,pretext<>"file:///"<>build->pretext];

importReplaceExport[filename_String]:=
	Export[filename,replacePath[Import[filename,"Text"]],"Text"];
