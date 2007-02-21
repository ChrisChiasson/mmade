(* ::Package:: *)
BeginPackage["Utilities`ExportDryRun`"]


ExportDryRun::chtype=StringReplace[Export::chtype,"Export"->"ExportDryRun"];


ExportDryRun::usage="ExportDryRun[\"file\",expr,\"format\"] pretends to export \
expr to \"file\" in \"format\" by returning {\"file\",ExportString[expr,\
\"format\"]}.";


Begin["`Private`"]


If[!NameQ[#<>"*"],Get@#]&/@{"Utilities`BadArgumentHandling`"}

old$ContextPath=$ContextPath
$ContextPath=Flatten@{$ContextPath,"Utilities`BadArgumentHandling`"}


(*ExportDryRun*)

Options@ExportDryRun=Options@Export;

ExportDryRun[file_String,expr_,type_String,opts:
	optionsOrNullPseudoPatternObject]:={file,ExportString[expr,type,opts]};

(*file_String will trigger the ExportDryRun::chtype message in more cases than
it should - such as when file is a stream and not a file name*)

ExportDryRun[file_,expr_,type_String,opts:optionsOrNullPseudoPatternObject]:=
	{Message[Export::chtype,file];file,ExportString[expr,type,opts]};

GeneralDownValue@ExportDryRun;


$ContextPath=old$ContextPath
Remove@old$ContextPath


End[]


EndPackage[]


(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)