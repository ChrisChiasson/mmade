BeginPackage["XML`MathML`Workarounds`"]


Begin["`Private`"]


old$ContextPath=$ContextPath
$ContextPath=Fold[Insert[##,2]&,$ContextPath,Reverse@{"XML`MathML`","XML`"}]



$ContextPath=old$ContextPath
Remove@old$ContextPath


End[]


EndPackage[]


(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)