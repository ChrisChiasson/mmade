(* ::Package:: *)
Begin["Utilities`BadArgumentHandling`"]


DownValueParts::"usage"="Utilities`BadArgumentHandling`DownValueParts is an \
option for Utilities`BadArgumentHandling`PickBadArguments that \
gives the appropriate Part argument to select the proper DownValue of the \
function call to be debugged."


GeneralDownValue::"usage"="Utilities`BadArgumentHandling`GeneralDownValue[symbo\
l] defines a DownValue that applies for one or more arguments to symbol that \
will issue a General::badargs message for that symbol. It is useful for quickly\
 defining error messages. After the error message, it issues an Abort[]."


GeneralDebugDownValue::"usage"="Utilities`BadArgumentHandling`GeneralDebugDownV\
alue[symbol] defines a DownValue that applies for one or more arguments to symb\
ol that enters a Dialog when it is triggered."


PadExpression::"usage"="Utilities`BadArgumentHandling`PadExpression is an \
option for Utilities`BadArgumentHandling`PickBadArguments that gives the \
expression with which to pad argument lists returned when the lhs of the \
DownValue specified by Utilities`BadArgumentHandling`DownValueParts and the \
actual number of arguments do not match."


PickBadArguments::"usage"="Utilities`BadArgumentHandling`PickBadArguments[\
heldFunctionCall,opts] \
heldFunctionCall must have Head Hold that contains an expression which does \
not match any DownValues for the Symbol immediately within Hold."


Begin["`Private`"]


GeneralDownValue[symbol_Symbol]:=
	Module[{args},
		symbol[args__]:=(
			Message[General::badargs,
				symbol,
				HoldForm[symbol[args]]
				];
			Abort[]
			)]

GeneralDownValue@GeneralDownValue


GeneralDebugDownValue[symbol_Symbol]:=
	Module[{args,debugString,debugSymbol,result,rules},
		rules={debugString->"debug`"<>SymbolName[symbol]};
		AppendTo[rules,debugSymbol->ToExpression[debugString/.rules]];
		result=ReleaseHold[
				Hold[symbol[args__]:=
					Dialog[DialogProlog:>Print[debugString],
						DialogSymbols:>{debugSymbol=Hold[symbol[args]]}
						]
					]/.rules
				];
		If[MatchQ[{result},{$Failed}],Abort[],result]
		]

GeneralDownValue@GeneralDebugDownValue


(*PickBadArguments (not robust)*)
Options@PickBadArguments={DownValueParts->Sequence[1,1],PadExpression->0};

PickBadArguments[heldFunctionCall_Hold,opts___?OptionQ]:=
	Module[
		{argumentLists,downValueParts,functionHead=heldFunctionCall[[1,0]],
			options=Flatten@{opts,Options@PickBadArguments},
			padLength},
		downValueParts=DownValueParts/.{options};
		argumentLists=ReleaseHold[
			{heldFunctionCall,
				Part[Block[{DownValues},DownValues[functionHead]],
					downValueParts
					]
				}/.functionHead->List
			];
		padLength=Max[Length/@argumentLists];
		MapThread[
			If[MatchQ[##],Unevaluated[Sequence[]],{##}]&,
			PadRight[#,padLength,PadExpression/.{options}]&/@argumentLists]
		];

GeneralDownValue@PickBadArguments;


End[]


End[]

(*
MMADE, a Mathematica DocBook Exporter
The license and Copyright information for MMADE is included in rights.txt
in the XML directory.
*)