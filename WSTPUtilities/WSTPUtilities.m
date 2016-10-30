(* ::Package:: *)

(******************************************************************************

    This file is part of WWBCommon
    Copyright (C) 2014 Jakub Kuczmarski <Jakub.Kuczmarski@gmail.com>

    WWBCommon is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    WWBCommon is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

******************************************************************************)


BeginPackage["WSTPUtilities`"]


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Usage messages*)


(* ::Subsection:: *)
(*Public symbols usage*)


LinkFullContextWrite::usage =
"\
LinkFullContextWrite[link, expr] \
writes given expression to given link preserving variables with explicitly \
used contexts."


LinkReadFullResponse::usage =
"\
LinkReadFullResponse[link] \
reads subsequent packets from given link until ReturnPacket or \
ReturnExpressionPacket is encountered. TextPackets are written to $Output and \
ReturnPacket or ReturnExpressionPacket is returned."


LinkDelegateEvaluation::usage =
"\
LinkDelegateEvaluation[link, expr] \
sends given expression to given link for evaluation and returns result. All \
messages and expressions printed during evaluation are printed in evaluator \
calling this function."


LinkDelegateDefinition::usage =
"\
LinkDelegateDefinition[link, sym] \
sends definition of given symbol sym to given link and returns value returned \
by evaluation of definition expression. All messages and expressions printed \
during evaluation are printed in evaluator calling this function."


(* ::Subsection:: *)
(*Private symbols usage*)
	

Begin["`Private`"]


ClearAll["`*"]


wstpUtilitiesThrowTag::usage =
"\
wstpUtilitiesThrowTag \
Throw tag used internally by WSTPUtilities package."


linkObjArgOptionsDecorator::usage =
"\
linkObjArgOptionsDecorator[sym] \
assigns, to given symbol sym, definitions printing messages when arguments \
passed to sym are different than sequence: LinkObject, arbitrary argument, \
options."


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*linkObjArgOptionsDecorator*)


linkObjArgOptionsDecorator[sym_] := (
	sym[link_LinkObject, expr_, args___] := "nothing" /;
		Message[sym::nonopt,
			HoldForm @@ Select[{args}, !OptionQ[#]&, 1],
			2,
			HoldForm[sym[link, expr, args]]
		];
	
	sym[nonLink:Except[_LinkObject], args__] := "nothing" /;
		Message[LinkObject::linkv,
			HoldForm[nonLink], HoldForm[sym[nonLink, args]]
		];
	
	sym[_] := "nothing" /; Message[sym::argrx, HoldForm[sym], 1, 2];
	
	sym[] := "nothing" /; Message[sym::argrx, HoldForm[sym], 0, 2];
)


(* ::Subsection:: *)
(*LinkFullContextWrite*)


SetAttributes[LinkFullContextWrite, HoldAllComplete]


LinkFullContextWrite[args___] :=
	Block[
		{$ContextPath = {}, $Context = "WSTPUtilities`Private`Dummy`"}
		,
		LinkWrite[args]
	]


(* ::Subsection:: *)
(*LinkReadFullResponse*)


LinkReadFullResponse::noResponse = "No response from link `1`."
LinkReadFullResponse::noResponseInTime =
"No response from link `1` in `2` seconds."


Options[LinkReadFullResponse] = {
	"Head" -> None,
	"WaitTime" -> Infinity,
	"ExpectedPacketPattern" -> Automatic,
	"HandlePacketDecorator" -> Identity
}


LinkReadFullResponse[link_LinkObject, OptionsPattern[]] :=
	Module[
		{
			$channel = $Output,
			head = OptionValue["Head"],
			waitTime = OptionValue["WaitTime"],
			expectedPacketPattern = OptionValue["ExpectedPacketPattern"],
			printPacketPattern,
			messagePacketPattern,
			linkRead,
			linkReadyQ,
			messageNoResponse,
			handlePacket
		}
		,	
		If[head === None,
			If[expectedPacketPattern === Automatic,
				expectedPacketPattern =
					_ReturnPacket | _ReturnExpressionPacket;
			];
			printPacketPattern = _TextPacket;
			messagePacketPattern = _MessagePacket;
			linkRead = LinkRead[link]&;
		(* else *),
			If[expectedPacketPattern === Automatic,
				expectedPacketPattern =
					head[_ReturnPacket | _ReturnExpressionPacket];
			];
			printPacketPattern = head[_TextPacket];
			messagePacketPattern = head[_MessagePacket];
			linkRead = LinkRead[link, head]&;
		];
		
		handlePacket[packet:expectedPacketPattern] :=
			Throw[packet, wstpUtilitiesThrowTag];
		handlePacket[packet:printPacketPattern] := (
			Cases[
				packet,
				TextPacket[text_] :> WriteString[$channel, text],
				{0, Infinity},
				1
			];
			$channel = $Output;
		);
		handlePacket[messagePacketPattern] := ($channel = $Messages);
		
		OptionValue["HandlePacketDecorator"][handlePacket];
		
		Catch[
			If[waitTime === Infinity,
				While[True,
					handlePacket[linkRead[]]
				];
			(* else *),
				If[waitTime === None,
					linkReadyQ = LinkReadyQ[link]&;
					messageNoResponse =
						Message[LinkReadFullResponse::noResponse, link]&;
				(* else *),
					linkReadyQ = LinkReadyQ[link, waitTime]&;
					messageNoResponse =
						Message[LinkReadFullResponse::noResponseInTime,
							link,
							waitTime
						]&;
				];
				
				While[True,
					If[linkReadyQ[],
						handlePacket[linkRead[]]
					(* else *),
						messageNoResponse[];
						Return[$Failed]
					]
				];
			];
			,
			wstpUtilitiesThrowTag
		]
	]

LinkReadFullResponse[link_LinkObject, args___] := "nothing" /;
	Message[LinkReadFullResponse::nonopt,
		HoldForm @@ Select[{args}, !OptionQ[#]&, 1],
		1,
		HoldForm[LinkReadFullResponse[link, args]]
	]

LinkReadFullResponse[nonLink:Except[_LinkObject], args___] := "nothing" /;
	Message[LinkObject::linkv,
		HoldForm[nonLink], HoldForm[LinkReadFullResponse[nonLink, args]]
	]

LinkReadFullResponse[] := "nothing" /;
	Message[LinkReadFullResponse::argx, HoldForm[LinkReadFullResponse], 0]


(* ::Subsection:: *)
(*LinkDelegateEvaluation*)


SetAttributes[LinkDelegateEvaluation, HoldRest]

Options[LinkDelegateEvaluation] =
	Append[
		FilterRules[
			Options[LinkReadFullResponse],
			Except["ExpectedPacketPattern"]
		]
		,
		"LinkWriteFunction" -> LinkFullContextWrite
	]


LinkDelegateEvaluation[link_LinkObject, expr_, opts:OptionsPattern[]] :=
	With[
		{
			head = OptionValue["Head"],
			wrapper =
				Unique["WSTPUtilities`Private`LinkDelegateEvaluationWrapper$"]
		}
		,
		OptionValue["LinkWriteFunction"][
			link,
			Unevaluated @ EvaluatePacket @ wrapper[expr]
		];
		
		LinkReadFullResponse[
			link,
			FilterRules[
				Flatten[{
					"ExpectedPacketPattern" ->
						If[head === None,
							ReturnPacket[_wrapper]
						(* else *),
							head[ReturnPacket[_wrapper]]
						]
					,
					opts
					,
					Options[LinkDelegateEvaluation]
				}]
				,
				Options[LinkReadFullResponse]
			]
		] /.
			ReturnPacket[wrapper[result_]] :> result
	]

linkObjArgOptionsDecorator[LinkDelegateEvaluation]


(* ::Subsection:: *)
(*LinkDelegateDefinition*)


SetAttributes[LinkDelegateDefinition, HoldRest]

Options[LinkDelegateDefinition] =
	Append[
		FilterRules[
			Options[LinkDelegateEvaluation],
			Except["LinkWriteFunction"]
		]
		,
		"Full" -> False
	]


LinkDelegateDefinition[link_LinkObject, sym_, opts:OptionsPattern[]] :=
	With[
		{
			definition =
				ToString[
					If[OptionValue["Full"], FullDefinition, Definition][sym],
					InputForm
				]
			,
			options =
				FilterRules[
					Flatten[{opts, Options[LinkDelegateDefinition]}],
					Options[LinkDelegateEvaluation]
				]
		}
		,
		LinkDelegateEvaluation[link, ToExpression[definition], options]
	]

linkObjArgOptionsDecorator[LinkDelegateDefinition]


End[]


(* ::Section:: *)
(*Public symbols protection*)


Protect["`*"]


EndPackage[]
