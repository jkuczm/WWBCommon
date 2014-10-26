(* Mathematica Test File *)

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


(* ::Section:: *)
(*SetUp*)


BeginPackage["TestEnvironment`LinkReadFullResponse`", {"MUnit`"}]


Get["WSTPUtilities`"]


command = First[$CommandLine] <> " -mathlink"


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


(* ::Subsubsection:: *)
(*No arguments*)


TestMatch[
	LinkReadFullResponse[]
	,
	HoldPattern @ LinkReadFullResponse[]
	,
	{HoldForm @ Message[
		LinkReadFullResponse::argx, HoldForm @ LinkReadFullResponse, 0
	]}
	,
	TestID -> "no args"
]


(* ::Subsection:: *)
(*One argument*)


TestMatch[
	LinkReadFullResponse[someSymbol]
	,
	HoldPattern @ LinkReadFullResponse[someSymbol]
	,
	{HoldForm @ Message[
		LinkObject::linkv,
		HoldForm @ someSymbol,
		HoldForm @ LinkReadFullResponse[someSymbol]
	]}
	,
	TestID -> "1 arg: non-LinkObject"
]


(* ::Subsection:: *)
(*Two arguments*)


With[
	{link = LinkLaunch[command]}
	,
	TestMatch[
		LinkReadFullResponse[link, someSymbol]
		,
		HoldPattern @ LinkReadFullResponse[link, someSymbol]
		,
		{HoldForm @ Message[
			LinkReadFullResponse::nonopt,
			HoldForm @ someSymbol,
			1,
			HoldForm @ LinkReadFullResponse[link, someSymbol]
		]}
		,
		TestID -> "2 args: LinkObject, non-option"
	];
	
	LinkClose[link];
]


TestMatch[
	LinkReadFullResponse[someSymbol, "WaitTime" -> 1]
	,
	HoldPattern @ LinkReadFullResponse[someSymbol, "WaitTime" -> 1]
	,
	{HoldForm @ Message[
		LinkObject::linkv,
		HoldForm @ someSymbol,
		HoldForm @ LinkReadFullResponse[someSymbol, "WaitTime" -> 1]
	]}
	,
	TestID -> "2 args: non-LinkObject, option"
]


TestMatch[
	LinkReadFullResponse[someSymbol, someSymbol2]
	,
	HoldPattern @ LinkReadFullResponse[someSymbol, someSymbol2]
	,
	{HoldForm @ Message[
		LinkObject::linkv,
		HoldForm @ someSymbol,
		HoldForm @ LinkReadFullResponse[someSymbol, someSymbol2]
	]}
	,
	TestID -> "2 args: non-LinkObject, non-option"
]


(* ::Subsection:: *)
(*Correct arguments*)


(* ::Subsubsection:: *)
(*Returned value*)


With[
	{link = LinkLaunch[command]}
	,
	LinkWrite[link, EvaluatePacket[someSymbol]];

	Test[
		LinkReadFullResponse[link]
		,
		ReturnPacket[someSymbol]
		,
		TestID -> "ReturnPacket"
	];
	
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	LinkWrite[link, EnterExpressionPacket[someSymbol]];

	Test[
		LinkReadFullResponse[link]
		,
		ReturnExpressionPacket[someSymbol]
		,
		TestID -> "ReturnExpressionPacket"
	];
	
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	LinkWrite[link, EvaluatePacket[someSymbol]];
	LinkWrite[link, EvaluatePacket[someSymbol2]];

	Test[
		LinkReadFullResponse[link]
		,
		ReturnPacket[someSymbol]
		,
		TestID -> "two return packets"
	];
	
	LinkClose[link];
]


(* ::Subsubsection:: *)
(*print and messages*)


With[
	{
		link = LinkLaunch[command],
		tmpStream = OpenWrite[]
	}
	,
	LinkWrite[
		link
		,
		Unevaluated @ EvaluatePacket[
			Print["first text"];
			Print["second text"];
		]
	];
	
	Test[
		Block[
			{$Output = {tmpStream}}
			,
			LinkReadFullResponse[link]
		]
		,
		ReturnPacket[Null]
		,
		TestID -> "Print: returned value"
	];
	
	LinkClose[link];
	Close[tmpStream];

	Test[
		Import[First[tmpStream], "String"]
		,
		"first text\nsecond text\n"
		,
		TestID -> "Print: $Output stream"
	];
]


With[
	{
		link = LinkLaunch[command],
		tmpStream = OpenWrite[]
	}
	,
	LinkWrite[
		link
		,
		Unevaluated @ EvaluatePacket[
			Message[testFunction::argx, testFunction, 5]
		]
	];
	
	Test[
		Block[
			{$Messages = {tmpStream}}
			,
			LinkReadFullResponse[link]
		]
		,
		ReturnPacket[Null]
		,
		TestID -> "Message: returned value"
	];
	
	LinkClose[link];
	Close[tmpStream];

	Test[
		Import[First[tmpStream], "String"]
		,
		"testFunction::argx: 
   testFunction called with 5 arguments; 1 argument is expected."
		,
		TestID -> "Message: $Messages stream"
	];
]


With[
	{
		link = LinkLaunch[command],
		tmpOututStream = OpenWrite[],
		tmpMessagesStream = OpenWrite[]
	}
	,
	LinkWrite[
		link
		,
		Unevaluated @ EvaluatePacket[
			Message[testFunction::argx, testFunction, 5];
			Print["printed text"];
		]
	];
	
	Test[
		Block[
			{
				$Output = {tmpOututStream},
				$Messages = {tmpMessagesStream}
			}
			,
			LinkReadFullResponse[link]
		]
		,
		ReturnPacket[Null]
		,
		TestID -> "Message and Print: returned value"
	];
	
	LinkClose[link];
	Close[tmpOututStream];
	Close[tmpMessagesStream];

	Test[
		Import[First[tmpOututStream], "String"]
		,
		"printed text\n"
		,
		TestID -> "Message and Print: $Output stream"
	];

	Test[
		Import[First[tmpMessagesStream], "String"]
		,
		"testFunction::argx: 
   testFunction called with 5 arguments; 1 argument is expected."
		,
		TestID -> "Message and Print: $Messages stream"
	];
]


(* ::Subsubsection:: *)
(*Head option*)


With[
	{link = LinkLaunch[command]}
	,
	LinkWrite[link, EvaluatePacket[someSymbol]];

	Test[
		LinkReadFullResponse[link, "Head" -> Hold]
		,
		Hold[ReturnPacket[someSymbol]]
		,
		TestID -> "Head option: ReturnPacket"
	];
	
	LinkClose[link];
]


With[
	{
		link = LinkLaunch[command],
		tmpStream = OpenWrite[]
	}
	,
	LinkWrite[
		link
		,
		Unevaluated @ EvaluatePacket[
			Print["first text"];
			Print["second text"];
		]
	];
	
	Test[
		Block[
			{$Output = {tmpStream}}
			,
			LinkReadFullResponse[link, "Head" -> Hold]
		]
		,
		Hold[ReturnPacket[Null]]
		,
		TestID -> "Head option: Print: returned value"
	];
	
	LinkClose[link];
	Close[tmpStream];

	Test[
		Import[First[tmpStream], "String"]
		,
		"first text\nsecond text\n"
		,
		TestID -> "Head option: Print: $Output stream"
	];
]


With[
	{
		link = LinkLaunch[command],
		tmpStream = OpenWrite[]
	}
	,
	LinkWrite[
		link
		,
		Unevaluated @ EvaluatePacket[
			Message[testFunction::argx, testFunction, 5]
		]
	];
	
	Test[
		Block[
			{$Messages = {tmpStream}}
			,
			LinkReadFullResponse[link, "Head" -> Hold]
		]
		,
		Hold[ReturnPacket[Null]]
		,
		TestID -> "Head option: Message: returned value"
	];
	
	LinkClose[link];
	Close[tmpStream];

	Test[
		Import[First[tmpStream], "String"]
		,
		"testFunction::argx: 
   testFunction called with 5 arguments; 1 argument is expected."
		,
		TestID -> "Head option: Message: $Messages stream"
	];
]


(* ::Subsubsection:: *)
(*WaitTime option*)


With[
	{link = LinkLaunch[command]}
	,
	Test[
		LinkReadFullResponse[link, "WaitTime" -> None]
		,
		$Failed
		,
		{HoldForm @ Message[LinkReadFullResponse::noResponse, link]}
		,
		TestID -> "WaitTime None: no return packet"
	];
	
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	LinkWrite[link, EvaluatePacket[Pause[1]; someSymbol]];

	Test[
		LinkReadFullResponse[link, "WaitTime" -> None]
		,
		$Failed
		,
		{HoldForm @ Message[LinkReadFullResponse::noResponse, link]}
		,
		TestID -> "WaitTime None: ReturnPacket after read"
	];
	
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	LinkWrite[link, EvaluatePacket[Pause[2]; someSymbol]];

	Test[
		LinkReadFullResponse[link, "WaitTime" -> 0.01]
		,
		$Failed
		,
		{HoldForm @ Message[
			LinkReadFullResponse::noResponseInTime, link, 0.01
		]}
		,
		TestID -> "WaitTime 0.01: ReturnPacket after wait time"
	];
	
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	LinkWrite[link, EvaluatePacket[someSymbol]];

	Test[
		LinkReadFullResponse[link, "WaitTime" -> 10]
		,
		ReturnPacket[someSymbol]
		,
		TestID -> "WaitTime 10: ReturnPacket in time"
	];
	
	LinkClose[link];
]


(* ::Subsubsection:: *)
(*ExpectedPacketPattern option*)


With[
	{link = LinkLaunch[command]}
	,
	LinkWrite[link, EvaluatePacket[someSymbol]];
	LinkWrite[link, EvaluatePacket[someSymbol2]];
	LinkWrite[link, EvaluatePacket[someSymbol3]];

	Test[
		LinkReadFullResponse[
			link,
			"ExpectedPacketPattern" -> ReturnPacket[someSymbol2]
		]
		,
		ReturnPacket[someSymbol2]
		,
		TestID -> "ExpectedPacketPattern option: two return packets"
	];
	
	LinkClose[link];
]


(* ::Subsubsection:: *)
(*HandlePacketDecorator option*)


Module[
	{$PacketLog = {}}
	,
	With[
		{link = LinkLaunch[command]}
		,
		LinkWrite[link, EvaluatePacket[someSymbol]];
		LinkWrite[link, EvaluatePacket[someSymbol2]];
		LinkWrite[link, EvaluatePacket[someSymbol3]];
	
		Test[
			LinkReadFullResponse[
				link,
				"ExpectedPacketPattern" -> ReturnPacket[someSymbol3],
				"HandlePacketDecorator" -> ((
					#[ReturnPacket[someSymbol2]] :=
						AppendTo[$PacketLog, "someSymbol2 returned"];
					#[ReturnPacket[someSymbol]] :=
						AppendTo[$PacketLog, "someSymbol returned"];
				)&)
			]
			,
			ReturnPacket[someSymbol3]
			,
			TestID -> "HandlePacketDecorator option: returned value"
		];
	
		LinkClose[link];
	];
	
	Test[
		$PacketLog
		,
		{
			"someSymbol returned",
			"someSymbol2 returned"
		}
		,
		TestID -> "HandlePacketDecorator option: $PacketLog"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
