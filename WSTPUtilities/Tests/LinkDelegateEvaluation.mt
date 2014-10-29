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


BeginPackage["TestEnvironment`LinkDelegateEvaluation`", {"MUnit`"}]


Get["WSTPUtilities`"]


command = First[$CommandLine] <> " -mathlink"


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


(* ::Subsubsection:: *)
(*No arguments*)


TestMatch[
	LinkDelegateEvaluation[]
	,
	HoldPattern @ LinkDelegateEvaluation[]
	,
	{HoldForm @ Message[
		LinkDelegateEvaluation::argrx, HoldForm @ LinkDelegateEvaluation, 0, 2
	]}
	,
	TestID -> "no args"
]


(* ::Subsection:: *)
(*One argument*)


TestMatch[
	LinkDelegateEvaluation[someSymbol]
	,
	HoldPattern @ LinkDelegateEvaluation[someSymbol]
	,
	{HoldForm @ Message[
		LinkDelegateEvaluation::argrx, HoldForm @ LinkDelegateEvaluation, 1, 2
	]}
	,
	TestID -> "1 arg"
]


(* ::Subsection:: *)
(*Two arguments*)


TestMatch[
	LinkDelegateEvaluation[someSymbol, someSymbol2]
	,
	HoldPattern @ LinkDelegateEvaluation[someSymbol, someSymbol2]
	,
	{HoldForm @ Message[
		LinkObject::linkv,
		HoldForm @ someSymbol,
		HoldForm @ LinkDelegateEvaluation[someSymbol, someSymbol2]
	]}
	,
	TestID -> "2 args: first non-LinkObject"
]


(* ::Subsection:: *)
(*Three arguments*)


With[
	{link = LinkLaunch[command]}
	,
	TestMatch[
		LinkDelegateEvaluation[link, someSymbol, someSymbol2]
		,
		HoldPattern @ LinkDelegateEvaluation[link, someSymbol, someSymbol2]
		,
		{HoldForm @ Message[
			LinkDelegateEvaluation::nonopt,
			HoldForm @ someSymbol2,
			2,
			HoldForm @ LinkDelegateEvaluation[link, someSymbol, someSymbol2]
		]}
		,
		TestID -> "3 args: first LinkObject, third non-option"
	];
	
	LinkClose[link];
]


TestMatch[
	LinkDelegateEvaluation[someSymbol, someSymbol2, "WaitTime" -> 1]
	,
	HoldPattern @
		LinkDelegateEvaluation[someSymbol, someSymbol2, "WaitTime" -> 1]
	,
	{HoldForm @ Message[
		LinkObject::linkv,
		HoldForm @ someSymbol,
		HoldForm @
			LinkDelegateEvaluation[someSymbol, someSymbol2, "WaitTime" -> 1]
	]}
	,
	TestID -> "3 args: first non-LinkObject, third option"
]


TestMatch[
	LinkDelegateEvaluation[someSymbol, someSymbol2, someSymbol3]
	,
	HoldPattern @ LinkDelegateEvaluation[someSymbol, someSymbol2, someSymbol3]
	,
	{HoldForm @ Message[
		LinkObject::linkv,
		HoldForm @ someSymbol,
		HoldForm @ LinkDelegateEvaluation[someSymbol, someSymbol2, someSymbol3]
	]}
	,
	TestID -> "3 args: first non-LinkObject, third non-option"
]


(* ::Subsection:: *)
(*Correct arguments*)


(* ::Subsubsection:: *)
(*Returned value*)


With[
	{link = LinkLaunch[command]}
	,
	
	Test[
		LinkDelegateEvaluation[link, someSymbol]
		,
		someSymbol
		,
		TestID -> "evaluation result returned"
	];
	
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	Module[
		{someSymbol = 5}
		,
		LinkFullContextWrite[
			link,
			Unevaluated @ EvaluatePacket[someSymbol = 10;]
		];
		While[LinkRead[link, Hold] =!= Hold[ReturnPacket[Null]]];
		
		Test[
			LinkDelegateEvaluation[link, someSymbol]
			,
			10
			,
			TestID -> "evaluation only in linked evaluator"
		];
		
		LinkClose[link];
	];
]


With[
	{link = LinkLaunch[command]}
	,
	LinkWrite[link, someSymbol];
	
	Test[
		LinkDelegateEvaluation[link, someSymbol2]
		,
		someSymbol2
		,
		TestID -> "return only result of evaluation of given expression"
	];
	
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	Block[
		{$ContextPath = Prepend[$ContextPath, "TestContext`"]}
		,
		Test[
			LinkDelegateEvaluation[link, Context[TestContext`someSymbol]]
			,
			"TestContext`"
			,
			TestID -> "preserve explicit context"
		];
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
	
	Test[
		Block[
			{$Output = {tmpStream}}
			,
			LinkDelegateEvaluation[
				link
				,
				Print["first text"];
				Print["second text"];
			]
		]
		,
		Null
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
	Test[
		Block[
			{$Messages = {tmpStream}}
			,
			LinkDelegateEvaluation[
				link,
				Message[testFunction::argx, testFunction, 5]
			]
		]
		,
		Null
		,
		TestID -> "Message: returned value"
	];
	
	LinkClose[link];
	Close[tmpStream];

	Test[
		Import[First[tmpStream], "String"]
		,
		"TestEnvironment`LinkDelegateEvaluation`testFunction::argx: 
   TestEnvironment`LinkDelegateEvaluation`testFunction called with 5
     arguments; 1 argument is expected."
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
	Test[
		Block[
			{
				$Output = {tmpOututStream},
				$Messages = {tmpMessagesStream}
			}
			,
			LinkDelegateEvaluation[
				link
				,
				Message[testFunction::argx, testFunction, 5];
				Print["printed text"];
			]
		]
		,
		Null
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
		"TestEnvironment`LinkDelegateEvaluation`testFunction::argx: 
   TestEnvironment`LinkDelegateEvaluation`testFunction called with 5
     arguments; 1 argument is expected."
		,
		TestID -> "Message and Print: $Messages stream"
	];
]


(* ::Subsubsection:: *)
(*Head option*)


With[
	{link = LinkLaunch[command]}
	,
	Test[
		LinkDelegateEvaluation[link, someSymbol, "Head" -> Hold]
		,
		Hold[someSymbol]
		,
		TestID -> "Head option: returned value"
	];
	
	LinkClose[link];
]


With[
	{
		link = LinkLaunch[command],
		tmpStream = OpenWrite[]
	}
	,
	Test[
		Block[
			{$Output = {tmpStream}}
			,
			LinkDelegateEvaluation[
				link
				,
				Print["first text"];
				Print["second text"];
				,
				"Head" -> Hold
			]
		]
		,
		Hold[Null]
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
	Test[
		Block[
			{$Messages = {tmpStream}}
			,
			LinkDelegateEvaluation[
				link,
				Message[testFunction::argx, testFunction, 5],
				"Head" -> Hold
			]
		]
		,
		Hold[Null]
		,
		TestID -> "Head option: Message: returned value"
	];
	
	LinkClose[link];
	Close[tmpStream];
	
	Test[
		Import[First[tmpStream], "String"]
		,
		"TestEnvironment`LinkDelegateEvaluation`testFunction::argx: 
   TestEnvironment`LinkDelegateEvaluation`testFunction called with 5
     arguments; 1 argument is expected."
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
		LinkDelegateEvaluation[
			link
			,
			Pause[1];
			someSymbol
			,
			"WaitTime" -> None
		]
		,
		$Failed
		,
		{HoldForm @ Message[LinkReadFullResponse::noResponse, link]}
		,
		TestID -> "WaitTime option: packet sent after read"
	];
	
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	Test[
		LinkDelegateEvaluation[
			link
			,
			Pause[2];
			someSymbol
			,
			"WaitTime" -> 0.01
		]
		,
		$Failed
		,
		{HoldForm @ Message[
			LinkReadFullResponse::noResponseInTime, link, 0.01
		]}
		,
		TestID -> "WaitTime option: packet sent after wait time"
	];
	
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	Test[
		LinkDelegateEvaluation[link, someSymbol, "WaitTime" -> 10]
		,
		someSymbol
		,
		TestID -> "WaitTime option: packet sent in time"
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
			LinkDelegateEvaluation[
				link
				,
				someSymbol3
				,
				"HandlePacketDecorator" -> ((
					#[ReturnPacket[someSymbol2]] :=
						AppendTo[$PacketLog, "someSymbol2 returned"];
					#[ReturnPacket[someSymbol]] :=
						AppendTo[$PacketLog, "someSymbol returned"];
				)&)
			]
			,
			someSymbol3
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


(* ::Subsubsection:: *)
(*LinkWriteFunction option*)


With[
	{link = LinkLaunch[command]}
	,
	Block[
		{$ContextPath = Prepend[$ContextPath, "TestContext`"]}
		,
		Test[
			LinkDelegateEvaluation[
				link,
				Context[TestContext`someSymbol],
				"LinkWriteFunction" -> LinkWrite
			]
			,
			"Global`"
			,
			TestID -> "LinkWriteFunction option: LinkWrite"
		];
	];
	
	LinkClose[link];
]



(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
