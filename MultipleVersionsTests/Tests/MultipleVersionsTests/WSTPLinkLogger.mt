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


BeginPackage[
	"TestEnvironment`MultipleVersionsTests`WSTPLinkLogger`",
	{"MUnit`"}
]


Needs["WSTPUtilities`"]


Get["MultipleVersionsTests`MultipleVersionsTests`"]


Get["MultipleVersionsTests`Tests`Utilities`"]


command = First[$CommandLine] <> " -mathlink -noprompt"
link = LinkLaunch[command]
logger = WSTPLinkLogger[link]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Formatting*)


Test[
	ToString[logger, OutputForm]
	,
	"-WSTP Link Logger-"
	,
	TestID -> "Formatting: OutputForm"
]


(* ::Subsection:: *)
(*Attributes*)


Test[
	OptionValue[logger, LinkObject]
	,
	link
	,
	TestID -> "Attributes: LinkObject"
]


(* ::Subsection:: *)
(*LinkWrite calls*)


Block[
	{WSTPUtilities`LinkFullContextWrite = testLinkWrite}
	,
	Test[
		LogMessage[logger, "Some message."]
		,
		testLinkWrite[link, TestLoggerPacket[LogMessage, "Some message."]]
		,
		TestID -> "Log function with 1 argument"
	];
	
	Test[
		LogEnd[logger, a, b, c, d, e, f, g]
		,
		testLinkWrite[link, TestLoggerPacket[LogEnd, a, b, c, d, e, f, g]]
		,
		TestID -> "Log function with 7 arguments"
	];
	
	Test[
		If[MUnit`Information`$VersionNumber >= 1.2,
			LogFailure[logger, $FakeTestResult]
		(* else *),
			Block[
				{MUnit`$CurrentFile = $FakeTestSource}
				,
				LogFailure[logger, $FakeTestResult]
			]
		]
		,
		testLinkWrite[
			link,
			TestLoggerPacket[LogFailure, $FakeTestResultSerialized]
		]
		,
		TestID -> "Log function with test result"
	];
]


(* ::Subsection:: *)
(*Remote evaluation*)


With[
	{
		path = $Path,
		workbenchMUnitPath =
			MultipleVersionsTests`MUnitVersionedLoader`$WorkbenchMUnitPath
	}
	,
	LinkDelegateEvaluation[link,
		$Path = Join[$Path, path] // DeleteDuplicates;
		MultipleVersionsTests`MUnitVersionedLoader`$WorkbenchMUnitPath =
			workbenchMUnitPath;
		Needs["MultipleVersionsTests`"];
	];
]

TestMatch[
	LinkDelegateEvaluation[
		link
		,
		remoteLogger = WSTPLinkLogger[$ParentLink];
		{Context[Evaluate[remoteLogger]], SymbolName[remoteLogger]}
		,
		"Head" -> HoldComplete
	]
	,
	HoldPattern[HoldComplete][
		{"MUnit`Loggers`Private`", _?(StringMatchQ[#, "logger*"]&)}
	]
	,
	TestID -> "Remote evaluation: logger creation"
]


Module[
	{$PacketLog = {}}
	,
	Test[
		LinkDelegateEvaluation[
			link
			,
			LogStart[remoteLogger, "test run title"]
			,
			"Head" -> HoldComplete
			,
			"HandlePacketDecorator" -> ((
				#[packet_] := AppendTo[$PacketLog, packet];
			)&)
		]
		,
		HoldComplete[Null]
		,
		TestID -> "Remote evaluation: LogStart: returned value"
	];
	
	Test[
		$PacketLog
		,
		{HoldComplete @ TestLoggerPacket[LogStart, "test run title"]}
		,
		TestID -> "Remote evaluation: LogStart: $PacketLog"
	];
]


Module[
	{$PacketLog = {}}
	,
	Test[
		LinkDelegateEvaluation[
			link
			,
			LogBeginTestSource[remoteLogger, "test_file_name"]
			,
			"Head" -> HoldComplete
			,
			"HandlePacketDecorator" -> ((
				#[packet_] := AppendTo[$PacketLog, packet];
			)&)
		]
		,
		HoldComplete[Null]
		,
		TestID -> "Remote evaluation: LogBeginTestSource: returned value"
	];
	
	Test[
		$PacketLog
		,
		{
			HoldComplete @ EvaluatePacket[
				If[MatchQ[MUnit`Package`testSourceStack, _List],
					AppendTo[MUnit`Package`testSourceStack, "test_file_name"]
				];
			]
			,
			HoldComplete @
				TestLoggerPacket[LogBeginTestSource, "test_file_name"]
		}
		,
		TestID -> "Remote evaluation: LogBeginTestSource: $PacketLog"
	];
]


Module[
	{$PacketLog = {}}
	,
	Test[
		LinkDelegateEvaluation[
			link
			,
			LogEndTestSource[remoteLogger]
			,
			"Head" -> HoldComplete
			,
			"HandlePacketDecorator" -> ((
				#[packet_] := AppendTo[$PacketLog, packet];
			)&)
		]
		,
		HoldComplete[Null]
		,
		TestID -> "Remote evaluation: LogEndTestSource: returned value"
	];
	
	Test[
		$PacketLog
		,
		{
			HoldComplete @ EvaluatePacket[
				If[MatchQ[MUnit`Package`testSourceStack, _List],
					MUnit`Package`testSourceStack =
						Most[MUnit`Package`testSourceStack]
				];
			]
			,
			HoldComplete @
				TestLoggerPacket[LogEndTestSource]
		}
		,
		TestID -> "Remote evaluation: LogEndTestSource: $PacketLog"
	];
]


(* ::Section:: *)
(*TearDown*)


LinkClose[link]


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
