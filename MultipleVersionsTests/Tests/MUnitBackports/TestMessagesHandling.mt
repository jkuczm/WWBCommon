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
	"TestEnvironment`MUnitBackports`TestMessagesHandling`",
	{"MUnit`"}
]


Get["MultipleVersionsTests`MUnitBackports`"]


$bitbucket =
	Which[
		MUnit`Information`$VersionNumber >= 1.4,
			MUnit`Package`$bitbucket,
		MUnit`Information`$VersionNumber >= 1.2,
			MUnit`Test`Private`$bitbucket,
		True,
			Module[
				{
					f =
						Switch[$OperatingSystem,
							"Windows",
								"NUL",
							"MacOSX" | "Unix" | "iOS",
								"/dev/null"
						]
					,
					strs
				}
				,
				strs = Streams[f];
				If[strs == {},
					(* bitbucket stream is not open yet, so open it *)
					OpenWrite[f]
					,
					strs[[1]]
				]
			]
	]


(* ::Section:: *)
(*Tests*)


(* ::Section:: *)
(*Test expected messages argument*)


Test[
	Message[testSymbol::argctu, HoldForm[testSymbol]]
	,
	Null
	,
	testSymbol::argctu
	,
	TestID -> "Expected messages argument: MessageName"
]
Test[
	Message[testSymbol::argctu, HoldForm[testSymbol]];
	Message[General::fnsym, HoldForm[5]];
	,
	Null
	,
	{testSymbol::argctu, General::fnsym}
	,
	TestID -> "Expected messages argument: {MessageName}"
]


Test[
	Message[testSymbol::argctu, HoldForm[testSymbol]]
	,
	Null
	,
	Message[testSymbol::argctu, testSymbol]
	,
	TestID -> "Expected messages argument: Message removed argument HoldForm"
]
Test[
	Message[testSymbol::argctu, HoldForm[testSymbol]];
	Message[General::fnsym, HoldForm[5]];
	,
	Null
	,
	{Message[testSymbol::argctu, testSymbol], Message[General::fnsym, 5]}
	,
	TestID -> "Expected messages argument: {Message removed argument HoldForm}"
]


Test[
	Message[testSymbol::argctu, testSymbol];
	Message[General::fnsym, 5];
	,
	Null
	,
	{
		HoldForm @ Message[testSymbol::argctu, testSymbol],
		HoldForm @ Message[General::fnsym, 5]
	}
	,
	TestID -> "Expected messages argument: \
{HoldForm Message without argument HoldForm}"
]
Test[
	Message[testSymbol::argctu, HoldForm[testSymbol]];
	Message[General::fnsym, HoldForm[5]];
	,
	Null
	,
	{
		HoldForm @ Message[testSymbol::argctu, HoldForm[testSymbol]],
		HoldForm @ Message[General::fnsym, HoldForm[5]]
	}
	,
	TestID -> "Expected messages argument: \
{HoldForm Message with argument HoldForm}"
]


Test[
	Message[testSymbol::argctu, HoldForm[testSymbol]];
	Message[General::fnsym, HoldForm[5]];
	Message[testSymbol2::argx, HoldForm[testSymbol2], 2];
	,
	Null
	,
	{
		testSymbol::argctu,
		Message[General::fnsym, 5],
		HoldForm @ Message[testSymbol2::argx, HoldForm[testSymbol2], 2]
	}
	,
	TestID -> "Expected messages argument: {\
MessageName, \
Message removed argument HoldForm, \
HoldForm Message 2 arguments with and without HoldForm\
}"
]


(* ::Section:: *)
(*$Messages channel manipulations*)


Block[
	{$Messages = {}}
	,
	Test[
		Message[testSymbol::argctu, HoldForm[testSymbol]]
		,
		Null
		,
		{testSymbol::argctu}
		,
		TestID -> "Print message in Test in empty $Messages Block"
	]
]


Block[
	{$Messages = {$bitbucket}}
	,
	Test[
		Message[testSymbol::argctu, HoldForm[testSymbol]]
		,
		Null
		,
		{testSymbol::argctu}
		,
		TestID -> "Print message in Test in $bitbucket $Messages Block"
	]
]


Test[
	Block[
		{$Messages = {$bitbucket}}
		,
		Message[testSymbol::argctu, HoldForm[testSymbol]]
	]
	,
	Null
	,
	{testSymbol::argctu}
	,
	TestID -> "Print message in $bitbucket $Messages Block in Test"
]


Test[
	Block[
		{
			(* Don't log internal test result in MUnit >=1.3. *)
			MUnit`Package`logTestResult = Identity,
			(* Don't count internal test in MUnit 1.0. *)
			MUnit`Package`$TestIndex = 0,
			(* Don't count internal test in MUnit 1.4. *)
			MUnit`Package`$dynamicTestIndex = 0
		}
		,
		Reap[
			(*
				Don't use explicit Test symbol to avoid warning about missing
				TestID.
			*)
			FailureMode @ Symbol["Test"][
				Message[testSymbol::argctu, HoldForm[testSymbol]]
				,
				Null
				,
				(*
					Don't use explicit TestID symbol to avoid incrementation of
					$lexicalTestIndex in MUnit 1.4.
				*)
				Symbol["TestID"] -> "fake internal test"
			]
			,
			"MUnitTest"
		][[1]]
	]
	,
	"MessagesFailure"
	,
	TestID -> "Message failure Test inside other Test"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
