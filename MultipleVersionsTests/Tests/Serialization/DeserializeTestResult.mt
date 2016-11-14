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
	"TestEnvironment`Serialization`DeserializeTestResult`",
	{"MUnit`"}
]


Get["MultipleVersionsTests`Serialization`"]


Get["MultipleVersionsTests`Package`"]
Get["MultipleVersionsTests`Tests`Utilities`"]


If[
	MUnit`Information`$VersionNumber >= 1.4 ||
	MUnit`Information`$VersionNumber == 1.3 &&
		TrueQ[$mUnitRevisionNumber >= 1.7]
(* then *),
	prepareTr = Context@#&
(* else *),
	prepareTr = Sort
]


(* ::Section:: *)
(*Tests*)


Module[
	{tr}
	,
	
	Test[
		prepareTr[tr = DeserializeTestResult[$FakeTestResultSerialized]]
		,
		prepareTr[$FakeTestResult]
		,
		TestID -> "deserialize test result: object"
		,
		TestFailureMessage -> "prepared with: " <> ToString[prepareTr]
	];
	
	With[
		{
			actualTR = tr,
			expectedTR = $FakeTestResult
		}
		,
		Scan[
			If[ValueQ[#[expectedTR]],
				Test[
					#[actualTR]
					,
					#[expectedTR]
					,
					TestID -> "selector evaluated"
					,
					TestFailureMessage -> ToString[#]
				]
			(* else *),
				TestMatch[
					#[actualTR]
					,
					HoldPattern[#[actualTR]]
					,
					TestID -> "selector unevaluated"
					,
					TestFailureMessage -> ToString[#]
				]
			]&
			
			,
			
			{
				ActualMessages,
				ActualOutput,
				ActualOutputSetFunction,
				ActualOutputWrapper,
				AllTestIndex,
				EquivalenceFunction,
				ErrorMessage,
				ExpectedMessages,
				ExpectedMessagesID,
				ExpectedMessagesWrapper,
				ExpectedOutput,
				ExpectedOutputID,
				ExpectedOutputSetFunction,
				ExpectedOutputWrapper,
				FailureMode,
				MessagesEquivalenceFunction,
				NTestFailureMessage,
				OrNTestFailureMessages,
				SameTest,
				TestAbsoluteTimeUsed,
				TestCPUTimeUsed,
				TestErrorAction,
				TestFailureAction,
				TestFailureMessage,
				TestFileChild,
				TestID,
				TestIndex,
				TestInput,
				TestInputSetFunction,
				TestMemoryUsed,
				TestOptionsID,
				TestResultQ,
				TestSource,
				TestTags,
				TestTimeUsed			
			}
		]
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
