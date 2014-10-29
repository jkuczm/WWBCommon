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
	"TestEnvironment`MUnitBackports`TestRunTestCounting`",
	{"MUnit`"}
]


Get["MultipleVersionsTests`MUnitBackports`"]


Get["MultipleVersionsTests`Tests`Utilities`"]


(*
	Using two loggers in calls to TestRun to test patch preventing double
	counting.
*)


(* ::Section:: *)
(*Tests*)


With[
	{$listLogger = listLogger[]}
	,
	Test[
		TestRun[
			FakeTestPath["test_success.mt"],
			Loggers -> {listLogger[], $listLogger}
		]
		,
		True
		,
		TestID -> "one test success: returned value"
	];
	
	Test[
		Cases[OptionValue[$listLogger, "Log"], Hold[LogEnd, __]]
		,
		{Hold[LogEnd, 1, 1, 0, 0, 0, 0, False]}
		,
		TestID -> "one test success: log: test counts"
	];
	
	If[MUnit`Information`$VersionNumber >= 1.3,
		Test[
			Cases[OptionValue[$listLogger, "Log"], Hold[LogTestInfo, __]]
			,
			{Hold[LogTestInfo, "PassedFakeTest", 1, True]}
			,
			TestID -> "one test success: log: LogTestInfo"
		]
	];
]


With[
	{$listLogger = listLogger[]}
	,
	Test[
		TestRun[
			FakeTestPath["tests.mt"],
			Loggers -> {listLogger[], $listLogger}
		]
		,
		False
		,
		TestID -> "three tests failure: returned value"
	];
	
	Test[
		Cases[OptionValue[$listLogger, "Log"], Hold[LogEnd, __]]
		,
		{Hold[LogEnd, 3, 1, 1, 1, 0, 0, False]}
		,
		TestID -> "three tests failure: log: test counts"
	];
	
	If[MUnit`Information`$VersionNumber >= 1.3,
		Test[
			Cases[OptionValue[$listLogger, "Log"], Hold[LogTestInfo, __]]
			,
			{
				Hold[LogTestInfo, "PassedFakeTest", 1, True],
				Hold[LogTestInfo, "FailedFakeTest", 2, True],
				Hold[LogTestInfo, "MessageFailedFakeTest", 3, True]
			}
			,
			TestID -> "three tests failure: log: LogTestInfo"
		]
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
