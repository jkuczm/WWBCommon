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


BeginPackage["TestEnvironment`MUnitBackports`TestRunBackported`", {"MUnit`"}]


Get["MultipleVersionsTests`MUnitBackports`"]
PrependTo[$ContextPath, "MultipleVersionsTests`MUnitBackports`Private`"]


Get["MultipleVersionsTests`Tests`Utilities`"]


(* ::Section:: *)
(*Tests*)


With[
	{tmpStream = OpenWrite[]}
	,
	
	Test[
		Block[
			{$Output = {tmpStream}}
			,
			TestRunBackported[
				FakeTestPath["test_success.mt"],
				Loggers -> {VerbosePrintLogger[]}
			]
		]
		,
		True
		,
		TestID -> "test file: success: returned value"
	];
	
	Close[tmpStream];

	Test[
		Import[First[tmpStream], "String"]
		,
		LoggedTestRunStart["test_success.mt"] <> "
." <> LoggedTestRunExpressionsNo[2] <> "

Tests run: 1,  Failures: 0,  Messages Failures: 0,  " <>
$LoggedTestRunSkipped <> ": 0, Errors: 0"
		,
		TestID -> "test file: success: $Output stream"
	];
]


With[
	{tmpStream = OpenWrite[]}
	,
	
	Test[
		Block[
			{$Output = {tmpStream}}
			,
			TestRunBackported[
				FakeTestPath["suite.mt"],
				Loggers -> {VerbosePrintLogger[]}
			]
		]
		,
		False
		,
		TestID -> "test suite file: failure: returned value"
	];
	
	Close[tmpStream];

	Test[
		Import[First[tmpStream], "String"]
		,
		$FakeTestSuiteLoggerOutput
		,
		TestID -> "test suite file: failure: $Output stream"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
