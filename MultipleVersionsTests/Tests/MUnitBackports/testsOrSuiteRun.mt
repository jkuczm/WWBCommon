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


BeginPackage["TestEnvironment`MUnitBackports`testsOrSuiteRun`", {"MUnit`"}]


<<MultipleVersionsTests`MUnitBackports`
PrependTo[$ContextPath, "MultipleVersionsTests`MUnitBackports`Private`"]


<<MultipleVersionsTests`Package`
<<MultipleVersionsTests`Tests`Utilities`


(*
	Fake conversion using ASCII encoding.
	For unknown reason MUnit logs loading of CharacterEncoding/ASCI.m file
	using LogBeginTestSource and LogEndTestSource.
*)
ToString["", InputForm, CharacterEncoding -> "ASCII"]


(* ::Section:: *)
(*Tests*)


With[
	{$listLogger = listLogger[]}
	,
	
	Test[
		testsOrSuiteRun["non_existent_file.mt", Loggers -> {$listLogger}]
		,
		False
		,
		TestID -> "non-existent file: returned value"
	];

	TestMatch[
		OptionValue[$listLogger, "Log"]
		,
		{
			testRunStartLogs["non_existent_file.mt", "non_existent_file.mt"],
			Hold[LogFatal, "Could not open test file\nnon_existent_file.mt."],
			Hold[LogEnd, 0, 0, 0, 0, 0, 0, True],
			Hold[LogEnd]
		} // Flatten // DeleteCases[#, Null]&
		,
		TestID -> "non-existent file: log"
	];
]


With[
	{$listLogger = listLogger[]}
	,
	Test[
		testsOrSuiteRun[
			FakeTestPath["test_success.mt"],
			Loggers -> {$listLogger}
		]
		,
		True
		,
		TestID -> "test file: 1 test success: returned value"
	];
	
	TestMatch[
		OptionValue[$listLogger, "Log"]
		,
		{
			testRunStartLogs["test_success.mt"],
			$testSuccessLogs,
			testRunEndLogs[2, True, 1, 1, 0, 0, 0, 0]
		} // Flatten // DeleteCases[#, Null]&
		,
		TestID -> "test file: 1 test success: log"
	];
]


With[
	{$listLogger = listLogger[]}
	,
	Test[
		testsOrSuiteRun[
			FakeTestPath["test_failure.mt"],
			Loggers -> {$listLogger}
		]
		,
		False
		,
		TestID -> "test file: 1 test failure: returned value"
	];

	TestMatch[
		OptionValue[$listLogger, "Log"]
		,
		{
			testRunStartLogs["test_failure.mt"],
			$testFailureLogs,
			testRunEndLogs[2, False, 1, 0, 1, 0, 0, 0]
		} // Flatten // DeleteCases[#, Null]&
		,
		TestID -> "test file: 1 test failure: log"
	];
]


With[
	{$listLogger = listLogger[]}
	,
	Test[
		testsOrSuiteRun[
			FakeTestPath["test_message_failure.mt"],
			Loggers -> {$listLogger}
		]
		,
		False
		,
		TestID -> "one file: message failure: returned value"
	];

	TestMatch[
		OptionValue[$listLogger, "Log"]
		,
		{
			testRunStartLogs["test_message_failure.mt"],
			$testMessageFailureLogs,
			testRunEndLogs[2, False, 1, 0, 0, 1, 0, 0]
		} // Flatten // DeleteCases[#, Null]&
		,
		TestID -> "test file: 1 test message failure: log"
	];
]


With[
	{$listLogger = listLogger[]}
	,
	Test[
		testsOrSuiteRun[FakeTestPath["tests.mt"], Loggers -> {$listLogger}]
		,
		False
		,
		TestID -> "test file: 3 tests: returned value"
	];

	TestMatch[
		OptionValue[$listLogger, "Log"]
		,
		{
			testRunStartLogs["tests.mt"],
			$testsLogs,
			testRunEndLogs[4, False, 3, 1, 1, 1, 0, 0]
		} // Flatten // DeleteCases[#, Null]&
		,
		TestID -> "test file: 3 tests: log"
	];
]


With[
	{
		$listLogger = listLogger[],
		testSource = FakeTestPath["suite.mt"]
	}
	,
	Test[
		testsOrSuiteRun[testSource, Loggers -> {$listLogger}]
		,
		False
		,
		TestID -> "test suite file: returned value"
	];

	TestMatch[
		OptionValue[$listLogger, "Log"]
		,
		{
			testRunStartLogs["suite.mt"],
			Hold[LogBeginTestSource, testSource],
			$testSuccessLogs,
			$testFailureLogs,
			$testMessageFailureLogs,
			$testsLogs,
			Hold[LogEndTestSource],
			testRunEndLogs[10, False, 6, 2, 2, 2, 0, 0]
		} // Flatten // DeleteCases[#, Null]&
		,
		TestID -> "test suite file: log"
	];
]


(*
	TODO: Following tests fail in MUnit v1.0, but testsOrSuiteRun works in
	v1.0, just not when run inside other TestRun.
*)
BeginTestSection["Syntax error", MUnit`Information`$VersionNumber =!= 1.0]
With[
	{
		$listLogger = listLogger[],
		testSource = FakeTestPath["suite_syntax_error.mt"]
	}
	,
	Test[
		testsOrSuiteRun[testSource, Loggers -> {$listLogger}]
		,
		False
		,
		If[
			MUnit`Information`$VersionNumber == 1.0 ||
			MUnit`Information`$VersionNumber == 1.3 &&
				TrueQ[$mUnitRevisionNumber >= 1.7]
		(* then *),
			{Read::readt}
		(* else *),
			{}
		]
		,
		TestID -> "don't run tests from other files when error encountered: \
returned value"
	];

	TestMatch[
		OptionValue[$listLogger, "Log"]
		,
		{
			testRunStartLogs["suite_syntax_error.mt"],
			Hold[LogBeginTestSource, testSource],
			$testSuccessLogs,
			Hold[
				LogBeginTestSource,
				Evaluate @ FakeTestPath["syntax_error.mt"]
			],
			If[MUnit`Information`$VersionNumber >= 1.3,
				Hold[LogTestRunProgress, _?NumberQ]
			(* else *),
				Hold[LogTestRunProgress, _Integer, _Integer]
			],
			Hold[LogFatal, _],
			Which[
				MUnit`Information`$VersionNumber >= 1.4,
					{
						Hold[LogFatal, _String],
						Hold[LogTestRunProgress, 0]
					},
				MUnit`Information`$VersionNumber < 1.3,
					Hold[LogTestRunProgress, _Integer, _Integer]
			],
			Hold[LogEndTestSource],
			testRunEndLogs[3, False, 1, 1, 0, 0, 0, 0, True, False,
				MUnit`Information`$VersionNumber < 1.4,
				True
			]
		} // Flatten // DeleteCases[#, Null]&
		,
		TestID ->
			"don't run tests from other files when error encountered: log"
	];
]
EndTestSection[]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
