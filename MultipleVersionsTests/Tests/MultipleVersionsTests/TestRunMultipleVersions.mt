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
	"TestEnvironment`MultipleVersionsTests`TestRunMultipleVersions`",
	{"MUnit`"}
]


Get["MultipleVersionsTests`MultipleVersionsTests`"]


Get["MultipleVersionsTests`Tests`Utilities`"]


(*
	Fake conversion using ASCII encoding.
	For unknown reason MUnit logs loading of CharacterEncoding/ASCI.m file
	using LogBeginTestSource and LogEndTestSource.
*)
ToString["", InputForm, CharacterEncoding -> "ASCII"]

(* Load Macros` here to avoid unecessary logging of its loading by MUnit. *)
If[$VersionNumber >= 10,
	Block[{$ContextPath}, Needs["Macros`"]]
]


$macrosTestSources = {
	Hold[LogBeginTestSource, "GeneralUtilitiesLoader`"],
	Hold[LogTestRunProgress, 0],
	Hold[LogBeginTestSource, "Macros`"],
	Hold[LogTestRunProgress, 0],
	Hold[LogBeginTestSource, "MacrosLoader`"],
	Hold[LogTestRunProgress, 0],
	Hold[LogBeginTestSource, _],
	Hold[LogTestRunProgress, 0],
	(* Since v10.1 CharacterEncodings/PrintableASCII.m is also loaded. *)
	If[$VersionNumber >= 10.1,
		{
			Hold[LogBeginTestSource, _],
			Hold[LogTestRunProgress, 0],
			Hold[LogTestRunProgress, _?NumberQ],
			Hold[LogEndTestSource]
		}
	],
	Hold[LogTestRunProgress, _?NumberQ],
	Hold[LogEndTestSource],
	Hold[LogTestRunProgress, _?NumberQ],
	Hold[LogEndTestSource],
	Hold[LogTestRunProgress, _?NumberQ],
	Hold[LogEndTestSource],
	Hold[LogTestRunProgress, _?NumberQ],
	Hold[LogEndTestSource]
} // Flatten // DeleteCases[#, Null]&


command = First[$CommandLine]


(* ::Section:: *)
(*Tests*)


Do[
	SetOptions[TestRunMultipleVersions,
		"LinkCallingVersion" -> linkCallingVersion
	];
	testFailureMessage =
		"LinkCallingVersion: " <> ToString[linkCallingVersion];
	
	With[
		{$listLogger = listLogger[]}
		,
		Test[
			TestRunMultipleVersions[
				FakeTestPath["test_success.mt"],
				command,
				Loggers -> {$listLogger}
			]
			,
			True
			,
			TestID -> "one evaluator: test file: success: returned value",
			TestFailureMessage -> testFailureMessage
		];

		TestMatch[
			OptionValue[$listLogger, "Log"]
			,
			{
				Hold[LogMessage,
					Evaluate[
						"Running tests in Mathematica version: " <>
						$Version <> ". " <> MUnit`Information`$Version
					]
				],
				testRunStartLogs["test_success.mt"],
				(*
					MUnit logs loading of files related to Macros` package
					using LogBeginTestSource and LogEndTestSource, although
					they are not really test sources. This is a feature/bug of
					MUnit.
				*)
				If[MUnit`Information`$VersionNumber >= 1.4 && linkCallingVersion,
					fakeOneTestFileLogs[
						"test_success.mt", "PassedFakeTest", LogSuccess,
						$macrosTestSources
					]
				(* else *),
					$testSuccessLogs
				],
				testRunEndLogs[2, True, 1, 1, 0, 0, 0, 0]
			} // Flatten // DeleteCases[#, Null]&
			,
			TestID -> "one evaluator: test file: success: log",
			TestFailureMessage -> testFailureMessage
		];
		
		If[linkCallingVersion,
			With[
				{
					tr = First @ Cases[
						OptionValue[$listLogger, "Log"],
						Hold[LogSuccess, tr_] :> tr,
						{1},
						1
					]
				}
				,
				Test[
					TestResultQ[tr]
					,
					True
					,
					TestID -> "one evaluator: test file: success: \
test result object: is test result",
					TestFailureMessage -> testFailureMessage
				];
				Test[
					TestID[tr]
					,
					"PassedFakeTest"
					,
					TestID -> "one evaluator: test file: success: \
test result object: TestID",
					TestFailureMessage -> testFailureMessage
				];
				If[MUnit`Information`$VersionNumber >= 1.3,
					Test[
						TestSource[tr]
						,
						FakeTestPath["test_success.mt"]
						,
						TestID -> "one evaluator: test file: success: \
test result object: TestSource",
						TestFailureMessage -> testFailureMessage
					]
				];
			]
		]
	];
	
	
	With[
		{
			$listLogger = listLogger[],
			testSource = FakeTestPath["suite.mt"]
		}
		,
		Test[
			TestRunMultipleVersions[
				testSource,
				command,
				Loggers -> {$listLogger}
			]
			,
			False
			,
			TestID ->
				"one evaluator: test suite file: failure: returned value",
			TestFailureMessage -> testFailureMessage
		];

		TestMatch[
			OptionValue[$listLogger, "Log"]
			,
			{
				Hold[LogMessage,
					Evaluate[
						"Running tests in Mathematica version: " <>
						$Version <> ". " <> MUnit`Information`$Version
					]
				],
				testRunStartLogs["suite.mt"],
				Hold[LogBeginTestSource, testSource],
				If[MUnit`Information`$VersionNumber >= 1.4,
					{
						Hold[LogTestRunProgress, 0],
						Hold[LogTestRunProgress, 0],
						Hold[LogTestRunProgress, _?NumberQ],
						Hold[LogTestRunProgress, _?NumberQ]
					}
				],
				(*
					MUnit logs loading of files related to Macros` package
					using LogBeginTestSource and LogEndTestSource, although
					they are not really test sources. This is a feature/bug of
					MUnit.
				*)
				If[MUnit`Information`$VersionNumber >= 1.4 && linkCallingVersion,
					fakeOneTestFileLogs[
						"test_success.mt", "PassedFakeTest", LogSuccess,
						$macrosTestSources
					]
				(* else *),
					$testSuccessLogs
				],
				$testFailureLogs,
				(*
					MUnit logs loading of CharacterEncoding/ASCI.m file using
					LogBeginTestSource and LogEndTestSource, although it is not
					really test source. This is a feature/bug of MUnit.
				*)
				If[MUnit`Information`$VersionNumber >= 1.4 && linkCallingVersion,
					fakeOneTestFileLogs[
						"test_message_failure.mt", "MessageFailedFakeTest",
						LogMessagesFailure,
						{
							Hold[LogBeginTestSource, _],
							Hold[LogTestRunProgress, 0],
							Hold[LogTestRunProgress, _?NumberQ],
							Hold[LogEndTestSource]
						}
					]
				(* else *),
					$testMessageFailureLogs
				],
				$testsLogs,
				If[MUnit`Information`$VersionNumber >= 1.4,
					Hold[LogTestRunProgress, 1]
				],
				Hold[LogEndTestSource],
				testRunEndLogs[10, False, 6, 2, 2, 2, 0, 0]
			} // Flatten // DeleteCases[#, Null]&
			,
			TestID -> "one evaluator: test suite file: failure: log",
			TestFailureMessage -> testFailureMessage
		];
	];


	With[
		{
			$listLogger = listLogger[],
			singleRunOutput = {
				Hold[LogMessage,
					Evaluate[
						"Running tests in Mathematica version: " <>
						$Version <> ". " <> MUnit`Information`$Version
					]
				],
				testRunStartLogs["test_failure.mt"],
				(*
					MUnit logs loading of files related to Macros` package
					using LogBeginTestSource and LogEndTestSource, although
					they are not really test sources. This is a feature/bug of
					MUnit.
				*)
				If[MUnit`Information`$VersionNumber >= 1.4 && linkCallingVersion,
					fakeOneTestFileLogs[
						"test_failure.mt", "FailedFakeTest", LogFailure,
						$macrosTestSources
					]
				(* else *),
					$testFailureLogs
				],
				testRunEndLogs[2, False, 1, 0, 1, 0, 0, 0]
			} // Flatten // DeleteCases[#, Null]&
		}
		,
		Test[
			TestRunMultipleVersions[
				FakeTestPath["test_failure.mt"],
				{command, command},
				Loggers -> {$listLogger}
			]
			,
			False
			,
			TestID -> "two evaluators: test file: failure: returned value",
			TestFailureMessage -> testFailureMessage
		];
		
		TestMatch[
			OptionValue[$listLogger, "Log"]
			,
			{
				singleRunOutput,
				Hold[LogMessage, "-------------------------------"],
				singleRunOutput
			} // Flatten
			,
			TestID -> "two evaluators: test file: failure: log",
			TestFailureMessage -> testFailureMessage
		];
	];
	
	
	Block[
		{fakeTest`testSymbol}
		,
		Test[
			TestRunMultipleVersions[
				FakeTestPath["init_required.mt"],
				command,
				Loggers -> {},
				"Init" :> (fakeTest`testSymbol = 1)
			]
			,
			True
			,
			TestID ->
				"Init option: one evaluator: test file: returned value",
			TestFailureMessage -> testFailureMessage
		];
		
		TestMatch[
			fakeTest`testSymbol
			,
			If[linkCallingVersion,
				HoldPattern[fakeTest`testSymbol]
			(* else *),
				1
			]
			,
			TestID -> "Init option: one evaluator: test file: \
Init code in calling evaluator",
			TestFailureMessage -> testFailureMessage
		];
	];
	
	,
	
	{linkCallingVersion, {False, True}}
]


With[
	{
		tmpStream = OpenWrite[],
		$listLogger = listLogger[]
	}
	,
	
	Test[
		Block[
			{$Output = {tmpStream}}
			,
			TestRunMultipleVersions[
				FakeTestPath["suite.mt"],
				command,
				Loggers -> {$listLogger},
				"RemoteLoggers" :> {VerbosePrintLogger[]},
				"LinkCallingVersion" -> True
			]
		]
		,
		False
		,
		TestID -> "RemoteLoggers option: one evaluator: \
test suite file: failure: returned value"
	];
	
	Close[tmpStream];
	
	
	TestMatch[
		OptionValue[$listLogger, "Log"]
		,
		{}
		,
		TestID -> "RemoteLoggers option: one evaluator: \
test suite file: failure: log"
	];
	
	
	Test[
		Import[First[tmpStream], "String"]
		,"
Running tests in Mathematica version: " <> $Version <> ". " <> MUnit`Information`$Version <> "
" <> $FakeTestSuiteLoggerOutput
		,
		TestID -> "RemoteLoggers option: one evaluator: \
test suite file: failure: $Output stream"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
