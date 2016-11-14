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


BeginPackage["MultipleVersionsTests`Tests`Utilities`"]


(* ::Section:: *)
(*Usage messages*)


(* ::Subsection:: *)
(*Public symbols usage*)


FakeTestPath::usage =
"\
FakeTestPath[file] \
returns path to file from fake tests directory."


listLogger::usage =
"\
listLogger[] \
returns an MUnit logger that saves all cals of logging functions in a list."


testRunStartLogs::usage =
"\
testRunStartLogs[testsFile, testFilePath] \
returns list of held log function calls that are expected to be logged by \
listLogger at beginig of test run when file from testFilePath with name \
testsFile is run.\

testRunStartLogs[testsFile] \
uses automatic value of testFilePath which is FakeTestPath[testsFile]."

testRunEndLogs::usage =
"\
testRunEndLogs[\
exprCnt, successful, \
testCnt, successCnt, failureCnt, msgFailureCnt, skippedCnt, errCnt, \
abortOrFatal, aborted, fatal, syntax\
] \
returns list of held log function calls that are expected to be logged by \
listLogger at the end of test run."

fakeOneTestFileLogs::usage =
"\
fakeOneTestFileLogs[testFile, testID, logFunction] \
returns list of held log function calls that are expected to be logged by \
listLogger when test from testFile is run. testID should be id of test \
present in testFile and logFunction should be apropriate for failure mode of \
the test (LogSuccess, LogFailure, LogMessagesFailure or LogError)."

$testSuccessLogs::usage =
"\
$testSuccessLogs \
is list of held log function calls that are expected to be logged by \
listLogger when fakeTests/test_success.mt file is run."

$testFailureLogs::usage =
"\
$testFailureLogs \
is list of held log function calls that are expected to be logged by \
listLogger when fakeTests/test_failure.mt file is run."

$testMessageFailureLogs::usage =
"\
$testMessageFailureLogs \
is list of held log function calls that are expected to be logged by \
listLogger when fakeTests/test_message_failure.mt file is run."

$testsLogs::usage =
"\
$testsLogs \
is list of held log function calls that are expected to be logged by \
listLogger when fakeTests/tests.mt file is run."


LoggedTestRunStart::usage =
"\
LoggedTestRunStart[file] \
returns string with text logged at test start by used version of MUnit."

LoggedTestRunExpressionsNo::usage =
"\
LoggedTestRunExpressionsNo[expressionsNo] \
returns string with information about number of expressions in test file \
logged by used version of MUnit."

$LoggedTestRunSkipped::usage =
"\
$LoggedTestRunSkipped \
a string with information about skipped sections/tests logged by used version \
of MUnit."


$FakeTestResult::usage =
"\
$FakeTestResult \
is a test result object created by used version of MUnit."

$FakeTestResultSerialized::usage =
"\
$FakeTestResultSerialized \
is an expected serialized version of $FakeTestResult."

$FakeTestSource::usage =
"\
$FakeTestSource \
is a symbol representing test source used in $FakeTestResult."


$FakeTestSuiteLoggerOutput::usage =
"\
$FakeTestSuiteLoggerOutput \
is a string with expected output from VerbosePrintLogger logging results of \
MultipleVersionsTests/Tests/fakeTests/suite.mt."


(* ::Section:: *)
(*Implementation*)
	

Begin["`Private`"]


(* ::Subsection:: *)
(*Imports*)


Needs["MUnit`"]


Get["MultipleVersionsTests`Package`"]
Get["MultipleVersionsTests`Serialization`"]


(* ::Subsection:: *)
(*FakeTestPath*)


FakeTestPath[file_String] :=
	FindFile @ FileNameJoin[{"MultipleVersionsTests/Tests/fakeTests", file}]


(* ::Subsection:: *)
(*listLogger*)


listLogger[] :=
	With[
		{logger = Unique["MUnit`Loggers`Private`logger"]}
		,
		Options[logger] = {"Log" -> {}};
		
		Scan[
			(
				logger /: #[logger, args___] :=
					SetOptions[logger,
						"Log" ->
							Append[OptionValue[logger, "Log"], Hold[#, args]]
					]
			)&
			,
			$logFunctions
		];
		
		logger /: Format[logger, StandardForm] :=
			Interpretation[
				Row[{
					RawBoxes["\[SkeletonIndicator]"],
					RawBoxes["List Logger"],
					RawBoxes["\[SkeletonIndicator]"]
				}]
				,
				logger
			];
		
		logger /: Format[logger, OutputForm] := "-List Logger-";
		
		logger
	]


(* ::Subsection:: *)
(*testRunStartLogs*)


testRunStartLogs[
	testsFile_String,
	testFilePath:(_String | Automatic):Automatic
] :=
	With[
		{
			title =
				If[$VersionNumber >= 7.0,
					"Test Report: " <> testsFile
				(* else *),
					None
				],
			path =
				If[testFilePath === Automatic,
					FakeTestPath[testsFile]
				(* else *),
					testFilePath
				]
		}
		,
		{
			Hold[LogStart, title],
			Hold[LogBegin, title],
			Hold[LogStart, title, path],
			Hold[LogBegin, title, path]
		}
	]


(* ::Subsection:: *)
(*testRunEndLogs*)


testRunEndLogs[
	exprCnt_Integer, successful:(True | False)
	,
	testCnt_Integer, successCnt_Integer, failureCnt_Integer,
	msgFailureCnt_Integer, skippedCnt_Integer, errCnt_Integer,
	abortOrFatal:(True | False):False
	,
	aborted:(True | False):False, fatal:(True | False):False,
	syntax:(True | False):False
] :=
	{
		If[MUnit`Information`$VersionNumber < 1.4,
			Hold[LogExpressionCount, exprCnt]
		],
		Hold[LogCPUTimeUsed, _?NumberQ],
		Hold[LogAbsoluteTimeUsed, _?NumberQ],
		Hold[LogMemoryUsed, _Integer],
		Hold[LogTestCount, testCnt],
		Hold[LogSuccessCount, successCnt],
		Hold[LogFailureCount, failureCnt],
		Hold[LogMessagesFailureCount, msgFailureCnt],
		Hold[LogSkippedTestCount, skippedCnt],
		Hold[LogErrorCount, errCnt],
		If[MUnit`Information`$VersionNumber >= 1.3,
			{
				Hold[LogWasAborted, aborted],
				Hold[LogWasFatal, fatal]
			}
		],
		If[MUnit`Information`$VersionNumber >= 1.4,
			Hold[LogWasSyntax, syntax]
		],
		Hold[LogWasSuccessful, successful],
		Hold[LogEnd,
			testCnt, successCnt, failureCnt, msgFailureCnt, skippedCnt, errCnt,
			abortOrFatal
		],
		Hold[LogEnd]
	} // Flatten // DeleteCases[#, Null]&


(* ::Subsection:: *)
(*fakeOneTestFileLogs*)


fakeOneTestFileLogs[
		testFile_String, testID_, logFunction_Symbol, additionalTestSources_:{}
] := {
	Hold[LogBeginTestSource, Evaluate @ FakeTestPath[testFile]],
	If[MUnit`Information`$VersionNumber >= 1.3,
		{
			If[MUnit`Information`$VersionNumber >= 1.4,
				{
					Hold[LogTestRunProgress, 0],
					Hold[LogTestRunProgress, 0]
				}
			],
			Hold[LogTestRunProgress, _?NumberQ],
			Hold[LogTestRunProgress, _?NumberQ],
			Hold[LogTestInfo, testID, 1, True]
		}
	(* else *),
		{
			Hold[LogTestRunProgress, _Integer, _Integer],
			Hold[LogTestRunProgress, _Integer, _Integer],
			If[MUnit`Information`$VersionNumber >= 1.2,
				Hold[LogTestID, testID]
			]
		}
	],
	additionalTestSources,
	Hold[logFunction, _],
	If[MUnit`Information`$VersionNumber >= 1.3,
		Hold[LogTestRunProgress, 1]
	(* else *),
		{
			Hold[LogTestRunProgress, _Integer, _Integer],
			Hold[LogTestRunProgress, _Integer, _Integer]
		}
	],
	Hold[LogEndTestSource]
} // Flatten


(* ::Subsection:: *)
(*$testSuccessLogs*)


$testSuccessLogs =
	fakeOneTestFileLogs["test_success.mt", "PassedFakeTest", LogSuccess]


(* ::Subsection:: *)
(*$testFailureLogs*)


$testFailureLogs =
	fakeOneTestFileLogs["test_failure.mt", "FailedFakeTest", LogFailure]


(* ::Subsection:: *)
(*$testMessageFailureLogs*)


$testMessageFailureLogs =
	fakeOneTestFileLogs[
		"test_message_failure.mt", "MessageFailedFakeTest", LogMessagesFailure
	]


(* ::Subsection:: *)
(*$testsLogs*)


$testsLogs = {
	Hold[LogBeginTestSource, Evaluate @ FakeTestPath["tests.mt"]],
	If[MUnit`Information`$VersionNumber >= 1.3,
		{
			If[MUnit`Information`$VersionNumber >= 1.4,
				{
					Hold[LogTestRunProgress, 0],
					Hold[LogTestRunProgress, 0]
				}
			],
			Hold[LogTestRunProgress, _?NumberQ],
			Hold[LogTestRunProgress, _?NumberQ],
			Hold[LogTestInfo, "PassedFakeTest", 1, True]
		}
	(* else *),
		{
			Hold[LogTestRunProgress, _Integer, _Integer],
			Hold[LogTestRunProgress, _Integer, _Integer],
			If[MUnit`Information`$VersionNumber >= 1.2,
				Hold[LogTestID, "PassedFakeTest"]
			]
		}
	],
	Hold[LogSuccess, _],
	If[MUnit`Information`$VersionNumber >= 1.3,
		{
			Hold[LogTestRunProgress, _?NumberQ],
			Hold[LogTestInfo, "FailedFakeTest", 2, True]
		}
	(* else *),
		{
			Hold[LogTestRunProgress, _Integer, _Integer],
			If[MUnit`Information`$VersionNumber >= 1.2,
				Hold[LogTestID, "FailedFakeTest"]
			]
		}
	],
	Hold[LogFailure, _],
	If[MUnit`Information`$VersionNumber >= 1.3,
		{
			Hold[LogTestRunProgress, _?NumberQ],
			Hold[LogTestInfo, "MessageFailedFakeTest", 3, True]
		}
	(* else *),
		{
			Hold[LogTestRunProgress, _Integer, _Integer],
			If[MUnit`Information`$VersionNumber >= 1.2,
				Hold[LogTestID, "MessageFailedFakeTest"]
			]
		}
	],
	Hold[LogMessagesFailure, _],
	If[MUnit`Information`$VersionNumber >= 1.3,
		Hold[LogTestRunProgress, 1]
	(* else *),
		{
			Hold[LogTestRunProgress, _Integer, _Integer],
			Hold[LogTestRunProgress, _Integer, _Integer]
		}
	],
	Hold[LogEndTestSource]
}


(* ::Subsection:: *)
(*LoggedTestRunStart*)


If[$VersionNumber >= 7.0,
	LoggedTestRunStart[filename_String] :=
		"Starting test run \"Test Report: " <> filename <> "\"";
(* else *),
	LoggedTestRunStart[(*filename*)_String] = "Starting Test Run";
]


(* ::Subsection:: *)
(*LoggedTestRunExpressionsNo*)


If[MUnit`Information`$VersionNumber >= 1.4,
	LoggedTestRunExpressionsNo[(*expressionsNo*)_Integer] = "";
(* else *),
	(*
		Use StringJoin instead of ToString@StringForm since the latter doesn't
		work as expected whith newlines.
	*)
	LoggedTestRunExpressionsNo[expressionsNo_Integer] :=
		"\nTest run had " <> ToString[expressionsNo] <> " expressions\n";
]


(* ::Subsection:: *)
(*$LoggedTestRunSkipped*)


If[MUnit`Information`$VersionNumber >= 1.3,
	$LoggedTestRunSkipped = "Skipped Tests";
(* else *),
	$LoggedTestRunSkipped = "Sections skipped";
]


(* ::Subsection:: *)
(*$FakeTestResult, $FakeTestResultSerialized, $FakeTestSource*)


Module[
	{
		(*
			FailureMode must be one of 4 acceptable strings otherwise any
			atempt to format test result will lead to infinite recursion.
		*)
		failureMode = "Success"
		,
		test, testContents, input, expectedOutput, actualOutput, expectedMsgs,
		actualMsgs, errorMsg
		,
		testID, testIndex, allTestIndex, actualOutputSetFunction,
		expectedOutputSetFunction, actualOutputWrapper, expectedOutputWrapper,
		sameTest, msgsEquivFunc, expectedMsgsWrapper, testFailureMessage,
		testFailureAction, testErrorAction, cpuTimeUsed, absTimeUsed,
		memoryUsed, testTags, testSource, ntestFailureMessage,
		orntestFailureMessages, testOptionsID, expectedOutputID,
		expectedMessagesID
	}
	,
	(*
		Test expression stored in test result must match HoldForm[_Test],
		otherwise TestResultQ in MUnit version <= 1.2 won't recognize test
		result.
	*)
	test = HoldForm[Test[testContents]];
	
	$FakeTestSource = testSource;
	
	Which[
		MUnit`Information`$VersionNumber >= 1.4,
			$FakeTestResult =
				MUnit`Test`Private`newTestResult[
					test,
					failureMode,
					input,
					expectedOutputWrapper[expectedOutput],
					actualOutputWrapper[actualOutput],
					expectedMsgsWrapper[expectedMsgs],
					actualMsgs,
					errorMsg
					,
					TestIndex -> testIndex,
					TestID -> testID,
					ActualOutputSetFunction -> actualOutputSetFunction,
					ExpectedOutputSetFunction -> expectedOutputSetFunction,
					ActualOutputWrapper -> actualOutputWrapper,
					ExpectedOutputWrapper -> expectedOutputWrapper,
					SameTest -> sameTest,
					MessagesEquivalenceFunction -> msgsEquivFunc,
					ExpectedMessagesWrapper -> expectedMsgsWrapper,
					TestFailureMessage -> testFailureMessage,
					TestFailureAction -> testFailureAction,
					TestErrorAction -> testErrorAction,
					TestCPUTimeUsed -> cpuTimeUsed,
					TestAbsoluteTimeUsed -> absTimeUsed,
					TestMemoryUsed -> memoryUsed,
					TestTags -> testTags,
					TestSource -> testSource
					,
					NTestFailureMessage -> ntestFailureMessage,
					OrNTestFailureMessages -> orntestFailureMessages
				];

			$FakeTestResultSerialized =
				Sort @ TestResultSerialized[
					"Test" -> None,
					"FailureMode" -> failureMode,
					"TestInput" -> input,
					"ExpectedOutput" -> expectedOutputWrapper[expectedOutput],
					"ActualOutput" -> actualOutputWrapper[actualOutput],
					"ExpectedMessages" -> expectedMsgsWrapper[expectedMsgs],
					"ActualMessages" -> actualMsgs,
					"ErrorMessage" -> errorMsg
					,
					"TestIndex" -> testIndex,
					"TestID" -> testID,
					(*
						Bug in MUnit v1.4 ExpectedOutputSetFunction is assigned
						to ActualOutputSetFunction
					*)
					"ActualOutputSetFunction" ->
						expectedOutputSetFunction(*actualOutputSetFunction*),
					"ExpectedOutputSetFunction" -> expectedOutputSetFunction,
					"ActualOutputWrapper" -> actualOutputWrapper,
					"ExpectedOutputWrapper" -> expectedOutputWrapper,
					"SameTest" -> sameTest,
					"MessagesEquivalenceFunction" -> msgsEquivFunc,
					"ExpectedMessagesWrapper" -> expectedMsgsWrapper,
					"TestFailureMessage" -> testFailureMessage,
					"TestFailureAction" -> testFailureAction,
					"TestErrorAction" -> testErrorAction,
					"TestCPUTimeUsed" -> cpuTimeUsed,
					"TestAbsoluteTimeUsed" -> absTimeUsed,
					"TestMemoryUsed" -> memoryUsed,
					"TestTags" -> testTags,
					"TestSource" -> testSource
					,
					"NTestFailureMessage" -> ntestFailureMessage,
					"OrNTestFailureMessages" -> orntestFailureMessages
				];
		,
		MUnit`Information`$VersionNumber >= 1.3 &&
			TrueQ[$mUnitRevisionNumber >= 1.7]
		,
			$FakeTestResult =
				MUnit`Test`Private`newTestResultObject[
					test,
					failureMode,
					input,
					expectedOutput,
					actualOutput,
					expectedMsgsWrapper[expectedMsgs],
					actualMsgs,
					errorMsg
					,
					TestIndex -> testIndex,
					AllTestIndex -> allTestIndex,
					TestID -> testID,
					ActualOutputSetFunction -> actualOutputSetFunction,
					ExpectedOutputSetFunction -> expectedOutputSetFunction,
					EquivalenceFunction -> sameTest,
					MessagesEquivalenceFunction -> msgsEquivFunc,
					ExpectedMessagesWrapper -> expectedMsgsWrapper,
					TestFailureMessage -> testFailureMessage,
					TestFailureAction -> testFailureAction,
					TestErrorAction -> testErrorAction,
					TestTimeUsed -> cpuTimeUsed,
					TestMemoryUsed -> memoryUsed,
					TestTags -> testTags,
					TestSource -> testSource
				];
			
			$FakeTestResultSerialized =
				Sort @ TestResultSerialized[
					"Test" -> None,
					"FailureMode" -> failureMode,
					"TestInput" -> input,
					"ExpectedOutput" -> expectedOutput,
					"ActualOutput" -> actualOutput,
					"ExpectedMessages" -> expectedMsgsWrapper[expectedMsgs],
					"ActualMessages" -> actualMsgs,
					"ErrorMessage" -> errorMsg
					,
					"TestIndex" -> testIndex,
					"AllTestIndex" -> allTestIndex,
					"TestID" -> testID,
					"ExpectedOutputSetFunction" -> expectedOutputSetFunction,
					"SameTest" -> sameTest,
					"MessagesEquivalenceFunction" -> msgsEquivFunc,
					"ExpectedMessagesWrapper" -> expectedMsgsWrapper,
					"TestFailureMessage" -> testFailureMessage,
					"TestFailureAction" -> testFailureAction,
					"TestErrorAction" -> testErrorAction,
					"TestCPUTimeUsed" -> cpuTimeUsed,
					"TestMemoryUsed" -> memoryUsed,
					"TestTags" -> testTags,
					"TestSource" -> testSource
				];
		,
		MUnit`Information`$VersionNumber >= 1.3,
			$FakeTestResult =
				Symbol["TestResultObject"][
					test,
					failureMode,
					HoldForm[input],
					HoldForm[expectedOutput],
					HoldForm[actualOutput],
					expectedMsgsWrapper[expectedMsgs],
					actualMsgs,
					errorMsg
					,
					TestIndex -> testIndex,
					AllTestIndex -> allTestIndex,
					TestID -> testID,
					TestInputSetFunction -> actualOutputSetFunction,
					ExpectedOutputSetFunction -> expectedOutputSetFunction,
					EquivalenceFunction -> sameTest,
					MessagesEquivalenceFunction -> msgsEquivFunc,
					ExpectedMessagesWrapper -> expectedMsgsWrapper,
					TestFailureMessage -> testFailureMessage,
					TestFailureAction -> testFailureAction,
					TestErrorAction -> testErrorAction,
					TestTimeUsed -> cpuTimeUsed,
					TestMemoryUsed -> memoryUsed,
					TestTags -> testTags,
					TestSource -> testSource
				];
			
			$FakeTestResultSerialized =
				Sort @ TestResultSerialized[
					"Test" -> test,
					"FailureMode" -> failureMode,
					"TestInput" -> HoldForm[input],
					"ExpectedOutput" -> HoldForm[expectedOutput],
					"ActualOutput" -> HoldForm[actualOutput],
					"ExpectedMessages" -> expectedMsgsWrapper[expectedMsgs],
					"ActualMessages" -> actualMsgs,
					"ErrorMessage" -> errorMsg
					,
					"TestIndex" -> testIndex,
					"AllTestIndex" -> allTestIndex,
					"TestID" -> testID,
					"ActualOutputSetFunction" -> actualOutputSetFunction,
					"ExpectedOutputSetFunction" -> expectedOutputSetFunction,
					"SameTest" -> sameTest,
					"MessagesEquivalenceFunction" -> msgsEquivFunc,
					"ExpectedMessagesWrapper" -> expectedMsgsWrapper,
					"TestFailureMessage" -> testFailureMessage,
					"TestFailureAction" -> testFailureAction,
					"TestErrorAction" -> testErrorAction,
					"TestCPUTimeUsed" -> cpuTimeUsed,
					"TestMemoryUsed" -> memoryUsed,
					"TestTags" -> testTags,
					"TestSource" -> testSource
				];
		,
		MUnit`Information`$VersionNumber >= 1.2,
			$FakeTestResult =
				Symbol["TestResultObject"][
					test,
					failureMode,
					HoldForm[input],
					HoldForm[expectedOutput],
					HoldForm[actualOutput],
					expectedMsgsWrapper[expectedMsgs],
					actualMsgs,
					errorMsg
					,
					TestIndex -> testIndex,
					TestID -> testID,
					EquivalenceFunction -> sameTest,
					MessagesEquivalenceFunction -> msgsEquivFunc,
					ExpectedMessagesWrapper -> expectedMsgsWrapper,
					TestFailureMessage -> testFailureMessage,
					TestFailureAction -> testFailureAction,
					TestErrorAction -> testErrorAction,
					TestTimeUsed -> cpuTimeUsed,
					TestMemoryUsed -> memoryUsed,
					TestTags -> testTags,
					TestFileChild -> testSource
				];
			
			$FakeTestResultSerialized =
				Sort @ TestResultSerialized[
					"Test" -> test,
					"FailureMode" -> failureMode,
					"TestInput" -> HoldForm[input],
					"ExpectedOutput" -> HoldForm[expectedOutput],
					"ActualOutput" -> HoldForm[actualOutput],
					"ExpectedMessages" -> expectedMsgsWrapper[expectedMsgs],
					"ActualMessages" -> actualMsgs,
					"ErrorMessage" -> errorMsg
					,
					"TestIndex" -> testIndex,
					"TestID" -> testID,
					"SameTest" -> sameTest,
					"MessagesEquivalenceFunction" -> msgsEquivFunc,
					"ExpectedMessagesWrapper" -> expectedMsgsWrapper,
					"TestFailureMessage" -> testFailureMessage,
					"TestFailureAction" -> testFailureAction,
					"TestErrorAction" -> testErrorAction,
					"TestCPUTimeUsed" -> cpuTimeUsed,
					"TestMemoryUsed" -> memoryUsed,
					"TestTags" -> testTags,
					"TestSource" -> testSource
				];
		,
		True,
			$FakeTestResult =
				Symbol["TestResultObject"][
					test,
					failureMode,
					HoldForm[input],
					HoldForm[expectedOutput],
					HoldForm[actualOutput],
					HoldForm[expectedMsgs],
					HoldForm[actualMsgs]
					,
					TestIndex -> testIndex,
					TestID -> testID,
					TestOptionsID -> testOptionsID,
					ExpectedMessagesID -> expectedMessagesID,
					ExpectedOutputID -> expectedOutputID,
					EquivalenceFunction -> sameTest,
					MessagesEquivalenceFunction -> msgsEquivFunc,
					TestFailureMessage -> testFailureMessage,
					TestFailureAction -> testFailureAction,
					TestTimeUsed -> cpuTimeUsed,
					TestMemoryUsed -> memoryUsed,
					TestTags -> testTags
				];
			
			$FakeTestResultSerialized =
				Sort @ TestResultSerialized[
					"Test" -> test,
					"FailureMode" -> failureMode,
					"TestInput" -> HoldForm[input],
					"ExpectedOutput" -> HoldForm[expectedOutput],
					"ActualOutput" -> HoldForm[actualOutput],
					"ExpectedMessages" -> HoldForm[expectedMsgs],
					"ActualMessages" -> HoldForm[actualMsgs],
					"ErrorMessage" -> ""
					,
					"TestIndex" -> testIndex,
					"TestID" -> testID,
					"TestOptionsID" -> testOptionsID,
					"ExpectedMessagesID" -> expectedMessagesID,
					"ExpectedOutputID" -> expectedOutputID,
					"SameTest" -> sameTest,
					"MessagesEquivalenceFunction" -> msgsEquivFunc,
					"TestFailureMessage" -> testFailureMessage,
					"TestFailureAction" -> testFailureAction,
					"TestCPUTimeUsed" -> cpuTimeUsed,
					"TestMemoryUsed" -> memoryUsed,
					"TestTags" -> testTags,
					"TestSource" -> testSource
				];
	]
]


(* ::Subsection:: *)
(*$FakeTestSuiteLoggerOutput*)


$FakeTestSuiteLoggerOutput =
	With[
		{
			expectedActualMessages =
				If[MUnit`Information`$VersionNumber >= 1.2,
					"Expected messages: {}
	Actual messages: {HoldForm[Message[Which::argctu, HoldForm[Which]]]}"
				(* else *),
					"Expected messages: HoldForm[{}]
	Actual messages: HoldForm[{HoldForm[Which::argctu]}]"
				]
		}
		,
		LoggedTestRunStart["suite.mt"] <> "
.!
Test number 1 with TestID FailedFakeTest had a failure.
	Input: HoldForm[1 + 2]
	Expected output: HoldForm[4]
	Actual output: HoldForm[3]
*
Test number 1 with TestID MessageFailedFakeTest had a messages failure.
	Input: HoldForm[Message[Which::argctu, HoldForm[Which]]]
	" <> expectedActualMessages <> "
.!
Test number 2 with TestID FailedFakeTest had a failure.
	Input: HoldForm[2 + 2]
	Expected output: HoldForm[5]
	Actual output: HoldForm[4]
*
Test number 3 with TestID MessageFailedFakeTest had a messages failure.
	Input: HoldForm[Message[Which::argctu, HoldForm[Which]]]
	" <> expectedActualMessages <> "
" <> LoggedTestRunExpressionsNo[10] <> "

Tests run: 6,  Failures: 2,  Messages Failures: 2,  " <>
$LoggedTestRunSkipped <> ": 0, Errors: 0"
	]


End[]


EndPackage[]
