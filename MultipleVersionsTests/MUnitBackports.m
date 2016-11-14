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


BeginPackage["MultipleVersionsTests`MUnitBackports`", {"MUnit`"}]


(* ::Section:: *)
(*Usage messages*)


(* ::Subsection:: *)
(*Public symbols usage*)


TestRunBackported::usage =
"\
TestRunBackported[tests] \
runs tests and returns True if all tests pass, and False otherwise. \
Can accept path to test suite file even if used version of MUnit does not \
support this feature."


(*
	Unprotect all symbols in this context
	(all public symbols provided by this package)
*)
Unprotect["`*"]


(* ::Subsection:: *)
(*Private symbols usage*)
	

Begin["`Private`"]


optNamePatt::usage =
"\
optNamePatt[\"name\"] \
returns a pattern object matching given string \"name\" and symbol with such \
name from any context."


$defaultEquivalenceFunction::usage =
"\
$defaultEquivalenceFunction \
is a function used as default value of EquivalenceFunction option of test \
functions that have both EquivalenceFunction and SameTest options. \
EquivalenceFunction options with literal $defaultEquivalenceFunction symbol \
as value are never renamed to SameTest options."


fixTestOpts::usage =
"\
fixTestOpts[optsList, wrongOptPatt, correctOpt] \
returns a list of given options with options, which name matches \
wrongOptPatt, renamed to correctOpt. Options with literal \
$defaultEquivalenceFunction symbol as value are not renamed."


fixTestOptsDownValue::usage =
"\
fixTestOptsDownValue[wrongOptPatt, correctOptName] \
returns a down value for Test converting options matching wrongOptPatt to \
options with correctOptName."


optionsWithLoggers::usage =
"\
optionsWithLoggers[loggers, explicitOpts, defaultFromFunc, toFunc] \
returns list of options for toFunc with values taken from given explicitOpts \
and value of Loggers option being loggers. If option is not present in \
explicitOpts it's value is taken from default options of defaultFromFunc. \
Also Automatic value of TestRunTitle option is replaced by None for \
compatibility with MUnit <= 1.3."


muteLoggers::usage =
"\
muteLoggers[loggers] \
modify up values of given logers switching off \"test suite level\" log \
functions and return list of unmodified loggers up values."


aggregateLogger::usage =
"\
aggregateLogger[] \
returns an MUnit logger that aggregates results from \"test suite level\" log \
functions in internal counters."


testListRun::usage =
"\
testListRun[{tests1, tests2, ...}] \
runs all testsi and returns True if all of them pass, and False otherwise."


getTestSuite::usage =
"\
getTestSuite[testStream, allowedExpressionsPattern] \
returns TestSuite expression if it's found in given input stream testStream. \
If end of file or expression other than TestSuite and non matching \
allowedExpressionsPattern is encountered $Failed is returned."


logFatalEnd::usage =
"\
logFatalEnd[loggers, message] \
logs fatal message and end messages on all given loggers."


testsOrSuiteRun::usage =
"\
testsOrSuiteRun[tests] \
runs tests and returns True if all tests pass, and False otherwise. \
If tests is a path to test suite file than all tests from test suite are used."


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*Imports*)


Get["MultipleVersionsTests`Package`"]


(* ::Subsection:: *)
(*MUnit symbols definitions*)


(*
	Make sure TestSuite is in MUnit` context even if it's not defined in used
	version of MUnit.
*)
MUnit`TestSuite


(* ::Subsection:: *)
(*MUnit v1.0 TestRun patches*)


If[MUnit`Information`$VersionNumber == 1.0,
	DownValues[TestRun] = DownValues[TestRun] /. {
		(*
			Wrap whole definition with Block localizing values of some global
			MUnit`Package` variables. This makes using TestRun inside other
			TestRun possible.
		*)
		RuleDelayed[
			(* TestRun definition for InputStream *)
			(lhs:HoldPattern[HoldPattern][HoldPattern[TestRun][
				HoldPattern[Pattern][_, HoldPattern[Blank][InputStream]],
				___
			]])
			,
			(* Match Module to prevent duplicate replacements. *)
			module_Module
		] :>
			RuleDelayed[
				lhs
				,
				(* Block global variables. *)
				Block[
					{
						MUnit`Package`$abort,
						MUnit`Package`$canary,
						MUnit`Package`$loggers,
						MUnit`Package`$TestIndex,
						MUnit`Package`$testRunExpressionCount,
						MUnit`Package`$TestRunByteCounter,
						MUnit`Package`$sectionStack
					}
					,
					module
				]
			]
	} /. {
		(* Abort test run if syntax error is encountered in test stream. *)
		(* Match whole CompoundExpression to prevent duplicate replacements. *)
		HoldPattern[CompoundExpression][
			pre__,
			HoldPattern[Set][
				exprVar_,
				HoldPattern[Read][testStreamVar_, HoldComplete[Expression]]
			],
			post__
		] :>
			CompoundExpression[
				pre,
				(* Report syntax errors in test stream. *)
				Check[
					exprVar = Read[testStreamVar, HoldComplete[Expression]]
					,
					(*Scan[LogFatal[#, "Syntax error in test file"]&, $loggers];*)
					MUnit`Package`$abort = True;
					Break[]
				],
				post
			]
	} /. {
		(*
			Subtract 1 from $TestIndex before logging test run end calculating
			whether test run was successful. If test index is 'n' then there
			where 'n-1' tests. Without this change test run always reports
			number of tests higher by 1 and since it's always higher than real
			number of successful tests, test run always returns False.
		*)
		(*
			Match whole CompoundExpression with LogTestRunProgres and LogEnd
			expressions next to each other to prevent duplicate replacements.
		*)
		HoldPattern[CompoundExpression][
			pre___
			,
			logTestRunProgress:HoldPattern[Scan][
				HoldPattern[Function][_LogTestRunProgress],
				_
			]
			,
			logEnd:HoldPattern[Scan][HoldPattern[Function][_LogEnd], _]
			,
			post___
		] :>
			CompoundExpression[
				pre,
				logTestRunProgress,
				(* If test index is 'n' then there where 'n-1' tests. *)
				MUnit`Package`$TestIndex--,
				logEnd,
				post
			]
	}
]


(* ::Subsection:: *)
(*Fix MUnit v1.3 revision 1.6 test section handling*)


(*	In this specific revision there's a bug that causes reporting of invalid
	section primitive when BeginTestSection with two arguments, or any
	EndTestSection, is encountered. *)
If[MUnit`Information`$VersionNumber == 1.3 && TrueQ[$mUnitRevisionNumber == 1.6],
	DownValues[MUnit`TestRun`Private`testRun] =
		DownValues[MUnit`TestRun`Private`testRun] /. {
			RuleDelayed[
				HoldComplete@BeginTestSection[patt_],
				HoldComplete@BeginTestSection[var_, True]
			] :>
				RuleDelayed[
					HoldComplete@BeginTestSection[patt, require_:True],
					HoldComplete@BeginTestSection[var, require]
				],
			RuleDelayed[
				HoldComplete@Verbatim[Alternatives][EndRequirement, EndIgnore][],
				HoldComplete@EndTestSection[]
			] :>
				RuleDelayed[
					HoldComplete@(EndTestSection|EndRequirement|EndIgnore)[],
					HoldComplete@EndTestSection[]
				]
		}
]


(* ::Subsection:: *)
(*Fix test index counting with multiple loggers*)


If[MatchQ[MUnit`Information`$VersionNumber, 1.3 | 1.4],
	With[
		{
			testRunFunction =
				If[MUnit`Information`$VersionNumber == 1.4,
					Symbol["MUnit`TestRun`Private`lowTestRun"]
				(* else *),
					Symbol["MUnit`TestRun`Private`testRun"]
				]
		}
		,
		DownValues[testRunFunction] = DownValues[testRunFunction] /.
			(*
				Increment test index only once per TestID not once per TestID
				per logger. Wthout this replacement for n loggers each test
				was logged with different test id in different logger. MUnit
				v1.4 reported n * realTestCount is as test count and used this
				value in calculations of whether test was successful. Since
				n * realTestCount is always higher than number of successful
				tests, TestRun always returned False.
			*)
			HoldPattern[Outer][
				HoldPattern[Function][
					HoldPattern[LogTestInfo][
						_,
						_,
						HoldPattern[PreIncrement][$testIndexVar_],
						and$sectionStackExpr_
					]
				],
				$loggersVar_,
				testidsVar_
			] :>
				Scan[
					Function[
						testid
						,
						$testIndexVar++;
						Scan[
							LogTestInfo[
								#,
								testid,
								$testIndexVar,
								and$sectionStackExpr
							]&
							,
							$loggersVar
						]
					]
					,
					testidsVar
				]
	]
]


(* ::Subsection:: *)
(*Test messages handling*)


(*
	Handling of messages changed in MUnit version 1.3.
	Backport new message handler to older versions.
*)
If[MUnit`Information`$VersionNumber < 1.3,
	Clear[MUnit`Test`Private`MUnitMessageHandler];
	
	MUnit`Test`Private`MUnitMessageHandler[
		(*ctlString*)_String,
		Hold[msg_MessageName],
		Hold[Message[msg_MessageName, args___]]
	] :=
		If[(* stdout would have been in $Messages *)
			$Messages === {MUnit`Test`Private`$bitbucket} ||
			(* $Messages has been changed, most likely to $Messages or {} *)
			(ListQ[$Messages] && MemberQ[$Messages, OutputStream["stdout", _]])
			,
			AppendTo[
				MUnit`Test`Private`$MessageWithArgsList,
				HoldForm[Message[msg, args]]
			];
		]
]



(*
	MUnit version >= 1.2 accepts as expected messages and handles correctly:
	* MessageName expressions,
	* List of MessageName expressions,
	* Message expressions with arguments not wrapped with HoldForm, all
	  arguments will be automatically wrapped with HoldForm, so this form can't
	  handle matching messages that don't have arguments wrapped with HoldForm,
	* List of above Message expressions,
	* List of HoldForm[Message[...]] expressions, no automatic wrapping of
	  arguments is performed.
	
	Earlier MUnit versions handles correctly only first two points from above
	list.
	
	In MUnit < 1.2 transform all valid forms of given messages to list of
	message names.
*)
If[MUnit`Information`$VersionNumber < 1.2,
	DownValues[Test] =
		DeleteDuplicates @ Prepend[
			DownValues[Test]
			,
			HoldPattern @ Test[
				input_,
				expected_,
				messages:(_Message | {___, _Message|HoldForm[_Message], ___}),
				opts___?OptionQ
			] :>
				Module[
					{heldMessageNames}
					,
					(* Replace one message with list containing one message. *)
					heldMessageNames =
						Replace[
							Hold[messages],
							Hold[msg_Message] :> Hold[{msg}]
						];
					(*
						Replace Message expressions and Message expressions
						wrapped in HoldForm with message names.
					*)
					heldMessageNames =
						Replace[
							heldMessageNames
							,
							{
								HoldPattern[Message][mn_MessageName, ___] :>
									mn
								,
								HoldPattern[HoldForm][
									HoldPattern[Message][mn_MessageName, ___]
								] :>
									mn
							}
							,
							{2}
						];
					
					(*
						Pass list of message names without evaluating it. Test
						has HoldAllComplete attribute and in MUnit < 1.2
						accepts only messages argument matching certain pattern
						so standard Unevaluated won't work.
					*)
					Function[
						messageNames,
						Test[input, expected, messageNames, opts],
						HoldAll
					] @@
						heldMessageNames
				]
		]
]


(* ::Subsection:: *)
(*Use all option names recognized by OptionValue*)


Composition[(#[name_, opts_, sym_] := OptionValue[sym, opts, name])&, Symbol] /@
	Names["MUnit`*`compatOptionValue"]


(* ::Subsection:: *)
(*optNamePatt*)


optNamePatt[name_String] := Alternatives[
	name,
	sym:Except[HoldPattern@Symbol[___], _Symbol] /;
		SymbolName@Unevaluated@sym === name
]


(* ::Subsection:: *)
(*$defaultEquivalenceFunction*)


$defaultEquivalenceFunction = SameQ


(* ::Subsection:: *)
(*fixTestOpts*)


fixTestOpts[optsList_List, wrongOptPatt_, correctOpt_Symbol] :=
	Replace[optsList,
		h_[wrongOptPatt, rhs:Except@HoldPattern@$defaultEquivalenceFunction] :>
			h[correctOpt, rhs],
		{1}
	]


(* ::Subsection:: *)
(*fixTestOptsDownValue*)


fixTestOptsDownValue[wrongOptPatt_, correctOpt_Symbol] =
	HoldPattern@Test[
		input_, expected_:True, Shortest[expectedMsgs_:{}],
		opts:OptionsPattern[]
	] :> With[{optsList = Flatten@{opts}},
		With[{newOptsList = fixTestOpts[optsList, wrongOptPatt, correctOpt]},
			Test[input, expected, expectedMsgs, newOptsList] /;
				newOptsList =!= optsList
		]
	]


(* ::Subsection:: *)
(*Test options compatibility*)


(*	MUnit v1.4 renamed EquivalenceFunction option to SameTest.
	
	Add definition of Test function that changes option name to one compatible
	with used version of MUnit (don't override option names symbols because it
	will change their behavior also in places other than Test options which is
	not desirable, especially for SameTest symbol). *)
With[
	{
		wrongOptPatt = optNamePatt@
			If[Quiet[Options[Test, SameTest] === {}, Options::optnf],
				"SameTest"
			(* else *),
				"EquivalenceFunction"
			],
		correctOpt =
			If[Quiet[Options[Test, SameTest] === {}, Options::optnf],
				MUnit`EquivalenceFunction
			(* else *),
				SameTest
			],
		setDefaultEquivalenceFunction =
			If[Quiet[
				Length@Options[Test, {SameTest, "EquivalenceFunction"}] === 2,
				Options::optnf
			]
			(* then *),
				(
					Quiet[# /: SetOptions[#, opts:OptionsPattern[]] =.,
						TagUnset::norep
					];
					SetOptions[#,
						"EquivalenceFunction" :> $defaultEquivalenceFunction
					]
				)&
			(* else *),
				Identity
			]
	}
	,
	(*	Add proper option fixing definition as first down value. *)
	DownValues[Test] = Prepend[
		DeleteCases[DownValues[Test],
			(* Remove option fixing definitions if they exist. *)
			Verbatim@fixTestOptsDownValue[optNamePatt@SymbolName@#1, #2]& @@@
				(# | Reverse@# &)@{SameTest, MUnit`EquivalenceFunction}
		]
		,
		fixTestOptsDownValue[wrongOptPatt, correctOpt]
	];
	
	(
		setDefaultEquivalenceFunction@#;
		# /: SetOptions[#, opts:OptionsPattern[]] :=
			With[{optsList = Flatten@{opts}},
				With[{newOptsList = fixTestOpts[optsList, wrongOptPatt, correctOpt]},
					SetOptions[#, newOptsList] /; newOptsList =!= optsList
				]
			]
	)& /@ {
		(* MUnit`Test` *)
		Test, TestMatch, TestStringMatch, TestFree, TestStringFree,
		(* MUnit`WRI` *)
		ConditionalTest, ExactTest, ExactTestCaveat, NTest, NTestCaveat,
		OrTest, TestCaveat
	}
]


(* ::Subsection:: *)
(*optionsWithLoggers*)


optionsWithLoggers[
	loggers_,
	explicitOpts___?OptionQ,
	defaultFromFunc_Symbol,
	toFunc_Symbol
] :=
	Replace[
		DeleteDuplicates[
			FilterRules[
				Flatten[{
					Loggers -> loggers,
					explicitOpts,
					Options[defaultFromFunc]
				}]
				,
				Options[toFunc]
			]
			,
			SameQ @@ First /@ {##}&
		]
		,
		(rule: Rule | RuleDelayed)[TestRunTitle, Automatic] :>
			rule[TestRunTitle, None]
		,
		{1}
	]


(* ::Subsection:: *)
(*muteLoggers*)


muteLoggers[loggers_List] :=
	With[
		{originalUpValues = UpValues /@ loggers}
		,
		Scan[
			(
				UpValues[#] =
					DeleteCases[
						UpValues[#]
						,
						HoldPattern[HoldPattern][
							_LogStart | _LogBegin | _LogEnd |
							_LogBeginTestSource | _LogEndTestSource |
							_LogExpressionCount | _LogCPUTimeUsed |
							_LogAbsoluteTimeUsed | _LogMemoryUsed |
							_LogTestCount | _LogSuccessCount |
							_LogFailureCount | _LogMessagesFailureCount |
							_LogSkippedTestCount | _LogErrorCount |
							_LogWasAborted | _LogWasFatal | _LogWasSyntax |
							_LogWasSuccessful
						] :> _
					];
			)&
			,
			loggers
		];
		
		originalUpValues
	]


(* ::Subsection:: *)
(*aggregateLogger*)


aggregateLogger[] :=
	With[
		{logger = Unique["MUnit`Loggers`Private`logger"]}
		,
		Options[logger] = {
			"UsedLogFunctions" -> {}
			,
			LogExpressionCount -> 0
			,
			LogCPUTimeUsed -> 0,
			LogAbsoluteTimeUsed -> 0,
			LogMemoryUsed -> 0
			,
			LogTestCount -> 0,
			LogSuccessCount -> 0,
			LogFailureCount -> 0,
			LogMessagesFailureCount -> 0,
			LogSkippedTestCount -> 0,
			LogErrorCount -> 0
			,
			"AbortOrFatal" -> False,
			LogWasAborted -> False,
			LogWasFatal -> False,
			LogWasSyntax -> False,
			LogWasSuccessful -> True
		};
		
		logger /: LogExpressionCount[logger, x_] :=
			SetOptions[logger,
				LogExpressionCount ->
					OptionValue[logger, LogExpressionCount] + x
				,
				"UsedLogFunctions" ->
					Append[
						OptionValue[logger, "UsedLogFunctions"],
						LogExpressionCount
					]
			];
		
		(
			logger /: #[logger, x_] :=
				SetOptions[logger, # -> OptionValue[logger, #] + x]
		)& /@
			{LogCPUTimeUsed, LogAbsoluteTimeUsed, LogMemoryUsed};
		
		(
			logger /: #[logger, x_] :=
				SetOptions[logger,
					# -> OptionValue[logger, #] || x
					,
					"UsedLogFunctions" ->
						Append[OptionValue[logger, "UsedLogFunctions"], #]
				]
		)& /@
			{LogWasAborted, LogWasFatal, LogWasSyntax};
		
		logger /: LogWasSuccessful[logger, x_] :=
			SetOptions[logger,
				LogWasSuccessful -> OptionValue[logger, LogWasSuccessful] && x
			];
		
		
		logger /:
			LogEnd[
				logger,
				testCnt_,
				successCnt_,
				failureCnt_,
				msgFailureCnt_,
				skippedTestCnt_,
				errorCnt_,
				abortOrFatal_
			] := (
				SetOptions[logger,
					(#1 -> OptionValue[logger, #1] + #2)& @@@
						{
							LogTestCount -> testCnt,
							LogSuccessCount -> successCnt,
							LogFailureCount -> failureCnt,
							LogMessagesFailureCount -> msgFailureCnt,
							LogSkippedTestCount -> skippedTestCnt,
							LogErrorCount -> errorCnt
						}
					,
					"AbortOrFatal" ->
						OptionValue[logger, "AbortOrFatal"] || abortOrFatal
				];
				
				(*
					Recalculate "was successful" here since MUnit <1.3 don't
					use LogWasSuccessful.
				*)
				SetOptions[logger,
					LogWasSuccessful ->
						OptionValue[logger, LogWasSuccessful] &&
						(OptionValue[logger, LogSuccessCount] ==
							OptionValue[logger, LogTestCount]) &&
						(OptionValue[logger, LogErrorCount] == 0) &&
						!OptionValue[logger, "AbortOrFatal"]
				];
				
				If[OptionValue[logger, "AbortOrFatal"],
					Throw["Test run interrupted.", multipleVersionsTestsTag];
				];
			);
		
		logger /: Format[logger, StandardForm] :=
			Interpretation[
				Row[{
					RawBoxes["\[SkeletonIndicator]"],
					RawBoxes["Aggregate Logger"],
					RawBoxes["\[SkeletonIndicator]"]
				}]
				,
				logger
			];
		
		logger /: Format[logger, OutputForm] := "-Aggregate Logger-";
		
		logger
	]


(* ::Subsection:: *)
(*getTestSuite*)


getTestSuite[testStream_InputStream, allowedPatt_] :=
	Module[{expr},
		While[True,
			expr = Read[testStream, HoldComplete[Expression]];

			Switch[expr,
				HoldComplete[_TestSuite],
					Return[First[expr]]
				,
				allowedPatt,
					Continue[]
				,
				_,
					Return[$Failed]
			];
		];

		$Failed
	]


(* ::Subsection:: *)
(*logFatalEnd*)


logFatalEnd[loggers_List, message_] :=
	Scan[
		(
			LogFatal[#, ToString[message]];
			LogEnd[#, 0, 0, 0, 0, 0, 0, True];
			LogEnd[#];
		)&
		,
		loggers
	]


(* ::Subsection:: *)
(*testsOrSuiteRun*)


Options[testsOrSuiteRun] = Options[TestRun];
AppendTo[
	Options[testsOrSuiteRun],
	"AllowedExpressionsPattern" -> HoldComplete[Null]
];
SetOptions[testsOrSuiteRun, TestRunTitle -> Automatic]

SetAttributes[testsOrSuiteRun, HoldFirst]


testsOrSuiteRun[testFileName_String, opts:OptionsPattern[]] :=
	Module[
		{
			title = OptionValue[TestRunTitle],
			loggers = OptionValue[Loggers],
			testStream = Quiet[OpenRead[testFileName]],
			$aggregateLogger = aggregateLogger[],
			testSuite,
			validTestSuite = False,
			testsList,
			testRunOptions,
			originalUpValues,
			usedLogFunctions,
			$results = {False}
		}
		,
		
		If[title === Automatic,
			(*
				FileNameTake was introduced in v7, handle also earlier
				versions.
			*)
			If[ValueQ[System`FileNameTake[testFileName]],
				title = "Test Report: " <> System`FileNameTake[testFileName]
			(* else *),
				title = None
			]
		];
		
		Scan[
			(
				LogStart[#, title];
				LogBegin[#, title];
				LogStart[#, title, testFileName];
				LogBegin[#, title, testFileName];
			)&
			,
			loggers
		];
		If[testStream === $Failed,
			logFatalEnd[
				loggers,
				"Could not open test file\n" <> testFileName <> "."
			];
			Return[False]
		];
		
		Block[
			{TestSuite}
			,
			testSuite =
				getTestSuite[
					testStream,
					OptionValue["AllowedExpressionsPattern"]
				];
			
			Close[testStream];
			
			Switch[testSuite,
				$Failed,
					(*
						This is not a valid test suite file so assume it's a
						test file.
					*)
					testsList = {testFileName}
				,
				HoldPattern[TestSuite][{___String}],
					(*
						Valid TestSuite found in given file.
						Run tests on all files in test suite.
					*)
					validTestSuite = True;
					testsList =
						FileNameJoin[{DirectoryName[testFileName], #}]& /@
			 				First[testSuite]
				,
				_,
					(* TestSuite found in given file, but it's invalid. *)
					logFatalEnd[
						loggers,
						StringForm["Invalid TestSuite expression: `1`.", testSuite]
					];
					Return[False]
				]
		];
		
		If[validTestSuite,
			Scan[LogBeginTestSource[#, testFileName]&, loggers]
		];
		
		testRunOptions =
			optionsWithLoggers[
				Append[loggers, $aggregateLogger],
				opts,
				testsOrSuiteRun,
				TestRun
			];
		originalUpValues = muteLoggers[loggers];
		
		Catch[
			$results =
				Function[
					tests
					,
					Module[
						{testRunResult}
						,
						MapThread[
							(LogBeginTestSource[#1, tests] /. #2)&,
							{loggers, originalUpValues}
						];
						
						testRunResult = TestRun[tests, testRunOptions];
						
						MapThread[
							(LogEndTestSource[#1] /. #2)&,
							{loggers, originalUpValues}
						];
						
						testRunResult
					]
				] /@
					testsList
			,
			multipleVersionsTestsTag
		];
		
		(* Restore original logerrs up values. *)
		MapThread[(UpValues[#1] = #2)&, {loggers, originalUpValues}];
		
		If[validTestSuite,
			Scan[LogEndTestSource, loggers]
		];
		
		usedLogFunctions = OptionValue[$aggregateLogger, "UsedLogFunctions"];
		
		Scan[
			Function[
				logger
				,
				If[MemberQ[usedLogFunctions, LogExpressionCount],
					LogExpressionCount[
						logger,
						OptionValue[$aggregateLogger, LogExpressionCount]
					]
				];
				
				Scan[
					#[logger, OptionValue[$aggregateLogger, #]]&
					,
					{
						LogCPUTimeUsed, LogAbsoluteTimeUsed, LogMemoryUsed,
						LogTestCount, LogSuccessCount, LogFailureCount,
						LogMessagesFailureCount, LogSkippedTestCount,
						LogErrorCount
					}
				];
				
				Scan[
					If[MemberQ[usedLogFunctions, #],
						#[logger, OptionValue[$aggregateLogger, #]]
					]&
					,
					{LogWasAborted, LogWasFatal, LogWasSyntax}
				];
				
				LogWasSuccessful[
					logger,
					OptionValue[$aggregateLogger, LogWasSuccessful]
				];
				
				LogEnd[logger,
					Sequence @@ OptionValue[$aggregateLogger, {
						LogTestCount, LogSuccessCount, LogFailureCount,
						LogMessagesFailureCount, LogSkippedTestCount,
						LogErrorCount, "AbortOrFatal"
					}]
				];
				LogEnd[logger];
			]
			,
			loggers
		];
		
		And @@ $results
	]

(*
	testsOrSuiteRun has attribute HoldFirst since it can accept list of tests
	and we want to evaluate them inside testsOrSuiteRun. But if first argument
	is not an explicit list assume it can be evaluated. If it evaluates to any
	form accepted by TestRun proceed with ordinary testsOrSuiteRun.
*)
testsOrSuiteRun[
	tests:Except[_String | _List | _NotebookObject | _InputStream],
	opts:OptionsPattern[]
] :=
	With[
		{testsEvaluated = tests}
		,
		testsOrSuiteRun[testsEvaluated, opts]
			/; MatchQ[testsEvaluated,
				_String | _List | _NotebookObject | _InputStream
			]
	]

testsOrSuiteRun[tests_, opts:OptionsPattern[]] :=
	TestRun[
		tests,
		optionsWithLoggers[
			OptionValue[Loggers],
			opts,
			testsOrSuiteRun,
			TestRun
		]
	]


(* ::Subsection:: *)
(*TestRunBackported*)


TestRunBackported =
	If[MUnit`Information`$VersionNumber >= 1.4,
		TestRun
	(* else *),
		testsOrSuiteRun
	]



End[]


(* ::Section:: *)
(*Public symbols protection*)


(*
	Protect all symbols in this context
	(all public symbols provided by this package)
*)
Protect["`*"]


EndPackage[]
