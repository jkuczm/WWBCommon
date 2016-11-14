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


BeginPackage["MultipleVersionsTests`", {"MUnit`"}]


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Usage messages*)


(* ::Subsection:: *)
(*Public symbols usage*)


TestRunMultipleVersions::usage =
"\
TestRunMultipleVersions[tests, mathExecutables] \
runs tests in all given mathematica versions. Returns True if all tests pass \
in all versions, and False otherwise. mathExecutables should be a list of \
strings with paths to mathematica executables."


WSTPLinkLogger::usage =
"\
WSTPLinkLogger[link] \
returns an MUnit logger that writes every logged message to given link."


TestLoggerPacket::usage =
"\
TestLoggerPacket[logFunction, arg1, arg2, ...] \
is a packet sent by WSTPLinkLogger to a WSTP link associated with \
wstpLoggerInstance when logFunction[wstpLoggerInstance, arg1, arg2, ...] is \
evaluated. logFunction can be any of MUnit`Log... functions."


(* ::Subsection:: *)
(*Private symbols usage*)
	

Begin["`Private`"]


ClearAll["`*"]


logVersionRunTests::usage =
"\
logVersionRunTests[tests, options] \
logs used mathematica version to Loggers from options and runs given tests \
with given options."


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*Imports*)


Get["MultipleVersionsTests`Package`"]
Get["MultipleVersionsTests`MUnitBackports`"]
Get["MultipleVersionsTests`Serialization`"]


Needs["WSTPUtilities`"]


(* ::Subsection:: *)
(*WSTPLinkLogger*)


WSTPLinkLogger[link_LinkObject] :=
	With[
		{logger = Unique["MUnit`Loggers`Private`logger"]}
		,
		Options[logger] = {LinkObject -> link};
		
		Scan[
			(
				logger /: #[logger, args___] := (
					LinkFullContextWrite[
						link
						,
						TestLoggerPacket[
							#,
							Sequence @@ Replace[
								{args},
								tr_?TestResultQ :> SerializeTestResult[tr],
								{1}
							]
						]
					]
				)
			)&
			,
			$logFunctions
		];
		
		logger /: LogBeginTestSource[logger, source_] := (
			LinkFullContextWrite[link,
				Unevaluated @ EvaluatePacket[
					If[MatchQ[MUnit`Package`testSourceStack, _List],
						AppendTo[MUnit`Package`testSourceStack, source]
					];
				]
			];
			LinkFullContextWrite[link,
				TestLoggerPacket[LogBeginTestSource, source]
			]
		);
		
		logger /: LogEndTestSource[logger] := (
			LinkFullContextWrite[link,
				Unevaluated @ EvaluatePacket[
					If[MatchQ[MUnit`Package`testSourceStack, _List],
						MUnit`Package`testSourceStack =
							Most[MUnit`Package`testSourceStack]
					];
				]
			];
			LinkFullContextWrite[link,
				TestLoggerPacket[LogEndTestSource]
			]
		);
		
		logger /: Format[logger, StandardForm] :=
			Interpretation[
				Row[{
					RawBoxes["\[SkeletonIndicator]"],
					RawBoxes["WSTP Link Logger"],
					RawBoxes["\[SkeletonIndicator]"]
				}]
				,
				logger
			];
		
		logger /: Format[logger, OutputForm] := "-WSTP Link Logger-";
		
		logger
	]


(* ::Subsection:: *)
(*LogVersionRunTest*)


logVersionRunTests =
	Function[
		{tests, options}
		,
		Scan[
			LogMessage[
				#,
				"Running tests in Mathematica version: " <> $Version <> ". " <>
				MUnit`Information`$Version
			]&
			,
			OptionValue[options, Loggers]
		];
		TestRunBackported[
			tests,
			FilterRules[options, Options[TestRunBackported]]
		]
		,
		HoldFirst
	]


(* ::Subsection:: *)
(*TestRunMultipleVersions*)


SetAttributes[TestRunMultipleVersions, HoldFirst];

Options[TestRunMultipleVersions] =
	Join[
		Options[TestRunBackported]
		,
		{
			"ExtraPath" -> Automatic,
			"LinkCallingVersion" -> False,
			"RemoteLoggers" :> {WSTPLinkLogger[$ParentLink]},
			"Init" -> None
		}
	];


TestRunMultipleVersions[
	tests:(_String | _List), mathExecutables:{__String}, opts:OptionsPattern[]
] :=
	And @@ Flatten[{
		TestRunMultipleVersions[tests, First[mathExecutables], opts]
		,
		(
			Scan[
				LogMessage[#, "-------------------------------"]&,
				OptionValue[Loggers]
			];
			TestRunMultipleVersions[tests, #, opts]
		)& /@
			Rest[mathExecutables]
	}]

TestRunMultipleVersions[
	tests:(_String | _List), mathExecutable_String, opts:OptionsPattern[]
] :=
	With[
		{
			options = Flatten[{opts, Options[TestRunMultipleVersions]}],
			extraPathOption = OptionValue["ExtraPath"],
			heldInit = OptionValue[Automatic, Automatic, "Init", Hold]
		}
		,
		If[
			!OptionValue["LinkCallingVersion"] &&
			mathExecutable === First[$CommandLine]
			,
			Block[
				{$Path = $Path}
				,
				If[extraPathOption =!= Automatic,
					$Path = Join[extraPathOption, $Path];
				];
				ReleaseHold[heldInit];
				
				Return @ logVersionRunTests[tests, options]
			]
		];
		With[
			{
				link = LinkLaunch[mathExecutable <> " -mathlink -noprompt"],
				workbenchMUnitPath =
					MultipleVersionsTests`MUnitVersionedLoader`$WorkbenchMUnitPath,
				optionsWithLoggers =
					Replace[
						FilterRules[options, Except[Loggers]]
						,
						{
							rule_["RemoteLoggers", val_] :>
								rule["Loggers", val]
							,
							(*
								Some option names where moved from MUnit` to
								System` context in MUnit v1.4, so make sure
								they are passed as strings not symbols.
							*)
							rule_[opt_Symbol, val_] :>
								rule[SymbolName[opt], val]
						}
						,
						{1}
					]
				,
				extraPath =
					If[extraPathOption === Automatic,
						Select[
							$Path,
							!StringMatchQ[#, $InstallationDirectory <> "/*"]&
						]
					(* else *),
						extraPathOption
					]
				,
				loggers = OptionValue[Loggers]
			}
			,
			Block[
				{
					MUnit`Package`testSourceStack = {None},
					MUnit`$CurrentTestSource,
					MUnit`$CurrentFile
				}
				,
				MUnit`$CurrentTestSource :=
					Last[MUnit`Package`testSourceStack];
				MUnit`$CurrentFile :=
					MUnit`$CurrentTestSource /. _NotebookObject -> None;
				
				Module[
					{result}
					,
					LinkDelegateEvaluation[
						link
						,
						$Path = DeleteDuplicates @ Join[extraPath, $Path];
						MultipleVersionsTests`MUnitVersionedLoader`$WorkbenchMUnitPath
							= workbenchMUnitPath;
						Needs["MultipleVersionsTests`"];
					];
					
					If[heldInit =!= Hold[None],
						LinkDelegateEvaluation[link, ReleaseHold[heldInit]];
					];
					
					result =
						LinkDelegateEvaluation[
							link
							,
							logVersionRunTests[tests, optionsWithLoggers]
							,
							"HandlePacketDecorator" -> ((
								#[TestLoggerPacket[logFunction_, args___]] :=
									Scan[
										logFunction[
											#
											,
											Sequence @@ Replace[
												{args}
												,
												tr_TestResultSerialized :>
													DeserializeTestResult[tr]
												,
												{1}
											]
										]&
										,
										loggers
									];
							)&)
						];
					
					LinkClose[link];
					
					result
				]
			]
		]
	]

(*
	TestRunMultipleVersions has attribute HoldFirst since it can accept list of
	tests and we want to evaluate them inside TestRunMultipleVersions. But if
	first argument is not an explicit list assume it can be evaluated. If it
	evaluates to string or list proceed with ordinary TestRunMultipleVersions.
*)
TestRunMultipleVersions[
	tests:Except[_String | _List],
	mathExecutables_,
	opts:OptionsPattern[]
] :=
	With[
		{testsEvaluated = tests}
		,
		TestRunMultipleVersions[testsEvaluated, mathExecutables, opts]
			/; MatchQ[testsEvaluated, _String | _List]
	]


End[]


(* ::Section:: *)
(*Public symbols protection*)


Protect["`*"]


EndPackage[]
