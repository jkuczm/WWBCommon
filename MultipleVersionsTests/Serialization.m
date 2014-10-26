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


BeginPackage["MultipleVersionsTests`Serialization`", {"MUnit`"}]


(* ::Section:: *)
(*Usage messages*)


(* ::Subsection:: *)
(*Public symbols usage*)


TestResultSerialized::usage =
"\
TestResultSerialized[\"selsctor1\" -> val1, \"selector2\" -> val2, ...] \
represents serialized version of test result. It's stateless, independent of
MUnit version and can be sent through WSTP link."


SerializeTestResult::usage =
"\
SerializeTestResult[tr] \
returns \"serialized\" version of given test result object tr suitable to \
transfer through WSTP link."


DeserializeTestResult::usage =
"\
DeserializeTestResult[trSerialized] \
returns, proper for used version of MUnit, test result object with data taken \
from given serialized test result trSerialized."


(*
	Unprotect all symbols in this context
	(all public symbols provided by this package)
*)
Unprotect["`*"]


(* ::Subsection:: *)
(*Private symbols usage*)
	

Begin["`Private`"]


optionNameReplacementRules::usage =
"\
optionNameReplacementRules[{old1 -> new1, old2 -> new2, ...}] \
returns rules that can be used to replace old option names with new names in \
oprion rules list."


$testResultOptNames::usage =
"\
$testResultOptNames \
is a list of test result option names, from currently used versions of MUnit."


$testResultNonOptSelectors::usage =
"\
$testResultNonOptSelectors \
is a list of test result selectors, that are not test result options, from \
currently used versions of MUnit."


$testResultSelectors::usage =
"\
$testResultSelectors \
is a list of test result selectors from currently used versions of MUnit."


$testResultOptSerialization::usage =
"\
$testResultOptSerialization \
is a list of transformation rules unifying test result option names from \
different versions of MUnit."


$testResultOptDeserialization::usage =
"\
$testResultOptDeserialization \
is a list of transformation rules translating option names from serialized \
test result to option names of test result from used version of MUnit."


extractTest::usage =
"\
extractTest[tr] \
returns test expression extracted from given test result object tr if used
version of MUnit supports this feature, otherwise returns None."


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*Imports*)


Get["MultipleVersionsTests`Package`"]


(* ::Subsection:: *)
(*optionNameReplacementRules*)


optionNameReplacementRules =
	Function[(rule_[#1, value_] :> rule[#2, value])& @@@ Flatten[{##}]]


(* ::Subsection:: *)
(*$testResultNonOptSelectors*)


$testResultNonOptSelectors = {
	(*
		Make sure all non-option selectors are in MUnit` context,
		even if they are not defined in used version of MUnit.
	*)
	MUnit`Test,
	MUnit`FailureMode,
	MUnit`TestInput,
	MUnit`ExpectedOutput,
	MUnit`ActualOutput,
	MUnit`ExpectedMessages,
	MUnit`ActualMessages,
	MUnit`ErrorMessage
}


(* ::Subsection:: *)
(*$testResultOptNames*)


$testResultOptNames =
	First /@ Which[
		(*
			Use Symbol["..."] to avoid declaring symbols in this context,
			when they are not declared in used version of MUnit.
		*)
		MUnit`Information`$VersionNumber >= 1.4,
			Options[Symbol["TestResult"]],
		MUnit`Information`$VersionNumber >= 1.2,
			Options[Symbol["TestResultObject"]],
		True,
			Append[
				Options[Symbol["TestResultObject"]],
				Symbol["TestTags"] -> {}
			]
	]


(* ::Subsection:: *)
(*$testResultSelectors*)


$testResultSelectors =
	Join[Rest[$testResultNonOptSelectors], $testResultOptNames]


(* ::Subsection:: *)
(*$testResultOptSerialization*)


$testResultOptSerialization =
	optionNameReplacementRules[
		"EquivalenceFunction" -> "SameTest",
		"TestTimeUsed" -> "TestCPUTimeUsed",
		"TestFileChild" -> "TestSource"
	]


(* ::Subsection:: *)
(*$testResultOptDeserialization*)


$testResultOptDeserialization =
	Which[
		MUnit`Information`$VersionNumber >= 1.4,
			{}
		,
		MUnit`Information`$VersionNumber >= 1.3,
			optionNameReplacementRules[
				"SameTest" -> "EquivalenceFunction",
				"TestCPUTimeUsed" -> "TestTimeUsed"
			]
		,
		MUnit`Information`$VersionNumber >= 1.2,
			optionNameReplacementRules[
				"SameTest" -> "EquivalenceFunction",
				"TestCPUTimeUsed" -> "TestTimeUsed",
				"TestSource" -> "TestFileChild"
			]
		,
		True,
			optionNameReplacementRules[
				"SameTest" -> "EquivalenceFunction",
				"TestCPUTimeUsed" -> "TestTimeUsed"
			]
	]


(* ::Subsection:: *)
(*extractTest*)


If[MUnit`Information`$VersionNumber >= 1.3,
	extractTest = None&
(* else *),
	extractTest = First
]


(* ::Subsection:: *)
(*SerializeTestResult*)


SerializeTestResult[tr_?TestResultQ] :=
	(*
		Use Sort @ DeleteDuplicates instead of Union since we want to keep
		first (before sorting) occurence of each option.
	*)
	TestResultSerialized @@ Sort @ DeleteDuplicates[
		Flatten[{
			Replace[
				Reap[
					Scan[
						If[ValueQ[#[tr]],
							Sow[SymbolName[#] -> #[tr], multipleVersionsTestsTag]
						]&
						,
						$testResultSelectors
					]
					,
					multipleVersionsTestsTag
				][[2, 1]]
				,
				$testResultOptSerialization
				,
				{1}
			]
			,
			"Test" -> extractTest[tr],
			"ErrorMessage" -> "",
			"TestSource" -> $CurrentFile
		}]
		,
		SameQ @@ First /@ {##}&
	]


(* ::Subsection:: *)
(*DeserializeTestResult*)


With[
	{
		trHead =
			(*
				Use Symbol["..."] to avoid declaring symbols, when they are not
				declared in used version of MUnit.
			*)
			Symbol @ Which[
				MUnit`Information`$VersionNumber >= 1.4,
					"MUnit`Test`Private`newTestResult",
				MUnit`Information`$VersionNumber >= 1.3,
					"MUnit`Test`Private`newTestResultObject",
				True,
					"TestResultObject"
			]
		,
		nonOptSelectors =
			If[MUnit`Information`$VersionNumber >= 1.2,
				$testResultNonOptSelectors
			(* else *),
				Most[$testResultNonOptSelectors]
			]
	}
	,
	DeserializeTestResult[trSerialized_TestResultSerialized] :=
		With[
			{opts = List @@ trSerialized}
			,
			trHead[
				Sequence @@ (OptionValue[opts, #]& /@ nonOptSelectors)
				,
				Sequence @@ (#[[0]][Symbol[#[[1]]], #[[2]]]&) /@
					FilterRules[
						Replace[opts, $testResultOptDeserialization, {1}],
						$testResultOptNames
					]
			]
		]
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
