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


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Usage messages*)


(* ::Subsection:: *)
(*Public symbols usage*)


TestResultSerialized::usage =
"\
TestResultSerialized[\"selector1\" -> val1, \"selector2\" -> val2, ...] \
represents serialized version of test result. It's stateless, independent of \
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


(* ::Subsection:: *)
(*Private symbols usage*)
	

Begin["`Private`"]


ClearAll["`*"]


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


serializeSelecorName::usage =
"\
serializeSelecorName[\"selector\"] \
returns name of given selector in form unified for all versions of MUnit."


deserializeSelecorName::usage =
"\
deserializeSelecorName[\"selector\"] \
returns selector name, apropriate for used version of MUnit, corresponding to \
given serialized selector name."


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*Imports*)


<<MultipleVersionsTests`Package`


(* ::Subsection:: *)
(*$testResultNonOptSelectors*)


$testResultNonOptSelectors = {
	(*	Make sure all non-option selectors are in MUnit` context,
		even if they are not defined in used version of MUnit. *)
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


(*	Use Symbol@"..." to avoid declaring symbols in this context, when they are
	not declared in used version of MUnit.*)
$testResultOptNames = Options[
	Symbol@If[MUnit`Information`$VersionNumber >= 1.4,
		"TestResult"
	(* else *),
		"TestResultObject"
	]
][[All, 1]]

If[MUnit`Information`$VersionNumber < 1.2,
	AppendTo[$testResultOptNames, Symbol@"TestTags"]
]


(* ::Subsection:: *)
(*$testResultSelectors*)


$testResultSelectors =
	Join[Rest@$testResultNonOptSelectors, $testResultOptNames]


(* ::Subsection:: *)
(*serializeSelecorName*)


serializeSelecorName@"EquivalenceFunction" = "SameTest"

serializeSelecorName@"TestInputSetFunction" = "ActualOutputSetFunction"

serializeSelecorName@"TestTimeUsed" = "TestCPUTimeUsed"

serializeSelecorName@"TestFileChild" = "TestSource"

serializeSelecorName@name_String := name


(* ::Subsection:: *)
(*deserializeSelecorName*)


If[MUnit`Information`$VersionNumber < 1.4,
	deserializeSelecorName@"SameTest" = "EquivalenceFunction";
	deserializeSelecorName@"TestCPUTimeUsed" = "TestTimeUsed";
	
	Which[
		MUnit`Information`$VersionNumber == 1.3 &&
				TrueQ[$mUnitRevisionNumber < 1.7],
			deserializeSelecorName@"ActualOutputSetFunction" =
				"TestInputSetFunction"
		,
		MUnit`Information`$VersionNumber == 1.2,
			deserializeSelecorName@"TestSource" = "TestFileChild"
	]
]

deserializeSelecorName@name_String := name


(* ::Subsection:: *)
(*SerializeTestResult*)


With[
	{
		extractTest =
			If[MUnit`Information`$VersionNumber <= 1.3 &&
					TrueQ[$mUnitRevisionNumber <= 1.6]
			(* then *),
				First
			(* else *),
				None&
			]
	}
	,
	SerializeTestResult@tr_?TestResultQ :=
		With[
			{
				rules = Cases[$testResultSelectors,
					sel_ /; ValueQ@sel@tr :>
						serializeSelecorName@SymbolName@sel -> sel@tr
				]
			},
			TestResultSerialized @@ Sort@Join[
				rules,
				FilterRules[
					{
						"Test" -> extractTest[tr],
						"ErrorMessage" -> "",
						"TestSource" -> $CurrentFile
					},
					Except@rules
				]
			]
		]
]


(* ::Subsection:: *)
(*DeserializeTestResult*)


With[
	{
		trHead =
			(*	Use Symbol["..."] to avoid declaring symbols, when they are not
				declared in used version of MUnit. *)
			Symbol@Which[
				MUnit`Information`$VersionNumber >= 1.4,
					"MUnit`Test`Private`newTestResult",
				MUnit`Information`$VersionNumber >= 1.3 &&
						TrueQ[$mUnitRevisionNumber >= 1.7],
					"MUnit`Test`Private`newTestResultObject",
				True,
					"TestResultObject"
			]
		,
		nonOptSelectors =
			If[MUnit`Information`$VersionNumber >= 1.2,
				$testResultNonOptSelectors
			(* else *),
				Most@$testResultNonOptSelectors
			]
	}
	,
	DeserializeTestResult@trSerialized_TestResultSerialized :=
		With[{opts = List @@ trSerialized},
			trHead @@ Join[
				OptionValue[opts, nonOptSelectors],
				FilterRules[
					Symbol@deserializeSelecorName@#1 -> #2& @@@ opts,
					$testResultOptNames
				]
			]
		]
]


End[]


(* ::Section:: *)
(*Public symbols protection*)


Protect["`*"]


EndPackage[]
