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
	"TestEnvironment`MUnitBackports`SameTestVsEquivalenceFunction`",
	{"MUnit`"}
]


Get["MultipleVersionsTests`MUnitBackports`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*SameTest*)


(* ::Subsubsection:: *)
(*Explicit option*)


Test[
	2.
	,
	2
	,
	TestID -> "SameTest: Explicit option: symbol"
	,
	SameTest -> Equal
]
TestMatch[
	2.
	,
	2
	,
	TestID -> "SameTest: Explicit option: symbol arbitrary context"
	,
	{`tmpContext`SameTest -> Equal}
]
TestStringMatch[
	2.
	,
	2
	,
	TestID -> "SameTest: Explicit option: string"
	,
	{{"SameTest" -> Equal}, EquivalenceFunction -> SameQ}
]


(* ::Subsubsection:: *)
(*SetOptions*)


Internal`InheritedBlock[{TestFree},
	SetOptions[TestFree, {EquivalenceFunction -> f, SameTest -> Equal}];
	
	TestFree[
		2.
		,
		2
		,
		TestID -> "SameTest: SetOptions: symbol"
	]
]
Internal`InheritedBlock[{TestStringFree},
	SetOptions[TestStringFree, `tmpContext`SameTest -> Equal];
	
	TestStringFree[
		2.
		,
		2
		,
		TestID -> "SameTest: SetOptions: symbol arbitrary context"
	]
]
Internal`InheritedBlock[{Test},
	SetOptions[Test, {"SameTest" -> Equal}];
	
	Test[
		2.
		,
		2
		,
		TestID -> "SameTest: SetOptions: string"
	]
]


(* ::Subsection:: *)
(*EquivalenceFunction*)


(* ::Subsubsection:: *)
(*Explicit option*)


TestMatch[
	2.
	,
	2
	,
	TestID -> "EquivalenceFunction: Explicit option: symbol"
	,
	{{EquivalenceFunction -> Equal}}
]
TestStringMatch[
	2.
	,
	2
	,
	TestID -> "EquivalenceFunction: Explicit option: symbol arbitrary context"
	,
	`tmpContext`EquivalenceFunction -> Equal
]
TestFree[
	2.
	,
	2
	,
	TestID -> "EquivalenceFunction: Explicit option: string"
	,
	{"EquivalenceFunction" -> Equal, SameTest -> SameQ}
]


(* ::Subsubsection:: *)
(*SetOptions*)


Internal`InheritedBlock[{TestStringFree},
	SetOptions[TestStringFree, EquivalenceFunction -> Equal];
	
	TestStringFree[
		2.
		,
		2
		,
		TestID -> "EquivalenceFunction: SetOptions: symbol"
	]
]
Internal`InheritedBlock[{Test},
	SetOptions[Test, {`tmpContext`EquivalenceFunction -> Equal}];
	
	Test[
		2.
		,
		2
		,
		TestID -> "EquivalenceFunction: SetOptions: symbol arbitrary context"
	]
]
Internal`InheritedBlock[{TestMatch},
	SetOptions[TestMatch, SameTest -> SameQ, "EquivalenceFunction" -> Equal];
	
	TestMatch[
		2.
		,
		2
		,
		TestID -> "EquivalenceFunction: SetOptions: string"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
