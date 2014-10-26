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


BeginPackage["TestEnvironment`MUnitBackports`getTestSuite`", {"MUnit`"}]


Get["MultipleVersionsTests`MUnitBackports`"]
PrependTo[$ContextPath, "MultipleVersionsTests`MUnitBackports`Private`"]


Get["MultipleVersionsTests`Tests`Utilities`"]


(* ::Section:: *)
(*Tests*)


With[
	{stream = OpenRead @ FakeTestPath["tests.mt"]}
	,
	Test[
		getTestSuite[stream, HoldComplete[Null]]
		,
		$Failed
		,
		TestID -> "test file"
	];
	
	Close[stream];
]


With[
	{stream = OpenRead @ FakeTestPath["suite.mt"]}
	,
	(* Put TestSuite in Block to prevent it's automatic evaluation. *)
	Block[
		{TestSuite}
		,
		Test[
			getTestSuite[stream, HoldComplete[Null]]
			,
			TestSuite[{
				"test_success.mt",
				"test_failure.mt",
				"test_message_failure.mt",
				"tests.mt"
			}]
			,
			TestID -> "test suite file"
		];
	];
	
	Close[stream];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
