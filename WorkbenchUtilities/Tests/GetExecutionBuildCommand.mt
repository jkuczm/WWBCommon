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


BeginPackage["TestEnvironment`GetExecutionBuildCommand`", {"MUnit`"}]


Get["WorkbenchUtilities`"]
Get["WorkbenchUtilities`Tests`Utilities`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


(* ::Subsubsection:: *)
(*No arguments*)


TestMatch[
	GetExecutionBuildCommand[]
	,
	HoldPattern @ GetExecutionBuildCommand[]
	,
	Message[GetExecutionBuildCommand::argx, GetExecutionBuildCommand, 0]
	,
	TestID -> "no args"
]


(* ::Subsection:: *)
(*One argument*)


TestMatch[
	GetExecutionBuildCommand[someSymbol]
	,
	HoldPattern @ GetExecutionBuildCommand[someSymbol]
	,
	Message[
		GetExecutionBuildCommand::string,
		1,
		GetExecutionBuildCommand[someSymbol]
	]
	,
	TestID -> "1 arg: non-String"
]


TestMatch[
	GetExecutionBuildCommand["not/a/valid/directory"]
	,
	HoldPattern @ GetExecutionBuildCommand["not/a/valid/directory"]
	,
	Message[GetExecutionBuildCommand::nonDir, "not/a/valid/directory"]
	,
	TestID -> "1 arg: String not a directory path"
]


(* ::Subsection:: *)
(*Two arguments*)


TestMatch[
	GetExecutionBuildCommand["someString", someSymbol]
	,
	HoldPattern @ GetExecutionBuildCommand["someString", someSymbol]
	,
	Message[GetExecutionBuildCommand::argx, GetExecutionBuildCommand, 2]
	,
	TestID -> "2 args"
]


(* ::Subsection:: *)
(*Correct arguments*)


(* ::Subsubsection:: *)
(*No .MathematicaResources file*)


With[
	{fakeProjectDir = fakeProjectPath["NoMathematicaResources"]}
	,
	Test[
		GetExecutionBuildCommand[fakeProjectDir]
		,
		With[
			{
				fakePackage1 =
					FileNameJoin[{
						fakeProjectDir, "fakePackage1/fakePackage1.m"
					}],
				fakePackage1Init =
					FileNameJoin[{
						fakeProjectDir, "fakePackage1/Kernel/init.m"
					}],
				fakePackage2 =
					FileNameJoin[{fakeProjectDir, "fakePackage2.m"}]
			}
			,
			Hold[
				Get[fakePackage1];
				Get[fakePackage1Init];
				Get[fakePackage2]
			]
		]
		,
		TestID -> "No .MathematicaResources"
	]
]


(* ::Subsubsection:: *)
(*.MathematicaResources file with malformed Resources*)


With[
	{
		mathematicaResourcesPath =
			fakeProjectPath[
				"MalformedMathematicaResources/.MathematicaResources"
			]
	}
	,
	Test[
		GetExecutionBuildCommand @
			fakeProjectPath["MalformedMathematicaResources"]
		,
		$Failed
		,
		Message[
			GetExecutionBuildCommand::invalidResources,
			mathematicaResourcesPath
		]
		,
		TestID -> "malformed Resources"
	]
]


(* ::Subsubsection:: *)
(*.MathematicaResources file without ExecutionBuildCommand*)


With[
	{fakeProjectDir = fakeProjectPath["NoExecutionBuildCommand"]}
	,
	Test[
		GetExecutionBuildCommand[fakeProjectDir]
		,
		With[
			{
				fakePackage3 =
					FileNameJoin[{
						fakeProjectDir, "fakePackage3/fakePackage3.m"
					}],
				fakePackage3Init =
					FileNameJoin[{
						fakeProjectDir, "fakePackage3/Kernel/init.m"
					}],
				fakePackage4 =
					FileNameJoin[{fakeProjectDir, "fakePackage4.m"}]
			}
			,
			Hold[
				Get[fakePackage3];
				Get[fakePackage3Init];
				Get[fakePackage4]
			]
		]
		,
		TestID -> "No ExecutionBuildCommand"
	]
]


(* ::Subsubsection:: *)
(*.MathematicaResources file with invalid ExecutionBuildCommand*)


With[
	{
		mathematicaResourcesPath =
			fakeProjectPath[
				"InvalidExecutionBuildCommand/.MathematicaResources"
			]
	}
	,
	Test[
		GetExecutionBuildCommand @
			fakeProjectPath["InvalidExecutionBuildCommand"]
		,
		$Failed
		,
		Message[GetExecutionBuildCommand::invalidEBC, mathematicaResourcesPath]
		,
		TestID -> "Invalid ExecutionBuildCommand"
	]
]


(* ::Subsubsection:: *)
(*.MathematicaResources file with syntax error in ExecutionBuildCommand*)


Test[
	GetExecutionBuildCommand @
		fakeProjectPath["ExecutionBuildCommandSyntaxError"]
	,
	$Failed
	,
	{ToExpression::sntx, GetExecutionBuildCommand::invalidEBC}
	,
	TestID -> "Syntax error in ExecutionBuildCommand"
]


(* ::Subsubsection:: *)
(*.MathematicaResources file with proper ExecutionBuildCommand*)


Test[
	GetExecutionBuildCommand @
		fakeProjectPath["ProperExecutionBuildCommand"]
	,
	Hold[Message[General::evaluated, "ExecutionBuildCommand"]]
	,
	TestID -> "Proper ExecutionBuildCommand"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
