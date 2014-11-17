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


BeginPackage["TestEnvironment`GetMathematicaResources`", {"MUnit`"}]


Get["WorkbenchUtilities`"]
Get["WorkbenchUtilities`Tests`Utilities`"]


AppendTo[$ContextPath, "WorkbenchUtilities`MathematicaResources`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


(* ::Subsubsection:: *)
(*No arguments*)


TestMatch[
	GetMathematicaResources[]
	,
	HoldPattern @ GetMathematicaResources[]
	,
	Message[GetMathematicaResources::argx, GetMathematicaResources, 0]
	,
	TestID -> "no args"
]


(* ::Subsection:: *)
(*One argument*)


TestMatch[
	GetMathematicaResources[someSymbol]
	,
	HoldPattern @ GetMathematicaResources[someSymbol]
	,
	Message[
		GetMathematicaResources::string,
		1,
		GetMathematicaResources[someSymbol]
	]
	,
	TestID -> "1 arg: non-String"
]


TestMatch[
	GetMathematicaResources["not/a/valid/directory"]
	,
	HoldPattern @ GetMathematicaResources["not/a/valid/directory"]
	,
	Message[GetMathematicaResources::nonDir, "not/a/valid/directory"]
	,
	TestID -> "1 arg: String not a directory path"
]


(* ::Subsection:: *)
(*Two arguments*)


TestMatch[
	GetMathematicaResources["someString", someSymbol]
	,
	HoldPattern @ GetMathematicaResources["someString", someSymbol]
	,
	Message[GetMathematicaResources::argx, GetMathematicaResources, 2]
	,
	TestID -> "2 args"
]


(* ::Subsection:: *)
(*Correct arguments*)


(* ::Subsubsection:: *)
(*No .MathematicaResources file*)


With[
	{
		resourcesFile =
			fakeProjectPath["NoMathematicaResources/.MathematicaResources"]
	}
	,
	Test[
		GetMathematicaResources @ fakeProjectPath["NoMathematicaResources"]
		,
		Resources[
			"ResourcesFile" -> resourcesFile
		]
		,
		TestID -> "No .MathematicaResources"
	]
]


(* ::Subsubsection:: *)
(*.MathematicaResources file with malformed Resources*)


With[
	{
		resourcesFile =
			fakeProjectPath[
				"MalformedMathematicaResources/.MathematicaResources"
			]
	}
	,
	Test[
		GetMathematicaResources @
			fakeProjectPath["MalformedMathematicaResources"]
		,
		$Failed
		,
		Message[Resources::invalidResources, resourcesFile]
		,
		TestID -> "Malformed Resources"
	]
]


(* ::Subsubsection:: *)
(*.MathematicaResources file with proper Resources*)


With[
	{
		resourcesFile =
			fakeProjectPath["ProperResources/.MathematicaResources"]
	}
	,
	Test[
		GetMathematicaResources @ fakeProjectPath["ProperResources"]
		,
		Resources[
			Version[1],
			Sources[
				Directory["~"],
				Directory["testSourcesDir1"],
				Directory["testSourcesDir2"]
			],
			Java[
				Directory["testJavaDir"]
			],
			GUI[
				Directory["testGUIDir"]
			],
			Palettes[
				Directory["testPalettesDir"]
			],
			StyleSheets[
				Directory["testStyleSheetsDir"]
			],
			IndexExcludes[
				File["testFile"]
			],
			ExecutionBuildCommand["testExecutionBuildCommand"],
			Paclet[
				DocumentationBuildFile["testDocbuild.xml"],
				PacletFolder["testPacletDir"],
				FunctionPaclet[False]
			],
			"ResourcesFile" -> resourcesFile
		]
		,
		TestID -> "Proper Resources"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
