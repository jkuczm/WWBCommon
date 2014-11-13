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


BeginPackage["TestEnvironment`Resources`", {"MUnit`"}]


Get["WorkbenchUtilities`"]
Get["WorkbenchUtilities`Tests`Utilities`"]


AppendTo[$ContextPath, "WorkbenchUtilities`MathematicaResources`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


(* ::Subsubsection:: *)
(*No arguments*)


Module[
	{res1, res2}
	,
	TestMatch[
		Resources[res1, res2][]
		,
		HoldPattern @ Resources[res1, res2][]
		,
		Message[Resources::argx, "Resources object", 0]
		,
		TestID -> "no args"
	]
]


(* ::Subsubsection:: *)
(*One argument*)


Module[
	{res1, res2}
	,
	TestMatch[
		Resources[res1, res2]["unknownResourceName"]
		,
		HoldPattern @ Resources[res1, res2]["unknownResourceName"]
		,
		Message[Resources::unknownSelector, "unknownResourceName"]
		,
		TestID -> "no args"
	]
]


(* ::Subsection:: *)
(*Two arguments*)


Module[
	{res1, res2, arg1, arg2}
	,
	TestMatch[
		Resources[res1, res2][arg1, arg2]
		,
		HoldPattern @ Resources[res1, res2][arg1, arg2]
		,
		Message[Resources::argx, "Resources object", 2]
		,	
		TestID -> "2 args"
	]
]


(* ::Subsection:: *)
(*ExecutionBuildCommand*)


(* ::Subsubsection:: *)
(*Non-existent*)


With[
	{projectDir = fakeProjectPath["NoExecutionBuildCommand"]}
	,
	With[
		{
			resourcesFile = getMRPath[projectDir],
			fakePackage3 =
				FileNameJoin[{
					projectDir, "fakePackage3/fakePackage3.m"
				}],
			fakePackage3Init =
				FileNameJoin[{
					projectDir, "fakePackage3/Kernel/init.m"
				}],
			fakePackage4 =
				FileNameJoin[{projectDir, "fakePackage4.m"}]
		}
		,
		Test[
			Resources["ResourcesFile" -> resourcesFile]@ExecutionBuildCommand
			,
			Hold[
				Get[fakePackage3];
				Get[fakePackage3Init];
				Get[fakePackage4]
			]
			,
			TestID -> "ExecutionBuildCommand: non-existent"
		]
	]
]


(* ::Subsubsection:: *)
(*Invalid*)


Module[
	{resourcesFile}
	,
	Test[
		Resources[
			ExecutionBuildCommand["a", "b"],
			"ResourcesFile" -> resourcesFile
		]@ExecutionBuildCommand
		,
		$Failed
		,
		Message[Resources::invalidEBC,
			ExecutionBuildCommand["a", "b"], resourcesFile
		]
		,
		TestID -> "ExecutionBuildCommand: invalid"
	]
]


(* ::Subsubsection:: *)
(*Syntax error*)


Module[
	{resourcesFile}
	,
	Test[
		Resources[
			ExecutionBuildCommand["(1 + )"],
			"ResourcesFile" -> resourcesFile
		]@ExecutionBuildCommand
		,
		$Failed
		,
		{ToExpression::sntx, Resources::invalidEBC}
		,
		TestID -> "ExecutionBuildCommand: syntax error"
	]
]


(* ::Subsubsection:: *)
(*Proper*)


Test[
	Resources[
		ExecutionBuildCommand[
			"Message[test::evaluated, \"ExecutionBuildCommand\"]"
		],
		"ResourcesFile" -> "resourcesFile"
	]@ExecutionBuildCommand
	,
	Hold[Message[test::evaluated, "ExecutionBuildCommand"]]
	,
	TestID -> "ExecutionBuildCommand: proper"
]


Module[
	{resource1, resource2, resource3, resourcesFile}
	,
	Test[
		Resources[
			resource1,
			ExecutionBuildCommand[
				"Message[test::evaluated, \"ExecutionBuildCommand\"]"
			],
			resource2,
			resource3,
			"ResourcesFile" -> resourcesFile
		]@ExecutionBuildCommand
		,
		Hold[Message[test::evaluated, "ExecutionBuildCommand"]]
		,
		TestID -> "ExecutionBuildCommand: proper, other resources"
	]
]


(* ::Subsection:: *)
(*Paths*)


Do[
	With[
		{resource = resource}
		,
		With[
			{resourcesFile = getMRPath[]}
			,
			Module[
				{wrongElement}
				,
				Test[
					Resources[
						resource[wrongElement],
						"ResourcesFile" -> resourcesFile
					]@resource
					,
					$Failed
					,
					Message[Resources::invalidPaths,
						resource[wrongElement], resourcesFile, resource
					]
					,
					TestID -> "Paths: invalid",
					TestFailureMessage -> ToString[resource]
				]
			]
		];
		
		With[
			{resourcesFile = getMRPath[]}
			,
			Test[
				Resources[
					resource[],
					"ResourcesFile" -> resourcesFile
				]@resource
				,
				{}
				,
				TestID -> "Paths: empty",
				TestFailureMessage -> ToString[resource]
			]
		];
		
		With[
			{projectDir = uniqueProjectPath[]}
			,
			With[
				{resourcesFile = getMRPath[projectDir]}
				,
				Test[
					Resources[
						resource[Directory["~"]],
						"ResourcesFile" -> resourcesFile
					]@resource
					,
					{projectDir}
					,
					TestID -> "Paths: ~",
					TestFailureMessage -> ToString[resource]
				]
			]
		];
		
		With[
			{
				projectDir = uniqueProjectPath[],
				testDir = FileNameJoin[{"a/test", ToString @ Unique["dir"]}]
			}
			,
			With[
				{
					resourcesFile = getMRPath[projectDir],
					testDirAbsolute = FileNameJoin[{projectDir, testDir}]
				}
				,
				Test[
					Resources[
						resource[Directory[testDir]],
						"ResourcesFile" -> resourcesFile
					]@resource
					,
					{testDirAbsolute}
					,
					TestID -> "Paths: test directory",
					TestFailureMessage -> ToString[resource]
				]
			]
		];
		
		With[
			{
				projectDir = uniqueProjectPath[],
				testDir =
					FileNameJoin[{"a/test", ToString @ Unique["dir"]}]
			}
			,
			With[
				{
					resourcesFile = getMRPath[projectDir],
					testDirAbsolute = FileNameJoin[{projectDir, testDir}]
				}
				,
				Test[
					Resources[
						resource[
							Directory["~"],
							Directory[testDir]
						],
						"ResourcesFile" -> resourcesFile
					]@resource
					,
					{projectDir, testDirAbsolute}
					,
					TestID -> "Paths: two directories",
					TestFailureMessage -> ToString[resource]
				]
			]
		];
		
		With[
			{
				projectDir = uniqueProjectPath[],
				testSourcesDir = FileNameJoin["a/test/Sources/dir"],
				testJavaDir = FileNameJoin["a/test/Java/dir"],
				testGUIDir = FileNameJoin["a/test/GUI/dir"],
				testPalettesDir = FileNameJoin["a/test/Palettes/dir"],
				testStyleSheetsDir = FileNameJoin["a/test/StyleSheets/dir"],
				testIndexExcludesDir = FileNameJoin["a/test/IndexExcludes/dir"]
			}
			,
			With[
				{
					resourcesFile = getMRPath[projectDir],
					resultDir =
						FileNameJoin[{
							projectDir, "a/test", SymbolName[resource], "dir"
						}]
				}
				,
				Test[
					Resources[
						Sources[Directory[testSourcesDir]],
						Java[Directory[testJavaDir]],
						GUI[Directory[testGUIDir]],
						Palettes[Directory[testPalettesDir]],
						StyleSheets[Directory[testStyleSheetsDir]],
						IndexExcludes[Directory[testIndexExcludesDir]]
						,
						"ResourcesFile" -> resourcesFile
					]@resource
					,
					{resultDir}
					,
					TestID -> "Paths: all path resources available",
					TestFailureMessage -> ToString[resource]
				]
			]
		];
	]
	,
	{resource, {Sources, Java, GUI, Palettes, StyleSheets, IndexExcludes}}
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
