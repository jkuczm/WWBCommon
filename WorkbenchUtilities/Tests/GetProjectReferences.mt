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


BeginPackage["TestEnvironment`GetProjectReferences`", {"MUnit`"}]


Get["WorkbenchUtilities`"]
Get["WorkbenchUtilities`Tests`Utilities`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


(* ::Subsubsection:: *)
(*No arguments*)


TestMatch[
	GetProjectReferences[]
	,
	HoldPattern @ GetProjectReferences[]
	,
	Message[GetProjectReferences::argx, GetProjectReferences, 0]
	,
	TestID -> "no args"
]


(* ::Subsection:: *)
(*One argument*)


TestMatch[
	GetProjectReferences[someSymbol]
	,
	HoldPattern @ GetProjectReferences[someSymbol]
	,
	Message[
		GetProjectReferences::string,
		1,
		GetProjectReferences[someSymbol]
	]
	,
	TestID -> "1 arg: non-String"
]


TestMatch[
	GetProjectReferences["not/a/valid/directory"]
	,
	HoldPattern @ GetProjectReferences["not/a/valid/directory"]
	,
	Message[GetProjectReferences::nonDir, "not/a/valid/directory"]
	,
	TestID -> "1 arg: String not a directory path"
]


(* ::Subsection:: *)
(*Two arguments*)


TestMatch[
	GetProjectReferences["someString", someSymbol]
	,
	HoldPattern @ GetProjectReferences["someString", someSymbol]
	,
	Message[GetProjectReferences::argx, GetProjectReferences, 2]
	,
	TestID -> "2 args"
]


(* ::Subsection:: *)
(*Correct arguments*)


(* ::Subsubsection:: *)
(*No project description file*)


With[
	{fakeProjectDir = fakeProjectPath["NoProjectDescriptionFile"]}
	,
	Test[
		GetProjectReferences[fakeProjectDir]
		,
		$Failed
		,
		{
			HoldForm @ Message[Import::nffil, Import],
			Message[GetProjectReferences::noValidProjectDescription,
				fakeProjectDir
			]
		}
		,
		TestID -> "No project description file"
	]
]


(* ::Subsubsection:: *)
(*Malformed project description file*)


With[
	{fakeProjectDir = fakeProjectPath["MalformedProjectDescriptionFile"]}
	,
	Test[
		GetProjectReferences[fakeProjectDir]
		,
		$Failed
		,
		{
			XML`Parser`XMLGet::prserr,
			HoldForm @ Message[Import::fmterr, "XML"],
			Message[GetProjectReferences::noValidProjectDescription,
				fakeProjectDir
			]
		}
		,
		TestID -> "Malformed project description file"
	]
]


(* ::Subsubsection:: *)
(*No projects xml element*)


Test[
	GetProjectReferences[fakeProjectPath["NoProjectsXmlElement"]]
	,
	{}
	,
	TestID -> "No projects xml element"
]


(* ::Subsubsection:: *)
(*No project references*)


Test[
	GetProjectReferences[fakeProjectPath["NoProjectReferences"]]
	,
	{}
	,
	TestID -> "No project references"
]


(* ::Subsubsection:: *)
(*One project reference*)


Test[
	GetProjectReferences[fakeProjectPath["OneProjectReference"]]
	,
	{"FakeReferencedProject"}
	,
	TestID -> "One project reference"
]


(* ::Subsubsection:: *)
(*Many project references*)


Test[
	GetProjectReferences[fakeProjectPath["ManyProjectReferences"]]
	,
	{
		"FakeReferencedProject1",
		"FakeReferencedProject2",
		"FakeReferencedProject3"
	}
	,
	TestID -> "Many project references"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
