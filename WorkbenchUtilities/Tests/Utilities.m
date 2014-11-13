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


BeginPackage["WorkbenchUtilities`Tests`Utilities`"]


(* ::Section:: *)
(*Usage messages*)


(* ::Subsection:: *)
(*Public symbols usage*)


fakeProjectPath::usage =
"\
fakeProjectPath[relativePath] \
returns absolute path treating given relativePath as relative to fake \
projects directory."


uniqueProjectPath::usage =
"\
uniqueProjectPath[] \
returns absolute path to unique fake project directory."


getMRPath::usage =
"\
getMRPath[] \
returns absolute path to .MathematicaResources file in unique fake temporary \
project directory.\

getMRPath[projectDir] \
returns absolute path to .MathematicaResources file in given project \
directory projectDir."


(* ::Section:: *)
(*Implementation*)
	

Begin["`Private`"]


(* ::Subsection:: *)
(*Messages*)


test::evaluated = "`1` was evaluated."


(* ::Subsection:: *)
(*fakeProjectPath*)


fakeProjectPath[fakeProjectName_String] :=
	FileNameJoin[{
		DirectoryName @ FindFile["WorkbenchUtilities`WorkbenchUtilities`"],
		"Tests/fakeProjects",
		fakeProjectName
	}]


(* ::Subsection:: *)
(*uniqueProjectPath*)


uniqueProjectPath[] := FileNameJoin[{"path/to", ToString @ Unique["project"]}]


(* ::Subsection:: *)
(*getMRPath*)


getMRPath[projectPath:(_String | Automatic):Automatic] :=
	FileNameJoin[{
		If[projectPath === Automatic,
			uniqueProjectPath[]
		(* else *),
			projectPath
		],
		".MathematicaResources"
	}]


End[]


EndPackage[]
