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
returns absolute path file with given path relative to fake projects \
directory."


(* ::Section:: *)
(*Implementation*)
	

Begin["`Private`"]


(* ::Subsection:: *)
(*General messages*)


General::evaluated = "`1` was evaluated."


(* ::Subsection:: *)
(*listLogger*)


fakeProjectPath[fakeProjectName_String] :=
	FileNameJoin[{
		DirectoryName @ FindFile["WorkbenchUtilities`WorkbenchUtilities`"],
		"Tests/fakeProjects",
		fakeProjectName
	}]


End[]


EndPackage[]
