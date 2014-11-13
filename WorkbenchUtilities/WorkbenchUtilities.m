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


BeginPackage["WorkbenchUtilities`"]


(* ::Section:: *)
(*Usage messages*)


Resources::usage =
"\
Resources[resource1, resource2, ...] \
represents resources of a project, extracted from .MathematicaResources file.\

Resources[...][\"name\"] \
returns preprocessed resource with given \"name\"."


GetMathematicaResources::usage =
"\
GetMathematicaResources[projectDir] \
returns resources of a project, extracted from .MathematicaResources file \
from given directory projectDir."


GetProjectReferences::usage =
"\
GetProjectReferences[projectDir] \
returns list of strings with names of projects referenced by project from \
given directory projectDir."


(* ::Section:: *)
(*Implementation*)


(*	Unprotect all symbols in this context
	(all public symbols provided by this package) *)
Unprotect["`*"]


(* ::Subsection:: *)
(*MathematicaResources symbols*)


(*	Put symbols that can be found in project .MathematicaResources file in
	proper context, except ones that are also in System` context.
	Prevent evaluation of their elements. *)
SetAttributes[Evaluate @ Symbol["`MathematicaResources`" <> #], HoldAll]& /@ {
	"Sources",
	"Java",
	"GUI",
	"Palettes",
	"StyleSheets",
	"IndexExcludes",
	"ExecutionBuildCommand",
	"Paclet",
	"DocumentationBuildFile",
	"PacletFolder",
	"FunctionPaclet"
}
	

Begin["`Private`"]


AppendTo[$ContextPath, "WorkbenchUtilities`MathematicaResources`"]


(* ::Subsection:: *)
(*Private symbols usage*)


oneDirectoryStringArgument::usage =
"\
oneDirectoryStringArgument[sym] \
adds down values, to given symbol sym, with definitions printing apropriate \
messages when arguments, different than one string with path to directory, \
are given."


getAllPackagesCommand::usage =
"\
getAllPackagesCommand[projectDir] \
returns held expression that calls Get on each package from given directory \
projectDir. This is default ExecutionBuildCommand used by Workbench."


(* ::Subsection:: *)
(*oneDirectoryStringArgument*)


oneDirectoryStringArgument::nonDir =
"String `1` does not represent valid path to a directory."


oneDirectoryStringArgument[sym_Symbol] := (
	sym::nonDir = oneDirectoryStringArgument::nonDir;
	
	sym[str_String] := "nothing" /;
		Message[sym::nonDir, HoldForm[str]];
	
	sym[arg:Except[_String]] := "nothing" /;
		Message[sym::string, HoldForm[1], HoldForm[sym[arg]]];
	
	sym[args___ /; Length[{args}] != 1] := "nothing" /;
		Message[sym::argx, HoldForm[sym], HoldForm[Evaluate @ Length[{args}]]];
)


(* ::Subsection:: *)
(*getAllPackagesCommand*)


getAllPackagesCommand =
	Function[projectDir,
		Map[
			Get
			,
			Hold[CompoundExpression[##]]& @@
				FileNames["*.m", projectDir, Infinity]
			,
			{2}
		]
	]


(* ::Subsection:: *)
(*Resources*)


Resources::invalidResources =
"File `1` does not contain valid project resources."

Resources::unknownSelector =
"An unknown resource selector used: `1`."

Resources::invalidElement =
"Resource element: `1` found in `2` file is invalid."

Resources::invalidPaths = Resources::invalidElement <>
" '3' element can contain only Directory[\"...\"] and File[\"...\"] elements."

Resources::invalidEBC = Resources::invalidElement <>
" ExecutionBuildCommand element should contain exactly one string with \
correct mathematica expression."


SetAttributes[Resources, HoldAll]


Resources[res___][sym_Symbol] := Resources[res][SymbolName[sym]]


Resources[args___, opts:OptionsPattern[]][
	sel:(
		"Sources" | "Java" | "GUI" | "Palettes" |
		"StyleSheets" | "IndexExcludes"
	)
] :=
	With[
		{
			resourcesFile = OptionValue[opts, "ResourcesFile"],
			heldElement = Select[Hold[args], SymbolName[Head[#]] === sel &, 1]
		}
		,
		If[MemberQ[heldElement, Except[(Directory | File)[_String]], {2}],
			Message[Resources::invalidPaths,
				HoldForm @@ heldElement,
				HoldForm[resourcesFile],
				HoldForm[Evaluate[heldElement[[1, 0]]]]
			];
			Return[$Failed]
		];
		
		With[
			{projectDir = FileNameJoin @ DirectoryName[resourcesFile]}
			,
			List @@ Replace[
				heldElement
				,
				{
					HoldPattern[Directory]["~"] -> projectDir
					,
					(Directory | File)[relPath_String] ->
						FileNameJoin[{projectDir, relPath}]
				}
				,
				{2}
			][[1]]
		]
	]


Resources[___, ExecutionBuildCommand[ebc_String], ___, opts:OptionsPattern[]][
	"ExecutionBuildCommand"
] :=
	Module[
		{result}
		,
		With[
			{resourcesFile = OptionValue[opts, "ResourcesFile"]}
			,
			Check[
				Block[
					{$Context = "WorkbenchUtilities`MathematicaResources`"}
					,
					result = ToExpression[ebc, InputForm, Hold]
				]
				,
				Message[
					Resources::invalidEBC,
					HoldForm[ExecutionBuildCommand[ebc]],
					HoldForm[resourcesFile]
				]
				,
				{
					General::sntx, General::sntxi, General::tsntxi,
					Syntax::sntufn, Syntax::sntunc
				}
			];
			
			result
		]
	]

Resources[___, ebc_ExecutionBuildCommand, ___, opts:OptionsPattern[]][
	"ExecutionBuildCommand"
] :=
	With[
		{resourcesFile = OptionValue[opts, "ResourcesFile"]}
		,
		Message[Resources::invalidEBC,
			HoldForm[ebc],
			HoldForm[resourcesFile]
		];
		Return[$Failed]
	]

Resources[___, opts:OptionsPattern[]]["ExecutionBuildCommand"] :=
	getAllPackagesCommand[DirectoryName @ OptionValue[opts, "ResourcesFile"]]


Resources[___][arg_String] := "nothing" /;
	Message[Resources::unknownSelector, HoldForm[arg]];

Resources[___][args___ /; Length[{args}] != 1] := "nothing" /;
	Message[Resources::argx,
		HoldForm["Resources object"],
		HoldForm[Evaluate @ Length[{args}]]
	];


(* ::Subsection:: *)
(*GetMathematicaResources*)


GetMathematicaResources[dir_String /; FileType[dir] === Directory] :=
	Module[
		{resources}
		,
		With[
			{resourcesFile = FileNameJoin[{dir, ".MathematicaResources"}]}
			,
			Quiet[
				Check[
					resources =
						(*	Make sure that all symbols from
							.MathematicaResources file are placed in correct
							context. *)
						Block[
							{
								$ContextPath = {
									"WorkbenchUtilities`MathematicaResources`",
									"WorkbenchUtilities`",
									"System`"
								},
								$Context =
									"WorkbenchUtilities`MathematicaResources`"
							}
							,
							ReadList[resourcesFile, HoldComplete[Expression]]
						]
					,
					(*	Project does not contain .MathematicaResources file,
						return empty Resources. *)
					Return[Resources["ResourcesFile" -> resourcesFile]]
					,
					ReadList::nffil
				]
				,
				ReadList::nffil
			];
			
			resources = Cases[resources, HoldComplete[Resources[___]], {1}, 1];
			
			If[resources === {},
				Message[Resources::invalidResources, HoldForm[resourcesFile]];
				Return[$Failed]
			];
			
			Append[resources[[1, 1]], "ResourcesFile" -> resourcesFile]
		]
	]

oneDirectoryStringArgument[GetMathematicaResources]


(* ::Subsection:: *)
(*GetProjectReferences*)


GetProjectReferences::noValidProjectDescription =
"Directory `1` does not contain valid project description file."


GetProjectReferences[dir_String /; FileType[dir] === Directory] :=
	Module[
		{result}
		,
		
		result = Import[FileNameJoin[{dir, ".project"}], "XML"];
		
		If[Head[result] =!= XMLObject["Document"],
			Message[GetProjectReferences::noValidProjectDescription,
				HoldForm[dir]
			];
			Return[$Failed]
		];
		
		(* Get list of children of "projects" XML element. *)
		result =
			Flatten @ Cases[
				result
				,
				XMLElement["projectDescription", _, {
					___,
					XMLElement["projects", {}, projects_List],
					___
				}] :>
					projects
				,
				Infinity
				,
				1
			];
		
		(* Return list of project names. *)
		Cases[
			result,
			XMLElement["project", {}, {name_String}] :> name
		]
	]

oneDirectoryStringArgument[GetProjectReferences]


End[]


(* ::Section:: *)
(*Public symbols protection*)


(*	Protect all symbols in this context
	(all public symbols provided by this package) *)
Protect["`*"]


EndPackage[]
