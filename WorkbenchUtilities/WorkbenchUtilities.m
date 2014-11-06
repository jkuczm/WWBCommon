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


GetExecutionBuildCommand::usage =
"\
GetExecutionBuildCommand[projectDir] \
returns execution build command, wrapped in Hold, for project from given \
directory projectDir."


GetProjectReferences::usage =
"\
GetProjectReferences[projectDir] \
returns list of strings with names of projects referenced by project from \
given directory projectDir."


(* ::Section:: *)
(*Implementation*)


(*
	Unprotect all symbols in this context
	(all public symbols provided by this package)
*)
Unprotect["`*"]


(* ::Subsection:: *)
(*MathematicaResources*)


(*
	Context with relevant symbols that can be found in project
	.MathematicaResources file.
*)
Begin["`MathematicaResources`"]

Resources

ExecutionBuildCommand

End[]
	

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


General::nonDir =
"String `1` does not represent valid path to a directory."


oneDirectoryStringArgument[sym_Symbol] := (
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
(*GetExecutionBuildCommand*)

GetExecutionBuildCommand::invalidResources =
"File `1` does not contain valid project resources."

GetExecutionBuildCommand::invalidEBC =
"ExecutionBuildCommand found in `1` file is invalid."


GetExecutionBuildCommand[dir_String /; FileType[dir] === Directory] :=
	Module[
		{resources, executionBuildCommand}
		,
		With[
			{resourcesFile = FileNameJoin[{dir, ".MathematicaResources"}]}
			,
			Quiet[
				Check[
					resources =
						(*
							Make sure that all symbols from
							.MathematicaResources file are placed in correct
							context.
						*)
						Block[
							{
								$Context =
									"WorkbenchUtilities`MathematicaResources`"
							}
							,
							ReadList[resourcesFile, Hold[Expression]]
						]
					,
					(*
						Project does not contain .MathematicaResources file,
						use default ExecutionBuildCommand.
					*)
					Return[getAllPackagesCommand[dir]]
					,
					{ReadList::nffil}
				]
				,
				{ReadList::nffil}
			];
			
			resources = Cases[resources, Hold[Resources[___]], {1}, 1];
			
			If[resources === {},
				Message[
					GetExecutionBuildCommand::invalidResources,
					HoldForm[resourcesFile]
				];
				Return[$Failed]
			];
			resources = First[resources];
			
			executionBuildCommand =
				Cases[resources, _ExecutionBuildCommand, {2}, 1];
			
			If[executionBuildCommand === {},
				(*
					.MathematicaResources contained no ExecutionBuildCommand,
					use default one.
				*)
				Return[getAllPackagesCommand[dir]]
			];
			
			executionBuildCommand = First[executionBuildCommand];
			
			If[!MatchQ[executionBuildCommand, ExecutionBuildCommand[_String]],
				Message[
					GetExecutionBuildCommand::invalidEBC,
					HoldForm[resourcesFile]
				];
				Return[$Failed]
			];
			
			Check[
				executionBuildCommand =
					Block[
						{$Context = "WorkbenchUtilities`MathematicaResources`"}
						,
						ToExpression[
							First[executionBuildCommand],
							InputForm,
							Hold
						]
					]
				,
				Message[
					GetExecutionBuildCommand::invalidEBC,
					HoldForm[resourcesFile]
				]
				,
				{
					General::sntx, General::sntxi, General::tsntxi,
					Syntax::sntufn, Syntax::sntunc
				}
			];
			
			executionBuildCommand
		]
	]

oneDirectoryStringArgument[GetExecutionBuildCommand]


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


(*
	Protect all symbols in this context
	(all public symbols provided by this package)
*)
Protect["`*"]


EndPackage[]
