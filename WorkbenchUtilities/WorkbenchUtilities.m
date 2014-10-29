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


getAllPackagesCommand::usage =
"\
getAllPackagesCommand[projectDir] \
returns held expression that calls Get on each package from given directory \
projectDir. This is default ExecutionBuildCommand used by Workbench."


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


GetExecutionBuildCommand::nonDir =
"String `1` does not represent valid path to a directory."

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

GetExecutionBuildCommand[str_String] := "nothing" /;
	Message[GetExecutionBuildCommand::nonDir, HoldForm[str]]

GetExecutionBuildCommand[arg:Except[_String]] := "nothing" /;
	Message[GetExecutionBuildCommand::string,
		HoldForm[1],
		HoldForm[GetExecutionBuildCommand[arg]]
	]

GetExecutionBuildCommand[args___ /; Length[{args}] != 1] := "nothing" /;
	Message[GetExecutionBuildCommand::argx,
		HoldForm[GetExecutionBuildCommand],
		HoldForm[Evaluate @ Length[{args}]]
	]


End[]


(* ::Section:: *)
(*Public symbols protection*)


(*
	Protect all symbols in this context
	(all public symbols provided by this package)
*)
Protect["`*"]


EndPackage[]
