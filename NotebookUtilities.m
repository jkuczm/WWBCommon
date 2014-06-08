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


BeginPackage["NotebookUtilities`"]


(* ::Section:: *)
(*Usage messages*)


(* ::Subsection:: *)
(*Public symbols usage*)


NotebookNoCache::usage =
"\
NotebookNoCache[nbObj] \
sets \"FileOutlineCache\" option to False on NotebookObject nbObj."


ProcessNotebookFiles::usage =
"\
ProcessNotebookFiles[\"file\"] \
processes notebook from given \"file\", saves changes, and returns file name.\

ProcessNotebookFiles[\"dir\"] \
processes all notebooks from given directory \"dir\", saves changes, and
returns List of saved file names.\

ProcessNotebookFiles[{\"file1\", \"dir2\", \"file3\", ...}] \
processes all notebooks from given files \"filei\" and directories \"diri\", \
saves changes, and returns List of saved file names."


(* ::Section:: *)
(*Implementation*)


(*
	Unprotect all symbols in this context
	(all public symbols provided by this package)
*)
Unprotect["`*"]
 

Begin["`Private`"]


(* ::Subsection:: *)
(*Start Front End*)


Developer`InstallFrontEnd[]


(* ::Subsection:: *)
(*Public symbols*)


(* ::Subsubsection:: *)
(*NotebookNoCache*)


NotebookNoCache[nbObj_NotebookObject] :=
	SetOptions[
		nbObj
		,
		(* Add "FileOutlineCache" -> False option *)
		PrivateNotebookOptions -> 
			Prepend[
				FilterRules[
					Flatten @ Cases[
						Options[nbObj],
						HoldPattern[PrivateNotebookOptions -> value_] :> value,
						{1},
						1
					]
					,
					Except["FileOutlineCache"]
				]
				,
				"FileOutlineCache" -> False
			]
	];


(* ::Subsubsection:: *)
(*ProcessNotebookFiles*)


ProcessNotebookFiles::wrongPath =
"Given string: `1` does not represent valid path to a directory nor to a file."

ProcessNotebookFiles::notANotebook =
"Last expression from `1` file is not a Notebook expression."


Options[ProcessNotebookFiles] = {
	"ExpressionProcessor" -> Identity,
	"ObjectProcessor" -> Identity,
	"FrontEndSessionOptions" -> {},
	"Verbose" -> 2,
	"PrintFunction" -> Print,
	"PrintIndentation" -> ""
}


ProcessNotebookFiles[
	file_String /; FileType[file] === File,
	OptionsPattern[]
] :=
	Module[
		{
			nb,
			nbObj,
			objectProcessor,
			feOptions
		}
		,
		If[OptionValue["Verbose"] >= 2,
			OptionValue["PrintFunction"][
				OptionValue["PrintIndentation"] <>
				"Processing " <> file <> " file"
			];
		];
		
		(*
			Importing nb files containing Manipulate objects can throw a bunch
			of benign newline interpretation warnings.
		*)
		Quiet[
			nb = Get[file],
			Syntax::newl
		];
		
		If[Head[nb] =!= Notebook,
			Message[ProcessNotebookFiles::notANotebook, file];
			Return[$Failed]
		];
		
		nb = OptionValue["ExpressionProcessor"][nb];

		
		objectProcessor = OptionValue["ObjectProcessor"];
		feOptions = OptionValue["FrontEndSessionOptions"];
		If[objectProcessor =!= None || feOptions =!= {},
			Developer`UseFrontEnd[
				SetOptions[$FrontEndSession, feOptions];
				
				nbObj = NotebookPut[nb];
				
				objectProcessor[nbObj];
				
				NotebookSave[nbObj, file];
				
				NotebookClose[nbObj];
			];
		(* else *),
			Put[nb, file]
		];
		
		file
	]

ProcessNotebookFiles[
	dir_String /; FileType[dir] === Directory,
	opts:OptionsPattern[]
] := (
	If[OptionValue["Verbose"] >= 1,
		OptionValue["PrintFunction"][
			OptionValue["PrintIndentation"] <>
			"Processing Notebooks in " <> dir <> " directory"
		];
	];
	ProcessNotebookFiles[FileNames["*.nb", dir, Infinity], opts]
)

ProcessNotebookFiles[files:{___String}, opts:OptionsPattern[]] :=
	With[
		{indentation = OptionValue["PrintIndentation"]}
		,
		If[OptionValue["Verbose"] >= 1,
			OptionValue["PrintFunction"][
				indentation <> "Processing " <>
				ToString @ Length @ files <> " Notebook files"
			];
		];
		
		ProcessNotebookFiles[
			#,
			"PrintIndentation" -> indentation <> "\t",
			opts
		]& /@ files
	]

ProcessNotebookFiles[unknownString_String, OptionsPattern[]] := (
	Message[ProcessNotebookFiles::wrongPath, unknownString];
	$Failed
)

ProcessNotebookFiles[args___] := (
	Message[
		ProcessNotebookFiles::strs,
		1,
		HoldForm[ProcessNotebookFiles[args]]
	];
	$Failed
)


End[]


(* ::Section:: *)
(*Public symbols protection*)


(*
	Protect all symbols in this context
	(all public symbols provided by this package)
*)
Protect["`*"]


EndPackage[]
