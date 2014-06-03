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
    
*******************************************************************************
    
    Attribution

    Code of this file is a derivative of code written by:
    Simon Rochester <http://mathematica.stackexchange.com/users/8253> and
    Teake Nutma <http://mathematica.stackexchange.com/users/5485>
    in "Creating cross-version compatible documentation with Workbench"
    thread on Mathematica Stack Exchange
    <http://mathematica.stackexchange.com/questions/28316>
    used under Creative Commons Attribution-ShareAlike 3.0 Unported License.
    To view a copy of this license, visit
    <http://creativecommons.org/licenses/by-sa/3.0/>.

******************************************************************************)

BeginPackage["CrossVersionDocumentation`"]


(* ::Section:: *)
(*Usage messages*)


(* ::Subsection:: *)
(*Public symbols usage*)


$HeadingStyles::usage =
"\
$HeadingStyles \
List of styles for section headings that, by default, will be subject of \
replacements."


NotebookCrossVersionFix::usage =
"\
NotebookCrossVersionFix[nb] \
with nb being Notebook expression, returns nb with cross version fixes.\

NotebookCrossVersionFix[\"file\"] \
saves notebook, from given file, with cross version fixes, returns file path.\

NotebookCrossVersionFix[{\"file1\", \"file2\", \"file3\", ...}] \
saves notebooks, from given files, with cross version fixes.\

NotebookCrossVersionFix[\"directory\"] \
saves all notebooks, found in given directory, with cross version fixes."


(*
	Unprotect all symbols in this context
	(all public symbols provided by this package)
*)
Unprotect["`*"]


(* ::Subsection:: *)
(*Private symbols usage*)
 

Begin["`Private`"]


ToTitleCase::usage =
"\
ToTitleCase[\"str\"] \
returns \"str\" converted to title case i.e. first letter of each word is \
converted to uppercase all other letters to lowercase."


IfPreVer9::usage =
"\
IfPreVer9[pre, post] \
returns pre in Mathematica versions 6--8, returns post otherwise."


SpacerCell::usage =
"\
SpacerCell \
a spacer cell"


SectionSpacerCell::usage =
"\
SectionSpacerCell[style] \
returns section spacer that displays as very thin cell in versions 6--8"


SectionCell::usage =
"\
SectionCell[style, {preVer9FrameLabel, ver9FrameLabel}, opts] \
returns Cell suitable for all versions of Mathematica, with given style, and \
CellFrameLabels option: \
{{IfPreVer9[preVer9FrameLabel ,ver9FrameLabel], None}, {None, None}}"


FrameLabelCell::usage =
"\
FrameLabelCell[content, style] \
returns Cell with given content and style and with options suitable for cell \
in frame label."


FrameLabelSpacerCell::usage =
"\
FrameLabelSpacerCell[\"content\", style] \
returns Cell with given style and content converted to title case and \
preceded by spacer call. Cell has options suitable for cell in frame label."


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*Start Front End*)


Developer`InstallFrontEnd[]


(* ::Subsection:: *)
(*Private symbols*)


(* ::Subsubsection:: *)
(*ToTitleCase*)


ToTitleCase[str_String] :=
	StringReplace[ToLowerCase[str], WordBoundary ~~ x_ :> ToUpperCase[x]];


(* ::Subsubsection:: *)
(*IfPreVer9*)


IfPreVer9[pre_, post_] :=
	FEPrivate`If[ 
		FEPrivate`Or[
			FEPrivate`SameQ[FEPrivate`$ProductVersion, "6.0"],
			FEPrivate`SameQ[FEPrivate`$ProductVersion, "7.0"],
			FEPrivate`SameQ[FEPrivate`$ProductVersion, "8.0"]
		]
		, 
		pre
		,
		post
	]


(* ::Subsubsection:: *)
(*SpacerCell*)


SpacerCell = Cell[BoxData[ToBoxes[Spacer[24]]]]


(* ::Subsubsection:: *)
(*SectionSpacerCell*)


SectionSpacerCell[style_] :=
	Cell[
		"",
		style
		,
		CellSize -> IfPreVer9[{Inherited, 1}, Inherited],
		CellElementSpacings -> {CellMinHeight -> IfPreVer9[1, Inherited]},
		CellMargins -> IfPreVer9[0, Inherited],
		Editable -> False,
		Selectable -> False,
		Deletable -> False, 
		ShowCellBracket -> False,
		ShowSelection -> False
	]


(* ::Subsubsection:: *)
(*SectionCell*)


SectionCell[style_, {preVer9FL_, ver9FL_}, opts___] :=
	Cell[
		"",
		style,
		"WholeCellGroupOpener" -> True,
		CellFrameLabelMargins -> 0,
		CellElementSpacings -> {"CellMinHeight" -> 3},
		CellSize -> {Inherited, IfPreVer9[11, 14]},
		opts, 
		CellFrameLabels -> {
			{
				IfPreVer9[
					preVer9FL, 
					ver9FL
				],
				None
			},
			{None, None}
		}
	]


(* ::Subsubsection:: *)
(*FrameLabelCell*)


FrameLabelCell[content_, style_] :=
	Cell[content, style, CellSize -> {5000, Inherited}]


(* ::Subsubsection:: *)
(*FrameLabelSpacerCell*)


FrameLabelSpacerCell[content_String, style_] :=
	FrameLabelCell[TextData[{SpacerCell, ToTitleCase[content]}], style]


(* ::Subsection:: *)
(*Public symbols*)


(* ::Subsubsection:: *)
(*$HeadingStyles*)


$HeadingStyles = {
	"GuideMoreAboutSection",
	"GuideTutorialsSection",
	"MoreAboutSection",
	"MoreInformationSection",
	"PrimaryExamplesSection",
	"RelatedDemonstrationsSection",
	"RelatedLinksSection",
	"SeeAlsoSection",
	"TutorialsSection",
	"RelatedTutorialsSection",
	"TutorialMoreAboutSection",
	"TutorialRelatedLinksSection",
	"NotesSection",
	"GuideRelatedLinksSection"
}


(* ::Subsubsection:: *)
(*NotebookCrossVersionFix*)


NotebookCrossVersionFix::wrongPath =
"Given string: `1` does not represent valid path to a directory nor to a file."


Options[NotebookCrossVersionFix] = {"Verbose" -> 2, "PrintFunction" -> Print}


NotebookCrossVersionFix[nb_Notebook, OptionsPattern[]] :=
	Module[
		{expr = nb}
		,
		
		expr = expr /. {
			
			(*"MORE INFORMATION" section-- get rid of frame*)
			Cell[con_, sty : "NotesSection", o___] :>
				SectionCell[
					sty,
					{
						FrameLabelCell[con, sty]
						,
						con /.
			  				TextData[Cell[BoxData[box : ButtonBox[__]]]] :> TextData[box] /. 
								_FrameBox -> 
									FrameLabelCell[
										TextData[{SpacerCell, "Details and Options"}], 
										sty
									]
					},
					o
				]
			,
			(*Guide Tutorials section-- need to mimic standard version 9 section style*)
			Cell[con_, sty : "GuideTutorialsSection", o___] :>
				SectionCell[
					sty,
					{
						FrameLabelCell[con, sty],
						con /. str_String -> FrameLabelSpacerCell[str, sty]
					},
					o,
					CellMargins ->
						IfPreVer9[
							Inherited
							,
							{{Inherited, Inherited}, {Inherited, 20}}
						]
				]
			,
			(*Examples section-- add placeholder for total example count*)
			Cell[con_, sty : "PrimaryExamplesSection", o___] :>
				SectionCell[
					sty,
					{
						FrameLabelCell[con, sty]
						, 
						con /. (ButtonBox[str_String, bbo___] :> 
							ButtonBox[
								FrameLabelCell[
									TextData[{
										SpacerCell,
										ToTitleCase[str],
										"  ", 
										"InsertExampleCount"
									}],
									sty
								],
								bbo
							]
						)
					},
					o
				]
			,
			(*All other section headings with text only in the title*)
			Cell[con_String, sty : Alternatives @@ $HeadingStyles, o___] :>
				SectionCell[
					sty,
					{
						FrameLabelCell[con, sty],
						FrameLabelSpacerCell[con, sty]
					},
					o
				]
			,
			(*All other section headings with buttons in the title*)
			Cell[con_, sty : Alternatives @@ $HeadingStyles, o___] :>
				SectionCell[
					sty,
					{
						FrameLabelCell[con, sty]
						,
						con /. (ButtonBox[str_String, bbo___] :>
							ButtonBox[FrameLabelSpacerCell[str, sty], bbo]
						)
					},
					o
				]
		};

		(* Replace "Details and Options" with "Details" if there is no mention of options in the notes section *)
		expr = expr /.
			notescell : CellGroupData[{Cell["", "NotesSection", ___], ___}, ___] :> (
				notescell /.
					If[Count[notescell, str_String /; ! StringFreeQ[str, {"option", "Option"}], Infinity] > 1,
						{},
						"Details and Options" -> "Details"
					]
				);

		(* Add total example count to Examples section heading in version 9 *)
		expr = expr /.
			(examplegroup : Cell[CellGroupData[{Cell[_, "PrimaryExamplesSection", ___], ___}, ___], ___]) :> (
				examplegroup /. "InsertExampleCount" ->
					Cell[
						"(" <> ToString@Total@Cases[
							examplegroup,
							Cell[countstring_, "ExampleCount"] :> ToExpression[countstring],
							Infinity
						] <> ")"
						,
						"ExampleCount"
					]
			);

		(* Add spacers before and after section content in version 9 *)
		expr = expr /. {
			Cell[CellGroupData[{c1 : Cell[_, "SeeAlsoSection", ___], c2__}, o2___], o3___] :>
				Cell[CellGroupData[{c1, c2}, o2], o3]
			,
			Cell[CellGroupData[{c1 : Cell[_, "PrimaryExamplesSection", ___], c2__}, o2___], o3___] :>
				Cell[CellGroupData[{c1, c2, SectionSpacerCell["SectionFooterSpacer"]}, o2], o3]
			,
			Cell[CellGroupData[{c1 : Cell[_, Alternatives @@ $HeadingStyles, ___], c2__}, o2___], o3___] :>
				Cell[CellGroupData[{c1, SectionSpacerCell["SectionHeaderSpacer"], c2, SectionSpacerCell["SectionFooterSpacer"]}, o2], o3]
		};


		(*Fix button behavior in Example subsections*)
		expr = expr /.
			Cell[con__, sty : "ExampleSection" | "ExampleSubsection", o___] :>
				Cell[con, sty, "WholeCellGroupOpener" -> True, o];

		(*Fix rendering bug pre-version 9 *)
		expr = expr /.
			Cell[c_, "GuideAbstract", o___] :>
				Cell[c, "GuideAbstract", CellFrame -> IfPreVer9[{{0, 0}, {1, 0}}, Inherited], o];

		(* Improve font appearance for tutorial links in Guide pages, and mimic section heading style for Guide Tutorial heading *)
		expr = expr /. {
			"GuideTutorial" -> "GuideMoreAbout",
			"GuideTutorialsSection" -> "GuideMoreAboutSection"
		};

		expr
	]

NotebookCrossVersionFix[
	file_String /; FileType[file] === File,
	opts:OptionsPattern[]
] :=
	Module[
		{nb}
		,
		If[OptionValue["Verbose"] >= 2,
			OptionValue["PrintFunction"][
				"\tRunning cross-version replacements on " <> file <> " file"
			];
		];
		
		(* Importing nb files containing Manipulate objects can throw a bunch of benign newline interpretation warnings *)
		Quiet[
			nb = Get[file],
			Syntax::newl
		];
		
		nb = NotebookCrossVersionFix[nb, opts];

		(* Save notebook using Front End *)
		Developer`UseFrontEnd@With[
			{nbObj = NotebookPut[nb]}
			,
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
			NotebookSave[nbObj, file];
			NotebookClose[nbObj];
		];
		
		file
	]

NotebookCrossVersionFix[fileList:{___String}, opts:OptionsPattern[]] := (
	If[OptionValue["Verbose"] >= 1,
		OptionValue["PrintFunction"][
			"Running cross-version replacements on " <>
			ToString@Length@fileList <>
			" files"
		];
	];
	
	Do[(* Loop over files *)
		NotebookCrossVersionFix[file, opts],
		{file, fileList}
	]
)

NotebookCrossVersionFix[
	dir_String /; FileType[dir] === Directory,
	opts:OptionsPattern[]
] := (
	If[OptionValue["Verbose"] >= 1,
		OptionValue["PrintFunction"][
			"Running cross-version replacements in " <> dir
		];
	];
	NotebookCrossVersionFix[FileNames["*.nb", dir, Infinity], opts]
)

NotebookCrossVersionFix[unknownString_String, OptionsPattern[]] := (
	Message[NotebookCrossVersionFix::wrongPath, unknownString];
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
