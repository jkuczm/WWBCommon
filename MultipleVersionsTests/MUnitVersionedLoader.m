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


BeginPackage["MultipleVersionsTests`MUnitVersionedLoader`"]


Unprotect["`*"]


(* ::Section:: *)
(*Usage messages*)


(* ::Subsection:: *)
(*Public symbols usage*)


$WorkbenchMUnitPath::usage =
"\
$WorkbenchMUnitPath \
absolute path to Wolfram Workbench MUnit."


ClearAll[NeedsVersionedMUnit]

NeedsVersionedMUnit::usage =
"\
NeedsVersionedMUnit[] \
loads proper MUnit package for used version of Mathematica."


(* ::Subsection:: *)
(*Private symbols usage*)
	

Begin["`Private`"]


ClearAll["`*"]


$minimalVersionNumber::usage =
"\
$minimalVersionNumber \
minimal supported version of Mathematica."


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*$minimalVersionNumber*)


$minimalVersionNumber = 6


(* ::Subsection:: *)
(*NeedsVersionedMUnit*)


NeedsVersionedMUnit::noSuitableMUnit =
"MUnit package suitable for Mathematica version `1` is not available. \
Minimal supported version is `2`."

NeedsVersionedMUnit::incorrect$WorkbenchMUnitPath =
"Cannot open \"MUnit`.`\" and no correct $WorkbenchMUnitPath provided.\

Either manually add directory containing MUnit package to $Path before \
loading MultipleVersionsTests`.`MUnitVersionedLoader`.` package or set \
$WorkbenchMUnitPath to string containing path to workbench munit versioned \
source directory similar to:\

MultipleVersionsTests`.`MUnitVersionedLoader`.`$WorkbenchMUnitPath = \
\"/path/to/eclipseOrWorkbench/configuration/org.eclipse.osgi/bundles/497/1/\
.cp/MathematicaSourceVersioned\""


NeedsVersionedMUnit[] :=
	Module[
		{result}
		,
		result = Quiet[Needs["MUnit`"]];
		
		If[result =!= $Failed, Return[result]];
		
		If[$VersionNumber < $minimalVersionNumber,
			Message[
				NeedsVersionedMUnit::noSuitableMUnit,
				$VersionNumber,
				$minimalVersionNumber
			];
			Return[$Failed];
		];
		
		If[Head[$WorkbenchMUnitPath] =!= String,
			Message[NeedsVersionedMUnit::incorrect$WorkbenchMUnitPath];
			Return[$Failed];
		];

		Needs[
			"MUnit`"
			,
			FileNameJoin[{
				$WorkbenchMUnitPath
				,
				Which[
					$VersionNumber >= 8,
						"Head",
					$VersionNumber >= 7,
						"Version6",
					True,
						"Version5.2"
				]
				,
				"MUnit/MUnit.m"
			}]
		]
	]



End[]


(* ::Section:: *)
(*Public symbols protection*)


(* Protect all symbols in this context except variables. *)
Protect@Evaluate@Names[Context[] ~~ Except["$"] ~~ Except["`"]...]


EndPackage[]
