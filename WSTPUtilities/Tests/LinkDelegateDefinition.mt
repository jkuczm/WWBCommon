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


BeginPackage["TestEnvironment`LinkDelegateDefinition`", {"MUnit`"}]


Get["WSTPUtilities`"]


command = First[$CommandLine] <> " -mathlink -noprompt"


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


(* ::Subsubsection:: *)
(*No arguments*)


TestMatch[
	LinkDelegateDefinition[]
	,
	HoldPattern @ LinkDelegateDefinition[]
	,
	{HoldForm @ Message[
		LinkDelegateDefinition::argrx, HoldForm @ LinkDelegateDefinition, 0, 2
	]}
	,
	TestID -> "no args"
]


(* ::Subsection:: *)
(*One argument*)


TestMatch[
	LinkDelegateDefinition[someSymbol]
	,
	HoldPattern @ LinkDelegateDefinition[someSymbol]
	,
	{HoldForm @ Message[
		LinkDelegateDefinition::argrx, HoldForm @ LinkDelegateDefinition, 1, 2
	]}
	,
	TestID -> "1 arg"
]


(* ::Subsection:: *)
(*Two arguments*)


TestMatch[
	LinkDelegateDefinition[someSymbol, someSymbol2]
	,
	HoldPattern @ LinkDelegateDefinition[someSymbol, someSymbol2]
	,
	{HoldForm @ Message[
		LinkObject::linkv,
		HoldForm @ someSymbol,
		HoldForm @ LinkDelegateDefinition[someSymbol, someSymbol2]
	]}
	,
	TestID -> "2 args: first non-LinkObject"
]


(* ::Subsection:: *)
(*Three arguments*)


With[
	{link = LinkLaunch[command]}
	,
	TestMatch[
		LinkDelegateDefinition[link, someSymbol, someSymbol2]
		,
		HoldPattern @ LinkDelegateDefinition[link, someSymbol, someSymbol2]
		,
		{HoldForm @ Message[
			LinkDelegateDefinition::nonopt,
			HoldForm @ someSymbol2,
			2,
			HoldForm @ LinkDelegateDefinition[link, someSymbol, someSymbol2]
		]}
		,
		TestID -> "3 args: first LinkObject, third non-option"
	];
	
	LinkClose[link];
]


TestMatch[
	LinkDelegateDefinition[someSymbol, someSymbol2, "Full" -> True]
	,
	HoldPattern @
		LinkDelegateDefinition[someSymbol, someSymbol2, "Full" -> True]
	,
	{HoldForm @ Message[
		LinkObject::linkv,
		HoldForm @ someSymbol,
		HoldForm @
			LinkDelegateDefinition[someSymbol, someSymbol2, "Full" -> True]
	]}
	,
	TestID -> "3 args: first non-LinkObject, third option"
]


TestMatch[
	LinkDelegateDefinition[someSymbol, someSymbol2, someSymbol3]
	,
	HoldPattern @ LinkDelegateDefinition[someSymbol, someSymbol2, someSymbol3]
	,
	{HoldForm @ Message[
		LinkObject::linkv,
		HoldForm @ someSymbol,
		HoldForm @ LinkDelegateDefinition[someSymbol, someSymbol2, someSymbol3]
	]}
	,
	TestID -> "3 args: first non-LinkObject, third non-option"
]


(* ::Subsection:: *)
(*Correct arguments*)


(* ::Subsubsection:: *)
(*Own value*)


With[
	{
		link = LinkLaunch[command],
		sym = Unique["testSymbol"]
	}
	,
	sym = 5;
	
	Test[
		LinkDelegateDefinition[link, sym]
		,
		5
		,
		TestID -> "Own value: LinkDelegateDefinition evaluation"
	];
	
	LinkWrite[link, Unevaluated @ EvaluatePacket[sym]];
	
	Test[
		LinkRead[link, Hold]
		,
		Hold[ReturnPacket[5]]
		,
		TestID -> "Own value: defined symbol remote evaluation"
	];
	
	LinkClose[link];
]


(* ::Subsubsection:: *)
(*Down value*)


With[
	{
		link = LinkLaunch[command],
		sym = Unique["testSymbol"]
	}
	,
	sym[x_] := 2 x;
	
	Test[
		LinkDelegateDefinition[link, sym]
		,
		Null
		,
		TestID -> "Down value: LinkDelegateDefinition evaluation"
	];
	
	LinkWrite[link, Unevaluated @ EvaluatePacket[sym[3]]];
	
	Test[
		LinkRead[link, Hold]
		,
		Hold[ReturnPacket[6]]
		,
		TestID -> "Down value: defined symbol remote evaluation"
	];
	
	LinkClose[link];
]


(* ::Subsubsection:: *)
(*Up value*)


With[
	{
		link = LinkLaunch[command],
		sym = Unique["testSymbol"]
	}
	,
	sym /: Abs[sym] = 0;
	
	Test[
		LinkDelegateDefinition[link, sym]
		,
		0
		,
		TestID -> "Up value: LinkDelegateDefinition evaluation"
	];
	
	LinkWrite[link, Unevaluated @ EvaluatePacket[Abs[sym]]];
	
	Test[
		LinkRead[link, Hold]
		,
		Hold[ReturnPacket[0]]
		,
		TestID -> "Up value: defined symbol remote evaluation"
	];
	
	LinkClose[link];
]


(* ::Subsubsection:: *)
(*Options*)


With[
	{
		link = LinkLaunch[command],
		sym = Unique["testSymbol"]
	}
	,
	Options[sym] = {"OptionName" -> "OptionValue"};
	
	Test[
		LinkDelegateDefinition[link, sym]
		,
		{"OptionName" -> "OptionValue"}
		,
		TestID -> "Options: LinkDelegateDefinition evaluation"
	];
	
	LinkWrite[link, Unevaluated @ EvaluatePacket[Options[sym]]];
	
	Test[
		LinkRead[link, Hold]
		,
		Hold[ReturnPacket[{"OptionName" -> "OptionValue"}]]
		,
		TestID -> "Options: defined symbol remote evaluation"
	];
	
	LinkClose[link];
]


(* ::Subsubsection:: *)
(*Attributes*)


With[
	{
		link = LinkLaunch[command],
		sym = Unique["testSymbol"]
	}
	,
	SetAttributes[sym, HoldAll];
	
	Test[
		LinkDelegateDefinition[link, sym]
		,
		{HoldAll}
		,
		TestID -> "Attributes: LinkDelegateDefinition evaluation"
	];
	
	LinkWrite[link, Unevaluated @ EvaluatePacket[Attributes[sym]]];
	
	Test[
		LinkRead[link, Hold]
		,
		Hold[ReturnPacket[{HoldAll}]]
		,
		TestID -> "Attributes: defined symbol remote evaluation"
	];
	
	LinkClose[link];
]


(* ::Subsubsection:: *)
(*Definition of dependency*)


With[
	{
		link = LinkLaunch[command],
		sym = Unique["testSymbol"],
		sym2 = Unique["testSymbol"]
	}
	,
	sym2 = 3;
	sym[x_] := sym2 x;
	
	Test[
		LinkDelegateDefinition[link, sym]
		,
		Null
		,
		TestID -> "Definition of dependency: LinkDelegateDefinition evaluation"
	];
	
	LinkWrite[link, Unevaluated @ EvaluatePacket[sym[4]]];
	
	Test[
		LinkRead[link, Hold]
		,
		Hold[ReturnPacket[4 sym2]]
		,
		TestID -> "Definition of dependency: defined symbol remote evaluation"
	];
		
	LinkClose[link];
]


(* ::Subsubsection:: *)
(*Option: "Full" -> True*)


With[
	{
		link = LinkLaunch[command],
		sym = Unique["testSymbol"],
		sym2 = Unique["testSymbol"]
	}
	,
	sym2 = 3;
	sym[x_] := sym2 x;
	
	Test[
		LinkDelegateDefinition[link, sym, "Full" -> True]
		,
		3
		,
		TestID -> "Option: \"Full\" -> True: LinkDelegateDefinition evaluation"
	];
	
	LinkWrite[link, Unevaluated @ EvaluatePacket[sym[4]]];
	
	Test[
		LinkRead[link, Hold]
		,
		Hold[ReturnPacket[12]]
		,
		TestID -> "Option: \"Full\" -> True: defined symbol remote evaluation"
	];
	
	LinkClose[link];
]


(* ::Subsubsection:: *)
(*Head option*)


With[
	{link = LinkLaunch[command]}
	,
	Test[
		LinkDelegateDefinition[link, someSymbol, "Head" -> Hold]
		,
		Hold[Null]
		,
		TestID -> "Head option: returned value"
	];
	
	LinkClose[link];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
