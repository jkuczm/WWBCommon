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


BeginPackage["TestEnvironment`MUnitBackports`muteLoggers`", {"MUnit`"}]


Get["MultipleVersionsTests`MUnitBackports`"]
PrependTo[$ContextPath, "MultipleVersionsTests`MUnitBackports`Private`"]


Get["MultipleVersionsTests`Package`"]
Get["MultipleVersionsTests`Tests`Utilities`"]


logger = listLogger[]

With[
	{logger = logger}
	,
	originalUpValues = UpValues[logger]
]


(* ::Section:: *)
(*Tests*)


Test[
	muteLoggers[{logger}]
	,
	{originalUpValues}
	,
	TestID -> "returned value"
]


(* Use all known log functions. *)
Scan[#[logger]&, $logFunctions]


Test[
	OptionValue[logger, "Log"]
	,
	{
		Hold[LogBeginTestSection],
		Hold[LogEndTestSection],
		Hold[LogError],
		Hold[LogFailure],
		Hold[LogFatal],
		Hold[LogMessage],
		Hold[LogMessagesFailure],
		Hold[LogMetaData],
		Hold[LogSuccess],
		Hold[LogTestID],
		Hold[LogTestInfo],
		Hold[LogTestRunProgress]
	}
	,
	TestID -> "saved log function calls"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
