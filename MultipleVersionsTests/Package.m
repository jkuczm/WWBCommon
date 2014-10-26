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


BeginPackage["MultipleVersionsTests`Package`"]


(* ::Section:: *)
(*Usage messages*)


(* ::Subsection:: *)
(*Public symbols usage*)


$logFunctions::usage =
"\
$logFunctions \
is a list of all MUnit logging functions from all supported versions of MUnit."


multipleVersionsTestsTag::usage =
"\
multipleVersionsTestsTag \
tag used internally by MultipleVersionsTests package."


(* ::Section:: *)
(*Implementation*)


(*
	Unprotect all symbols in this context
	(all public symbols provided by this package)
*)
Unprotect["`*"]
	

Begin["`Private`"]


(* ::Subsection:: *)
(*$logFunctions*)


$logFunctions = {
	(*
		Make sure all Log... functions are in MUnit` context even if they are
		not defined in used version of MUnit.
	*)
	MUnit`LogAbsoluteTimeUsed,
	MUnit`LogBegin,
	MUnit`LogBeginTestSection,
	MUnit`LogBeginTestSource,
	MUnit`LogCPUTimeUsed,
	MUnit`LogEnd,
	MUnit`LogEndTestSection,
	MUnit`LogEndTestSource,
	MUnit`LogError,
	MUnit`LogErrorCount,
	MUnit`LogExpressionCount,
	MUnit`LogFailure,
	MUnit`LogFailureCount,
	MUnit`LogFatal,
	MUnit`LogMemoryUsed,
	MUnit`LogMessage,
	MUnit`LogMessagesFailure,
	MUnit`LogMessagesFailureCount,
	MUnit`LogMetaData,
	MUnit`LogSkippedTestCount,
	MUnit`LogStart,
	MUnit`LogSuccess,
	MUnit`LogSuccessCount,
	MUnit`LogTestCount,
	MUnit`LogTestID,
	MUnit`LogTestInfo,
	MUnit`LogTestRunProgress,
	MUnit`LogWasAborted,
	MUnit`LogWasFatal,
	MUnit`LogWasSuccessful,
	MUnit`LogWasSyntax
}


End[]


(* ::Section:: *)
(*Public symbols protection*)


(*
	Protect all symbols in this context
	(all public symbols provided by this package)
*)
Protect["`*"]


EndPackage[]
