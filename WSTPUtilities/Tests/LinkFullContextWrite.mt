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


BeginPackage["TestEnvironment`LinkFullContextWrite`", {"MUnit`"}]


Get["WSTPUtilities`"]


command = First[$CommandLine] <> " -mathlink -noprompt"


(* ::Section:: *)
(*Tests*)


With[
	{link = LinkLaunch[command]}
	,
	Module[
		{someSymbol = 5}
		,
		Test[
			LinkFullContextWrite[
				link,
				Unevaluated @ EvaluatePacket[someSymbol]
			]
			,
			Null
			,
			TestID -> "no additional evaluation: \
LinkFullContextWrite evaluation"
		];
		
		Test[
			LinkRead[link, Hold]
			,
			Hold[ReturnPacket[someSymbol]]
			,
			TestID -> "no additional evaluation: returned packet"
		];
		
		LinkClose[link];
	];
]


With[
	{link = LinkLaunch[command]}
	,
	Test[
		LinkFullContextWrite[
			link,
			Unevaluated @ EvaluatePacket[Context[TestContext`test]]
		]
		,
		Null
		,
		TestID -> "not in context path: LinkFullContextWrite evaluation"
	];
	
	Test[
		LinkRead[link]
		,
		ReturnPacket["TestContext`"]
		,
		TestID -> "not in context path: returned packet"
	];
	
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	Block[
		{$ContextPath = Prepend[$ContextPath, "TestContext`"]}
		,
		Test[
			LinkFullContextWrite[
				link,
				Unevaluated @ EvaluatePacket[Context[TestContext`test]]
			]
			,
			Null
			,
			TestID -> "in context path: LinkFullContextWrite evaluation"
		];
	];
	
	Test[
		LinkRead[link]
		,
		ReturnPacket["TestContext`"]
		,
		TestID -> "in context path: returned packet"
	];
	
	LinkClose[link];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
