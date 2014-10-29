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
	Module[
		{someSymbol = 5}
		,
		Test[
			LinkFullContextWrite[
				link,
				Unevaluated @ EvaluatePacket[Context[someSymbol]]
			]
			,
			Null
			,
			TestID -> "implicit context: LinkFullContextWrite evaluation"
		];
		
		Test[
			LinkRead[link, Hold]
			,
			Hold[ReturnPacket["TestEnvironment`LinkFullContextWrite`"]]
			,
			TestID -> "implicit context: returned packet"
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
		TestID -> "not in $ContextPath, not in $Context: \
LinkFullContextWrite evaluation"
	];
	
	Test[
		LinkRead[link, Hold]
		,
		Hold[ReturnPacket["TestContext`"]]
		,
		TestID -> "not in $ContextPath, not in $Context: returned packet"
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
			TestID -> "in $ContextPath, not in $Context: \
LinkFullContextWrite evaluation"
		];
	];
	
	Test[
		LinkRead[link, Hold]
		,
		Hold[ReturnPacket["TestContext`"]]
		,
		TestID -> "in $ContextPath, not in $Context: returned packet"
	];
	
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	Block[
		{$Context = "TestContext`"}
		,
		Test[
			LinkFullContextWrite[
				link,
				Unevaluated @ EvaluatePacket[Context[TestContext`test]]
			]
			,
			Null
			,
			TestID -> "not in $ContextPath, in $Context: \
LinkFullContextWrite evaluation"
		];
	];
	
	Test[
		LinkRead[link, Hold]
		,
		Hold[ReturnPacket["TestContext`"]]
		,
		TestID -> "not in $ContextPath, in $Context: returned packet"
	];
	
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	Block[
		{
			$ContextPath = Prepend[$ContextPath, "TestContext`"],
			$Context = "TestContext`"
		}
		,
		Test[
			LinkFullContextWrite[
				link,
				Unevaluated @ EvaluatePacket[Context[TestContext`test]]
			]
			,
			Null
			,
			TestID ->
				"in $ContextPath, in $Context: LinkFullContextWrite evaluation"
		];
	];
	
	Test[
		LinkRead[link, Hold]
		,
		Hold[ReturnPacket["TestContext`"]]
		,
		TestID -> "in $ContextPath, in $Context: returned packet"
	];
	
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	Test[
		LinkFullContextWrite[
			link,
			Unevaluated @ EvaluatePacket[Context[System`test2]]
		]
		,
		Null
		,
		TestID -> "System` context: LinkFullContextWrite evaluation"
	];
	
	Test[
		LinkRead[link, Hold]
		,
		Hold[ReturnPacket["System`"]]
		,
		TestID -> "System` context: returned packet"
	];
	
	Remove[System`test2];
	LinkClose[link];
]


With[
	{link = LinkLaunch[command]}
	,
	LinkWrite[link, Unevaluated @ EvaluatePacket[Begin["TestContext`"];]];
	While[LinkRead[link, Hold] =!= Hold[ReturnPacket[Null]]];
	
	Test[
		LinkFullContextWrite[
			link,
			Unevaluated @ EvaluatePacket[Context[Global`test3]]
		]
		,
		Null
		,
		TestID -> "Global` context: LinkFullContextWrite evaluation"
	];
	
	Test[
		LinkRead[link, Hold]
		,
		Hold[ReturnPacket["Global`"]]
		,
		TestID -> "Global` context: returned packet"
	];
	
	Remove[Global`test3];
	LinkClose[link];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["TestContext`*"]
Quiet[Remove["TestContext`*"], {Remove::rmnsm}]
Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
