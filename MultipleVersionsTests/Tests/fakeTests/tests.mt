(* Mathematica Test File *)


Test[
	2 + 2
	,
	4
	,
	TestID -> "PassedFakeTest"
]


Test[
	2 + 2
	,
	5
	,
	TestID -> "FailedFakeTest"
]


TestMatch[
	Message[Which::argctu, HoldForm[Which]]
	,
	Null
	,
	TestID -> "MessageFailedFakeTest"
]
