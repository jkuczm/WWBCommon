(* Mathematica Init File *)


(* Load proper version of MUnit package *)
Block[
	{$ContextPath}
	,
	Get["MultipleVersionsTests`MUnitVersionedLoader`"]
]
MultipleVersionsTests`MUnitVersionedLoader`NeedsVersionedMUnit[]

Get["MultipleVersionsTests`MUnitBackports`"]
Get["MultipleVersionsTests`MultipleVersionsTests`"]
