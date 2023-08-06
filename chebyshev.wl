SymbolList[str_, start_, length_] := Table[Symbol[str <> ToString[i]], {i, start, start + length - 1}]

(* s Polynomial with n0 prepended *)
sBase[{}] := 0
sBase[{_}] := 1
sBase[{ns___, nk1_, nk_}] := sBase[{ns, nk1, nk}] =
  nk * u * sBase[{ns, nk1}] - sBase[{ns}]

(* Normal s polynomial *)
sPoly[{ns___}] := sBase[{err, ns}]
