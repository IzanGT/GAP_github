#############################################################################
##
##  Centre.g
##
##  Everything related to the centre Z(S_hat).  Builds the canonical cyclic
##  table, the fusion maps t_Z -> table, and applies Schur's lemma to assign
##  to each character chi its lambda in Irr(Z(S_hat)).
##
##  Conventions (essential to make condition (3) of McKay comparable across
##  classifications):
##
##    - The canonical centre table is always CharacterTable("Cyclic", n)
##      with n = |Z(S_hat)| = schur_multiplier.  Sharing the SAME t_Z
##      between the S_hat side and the N_{S_hat}(P) side guarantees that
##      the lambda indices are directly comparable.
##
##    - The generator "z" of the centre is chosen canonically from either
##      the table (lowest-indexed central class of order n) or the group
##      (MinimalGeneratingSet(Z)[1]).  In the standard path, the same
##      Z(S_hat) is reused to build fus_Z_N (in N_{S_hat}(P)).
##
##    - In the library path, fus_Z_N is built preferably by composing
##      fus_Z_S with the CTblLib stored fusion map table_N -> table_S_hat
##      (via FusionMapToCentreViaFusionMap), which is robust even when
##      |Z(N)| > |Z(S_hat)|.
##
#############################################################################

#############################################################################
##
##  ChooseCentreGeneratorIndex( table, n )
##
##  Index (in the table) of a central class containing a generator of the
##  cyclic centre of order n.  Deterministic canonical criterion: the
##  LOWEST index among classes of size 1 with element order exactly n.
##
##  Case n = 1 (trivial centre): returns 1 (the identity class).
##
#############################################################################
ChooseCentreGeneratorIndex := function(table, n)
    local sizes, orders, candidates;;

    if n = 1 then
        return 1;;
    fi;;

    sizes := SizesConjugacyClasses(table);;
    orders := OrdersClassRepresentatives(table);;

    candidates := Filtered(
        [1 .. NrConjugacyClasses(table)],
        i -> sizes[i] = 1 and orders[i] = n
    );;

    if Length(candidates) <> Phi(n) then
        Error(
            "Expected Phi(", n, ") = ", Phi(n),
            " centre-generator classes (order ", n,
            ", size 1) but found ", Length(candidates), "."
        );;
    fi;;

    return Minimum(candidates);;
end;;

#############################################################################
##
##  FusionMapToCentreFromTable( table, n )
##
##  Builds fus_Z : t_Z -> table, where t_Z = CharacterTable("Cyclic", n)
##  and the k-th class of t_Z represents z^{k-1}.  Locates the powers
##  z^0, ..., z^{n-1} among the central classes of the table via PowerMap.
##
##  Returns [1] when n = 1.
##
##  Validation: the n powers must cover exactly the n central classes of
##  the table without repetition.  If |Z(group)| > n (case of N with a
##  centre larger than that of S_hat), this function raises an error and
##  the caller must use FusionMapToCentreViaFusionMap.
##
#############################################################################
FusionMapToCentreFromTable := function(table, n)
    local sizes, orders, central_classes, idx_z, fus_Z, k, ord;;

    if n = 1 then
        return [1];;
    fi;;

    sizes := SizesConjugacyClasses(table);;
    orders := OrdersClassRepresentatives(table);;

    central_classes := Filtered(
        [1 .. NrConjugacyClasses(table)],
        i -> sizes[i] = 1
    );;

    if Length(central_classes) <> n then
        Error(
            "Expected ", n, " central classes (|Z|=", n,
            ") but the table has ", Length(central_classes),
            ". Use FusionMapToCentreViaFusionMap if |Z(table)| > |Z(S_hat)|."
        );;
    fi;;

    idx_z := ChooseCentreGeneratorIndex(table, n);;
    fus_Z := List([0 .. n - 1], k -> PowerMap(table, k)[idx_z]);;

    if fus_Z[1] <> 1 then
        Error(
            "PowerMap(table, 0)[idx_z] = ", fus_Z[1],
            " but expected the identity class (1)."
        );;
    fi;;
    if SortedList(fus_Z) <> central_classes then
        Error(
            "The powers of the centre generator do not cover the ",
            n, " expected central classes."
        );;
    fi;;
    for k in [1 .. n] do
        ord := orders[fus_Z[k]];;
        if n mod ord <> 0 then
            Error(
                "Order of class ", fus_Z[k], " (=", ord,
                ") does not divide |Z|=", n, "."
            );;
        fi;;
    od;;

    return fus_Z;;
end;;

#############################################################################
##
##  FusionMapToCentreViaFusionMap( fus_Z_S, fus_N_S )
##
##  Builds fus_Z_N : t_Z -> table_N by composing fus_Z_S : t_Z -> table_S
##  with the (partial) inverse of fus_N_S : table_N -> table_S.  For each
##  power z^{k-1}:
##      fus_Z_N[k] = the unique i such that fus_N_S[i] = fus_Z_S[k]
##  i.e. the unique class of N (of size 1) that maps to the corresponding
##  central class of S.
##
##  Robust when |Z(N)| > |Z(S_hat)|: extra central classes of Z(N) map to
##  NON-central classes of S and are discarded automatically.
##
#############################################################################
FusionMapToCentreViaFusionMap := function(fus_Z_S, fus_N_S)
    local fus_Z_N, k, idx_S, idx_N;;

    fus_Z_N := [];;
    for k in [1 .. Length(fus_Z_S)] do
        idx_S := fus_Z_S[k];;
        idx_N := PositionProperty(
            [1 .. Length(fus_N_S)],
            i -> fus_N_S[i] = idx_S
        );;
        if idx_N = fail then
            Error(
                "No preimage found for class ", idx_S,
                " (power z^", k - 1, ") in table_N via fus_N_S."
            );;
        fi;;
        Add(fus_Z_N, idx_N);;
    od;;

    return fus_Z_N;;
end;;

#############################################################################
##
##  FusionMapOfCentralSubgroupInGroup( Z, G, t_Z )
##
##  Standard path (no library tables): given a cyclic central subgroup Z
##  of G and the canonical centre table t_Z = CharacterTable("Cyclic", |Z|),
##  builds the fusion map fus_Z : t_Z -> CharacterTable(G).
##
##  t_Z is supplied by the caller (built ONCE in PrecomputeResources) so
##  that the SAME table object is shared by every classification and the
##  lambda indices are directly comparable (invariant I5).
##
##  The generator z is chosen as MinimalGeneratingSet(Z)[1]; the powers
##  z^k are located in ConjugacyClasses(G) via PositionProperty.
##
##  Returns rec(t_Z, fus_Z, n, generator).
##
#############################################################################
FusionMapOfCentralSubgroupInGroup := function(Z, G, t_Z)
    local n, z, classes_G, fus_Z, k, idx;;

    n := Size(Z);;
    if NrConjugacyClasses(t_Z) <> n then
        Error(
            "FusionMapOfCentralSubgroupInGroup: |Z| = ", n,
            " but the supplied t_Z has ", NrConjugacyClasses(t_Z),
            " classes."
        );;
    fi;;

    if n = 1 then
        return rec(t_Z := t_Z, fus_Z := [1], n := 1, generator := One(Z));;
    fi;;

    z := MinimalGeneratingSet(Z)[1];;
    classes_G := ConjugacyClasses(G);;

    fus_Z := [];;
    for k in [0 .. n - 1] do
        idx := PositionProperty(classes_G, c -> z^k in c);;
        if idx = fail then
            Error(
                "No class found for z^", k,
                " in ConjugacyClasses(G)."
            );;
        fi;;
        Add(fus_Z, idx);;
    od;;

    return rec(t_Z := t_Z, fus_Z := fus_Z, n := n, generator := z);;
end;;

#############################################################################
##
##  CentralIndicesOfIrr( table, t_Z, fus_Z, pprime_indices )
##
##  For each chi in table whose index is in pprime_indices, computes the
##  unique lambda in Irr(t_Z) such that chi_{Z} = chi(1) * lambda (Schur's
##  lemma).  Returns the index of lambda in Irr(t_Z).
##
##  If n_Z = 1 (trivial centre), every p'-character gets lambda = 1 (the
##  unique character of the trivial group).
##
##  Validation: the decomposition must have support of size 1 with
##  multiplicity exactly chi(1).
##
##  Returns: a list central_lambda of length Length(Irr(table)) with
##    central_lambda[i] = lambda index   (if i in pprime_indices)
##    central_lambda[i] = fail           (otherwise).
##
#############################################################################
CentralIndicesOfIrr := function(table, t_Z, fus_Z, pprime_indices)
    local irr, num_irr, n_Z, irr_Z, central_lambda, i, chi, rest,
          desc, support;;

    irr := Irr(table);;
    num_irr := Length(irr);;
    n_Z := NrConjugacyClasses(t_Z);;
    central_lambda := List([1 .. num_irr], i -> fail);;

    if n_Z = 1 then
        for i in pprime_indices do
            central_lambda[i] := 1;;
        od;;
        return central_lambda;;
    fi;;

    irr_Z := Irr(t_Z);;

    for i in pprime_indices do
        chi := irr[i];;
        rest := ClassFunctionSameType(
            t_Z, chi, ValuesOfClassFunction(chi){fus_Z}
        );;
        desc := MatScalarProducts(t_Z, irr_Z, [rest])[1];;
        support := Filtered([1 .. n_Z], j -> desc[j] <> 0);;

        if Length(support) <> 1 then
            Error(
                "Schur's lemma violated: chi[", i, "]_Z has support ",
                Length(support), " (expected 1)."
            );;
        fi;;
        if desc[support[1]] <> chi[1] then
            Error(
                "Schur's lemma violated: multiplicity ", desc[support[1]],
                " <> chi(1) = ", chi[1], " for chi[", i, "]."
            );;
        fi;;

        central_lambda[i] := support[1];;
    od;;

    return central_lambda;;
end;;

#############################################################################
##
##  CentreOfNFromLibrary( N_lib_table, S_hat_lib_table, Z_S_lib_fus,
##      schur_multiplier, t_Z )
##
##  Builds fus_Z_N : t_Z -> N_lib_table preferably using the CTblLib stored
##  fusion map N_lib_table -> S_hat_lib_table (the robust path).  If that
##  is not available, falls back to FusionMapToCentreFromTable on
##  N_lib_table (raises a WARNING because it can be off when
##  |Z(N)| > |Z(S_hat)|).
##
##  t_Z is the canonical centre table supplied by the caller (shared with
##  the S_hat side, invariant I5).
##
##  Returns rec(t_Z, fus_Z, robust).
##
#############################################################################
CentreOfNFromLibrary := function(N_lib_table, S_hat_lib_table,
        Z_S_lib_fus, schur_multiplier, t_Z)
    local fus_N_S_lib, fus_Z;;

    fus_N_S_lib := GetFusionMap(N_lib_table, S_hat_lib_table);;

    if fus_N_S_lib <> fail then
        fus_Z := FusionMapToCentreViaFusionMap(Z_S_lib_fus, fus_N_S_lib);;
        return rec(t_Z := t_Z, fus_Z := fus_Z, robust := true);;
    fi;;

    Print(
        "WARNING: no CTblLib fusion map N -> S_hat. ",
        "Using heuristic for fus_Z_N. Condition (3) may be off by ",
        "an automorphism of Z(S_hat).\n"
    );;
    fus_Z := FusionMapToCentreFromTable(N_lib_table, schur_multiplier);;
    return rec(t_Z := t_Z, fus_Z := fus_Z, robust := false);;
end;;
