#############################################################################
##
##  TableClassifier.g
##
##  Classification of Irr_{p'}(G) using ONLY character tables from CTblLib
##  (no group construction).  Applies symmetrically to S_hat and to
##  N_{S_hat}(P).
##
##  Mechanism:
##    - Case |Out| = 1 over G (out_cardinal = 1, or trivial local outer
##      action on P): every p'-character is type 1.  Only the base table
##      is needed for degrees, lambda and conductor.
##    - Case |Out| = 2: use Clifford theory via the extended table
##      table_ext (= table of A or of N_A) and a fusion map
##      table_base -> table_ext.  For each psi in Irr(table_ext), its
##      restriction to Irr(table_base) follows one of two patterns:
##        (a) support 1, multiplicity 1: type-1 witness
##        (b) support 2, both multiplicities 1: type-2 witness
##      Each p'-character must be classified by the witnesses found.
##      This classification IS the Clifford verification by construction.
##
##  The caller supplies t_Z and fus_Z so that the lambda indices are
##  comparable across classifications (see Centre.g).
##
##  Returns a record with the common shape defined in the dispatcher
##  (Classification.g) and via := "library".
##
#############################################################################

#############################################################################
##
##  ClassifyByTables( table_base, table_ext_or_fail, fus_or_fail, p,
##                    t_Z, fus_Z )
##
##  Arguments:
##    table_base         - CharacterTable of the group G (S_hat or N)
##    table_ext_or_fail  - CharacterTable of the cyclic extension (A or
##                         N_A); fail if not applicable (out_cardinal = 1
##                         or trivial local action)
##    fus_or_fail        - fusion map table_base -> table_ext, or fail
##    p                  - prime
##    t_Z, fus_Z         - canonical centre table and fus_Z (from Centre.g)
##
##  Returns a record with fields:
##    via, table, irr, degrees, pprime_indices,
##    case, partner, orbits, central_lambda,
##    t_Z, fus_Z,
##    case1_witnesses, case2_witnesses  (lists parallel to Irr(table_base);
##                                       empty if table_ext_or_fail = fail)
##
#############################################################################
ClassifyByTables := function(table_base, table_ext_or_fail, fus_or_fail, p,
        t_Z, fus_Z)
    local irr_base, n, degrees, pprime_indices, case, partner, orbits,
          irr_ext, restrictions, case1_witnesses, case2_witnesses,
          i, rest, desc, support, j, idx_psi, central_lambda;;

    irr_base := Irr(table_base);;
    n := Length(irr_base);;
    degrees := List(irr_base, chi -> chi[1]);;
    pprime_indices := Filtered([1 .. n], i -> degrees[i] mod p <> 0);;

    case := List([1 .. n], i -> 0);;
    partner := List([1 .. n], i -> fail);;
    orbits := [];;
    case1_witnesses := List([1 .. n], i -> []);;
    case2_witnesses := List([1 .. n], i -> []);;

    if table_ext_or_fail = fail or fus_or_fail = fail then
        # No outer action: every p'-character is type 1.  Each one is its
        # own size-1 orbit.  No witnesses because there is no extension.
        for i in pprime_indices do
            case[i] := 1;;
            Add(orbits, [i]);;
        od;;
    else
        # ---- Classification via Clifford theory ----
        irr_ext := Irr(table_ext_or_fail);;

        ProgressMessage(
            "Restricting Irr(extension) to Irr(base) via fusion map."
        );;
        restrictions := List(
            irr_ext,
            psi -> ClassFunctionSameType(
                table_base, psi, ValuesOfClassFunction(psi){fus_or_fail}
            )
        );;

        # Collect witnesses for each chi of the base.  If the extension
        # H = table_ext_or_fail is not the standard index-2 extension
        # (e.g. [H : G] > 2), restrictions with patterns other than
        # (a) / (b) may appear.  We ignore them: for the sporadic groups
        # it suffices that each p'-character has at least one witness of
        # the correct type.
        for i in [1 .. Length(restrictions)] do
            LoopProgress(
                "Clifford restrictions", i, Length(restrictions)
            );;
            rest := restrictions[i];;
            desc := MatScalarProducts(table_base, irr_base, [rest])[1];;
            support := Filtered([1 .. n], j -> desc[j] <> 0);;

            if Length(support) = 1 and desc[support[1]] = 1 then
                Add(case1_witnesses[support[1]], i);;
            elif Length(support) = 2 and
                 desc[support[1]] = 1 and desc[support[2]] = 1 then
                Add(case2_witnesses[support[1]], i);;
                Add(case2_witnesses[support[2]], i);;
            fi;;
        od;;

        # Assign a case to each p'-character.
        for i in pprime_indices do
            if case[i] <> 0 then
                continue;;
            fi;;

            if Length(case1_witnesses[i]) > 0 and
               Length(case2_witnesses[i]) = 0 then
                case[i] := 1;;
                Add(orbits, [i]);;
            elif Length(case2_witnesses[i]) > 0 and
                 Length(case1_witnesses[i]) = 0 then
                # The partner is the other character in the support of
                # the witnessing restriction.
                idx_psi := case2_witnesses[i][1];;
                rest := restrictions[idx_psi];;
                desc := MatScalarProducts(table_base, irr_base, [rest])[1];;
                support := Filtered([1 .. n], j -> desc[j] <> 0);;
                j := First(support, x -> x <> i);;

                case[i] := 2;;
                case[j] := 2;;
                partner[i] := j;;
                partner[j] := i;;
                Add(orbits, Set([i, j]));;
            else
                Error(
                    "p'-character ", i,
                    " has ambiguous witnesses in the Clifford decomposition ",
                    "(case1_witnesses=", case1_witnesses[i],
                    ", case2_witnesses=", case2_witnesses[i], ")."
                );;
            fi;;
        od;;
    fi;;

    # ---- Central characters (Schur's lemma) ----
    central_lambda := CentralIndicesOfIrr(
        table_base, t_Z, fus_Z, pprime_indices
    );;

    return rec(
        via := "library",
        table := table_base,
        irr := irr_base,
        degrees := degrees,
        pprime_indices := pprime_indices,
        case := case,
        partner := partner,
        orbits := orbits,
        central_lambda := central_lambda,
        t_Z := t_Z,
        fus_Z := fus_Z,
        case1_witnesses := case1_witnesses,
        case2_witnesses := case2_witnesses
    );;
end;;
