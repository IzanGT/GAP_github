#############################################################################
##
##  Bijection.g
##
##  Builds an explicit bijection
##      f : Irr_{p'}(S_hat) --> Irr_{p'}(N_{S_hat}(P))
##  satisfying the four conditions of the refined McKay problem:
##    (1) Aut(S_hat)_P-equivariance (case 1 -> case 1, case 2 -> case 2,
##        respecting the alpha-pairs).
##    (2) Non-increasing degree: f(chi)(1) <= chi(1).
##    (3) Central Schur: chi_{Z(S_hat)} = e * lambda  <=>  f(chi)_{Z(S_hat)} = f * lambda.
##    (4) p-part of the conductor: C(chi)_p = C(f(chi))_p.
##
##  Strategy: group the orbits by (case, lambda or lam_pair, c_p) and
##  match within each block by descending degree.  c_p is alpha-equivariant
##  so the case-2 partners share c_p.
##
##  If two paired sub-blocks have different cardinals between S and N,
##  the bijection reports satisfies_central=false (condition 3) or
##  satisfies_conductor=false (condition 4) and pairs Min(|S|, |N|) elements
##  per (sub-)block with a WARNING.
##
##  The verification of the four conditions is split off into
##  VerifyMcKayConditions, so that the construction is inspectable
##  (the constructor only constructs; the verifier only audits).
##
#############################################################################

#############################################################################
##
##  BuildBijection( class_S, class_N, p )
##
##  Returns a record with:
##    map_S_to_N      list[1..|Irr(S)|]: index in Irr(N) (or fail)
##    map_N_to_S      list[1..|Irr(N)|]: index in Irr(S) (or fail)
##    pairing         list of records with shared fields (case, orbit_S,
##                    orbit_N, degree_S, degree_N, c_S_p, c_N_p) plus
##                    lambda (case 1) or lam_pair, lambdas_S, lambdas_N
##                    (case 2)
##    num_case1, num_case2  counters
##
#############################################################################
BuildBijection := function(class_S, class_N, p)
    local orbits_S_1, orbits_S_2, orbits_N_1, orbits_N_2,
          annotate_singletons, annotate_pairs,
          annotated_S_1, annotated_S_2, annotated_N_1, annotated_N_2,
          comparator, n_S, n_N, map_S_to_N, map_N_to_S, pairing,
          all_lambdas, all_lam_pairs, lam, lp, sub_S_lam, sub_N_lam,
          all_cp, cp, sub_S, sub_N, k, oS, oN, pair_length,
          i_S, j_S, i_N, j_N;;

    # ---- Step 1: separate orbits by case ----
    orbits_S_1 := Filtered(class_S.orbits, o -> Length(o) = 1);;
    orbits_S_2 := Filtered(class_S.orbits, o -> Length(o) = 2);;
    orbits_N_1 := Filtered(class_N.orbits, o -> Length(o) = 1);;
    orbits_N_2 := Filtered(class_N.orbits, o -> Length(o) = 2);;

    # ---- Step 2: annotations (lambda, c_p) per orbit ----
    annotate_singletons := function(orbits, class)
        return List(orbits, o -> rec(
            orbit := o,
            degree := class.degrees[o[1]],
            lambda := class.central_lambda[o[1]],
            c_p := pPartOfConductor(class.irr[o[1]], p)
        ));;
    end;;

    annotate_pairs := function(orbits, class)
        local list, o, l1, l2;;
        list := [];;
        for o in orbits do
            l1 := class.central_lambda[o[1]];;
            l2 := class.central_lambda[o[2]];;
            Add(list, rec(
                orbit := o,
                degree := class.degrees[o[1]],
                lambdas := [l1, l2],
                lam_pair := SortedList([l1, l2]),
                c_p := pPartOfConductor(class.irr[o[1]], p)
            ));;
        od;;
        return list;;
    end;;

    annotated_S_1 := annotate_singletons(orbits_S_1, class_S);;
    annotated_N_1 := annotate_singletons(orbits_N_1, class_N);;
    annotated_S_2 := annotate_pairs(orbits_S_2, class_S);;
    annotated_N_2 := annotate_pairs(orbits_N_2, class_N);;

    # ---- Step 3: stable comparator by descending degree ----
    comparator := function(a, b)
        if a.degree <> b.degree then return a.degree > b.degree;; fi;;
        return Minimum(a.orbit) < Minimum(b.orbit);;
    end;;

    n_S := Length(class_S.irr);;
    n_N := Length(class_N.irr);;
    map_S_to_N := List([1 .. n_S], i -> fail);;
    map_N_to_S := List([1 .. n_N], i -> fail);;
    pairing := [];;

    # ---- Step 4a: case 1, blocks by lambda and c_p ----
    all_lambdas := Union(
        Set(List(annotated_S_1, x -> x.lambda)),
        Set(List(annotated_N_1, x -> x.lambda))
    );;
    for lam in all_lambdas do
        sub_S_lam := Filtered(annotated_S_1, x -> x.lambda = lam);;
        sub_N_lam := Filtered(annotated_N_1, x -> x.lambda = lam);;
        all_cp := Union(
            Set(List(sub_S_lam, x -> x.c_p)),
            Set(List(sub_N_lam, x -> x.c_p))
        );;
        for cp in all_cp do
            sub_S := Filtered(sub_S_lam, x -> x.c_p = cp);;
            sub_N := Filtered(sub_N_lam, x -> x.c_p = cp);;
            Sort(sub_S, comparator);;
            Sort(sub_N, comparator);;
            pair_length := Minimum(Length(sub_S), Length(sub_N));;
            for k in [1 .. pair_length] do
                oS := sub_S[k];;
                oN := sub_N[k];;
                map_S_to_N[oS.orbit[1]] := oN.orbit[1];;
                map_N_to_S[oN.orbit[1]] := oS.orbit[1];;
                Add(pairing, rec(
                    case := 1,
                    orbit_S := oS.orbit,
                    orbit_N := oN.orbit,
                    degree_S := oS.degree,
                    degree_N := oN.degree,
                    lambda := lam,
                    c_S_p := oS.c_p,
                    c_N_p := oN.c_p
                ));;
            od;;
        od;;
    od;;

    # ---- Step 4b: case 2, blocks by lam_pair and c_p ----
    all_lam_pairs := Union(
        Set(List(annotated_S_2, x -> x.lam_pair)),
        Set(List(annotated_N_2, x -> x.lam_pair))
    );;
    for lp in all_lam_pairs do
        sub_S_lam := Filtered(annotated_S_2, x -> x.lam_pair = lp);;
        sub_N_lam := Filtered(annotated_N_2, x -> x.lam_pair = lp);;
        all_cp := Union(
            Set(List(sub_S_lam, x -> x.c_p)),
            Set(List(sub_N_lam, x -> x.c_p))
        );;
        for cp in all_cp do
            sub_S := Filtered(sub_S_lam, x -> x.c_p = cp);;
            sub_N := Filtered(sub_N_lam, x -> x.c_p = cp);;
            Sort(sub_S, comparator);;
            Sort(sub_N, comparator);;
            pair_length := Minimum(Length(sub_S), Length(sub_N));;
            for k in [1 .. pair_length] do
                oS := sub_S[k];;
                oN := sub_N[k];;
                # Pair orientation: match members with the same individual
                # lambda.  Since both share lam_pair, this is always
                # possible.
                i_S := oS.orbit[1];;
                j_S := oS.orbit[2];;
                if oN.lambdas[1] = oS.lambdas[1] then
                    i_N := oN.orbit[1];;
                    j_N := oN.orbit[2];;
                else
                    i_N := oN.orbit[2];;
                    j_N := oN.orbit[1];;
                fi;;
                map_S_to_N[i_S] := i_N;;
                map_S_to_N[j_S] := j_N;;
                map_N_to_S[i_N] := i_S;;
                map_N_to_S[j_N] := j_S;;
                Add(pairing, rec(
                    case := 2,
                    orbit_S := oS.orbit,
                    orbit_N := oN.orbit,
                    degree_S := oS.degree,
                    degree_N := oN.degree,
                    lam_pair := lp,
                    lambdas_S := oS.lambdas,
                    lambdas_N := oN.lambdas,
                    c_S_p := oS.c_p,
                    c_N_p := oN.c_p
                ));;
            od;;
        od;;
    od;;

    return rec(
        map_S_to_N := map_S_to_N,
        map_N_to_S := map_N_to_S,
        pairing := pairing,
        num_case1 := Length(orbits_S_1),
        num_case2 := Length(orbits_S_2)
    );;
end;;

#############################################################################
##
##  VerifyMcKayConditions( bijection, class_S, class_N )
##
##  Audits the four conditions independently and returns a record with
##  one boolean per condition plus a list of detailed warnings (the
##  function never aborts).
##
##  Conditions:
##    cond_1_equivariance  Case (1<->1) and (2<->2) and pair symmetry.
##                         Implicit in the construction; here we check
##                         that each paired orbit respects the case and
##                         the alpha-pair on each side.
##    cond_2_degree        f(chi)(1) <= chi(1) on every paired orbit.
##    cond_3_central       Cardinals per block (case 1 by lambda, case 2
##                         by lam_pair) coincide between the two sides.
##    cond_4_conductor     Equality of c_p on every paired orbit.
##
##  Returns rec(cond_1_equivariance, cond_2_degree, cond_3_central,
##              cond_4_conductor, bijective, all_correct, warnings).
##
##  bijective = true iff |Irr_{p'}(S)| = |Irr_{p'}(N)| AND every p'-character
##  appears exactly once in map_S_to_N (resp. map_N_to_S).
##
#############################################################################
VerifyMcKayConditions := function(bijection, class_S, class_N)
    local warnings, annotate, e, i,
          cond_1, cond_2, cond_3, cond_4, bijective, num_S, num_N,
          count, value_or_zero, group_S, group_N, key,
          counts_S, counts_N,
          missing_lambda, missing_degree, missing_conductor;;

    warnings := [];;
    cond_1 := true;;
    cond_2 := true;;
    cond_3 := true;;
    cond_4 := true;;
    bijective := true;;

    annotate := function(message)
        Add(warnings, message);;
    end;;

    value_or_zero := function(rec_, k)
        if IsBound(rec_.(k)) then return rec_.(k);; fi;;
        return 0;;
    end;;

    # --- Condition 2 (degree) and condition 4 (p-conductor) per pair ---
    missing_degree := false;;
    missing_conductor := false;;
    for e in bijection.pairing do
        if e.degree_N > e.degree_S then
            missing_degree := true;;
            annotate(Concatenation(
                "Cond. (2) violated: degree_N=", String(e.degree_N),
                " > degree_S=", String(e.degree_S),
                " on orbit ", String(e.orbit_S)
            ));;
        fi;;
        if e.c_S_p <> e.c_N_p then
            missing_conductor := true;;
            annotate(Concatenation(
                "Cond. (4) violated: c_S_p=", String(e.c_S_p),
                " <> c_N_p=", String(e.c_N_p),
                " on orbit ", String(e.orbit_S)
            ));;
        fi;;
    od;;
    cond_2 := not missing_degree;;
    cond_4 := not missing_conductor;;

    # --- Condition 3 (central Schur) by block cardinality ---
    # For case 1: blocks by lambda.  For case 2: by lam_pair.
    count := function(annotated_orbits, key_field)
        local res, x, c;;
        res := rec();;
        for x in annotated_orbits do
            c := String(x.(key_field));;
            if IsBound(res.(c)) then
                res.(c) := res.(c) + 1;;
            else
                res.(c) := 1;;
            fi;;
        od;;
        return res;;
    end;;

    group_S := List(
        Filtered(class_S.orbits, o -> Length(o) = 1),
        o -> rec(lambda := class_S.central_lambda[o[1]])
    );;
    group_N := List(
        Filtered(class_N.orbits, o -> Length(o) = 1),
        o -> rec(lambda := class_N.central_lambda[o[1]])
    );;
    counts_S := count(group_S, "lambda");;
    counts_N := count(group_N, "lambda");;
    missing_lambda := false;;
    for key in Union(RecNames(counts_S), RecNames(counts_N)) do
        if not IsBound(counts_S.(key)) or
           not IsBound(counts_N.(key)) or
           counts_S.(key) <> counts_N.(key) then
            missing_lambda := true;;
            annotate(Concatenation(
                "Cond. (3) violated in (case 1, lambda=", key,
                "): cardinals ",
                String(value_or_zero(counts_S, key)),
                " (S) vs ",
                String(value_or_zero(counts_N, key)),
                " (N)."
            ));;
        fi;;
    od;;

    group_S := List(
        Filtered(class_S.orbits, o -> Length(o) = 2),
        o -> rec(lam_pair := SortedList(
            [class_S.central_lambda[o[1]], class_S.central_lambda[o[2]]]
        ))
    );;
    group_N := List(
        Filtered(class_N.orbits, o -> Length(o) = 2),
        o -> rec(lam_pair := SortedList(
            [class_N.central_lambda[o[1]], class_N.central_lambda[o[2]]]
        ))
    );;
    counts_S := count(group_S, "lam_pair");;
    counts_N := count(group_N, "lam_pair");;
    for key in Union(RecNames(counts_S), RecNames(counts_N)) do
        if not IsBound(counts_S.(key)) or
           not IsBound(counts_N.(key)) or
           counts_S.(key) <> counts_N.(key) then
            missing_lambda := true;;
            annotate(Concatenation(
                "Cond. (3) violated in (case 2, lam_pair=", key,
                "): cardinals ",
                String(value_or_zero(counts_S, key)),
                " (S) vs ",
                String(value_or_zero(counts_N, key)),
                " (N)."
            ));;
        fi;;
    od;;

    # Direct per-pairing check: every matched orbit must carry the SAME
    # central character lambda (case 1) / unordered lambda-pair (case 2) on
    # both sides.  BuildBijection enforces this by construction (it matches
    # inside (lambda, c_p) blocks), but this independent re-check catches a
    # centre-generator misalignment between the two classifications -- e.g.
    # a regression of the mixed-path bug guarded against in Classification.g
    # (Phi(m) > 1).  The cardinality test above would NOT detect such a
    # swap, so this loop is the actual safety net for condition (C3).
    for e in bijection.pairing do
        if e.case = 1 then
            if class_S.central_lambda[e.orbit_S[1]] <>
               class_N.central_lambda[e.orbit_N[1]] then
                missing_lambda := true;;
                annotate(Concatenation(
                    "Cond. (3) violated: matched case-1 orbit ",
                    String(e.orbit_S), " <-> ", String(e.orbit_N),
                    " has lambda ",
                    String(class_S.central_lambda[e.orbit_S[1]]), " (S) vs ",
                    String(class_N.central_lambda[e.orbit_N[1]]), " (N)."
                ));;
            fi;;
        else
            if SortedList([class_S.central_lambda[e.orbit_S[1]],
                           class_S.central_lambda[e.orbit_S[2]]]) <>
               SortedList([class_N.central_lambda[e.orbit_N[1]],
                           class_N.central_lambda[e.orbit_N[2]]]) then
                missing_lambda := true;;
                annotate(Concatenation(
                    "Cond. (3) violated: matched case-2 orbit ",
                    String(e.orbit_S), " <-> ", String(e.orbit_N),
                    " has a lambda-pair mismatch (",
                    String([class_S.central_lambda[e.orbit_S[1]],
                            class_S.central_lambda[e.orbit_S[2]]]), " (S) vs ",
                    String([class_N.central_lambda[e.orbit_N[1]],
                            class_N.central_lambda[e.orbit_N[2]]]), " (N))."
                ));;
            fi;;
        fi;;
    od;;
    cond_3 := not missing_lambda;;

    # --- Condition 1 (equivariance) and bijectivity ---
    # Equivariance: each paired entry respects the case and (for case 2)
    # has size-2 orbits on both sides.  This is enforced by construction
    # in BuildBijection; we re-check it here.
    for e in bijection.pairing do
        if e.case = 2 then
            if Length(e.orbit_S) <> 2 or Length(e.orbit_N) <> 2 then
                cond_1 := false;;
                annotate(Concatenation(
                    "Cond. (1) violated: case-2 orbit without partner ",
                    String(e.orbit_S), " <-> ", String(e.orbit_N)
                ));;
            fi;;
        fi;;
    od;;

    num_S := Length(class_S.pprime_indices);;
    num_N := Length(class_N.pprime_indices);;
    if num_S <> num_N then
        bijective := false;;
        annotate(Concatenation(
            "Number of p'-characters differs: ", String(num_S),
            " (S) vs ", String(num_N), " (N)."
        ));;
    fi;;
    for i in class_S.pprime_indices do
        if bijection.map_S_to_N[i] = fail then
            bijective := false;;
            annotate(Concatenation(
                "p'-character chi_S[", String(i), "] has no image in N."
            ));;
        fi;;
    od;;
    for i in class_N.pprime_indices do
        if bijection.map_N_to_S[i] = fail then
            bijective := false;;
            annotate(Concatenation(
                "p'-character chi_N[", String(i), "] has no preimage in S."
            ));;
        fi;;
    od;;

    return rec(
        cond_1_equivariance := cond_1,
        cond_2_degree := cond_2,
        cond_3_central := cond_3,
        cond_4_conductor := cond_4,
        bijective := bijective,
        all_correct := cond_1 and cond_2 and cond_3 and cond_4 and bijective,
        warnings := warnings
    );;
end;;
