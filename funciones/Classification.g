#############################################################################
##
##  Classification.g
##
##  Per-prime dispatcher: decides independently for each side (the S_hat
##  side and the N_{S_hat}(P) side) whether the classification goes via
##  the library tables or via group construction, and orchestrates the
##  optional Clifford verification.
##
##  The decision is frozen in a per-prime record `plan_p` BEFORE the main
##  loop, which makes it possible to aggregate global flags (what to
##  precompute) and avoid repeating predicates inside the loop.
##
##  Shortcut policy (justified in the appendix of proyecto2.tex,
##  Theorem "appendix-final-conclusion"):
##
##    All that the table-based Clifford restriction needs to classify
##    Irr_{p'}(G) is, in CTblLib:
##      - an index-2 extension H of G realising the non-trivial outer
##        class, together with the fusion map G -> H,
##      - analogously for N_G(P) -> N_H(P) on the normalizer side (the
##        extension N_H(P) exists automatically by a Frattini-style
##        argument).
##
##    For the sporadics the database extension names follow the ATLAS
##    convention with a dot (e.g. "2.M12.2", "2.HS.2N3"), which denotes
##    genuine extensions (not direct products), so the mere availability
##    of the table in CTblLib is enough.
##
##    Decision per side:
##
##      S_hat side:
##        out_cardinal = 1                           -> "library"
##        out_cardinal = 2 and extension_table_name in lib
##                                                   -> "library"
##        otherwise                                  -> "standard"
##
##      N_{S_hat}(P) side:
##        out_cardinal = 1 and N_S_hat_name in lib   -> "library"
##        out_cardinal = 2 and N_S_hat_name and N_A_name in lib
##                                                   -> "library"
##        otherwise                                  -> "standard"
##
##    Clifford verification (only if clifford_verification = true):
##      via = "library"   -> "implicit" (the classification IS based on
##                           Clifford restriction)
##      via = "standard"  -> "group"    (build A or N_A and call
##                           VerifyCliffordInGroup)
##
##  The database field `lift_outer_orden_2_garantizado` was a stronger
##  guarantee (sufficient but not necessary); the algorithm no longer
##  consults it.
##
#############################################################################

#############################################################################
##
##  TableInLibrary( name )
##
##  Predicate: name <> fail and LibInfoCharacterTable(name) <> fail.
##  Returns true iff the table is available in CTblLib.
##
#############################################################################
TableInLibrary := function(name)
    if name = fail then
        return false;;
    fi;;
    if not IsBound(LibInfoCharacterTable) then
        return false;;
    fi;;
    return LibInfoCharacterTable(name) <> fail;;
end;;

#############################################################################
##
##  ComputePlanForPrime( sporadic_data, p, clifford_verification )
##
##  Decides the route (library or standard) per side, the library tables
##  required and the verification mode for the prime p.
##
##  Returns a record with:
##    p, norm_info,
##    via_S, via_N,                # "library" or "standard"
##    verify_S, verify_N,          # "none", "implicit" or "group"
##    needs_S_hat_group,           # build S_hat via AtlasGroup
##    needs_S_hat_aut,             # AutomorphismGroup(S_hat) and outer_gen
##    needs_lib_S_hat,             # S_hat_lib_table (= base_table_name)
##    needs_lib_S_hat_ext,         # extension_lib_table
##    needs_lib_N,                 # N_lib_table for this prime
##    needs_lib_N_ext,             # N_A_lib_table for this prime
##    needs_to_build_A,            # SemidirectProduct(<outer_gen>, S_hat)
##    needs_to_build_N_A           # Normalizer in A of the p-Sylow
##
#############################################################################
ComputePlanForPrime := function(sporadic_data, p, clifford_verification)
    local norm_info, base_in_lib, ext_in_lib,
          N_in_lib, N_ext_in_lib, via_S, via_N,
          verify_S, verify_N, needs_S_hat_group, needs_S_hat_aut,
          needs_to_build_A, needs_to_build_N_A;;

    norm_info := NormalizerTablesByPrime(sporadic_data, p);;

    base_in_lib   := TableInLibrary(sporadic_data.base_table_name);;
    ext_in_lib    := TableInLibrary(sporadic_data.extension_table_name);;
    N_in_lib      := TableInLibrary(norm_info.N_S_hat_name);;
    N_ext_in_lib  := TableInLibrary(norm_info.N_A_name);;

    # ---- Decision for the S_hat side ----
    if sporadic_data.out_cardinal = 1 then
        # |Out| = 1 implies every character is type 1: only the base
        # table is needed.
        via_S := "standard";;
        if base_in_lib then via_S := "library";; fi;;
    else
        # out_cardinal = 2: the Clifford restriction via the extension
        # table only needs CTblLib to provide the index-2 extension (see
        # the appendix of proyecto2.tex).
        via_S := "standard";;
        if base_in_lib and ext_in_lib then via_S := "library";; fi;;
    fi;;

    # ---- Decision for the N_{S_hat}(P) side ----
    if sporadic_data.out_cardinal = 1 then
        via_N := "standard";;
        if N_in_lib then via_N := "library";; fi;;
    else
        via_N := "standard";;
        if N_in_lib and N_ext_in_lib then via_N := "library";; fi;;
    fi;;

    # ---- Consistency guard for the central character (condition C3) ----
    # When Z(S_hat) ~ Z/m has more than one generator (Phi(m) > 1, i.e.
    # m in {3,4,6,12}: M22, McL, J3, O'N, Fi24', Suz), the library path and
    # the standard path choose the centre generator z by DIFFERENT rules
    # (ChooseCentreGeneratorIndex picks the lowest-indexed central class;
    # FusionMapOfCentralSubgroupInGroup picks MinimalGeneratingSet(Z)[1]).
    # Those rules can pick different generators z, z^k of Z/m, which makes
    # CentralIndicesOfIrr record DIFFERENT lambda indices for the same
    # central character on the two sides (verified on 3.J3: 16 of 24
    # 2'-characters get lambda 2 vs 3 depending on the generator).  In a
    # MIXED plan (one side "library", the other "standard") this silently
    # breaks the lambda comparison of condition (C3) and invariant I5.
    #
    # Both HOMOGENEOUS plans are safe: all-"library" composes through the
    # single Z_S_lib_fus (CentreOfNFromLibrary), and all-"standard" reuses
    # the single Z_S_hat for both sides.  So when the plan would be mixed
    # and Phi(m) > 1, force both sides to "standard".  This adds essentially
    # no cost: the mixed plan already needs S_hat and Aut(S_hat) for its
    # standard side, and the standard S_hat classification simply reuses
    # them.  For m in {1,2} (Phi(m) = 1) the generator is unique, so a mixed
    # plan is harmless and is left untouched.
    if via_S <> via_N and Phi(sporadic_data.schur_multiplier) > 1 then
        via_S := "standard";;
        via_N := "standard";;
    fi;;

    # ---- Decision for the Clifford verification ----
    if clifford_verification then
        if via_S = "library" then
            verify_S := "implicit";;
        else
            verify_S := "group";;
        fi;;
        if via_N = "library" then
            verify_N := "implicit";;
        else
            verify_N := "group";;
        fi;;
    else
        verify_S := "none";;
        verify_N := "none";;
    fi;;

    # ---- Required resources ----
    needs_S_hat_group := via_S = "standard" or via_N = "standard";;
    needs_S_hat_aut := needs_S_hat_group and
                      sporadic_data.out_cardinal = 2;;
    needs_to_build_A := verify_S = "group" and
                       sporadic_data.out_cardinal = 2;;
    needs_to_build_N_A := verify_N = "group" and
                         sporadic_data.out_cardinal = 2;;

    return rec(
        p := p,
        norm_info := norm_info,
        via_S := via_S,
        via_N := via_N,
        verify_S := verify_S,
        verify_N := verify_N,
        needs_S_hat_group := needs_S_hat_group,
        needs_S_hat_aut := needs_S_hat_aut,
        # S_hat_lib_table is needed whenever any side goes via the
        # library: the N side uses it to build fus_Z_N via the fusion
        # stored in CTblLib (CentreOfNFromLibrary).
        needs_lib_S_hat := via_S = "library" or via_N = "library",
        needs_lib_S_hat_ext := via_S = "library" and
                               sporadic_data.out_cardinal = 2,
        needs_lib_N := via_N = "library",
        needs_lib_N_ext := via_N = "library" and
                           sporadic_data.out_cardinal = 2,
        needs_to_build_A := needs_to_build_A,
        needs_to_build_N_A := needs_to_build_N_A
    );;
end;;

#############################################################################
##
##  AggregatePlans( plans )
##
##  Combines the boolean need-flags of several plans (one per prime) into
##  a record of logical OR's, used to decide which global precomputations
##  to run before the main loop.
##
##  Fields of the resulting record:
##    any_S_hat_group, any_S_hat_aut,
##    any_lib_S_hat, any_lib_S_hat_ext,
##    any_build_A, any_build_N_A
##
#############################################################################
AggregatePlans := function(plans)
    local aggregate;;
    aggregate := function(field)
        return ForAny(plans, plan -> plan.(field) = true);;
    end;;
    return rec(
        any_S_hat_group   := aggregate("needs_S_hat_group"),
        any_S_hat_aut     := aggregate("needs_S_hat_aut"),
        any_lib_S_hat     := aggregate("needs_lib_S_hat"),
        any_lib_S_hat_ext := aggregate("needs_lib_S_hat_ext"),
        any_build_A       := aggregate("needs_to_build_A"),
        any_build_N_A     := aggregate("needs_to_build_N_A")
    );;
end;;

#############################################################################
##
##  ExecutePrime( plan_p, sporadic_data, resources, clifford_verification )
##
##  Runs the full classification for one prime: classifies
##  Irr_{p'}(S_hat), classifies Irr_{p'}(N_{S_hat}(P)), optionally
##  verifies via Clifford, and returns everything main.g (Printing.g)
##  needs to report.
##
##  resources is a record with the hoisted precomputations:
##    S_hat               - cover as a GAP group (or fail if not needed)
##    Aut_S_hat           - AutomorphismGroup(S_hat) (or fail)
##    Inn_S_hat           - Inn(S_hat) as a subgroup of Aut_S_hat (or fail)
##    Z_S_info            - rec(t_Z, fus_Z, n, generator) computed in S_hat
##    Z_S_hat             - Centre(S_hat) (or fail)
##    S_hat_lib_table     - CharacterTable(base_table_name) (or fail)
##    extension_lib_table - CharacterTable(extension_table_name) (or fail)
##    S_lib_fus           - GetFusionMap(S_hat_lib_table, extension_lib_table)
##                          (or fail)
##    t_Z_canonical       - CharacterTable("Cyclic", schur_multiplier)
##                          (or fail)
##    Z_S_lib_fus         - FusionMapToCentreFromTable(S_hat_lib_table, n)
##                          (or fail)
##
##  Returns rec:
##    plan, p,
##    class_S, class_N,                     # common-shape classification
##                                          # records
##    verification_S, verification_N,       # rec with .correct, or fail
##                                          # if not run
##    standard_info                         # rec with computed groups
##                                          # (Aut_p, P, N_S_hat_P, A,
##                                          #  N_A_P, outer_*) or fail
##
#############################################################################
ExecutePrime := function(plan_p, sporadic_data, resources,
        clifford_verification)
    local p, S_hat, P, P_A, outer_info, outer_gen, has_local_outer_action,
          N_S_hat_P, Z_N_info_standard, A, emb_S_hat, N_A_P,
          emb_N_S_hat_P, class_S, class_N, verify_S, verify_N,
          norm_info, N_lib_table, N_ext_lib_table, N_lib_fus, Z_N_info_lib,
          centre_N_robust, standard_info;;

    p := plan_p.p;;
    norm_info := plan_p.norm_info;;

    # ============================================================
    # Precomputations common to both classifications (standard path)
    # ============================================================
    P := fail;;
    outer_info := fail;;
    outer_gen := fail;;
    has_local_outer_action := false;;
    N_S_hat_P := fail;;
    Z_N_info_standard := fail;;
    A := fail;;
    emb_S_hat := fail;;
    N_A_P := fail;;
    emb_N_S_hat_P := fail;;
    # true unless the N library path falls back to the non-robust centre
    # fusion (no stored CTblLib fusion N -> S_hat); see CentreOfNFromLibrary.
    centre_N_robust := true;;

    if plan_p.needs_S_hat_group then
        S_hat := resources.S_hat;;
        ProgressMessage(
            Concatenation("Computing the p-Sylow for p = ", String(p), ".")
        );;
        P := SylowSubgroup(S_hat, p);;

        if sporadic_data.out_cardinal = 2 then
            outer_info := ComputeLocalOuterGen(
                S_hat, P, resources.Aut_S_hat, resources.Inn_S_hat
            );;
            outer_gen := outer_info.outer_gen;;
            has_local_outer_action := outer_gen <> fail;;
        else
            outer_info := rec(
                Aut_p := fail, Inn_p := fail,
                outer_order := 1, outer_gen := fail, outer_gen_order := 0
            );;
        fi;;

        # Normalizer of P in S_hat (needed for the standard N side and/or
        # for the construction of N_A).
        if plan_p.via_N = "standard" or plan_p.needs_to_build_N_A then
            ProgressMessage("Computing N_{S_hat}(P).");;
            N_S_hat_P := Normalizer(S_hat, P);;

            # Centre fusion towards N_{S_hat}(P) on the standard path
            # (reusing Z(S_hat), not Z(N), so that lambda is comparable, and
            # the shared canonical t_Z, invariant I5).
            Z_N_info_standard := FusionMapOfCentralSubgroupInGroup(
                resources.Z_S_hat, N_S_hat_P, resources.t_Z_canonical
            );;
        fi;;

        # Build A only if there is a local outer action; if outer_gen =
        # fail (out_cardinal=2 but the lift of Out vanishes on P), there
        # is nothing to verify and A is not needed.
        if (plan_p.needs_to_build_A or plan_p.needs_to_build_N_A)
           and has_local_outer_action then
            ProgressMessage("Building A = S_hat semidirect <outer_gen>.");;
            A := SemidirectProduct(Group(outer_gen), S_hat);;
            emb_S_hat := Embedding(A, 2);;

            if plan_p.needs_to_build_N_A then
                P_A := Image(emb_S_hat, P);;
                ProgressMessage("Computing N_A(P).");;
                N_A_P := Normalizer(A, P_A);;
                emb_N_S_hat_P := RestrictedMapping(emb_S_hat, N_S_hat_P);;
            fi;;
        fi;;
    fi;;

    # ============================================================
    # S_hat side
    # ============================================================
    if plan_p.via_S = "library" then
        ProgressMessage("Classifying Irr_{p'}(S_hat) via library tables.");;
        if sporadic_data.out_cardinal = 2 then
            class_S := ClassifyByTables(
                resources.S_hat_lib_table, resources.extension_lib_table,
                resources.S_lib_fus, p,
                resources.t_Z_canonical, resources.Z_S_lib_fus
            );;
        else
            class_S := ClassifyByTables(
                resources.S_hat_lib_table, fail, fail, p,
                resources.t_Z_canonical, resources.Z_S_lib_fus
            );;
        fi;;
    else
        ProgressMessage("Classifying Irr_{p'}(S_hat) via group construction.");;
        class_S := ClassifyByGroup(
            resources.S_hat, p, outer_gen,
            resources.Z_S_info.t_Z, resources.Z_S_info.fus_Z
        );;
    fi;;

    # Clifford verification on the S_hat side.
    verify_S := fail;;
    if plan_p.verify_S = "group" then
        if has_local_outer_action then
            ProgressMessage("Verifying Clifford on S_hat (standard path).");;
            verify_S := VerifyCliffordInGroup(
                resources.S_hat, A, emb_S_hat, class_S
            );;
        else
            # out_cardinal = 1 globally, or trivial outer action on P:
            # every p'-character is type 1 trivially, nothing to verify.
            verify_S := rec(
                correct := true,
                trivial := true,
                comment := "no local outer action: nothing to verify"
            );;
        fi;;
    elif plan_p.verify_S = "implicit" then
        verify_S := rec(
            correct := true,
            implicit := true,
            comment :=
              "classification derived from Clifford restriction in CTblLib"
        );;
    fi;;

    # ============================================================
    # N_{S_hat}(P) side
    # ============================================================
    if plan_p.via_N = "library" then
        ProgressMessage("Classifying Irr_{p'}(N_S_hat(P)) via library.");;
        N_lib_table := CharacterTable(norm_info.N_S_hat_name);;
        if sporadic_data.out_cardinal = 2 then
            N_ext_lib_table := CharacterTable(norm_info.N_A_name);;
            N_lib_fus := GetFusionMap(N_lib_table, N_ext_lib_table);;
            if N_lib_fus = fail then
                Error(
                    "No CTblLib fusion map from ",
                    norm_info.N_S_hat_name, " to ", norm_info.N_A_name,
                    "."
                );;
            fi;;
        else
            N_ext_lib_table := fail;;
            N_lib_fus := fail;;
        fi;;

        # Centre on N via library (preferably via fusion N -> S_hat).
        Z_N_info_lib := CentreOfNFromLibrary(
            N_lib_table, resources.S_hat_lib_table, resources.Z_S_lib_fus,
            sporadic_data.schur_multiplier, resources.t_Z_canonical
        );;
        centre_N_robust := Z_N_info_lib.robust;;

        class_N := ClassifyByTables(
            N_lib_table, N_ext_lib_table, N_lib_fus, p,
            Z_N_info_lib.t_Z, Z_N_info_lib.fus_Z
        );;
    else
        ProgressMessage("Classifying Irr_{p'}(N_S_hat(P)) via construction.");;
        class_N := ClassifyByGroup(
            N_S_hat_P, p, outer_gen,
            Z_N_info_standard.t_Z, Z_N_info_standard.fus_Z
        );;
    fi;;

    # Clifford verification on the N side.
    verify_N := fail;;
    if plan_p.verify_N = "group" then
        if has_local_outer_action then
            ProgressMessage("Verifying Clifford on N_S_hat(P).");;
            verify_N := VerifyCliffordInGroup(
                N_S_hat_P, N_A_P, emb_N_S_hat_P, class_N
            );;
        else
            verify_N := rec(
                correct := true,
                trivial := true,
                comment :=
                  "no local outer action: nothing to verify"
            );;
        fi;;
    elif plan_p.verify_N = "implicit" then
        verify_N := rec(
            correct := true,
            implicit := true,
            comment :=
              "classification derived from Clifford restriction in CTblLib"
        );;
    fi;;

    # ============================================================
    # Pack the standard-side info for the report.
    # ============================================================
    if plan_p.needs_S_hat_group then
        standard_info := rec(
            P := P,
            outer := outer_info,
            N_S_hat_P := N_S_hat_P,
            A := A,
            N_A_P := N_A_P
        );;
    else
        standard_info := fail;;
    fi;;

    return rec(
        plan := plan_p,
        p := p,
        class_S := class_S,
        class_N := class_N,
        verification_S := verify_S,
        verification_N := verify_N,
        centre_N_robust := centre_N_robust,
        standard_info := standard_info
    );;
end;;
