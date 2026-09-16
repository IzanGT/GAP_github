#############################################################################
##
##  main.g  -  Entry point
##
##  Verification of the refined McKay conjecture for finite simple groups
##  (sporadic and non-sporadic). For each prime in the input list, classifies
##  Irr_{p'}(S_hat) and Irr_{p'}(N_{S_hat}(P)) into types 1/2 under the 
##  outer-automorphism action, optionally verifies via Clifford theory and 
##  constructs an explicit equivariant bijection.
##
##  Usage:
##    gap> LoadPackage("CTblLib");
##    gap> Read("main.g");
##    gap> ShowNonSporadicSupportList();
##    gap> Classify("A5", [2,3,5], true, true, false);
##
##  Public API:
##
##    1. ShowSporadicSupportList()
##       - Prints a detailed table of the 26 sporadic groups supported by the
##         algorithm, including their Universal Cover, simple name, Schur
##         multiplier, outer automorphism order (|Out|), and whether they
##         have the Clifford shortcut available in CTblLib.
##
##    2. ShowNonSporadicSupportList()
##       - Prints an identical table but for the supported non-sporadic
##         groups (like Alternating and Lie type groups), sorted by order.
##         This is useful to know exactly what 'string' name you must pass
##         to the Classify function.
##
##    3. Classify( name_S, prime_list,
##                 clifford_verification, show_progress, show_tables )
##       - Main execution function.
##       - `name_S` must be a string exactly matching a group in one of the
##         two support lists (e.g., "M11", "A5").
##       - The function automatically routes to the Sporadic or Non-Sporadic
##         database, retrieves the mathematical metadata, and executes the
##         classification transparently.
##
##  Shortcut policy (summary):
##    - For each prime and each side (S_hat, N_{S_hat}(P)) the program
##      decides INDEPENDENTLY whether to classify via the library
##      (CTblLib) or by explicitly computing groups.
##    - Justification (appendix of proyecto2.tex, Final Theorem): the
##      Clifford restriction via tables works whenever CTblLib provides
##      an index-2 extension realising the non-trivial outer class.
##    - The expensive precomputations (AtlasGroup, EpimorphismSchurCover, 
##      AutomorphismGroup, Centre, fusion maps to the centre, etc.)
##      are done ONCE per call and shared across all primes.
##
#############################################################################

# Load CTblLib upfront: if it is missing, IsBound(LibInfoCharacterTable)
# would be false and every prime would silently fall to the standard
# path.  Loading it here avoids that pitfall.
LoadPackage("CTblLib");;

Read("funciones/Utilities.g");;
Read("funciones/SporadicData.g");;
Read("funciones/NonSporadicData.g");;
Read("funciones/Centre.g");;
Read("funciones/TableClassifier.g");;
Read("funciones/GroupClassifier.g");;
Read("funciones/Classification.g");;
Read("funciones/Bijection.g");;
Read("funciones/Printing.g");;

#############################################################################
##
##  PrecomputeResources( sporadic_data, aggregated, show_tables )
##
##  Given the aggregated need-flags (what some prime requires), computes
##  once all the shared resources: groups, AutomorphismGroup, Inn(S_hat),
##  Centre, fusion to the centre, S_hat and S_hat.2 library tables,
##  fusion S_hat -> S_hat.2 and the canonical cyclic table of Z.
##
##  Returns a record with the names described in Classification.g (see
##  the `resources` argument of ExecutePrime).
##
#############################################################################
PrecomputeResources := function(sporadic_data, aggregated, show_tables, predefined_S_hat)
    local n_centre, S_hat, Aut_S_hat, Inn_S_hat, Z_S_hat, Z_S_info,
          S_hat_lib_table, extension_lib_table, S_lib_fus,
          t_Z_canonical, Z_S_lib_fus, G_simple;;

    n_centre := sporadic_data.schur_multiplier;;

    S_hat := fail;;
    Aut_S_hat := fail;;
    Inn_S_hat := fail;;
    Z_S_hat := fail;;
    Z_S_info := fail;;
    S_hat_lib_table := fail;;
    extension_lib_table := fail;;
    S_lib_fus := fail;;
    t_Z_canonical := fail;;
    Z_S_lib_fus := fail;;

    # The canonical centre table is built ONCE and shared by EVERY
    # classification (both the S_hat and N sides, library and standard
    # paths) so that the lambda indices are directly comparable
    # (invariant I5).  It is cheap (a cyclic table of order m = |Z(S_hat)|).
    t_Z_canonical := CharacterTable("Cyclic", n_centre);;

    # ---- Group resources (standard path) ----
    if aggregated.any_S_hat_group then
        if predefined_S_hat <> fail then
            ProgressMessage("Using provided S_hat (Universal Cover).");;
            S_hat := predefined_S_hat;;
        else
            ProgressMessage(
                Concatenation(
                    "Attempting to build ", sporadic_data.cover_name,
                    " from AtlasGroup..."
                )
            );;
            S_hat := AtlasGroup(sporadic_data.cover_name);;
            if S_hat = fail then
                ProgressMessage(
                    Concatenation(
                        "AtlasGroup representation not found. Falling back to SimpleGroup(\"", 
                        sporadic_data.simple_name, "\")."
                    )
                );;
                G_simple := SimpleGroup(sporadic_data.simple_name);;
                if G_simple <> fail then
                    if sporadic_data.cover_name = sporadic_data.simple_name then
                        S_hat := G_simple;;
                    else
                        ProgressMessage(
                            Concatenation(
                                "Computing EpimorphismSchurCover to build the universal cover ",
                                sporadic_data.cover_name, "."
                            )
                        );;
                        S_hat := Source(EpimorphismSchurCover(G_simple));;
                    fi;;
                fi;;
                
                if S_hat = fail then
                    Error(
                        "AtlasGroup and SimpleGroup could not build ",
                        sporadic_data.cover_name, "."
                    );;
                fi;;
            else
                ProgressMessage("Successfully built from AtlasGroup.");;
            fi;;
        fi;;

        if aggregated.any_S_hat_aut then
            ProgressMessage("Computing AutomorphismGroup(S_hat).");;
            Aut_S_hat := AutomorphismGroup(S_hat);;
            ProgressMessage("Building Inn(S_hat) as a subgroup of Aut.");;
            Inn_S_hat := InnerAutomorphismsSubgroup(S_hat);;
        fi;;

        ProgressMessage("Computing Z(S_hat) and the fusion to the centre.");;
        Z_S_hat := Centre(S_hat);;
        if Size(Z_S_hat) <> n_centre then
            Error(
                "Inconsistency: |Z(S_hat)| = ", Size(Z_S_hat),
                " <> schur_multiplier = ", n_centre, "."
            );;
        fi;;
        Z_S_info := FusionMapOfCentralSubgroupInGroup(
            Z_S_hat, S_hat, t_Z_canonical
        );;
    fi;;

    # ---- Library resources ----
    if aggregated.any_lib_S_hat then
        ProgressMessage(
            Concatenation("Loading library table ",
                          sporadic_data.base_table_name, ".")
        );;
        S_hat_lib_table := CharacterTable(sporadic_data.base_table_name);;

        Z_S_lib_fus := FusionMapToCentreFromTable(
            S_hat_lib_table, n_centre
        );;

        if aggregated.any_lib_S_hat_ext then
            ProgressMessage(
                Concatenation("Loading extension table ",
                              sporadic_data.extension_table_name, ".")
            );;
            extension_lib_table := CharacterTable(
                sporadic_data.extension_table_name
            );;
            S_lib_fus := GetFusionMap(
                S_hat_lib_table, extension_lib_table
            );;
            if S_lib_fus = fail then
                Error(
                    "No CTblLib fusion map ",
                    sporadic_data.base_table_name, " -> ",
                    sporadic_data.extension_table_name, "."
                );;
            fi;;
        fi;;
    fi;;

    # ---- Display preloaded tables if requested ----
    if show_tables then
        if S_hat_lib_table <> fail then
            Print("\n--- CHARACTER TABLE OF S_hat = ",
                  sporadic_data.cover_name, " (library) ---\n");;
            Display(S_hat_lib_table);;
        elif S_hat <> fail then
            Print("\n--- CHARACTER TABLE OF S_hat = ",
                  sporadic_data.cover_name, " (computed) ---\n");;
            Display(CharacterTable(S_hat));;
        fi;;

        if t_Z_canonical <> fail then
            Print("\n--- CHARACTER TABLE OF Z(S_hat) ",
                  "(cyclic of order ", n_centre, ") ---\n");;
            Display(t_Z_canonical);;
        elif Z_S_info <> fail then
            Print("\n--- CHARACTER TABLE OF Z(S_hat) ",
                  "(cyclic of order ", n_centre, ") ---\n");;
            Display(Z_S_info.t_Z);;
        fi;;
    fi;;

    return rec(
        S_hat := S_hat,
        Aut_S_hat := Aut_S_hat,
        Inn_S_hat := Inn_S_hat,
        Z_S_hat := Z_S_hat,
        Z_S_info := Z_S_info,
        S_hat_lib_table := S_hat_lib_table,
        extension_lib_table := extension_lib_table,
        S_lib_fus := S_lib_fus,
        t_Z_canonical := t_Z_canonical,
        Z_S_lib_fus := Z_S_lib_fus
    );;
end;;

ExecuteClassification := function(data, prime_list,
        clifford_verification, show_progress, show_tables, predefined_S_hat)
    local plans, aggregated, resources, p, plan_p, prime_result,
          bijection, conditions;;

    SHOW_PROGRESS := show_progress;;

    plans := List(
        prime_list,
        p -> ComputePlanForPrime(data, p, clifford_verification)
    );;
    aggregated := AggregatePlans(plans);;

    resources := PrecomputeResources(data, aggregated, show_tables, predefined_S_hat);;

    for plan_p in plans do
        p := plan_p.p;;

        prime_result := ExecutePrime(
            plan_p, data, resources, clifford_verification
        );;

        ProgressMessage("Building the McKay bijection.");;
        bijection := BuildBijection(
            prime_result.class_S, prime_result.class_N, p
        );;

        ProgressMessage("Verifying the four McKay conditions.");;
        conditions := VerifyMcKayConditions(
            bijection, prime_result.class_S, prime_result.class_N
        );;

        PrintPrimeResults(
            data, prime_result, bijection, conditions, show_tables
        );;
    od;;
end;;

Classify := function(name_S, prime_list,
        clifford_verification, show_progress, show_tables)
    local data, entry;;
    
    if not IsString(name_S) then
        Error("Classify: first argument must be a string (the name of the group).");;
    fi;;

    data := First(SPORADIC_DATA_CACHE, entry -> entry.simple_name = name_S);;
    
    if data <> fail then
        data := ShallowCopy(data);;
    else
        data := First(NON_SPORADIC_DATA_CACHE, entry -> entry.simple_name = name_S);;
        if data <> fail then
            data := ShallowCopy(data);;
        else
            Error("Unknown simple group name: ", name_S);;
        fi;;
    fi;;
    
    ExecuteClassification(data, prime_list, clifford_verification, show_progress, show_tables, fail);;
end;;

