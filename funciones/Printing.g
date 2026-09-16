#############################################################################
##
##  Printing.g
##
##  Presentation functions: print classifications, bijections, per-prime
##  results and the global support listing.
##
##  All input/output lives here; the rest of the code is pure.
##
#############################################################################

#############################################################################
##
##  TableIdentifier( table )
##
##  Returns the Identifier of a table when it has one (e.g. CTblLib
##  tables).  For tables computed from a group, returns "computed".
##
#############################################################################
TableIdentifier := function(table)
    if HasIdentifier(table) then
        return Identifier(table);;
    fi;;
    return "computed";;
end;;

#############################################################################
##
##  PrintClassification( name, class )
##
##  Lists the p'-characters with their degree, conductor, case and (when
##  available) partner and central lambda.
##
#############################################################################
PrintClassification := function(name, class)
    local i, lam_suffix, conductor;;

    Print("\n--- CLASSIFICATION OF Irr_{p'}(", name, ") ---\n");;

    for i in class.pprime_indices do
        if IsBound(class.central_lambda) and class.central_lambda[i] <> fail then
            lam_suffix := Concatenation(
                ", lambda=", String(class.central_lambda[i])
            );;
        else
            lam_suffix := "";;
        fi;;

        conductor := Conductor(ValuesOfClassFunction(class.irr[i]));;

        if class.case[i] = 1 then
            Print(
                "chi[", i, "] degree ", class.degrees[i],
                ", conductor ", conductor,
                ": case 1", lam_suffix, "\n"
            );;
        else
            Print(
                "chi[", i, "] degree ", class.degrees[i],
                ", conductor ", conductor,
                ": case 2, partner chi[", class.partner[i], "]",
                lam_suffix, "\n"
            );;
        fi;;
    od;;
end;;

#############################################################################
##
##  PrintBijection( bijection, conditions )
##
##  Prints the orbit-by-orbit pairing followed by the verdict on the four
##  conditions.
##
#############################################################################
PrintBijection := function(bijection, conditions)
    local e, lam_suffix, cond_suffix, msg;;

    Print("\n--- BIJECTION Irr_{p'}(S_hat) <-> Irr_{p'}(N_S_hat(P)) ---\n");;
    Print("Case-1 orbits: ", bijection.num_case1,
          " | Case-2 orbits: ", bijection.num_case2, "\n\n");;

    for e in bijection.pairing do
        cond_suffix := Concatenation(
            ", C_p(S)=", String(e.c_S_p),
            ", C_p(N)=", String(e.c_N_p)
        );;
        if e.case = 1 then
            lam_suffix := Concatenation(", lambda=", String(e.lambda));;
            Print(
                "  chi_S[", e.orbit_S[1], "] (degree ", e.degree_S,
                ") --> chi_N[", e.orbit_N[1], "] (degree ", e.degree_N, ")",
                "  [case 1", lam_suffix, cond_suffix, "]\n"
            );;
        else
            if e.lam_pair[1] <> e.lam_pair[2] then
                # alpha swaps lambda: show explicit orientation.
                if e.lambdas_N[1] = e.lambdas_S[1] then
                    Print(
                        "  {chi_S[", e.orbit_S[1], "]@lam=", e.lambdas_S[1],
                        ", chi_S[", e.orbit_S[2], "]@lam=", e.lambdas_S[2],
                        "} (degree ", e.degree_S,
                        ") --> {chi_N[", e.orbit_N[1], "]@lam=", e.lambdas_N[1],
                        ", chi_N[", e.orbit_N[2], "]@lam=", e.lambdas_N[2],
                        "} (degree ", e.degree_N, ")",
                        "  [case 2, lambdas={",
                        e.lam_pair[1], ",", e.lam_pair[2], "}",
                        cond_suffix, "]\n"
                    );;
                else
                    Print(
                        "  {chi_S[", e.orbit_S[1], "]@lam=", e.lambdas_S[1],
                        ", chi_S[", e.orbit_S[2], "]@lam=", e.lambdas_S[2],
                        "} (degree ", e.degree_S,
                        ") --> {chi_N[", e.orbit_N[2], "]@lam=", e.lambdas_N[2],
                        ", chi_N[", e.orbit_N[1], "]@lam=", e.lambdas_N[1],
                        "} (degree ", e.degree_N, ")",
                        "  [case 2, lambdas={",
                        e.lam_pair[1], ",", e.lam_pair[2], "}",
                        cond_suffix, "]\n"
                    );;
                fi;;
            else
                Print(
                    "  {chi_S[", e.orbit_S[1], "], chi_S[", e.orbit_S[2],
                    "]} (degree ", e.degree_S,
                    ") --> {chi_N[", e.orbit_N[1], "], chi_N[", e.orbit_N[2],
                    "]} (degree ", e.degree_N, ")",
                    "  [case 2, lambda=", e.lam_pair[1],
                    cond_suffix, "]\n"
                );;
            fi;;
        fi;;
    od;;

    Print("\n");;
    if conditions.cond_1_equivariance then
        Print("Condition (1) equivariance (case preserved): verified.\n");;
    else
        Print("WARNING: Condition (1) equivariance does NOT hold.\n");;
    fi;;
    if conditions.cond_2_degree then
        Print("Condition (2) non-increasing degree: verified.\n");;
    else
        Print("WARNING: Condition (2) non-increasing degree does NOT hold.\n");;
    fi;;
    if conditions.cond_3_central then
        Print("Condition (3) Schur over Z(S_hat): verified.\n");;
    else
        Print("WARNING: Condition (3) Schur does NOT hold.\n");;
    fi;;
    if conditions.cond_4_conductor then
        Print("Condition (4) p-part of the conductor: verified.\n");;
    else
        Print("WARNING: Condition (4) p-part of the conductor does NOT hold.\n");;
    fi;;
    if not conditions.bijective then
        Print("WARNING: the pairing is NOT bijective.\n");;
    fi;;
    if Length(conditions.warnings) > 0 then
        Print("\nDetailed warnings:\n");;
        for msg in conditions.warnings do
            Print("  - ", msg, "\n");;
        od;;
    fi;;
end;;

#############################################################################
##
##  PrintPrimeHeader( data, plan, standard_info )
##
##  Header with group data, prime, chosen routes and (when applicable)
##  the orders of the computed groups.
##
#############################################################################
PrintPrimeHeader := function(data, plan, standard_info)
    Print("\n========================================================\n");;
    Print("=== PRIME p = ", plan.p, "\n");;
    Print("========================================================\n");;
    Print("Simple group   = ", data.simple_name, "\n");;
    Print("Cover S_hat    = ", data.cover_name, "\n");;
    Print("Schur multiplier = ", data.schur_multiplier, "\n");;
    Print("|Out(S)| = ", data.out_cardinal, "\n");;
    Print("Route, S_hat side       : ", plan.via_S, "\n");;
    Print("Route, N_{S_hat}(P) side: ", plan.via_N, "\n");;
    Print("Verification, S_hat     : ", plan.verify_S, "\n");;
    Print("Verification, N         : ", plan.verify_N, "\n");;

    if standard_info <> fail then
        if standard_info.outer.outer_order = 1 then
            Print("Local outer order on P: 1 (no outer action)\n");;
        else
            Print("Local outer order on P: ",
                  standard_info.outer.outer_order,
                  " | Order(outer_gen) = ",
                  standard_info.outer.outer_gen_order, "\n");;
        fi;;
        if standard_info.N_S_hat_P <> fail then
            Print("|N_{S_hat}(P)| = ", Size(standard_info.N_S_hat_P), "\n");;
        fi;;
        if standard_info.A <> fail then
            Print("|A|          = ", Size(standard_info.A), "\n");;
        fi;;
        if standard_info.N_A_P <> fail then
            Print("|N_A(P)|     = ", Size(standard_info.N_A_P), "\n");;
        fi;;
    fi;;
end;;

#############################################################################
##
##  PrintSideVerification( label, verify )
##
##  Reports the Clifford verification result on one side (S or N).
##  verify may be fail (no verification) or a record with .correct and
##  diagnostic fields.
##
#############################################################################
PrintSideVerification := function(label, verify)
    if verify = fail then
        return;;
    fi;;
    if IsBound(verify.implicit) and verify.implicit = true then
        Print("Clifford verification ", label,
              ": implicit (classification via library tables).\n");;
    elif IsBound(verify.trivial) and verify.trivial = true then
        Print("Clifford verification ", label,
              ": trivial (no outer action).\n");;
    elif verify.correct = true then
        Print("Clifford verification ", label,
              ": correct (witnesses found in the group).\n");;
    else
        Print("WARNING: Clifford verification ", label, " incorrect.\n");;
    fi;;
end;;

#############################################################################
##
##  PrintPrimeResults( data, prime_result, bijection, conditions,
##      show_tables )
##
##  Prints the full block for one prime: header, classifications,
##  verifications, bijection and verdict.  If show_tables = true also
##  displays the character table of N_{S_hat}(P).
##
#############################################################################
PrintPrimeResults := function(data, prime_result, bijection, conditions,
        show_tables)
    local plan, norm_info;;

    plan := prime_result.plan;;
    norm_info := plan.norm_info;;

    PrintPrimeHeader(data, plan, prime_result.standard_info);;

    Print("Table used for S_hat    : ",
          TableIdentifier(prime_result.class_S.table), "\n");;
    if plan.via_S = "library" and data.out_cardinal = 2 then
        Print("Table for the S_hat extension : ",
              data.extension_table_name, "\n");;
    fi;;
    if plan.via_N = "library" then
        Print("Table for N_{S_hat}(P)  : ", norm_info.N_S_hat_name,
              " (library)\n");;
    else
        Print("Table for N_{S_hat}(P)  : computed\n");;
    fi;;
    if plan.via_N = "library" and data.out_cardinal = 2 then
        Print("Table for N_A(P)        : ", norm_info.N_A_name, "\n");;
    fi;;

    PrintClassification("S_hat", prime_result.class_S);;
    PrintSideVerification("S_hat", prime_result.verification_S);;

    if show_tables then
        Print("\n--- CHARACTER TABLE OF N_S_hat(P) (p = ",
              plan.p, ") ---\n");;
        Display(prime_result.class_N.table);;
    fi;;

    PrintClassification("N_S_hat(P)", prime_result.class_N);;
    PrintSideVerification("N_S_hat(P)", prime_result.verification_N);;

    if IsBound(prime_result.centre_N_robust) and
       prime_result.centre_N_robust = false then
        Print("WARNING: centre fusion on the N side used the non-robust ",
              "fallback (no stored CTblLib fusion N -> S_hat); the lambda ",
              "indices of condition (3) may be off by an automorphism of ",
              "Z(S_hat).\n");;
    fi;;

    PrintBijection(bijection, conditions);;
end;;

#############################################################################
##
##  ShowSporadicSupportList()
##
##  Prints the summary table of the 26 sporadics: cover name, simple
##  name, Schur multiplier, |Out(S)| and the Clifford-shortcut
##  availability flag.
##
#############################################################################
ShowSporadicSupportList := function()
    local data;;

    Print("\n--- LIBRARY-SHORTCUT SUPPORT FOR THE 26 SPORADICS ---\n");;
    Print("cover_name | simple | M | Out | Clifford shortcut\n");;
    for data in SPORADIC_DATA_CACHE do
        Print(
            data.cover_name,
            " | ", data.simple_name,
            " | M=", data.schur_multiplier,
            " | Out=", data.out_cardinal,
            " | shortcut=", data.clifford_shortcut_available,
            "\n"
        );;
    od;;
end;;

#############################################################################
##
##  ShowNonSporadicSupportList()
##
##  Prints the summary table of the supported non-sporadic groups:
##  cover name, simple name, Schur multiplier, |Out(S)| and the
##  Clifford-shortcut availability flag.
##
#############################################################################
ShowNonSporadicSupportList := function()
    local groups, data;;
    
    # Copy the cache and sort it by group order
    groups := ShallowCopy(NON_SPORADIC_DATA_CACHE);;
    Sort(groups, function(a, b) return a.order < b.order;; end);;
    
    Print("\n--- LIBRARY-SHORTCUT SUPPORT FOR NON-SPORADICS ---\n");;
    Print("cover_name | simple | M | Out | Clifford shortcut\n");;
    for data in groups do
        Print(
            data.cover_name,
            " | ", data.simple_name,
            " | M=", data.schur_multiplier,
            " | Out=", data.out_cardinal,
            " | shortcut=", data.clifford_shortcut_available,
            "\n"
        );;
    od;;
end;;
