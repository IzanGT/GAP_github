#############################################################################
##
##  SporadicData.g
##
##  Database of the 26 sporadic finite simple groups.  Each record holds
##  the invariants needed for the classification of irreducible characters
##  of p'-degree and for the Clifford-theoretic verification.
##
#############################################################################

#############################################################################
##
##  FixedSporadicData()
##
##  Returns a list with one record per sporadic simple group.  The fields
##  of each record are:
##
##    simple_name                Atlas name of the simple group S
##    cover_name                 Atlas name of the universal cover S_hat
##    schur_multiplier           |M(S)|, order of the Schur multiplier
##    out_cardinal               |Out(S)| = |Aut(S)/Inn(S)|
##    order                      |S|, written as a product of prime powers
##    clifford_shortcut_available true if GAP's CTblLib provides both the
##                               table of S_hat and the table of S_hat.2
##                               for the Clifford table-shortcut
##    base_table_name            CTblLib name of CharacterTable(S_hat)
##    extension_table_name       CTblLib name of CharacterTable(S_hat.2)
##                               (fail if |Out(S)| = 1 or absent)
##    normalizer_tables          List of rec(prime, N_S_hat_name, N_A_name)
##                               with the CTblLib names of the tables of
##                               N_{S_hat}(P) and N_A(P) when available.
##                               Only primes for which at least one of the
##                               two tables is in CTblLib are listed; the
##                               list is empty if no normalizer table is
##                               available for that group.
##
#############################################################################
FixedSporadicData := function()
    return [
        rec(
            simple_name := "M11",
            cover_name := "M11",
            schur_multiplier := 1,
            out_cardinal := 1,
            order := 2^4 * 3^2 * 5 * 11,
            clifford_shortcut_available := false,
            base_table_name := "M11",
            extension_table_name := fail,
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "M11N2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "3^2:Q8.2", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "5:4", N_A_name := fail),
                rec(prime := 11, N_S_hat_name := "11:5", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "M12",
            cover_name := "2.M12",
            schur_multiplier := 2,
            out_cardinal := 2,
            order := 2^7 * 3^3 * 5 * 11,
            clifford_shortcut_available := true,
            base_table_name := "2.M12",
            extension_table_name := "2.M12.2",
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "2.M12N2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "2xM12N3", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "2.M12N5", N_A_name := fail),
                rec(prime := 11, N_S_hat_name := "2x11:5", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "M22", # Out of RAM
            cover_name := "12.M22",
            schur_multiplier := 12,
            out_cardinal := 2,
            order := 2^9 * 3^3 * 5 * 7 * 11,
            clifford_shortcut_available := true,
            base_table_name := "12.M22",
            extension_table_name := "12.M22.2",
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "3x4.M22N2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "12.M22N3", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "3xIsoclinic(2x5:8)", N_A_name := fail),
                rec(prime := 7, N_S_hat_name := "12x7:3", N_A_name := fail),
                rec(prime := 11, N_S_hat_name := "12x11:5", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "M23",
            cover_name := "M23",
            schur_multiplier := 1,
            out_cardinal := 1,
            order := 2^7 * 3^2 * 5 * 7 * 11 * 23,
            clifford_shortcut_available := false,
            base_table_name := "M23",
            extension_table_name := fail,
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "2^4:D8", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "3^2:Q8.2", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "(3xD10).2", N_A_name := fail),
                rec(prime := 7, N_S_hat_name := "2x7:3", N_A_name := fail),
                rec(prime := 11, N_S_hat_name := "11:5", N_A_name := fail),
                rec(prime := 23, N_S_hat_name := "23:11", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "M24",
            cover_name := "M24",
            schur_multiplier := 1,
            out_cardinal := 1,
            order := 2^10 * 3^3 * 5 * 7 * 11 * 23,
            clifford_shortcut_available := false,
            base_table_name := "M24",
            extension_table_name := fail,
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "M24N2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "3^(1+2):D8", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "(A4xD10).2", N_A_name := fail),
                rec(prime := 7, N_S_hat_name := "7:3xS3", N_A_name := fail),
                rec(prime := 11, N_S_hat_name := "11:10", N_A_name := fail),
                rec(prime := 23, N_S_hat_name := "23:11", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "J2",
            cover_name := "2.J2",
            schur_multiplier := 2,
            out_cardinal := 2,
            order := 2^8 * 3^3 * 5^2 * 7,
            clifford_shortcut_available := true,
            base_table_name := "2.J2",
            extension_table_name := "2.J2.2",
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "2.J2N2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "2.J2N3", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := fail, N_A_name := "2.J2.2N5")
            ]
        ),
        rec(
            simple_name := "Suz", # Not viable
            cover_name := "6.Suz",
            schur_multiplier := 6,
            out_cardinal := 2,
            order := 2^14 * 3^8 * 5^2 * 7 * 11 * 13,
            clifford_shortcut_available := true,
            base_table_name := "6.Suz",
            extension_table_name := "6.Suz.2",
            normalizer_tables := []
        ),
        rec(
            simple_name := "HS",  # Out of RAM
            cover_name := "2.HS",
            schur_multiplier := 2,
            out_cardinal := 2,
            order := 2^10 * 3^2 * 5^3 * 7 * 11,
            clifford_shortcut_available := true,
            base_table_name := "2.HS",
            extension_table_name := "2.HS.2",
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "2.HSN2", N_A_name := "2.HS.2N2"),
                rec(prime := 3, N_S_hat_name := "2.HSN3", N_A_name := "2.HS.2N3"),
                rec(prime := 5, N_S_hat_name := fail,     N_A_name := "2.HS.2N5")
            ]
        ),
        rec(
            simple_name := "McL",
            cover_name := "3.McL", # Out of RAM
            schur_multiplier := 3,
            out_cardinal := 2,
            order := 2^7 * 3^7 * 5^3 * 7 * 11,
            clifford_shortcut_available := true,
            base_table_name := "3.McL",
            extension_table_name := "3.McL.2",
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "3xMcLN2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "3.3^4.3^2.Q8", N_A_name := "3.McL.2N3"),
                rec(prime := 5, N_S_hat_name := "3x5^(1+2):3:8", N_A_name := "3.McL.2N5"),
                rec(prime := 7, N_S_hat_name := "6x7:3", N_A_name := fail),
                rec(prime := 11, N_S_hat_name := "3x11:5", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "Co3",
            cover_name := "Co3",
            schur_multiplier := 1,
            out_cardinal := 1,
            order := 2^10 * 3^7 * 5^3 * 7 * 11 * 23,
            clifford_shortcut_available := false,
            base_table_name := "Co3",
            extension_table_name := fail,
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "Co3N2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "Co3N3", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "5^(1+2):(24:2)", N_A_name := fail),
                rec(prime := 7, N_S_hat_name := "S3x7:6", N_A_name := fail),
                rec(prime := 11, N_S_hat_name := "2x11:5", N_A_name := fail),
                rec(prime := 23, N_S_hat_name := "23:11", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "Co2",
            cover_name := "Co2",
            schur_multiplier := 1,
            out_cardinal := 1,
            order := 2^18 * 3^6 * 5^3 * 7 * 11 * 23,
            clifford_shortcut_available := false,
            base_table_name := "Co2",
            extension_table_name := fail,
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "Co2N2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "3^(1+4)_+:(S3xQD16)", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "5^(1+2):4S4", N_A_name := fail),
                rec(prime := 7, N_S_hat_name := "Co2N7", N_A_name := fail),
                rec(prime := 11, N_S_hat_name := "11:10", N_A_name := fail),
                rec(prime := 23, N_S_hat_name := "23:11", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "Co1",
            cover_name := "2.Co1",
            schur_multiplier := 2,
            out_cardinal := 1,
            order := 2^22 * 3^9 * 5^4 * 7^2 * 11 * 13 * 23,
            clifford_shortcut_available := false,
            base_table_name := "2.Co1",
            extension_table_name := fail,
            normalizer_tables := [
                rec(prime := 5, N_S_hat_name := "2xCo1N5", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "He",
            cover_name := "He",
            schur_multiplier := 1,
            out_cardinal := 2,
            order := 2^10 * 3^3 * 5^2 * 7^3 * 17,
            clifford_shortcut_available := true,
            base_table_name := "He",
            extension_table_name := "He.2",
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "M24N2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "3^(1+2):D8", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "5^2:4A4", N_A_name := fail),
                rec(prime := 7, N_S_hat_name := "7^(1+2):(S3x3)", N_A_name := fail),
                rec(prime := 17, N_S_hat_name := "17:8", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "Fi22",
            cover_name := "Fi22",
            schur_multiplier := 1,
            out_cardinal := 2,
            order := 2^17 * 3^9 * 5^2 * 7 * 11 * 13,
            clifford_shortcut_available := true,
            base_table_name := "Fi22",
            extension_table_name := "Fi22.2",
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "f22s2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "Fi22N3", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "Fi22N5", N_A_name := fail),
                rec(prime := 7, N_S_hat_name := "S3x7:6", N_A_name := fail),
                rec(prime := 11, N_S_hat_name := "2x11:5", N_A_name := fail),
                rec(prime := 13, N_S_hat_name := "13:6", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "Fi23",
            cover_name := "Fi23",
            schur_multiplier := 1,
            out_cardinal := 1,
            order := 2^18 * 3^13 * 5^2 * 7 * 11 * 13 * 17 * 23,
            clifford_shortcut_available := false,
            base_table_name := "Fi23",
            extension_table_name := fail,
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "2.Fi22N2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "Fi23N3", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "2xFi22N5", N_A_name := fail),
                rec(prime := 7, N_S_hat_name := "7:6xA5.2", N_A_name := fail),
                rec(prime := 11, N_S_hat_name := "(2^2x11:5).2", N_A_name := fail),
                rec(prime := 13, N_S_hat_name := "S3x13:6", N_A_name := fail),
                rec(prime := 17, N_S_hat_name := "17:16", N_A_name := fail),
                rec(prime := 23, N_S_hat_name := "23:11", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "Fi24'",
            cover_name := "3.Fi24'",
            schur_multiplier := 3,
            out_cardinal := 2,
            order := 2^21 * 3^17 * 5^2 * 7^3 * 11 * 13 * 17 * 23 * 29,
            clifford_shortcut_available := true,
            base_table_name := "3.F3+",
            extension_table_name := "3.F3+.2",
            normalizer_tables := [
                rec(prime := 5, N_S_hat_name := "3.F3+N5", N_A_name := "3.F3+.2N5"),
                rec(prime := 7, N_S_hat_name := "3xF3+N7",  N_A_name := "3.F3+.2N7"),
                rec(prime := 11, N_S_hat_name := "3x(A4x11:5).2", N_A_name := fail),
                rec(prime := 17, N_S_hat_name := "3x17:16", N_A_name := fail),
                rec(prime := 23, N_S_hat_name := "3x23:11", N_A_name := fail),
                rec(prime := 29, N_S_hat_name := "3x29:14", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "HN",
            cover_name := "HN",
            schur_multiplier := 1,
            out_cardinal := 2,
            order := 2^14 * 3^6 * 5^6 * 7 * 11 * 19,
            clifford_shortcut_available := true,
            base_table_name := "HN",
            extension_table_name := "HN.2",
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "HNN2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "HNN3", N_A_name := "HN.2N3"),
                rec(prime := 5, N_S_hat_name := "HNN5", N_A_name := "LyN5")
            ]
        ),
        rec(
            simple_name := "Th",
            cover_name := "Th",
            schur_multiplier := 1,
            out_cardinal := 1,
            order := 2^15 * 3^10 * 5^3 * 7^2 * 13 * 19 * 31,
            clifford_shortcut_available := false,
            base_table_name := "Th",
            extension_table_name := fail,
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "ThN2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "ThN3", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "5^(1+2):4S4", N_A_name := fail),
                rec(prime := 7, N_S_hat_name := "7^2:(3x2S4)", N_A_name := fail),
                rec(prime := 13, N_S_hat_name := "(3x13:6).2", N_A_name := fail),
                rec(prime := 19, N_S_hat_name := "19:18", N_A_name := fail),
                rec(prime := 31, N_S_hat_name := "31:15", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "B",
            cover_name := "2.B",
            schur_multiplier := 2,
            out_cardinal := 1,
            order := 2^42 * 3^13 * 5^6 * 7^2 * 11 * 13 * 17 * 19 * 23 * 31 * 47,
            clifford_shortcut_available := false,
            base_table_name := "2.B",
            extension_table_name := fail,
            normalizer_tables := [
                rec(prime := 7, N_S_hat_name := "2.BN7", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "M",
            cover_name := "M",
            schur_multiplier := 1,
            out_cardinal := 1,
            order := 2^46 * 3^20 * 5^9 * 7^6 * 11^2 * 13^3 * 17 * 19 * 23 * 29 * 31 * 41 * 47 * 59 * 71,
            clifford_shortcut_available := false,
            base_table_name := "M",
            extension_table_name := fail,
            normalizer_tables := []
        ),
        rec(
            simple_name := "J1",
            cover_name := "J1",
            schur_multiplier := 1,
            out_cardinal := 1,
            order := 2^3 * 3 * 5 * 7 * 11 * 19,
            clifford_shortcut_available := false,
            base_table_name := "J1",
            extension_table_name := fail,
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "2^3.7.3", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "D6xD10", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "D6xD10", N_A_name := fail),
                rec(prime := 7, N_S_hat_name := "7:6", N_A_name := fail),
                rec(prime := 11, N_S_hat_name := "11:10", N_A_name := fail),
                rec(prime := 19, N_S_hat_name := "19:6", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "O'N",
            cover_name := "3.ON",
            schur_multiplier := 3,
            out_cardinal := 2,
            order := 2^9 * 3^5 * 5 * 7^3 * 11 * 19 * 31,
            clifford_shortcut_available := true,
            base_table_name := "3.ON",
            extension_table_name := "3.ON.2",
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "3xONN2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "3.ONM6", N_A_name := "3.ON.2N3"),
                rec(prime := 5, N_S_hat_name := "(3^(1+2):4xD10).2", N_A_name := fail),
                rec(prime := 7, N_S_hat_name := "3x7^(1+2):(D8x3)", N_A_name := "3.ON.2N7"),
                rec(prime := 11, N_S_hat_name := "3x11:10", N_A_name := fail),
                rec(prime := 19, N_S_hat_name := "3x19:6", N_A_name := fail),
                rec(prime := 31, N_S_hat_name := "3x31:15", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "J3",
            cover_name := "3.J3",
            schur_multiplier := 3,
            out_cardinal := 2,
            order := 2^7 * 3^6 * 5 * 17 * 19,
            clifford_shortcut_available := true,
            base_table_name := "3.J3",
            extension_table_name := "3.J3.2",
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "3xJ3N2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "3^3.3^(1+2):8", N_A_name := "3^3.3^(1+2):8.2"),
                rec(prime := 5, N_S_hat_name := "3xD6xD10", N_A_name := fail),
                rec(prime := 17, N_S_hat_name := "3x17:8", N_A_name := fail),
                rec(prime := 19, N_S_hat_name := "3x19:9", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "Ly",
            cover_name := "Ly",
            schur_multiplier := 1,
            out_cardinal := 1,
            order := 2^8 * 3^7 * 5^6 * 7 * 11 * 31 * 37 * 67,
            clifford_shortcut_available := false,
            base_table_name := "Ly",
            extension_table_name := fail,
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "LyN2", N_A_name := fail),
                rec(prime := 3, N_S_hat_name := "LyN3", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "LyN5", N_A_name := fail),
                rec(prime := 7, N_S_hat_name := "(SL2(3)x7:3).2", N_A_name := fail),
                rec(prime := 11, N_S_hat_name := "11:5xS3", N_A_name := fail),
                rec(prime := 31, N_S_hat_name := "31:6", N_A_name := fail),
                rec(prime := 37, N_S_hat_name := "37:18", N_A_name := fail),
                rec(prime := 67, N_S_hat_name := "67:22", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "Ru",
            cover_name := "2.Ru",
            schur_multiplier := 2,
            out_cardinal := 2,
            order := 2^15 * 3^3 * 5^3 * 7 * 13 * 29,
            clifford_shortcut_available := false,
            base_table_name := "2.Ru",
            extension_table_name := fail,
            normalizer_tables := [
                rec(prime := 2, N_S_hat_name := "2.RuN2", N_A_name := fail)
            ]
        ),
        rec(
            simple_name := "J4",
            cover_name := "J4",
            schur_multiplier := 1,
            out_cardinal := 1,
            order := 2^21 * 3^3 * 5 * 7 * 11^3 * 23 * 29 * 31 * 37 * 43,
            clifford_shortcut_available := false,
            base_table_name := "J4",
            extension_table_name := fail,
            normalizer_tables := [
                rec(prime := 3, N_S_hat_name := "(2x3^(1+2)_+:8):2", N_A_name := fail),
                rec(prime := 5, N_S_hat_name := "(D10x2^3.L3(2)).2", N_A_name := fail),
                rec(prime := 7, N_S_hat_name := "7:3xS5", N_A_name := fail)
            ]
        )
    ];;
end;;

# Global cache built once when this file is loaded.  Every lookup uses
# this cache instead of rebuilding the 26-record list on each call.
SPORADIC_DATA_CACHE := FixedSporadicData();;

#############################################################################
##
##  SporadicCoverNames()
##
##  Returns the list of ATLAS names of the universal covers of the 26
##  sporadic groups (e.g. ["M11", "2.M12", ...]).
##
#############################################################################
SporadicCoverNames := function()
    return List(SPORADIC_DATA_CACHE, data -> data.cover_name);;
end;;

#############################################################################
##
##  SporadicDataByCover( cover_name )
##
##  Looks up the database record for the universal cover whose ATLAS name
##  is <cover_name> (e.g. "2.J2").  Returns a shallow copy.  Raises an
##  error if the name is not in the database.
##
#############################################################################
SporadicDataByCover := function(cover_name)
    local data;;

    data := First(
        SPORADIC_DATA_CACHE,
        entry -> entry.cover_name = cover_name
    );;

    if data = fail then
        Error(
            "Unknown universal cover name: ",
            cover_name,
            ". Valid names: ",
            SporadicCoverNames(),
            "."
        );;
    fi;;

    return ShallowCopy(data);;
end;;

#############################################################################
##
##  SporadicDataBySimple( simple_name )
##
##  Looks up the database record for the simple group whose ATLAS name is
##  <simple_name> (e.g. "J2", "HS", "Fi24'").  Returns a shallow copy.
##  Raises an error if the name is not in the database.
##
#############################################################################
SporadicDataBySimple := function(simple_name)
    local data;;

    data := First(
        SPORADIC_DATA_CACHE,
        entry -> entry.simple_name = simple_name
    );;

    if data = fail then
        Error(
            "Unknown simple group name: ",
            simple_name,
            ". Valid names: ",
            List(SPORADIC_DATA_CACHE, e -> e.simple_name),
            "."
        );;
    fi;;

    return ShallowCopy(data);;
end;;

#############################################################################
##
##  SporadicDataByOrder( group_order )
##
##  Looks up the database record whose 'order' field equals <group_order>.
##  Returns a shallow copy.  Raises an error if no record matches.
##
#############################################################################
SporadicDataByOrder := function(group_order)
    local data;;

    data := First(
        SPORADIC_DATA_CACHE,
        entry -> entry.order = group_order
    );;

    if data = fail then
        Error(
            "No sporadic descriptor found for order ",
            group_order,
            "."
        );;
    fi;;

    return ShallowCopy(data);;
end;;

#############################################################################
##
##  NormalizerTablesByPrime( sporadic_data, p )
##
##  Looks up sporadic_data.normalizer_tables for the entry corresponding
##  to the prime <p>.  Returns the record
##      rec( prime, N_S_hat_name, N_A_name )
##  with the CTblLib names of the tables of N_{S_hat}(P) and N_A(P) (fail
##  if not in the library).  If no entry exists for <p>, returns the
##  record with both names set to fail.
##
#############################################################################
NormalizerTablesByPrime := function(sporadic_data, p)
    local entry;;

    entry := First(
        sporadic_data.normalizer_tables,
        e -> e.prime = p
    );;

    if entry = fail then
        return rec(prime := p, N_S_hat_name := fail, N_A_name := fail);;
    fi;;

    return ShallowCopy(entry);;
end;;

#############################################################################
##
##  CliffordSupportText( available )
##
##  Converts a boolean to the human-readable string "yes" or "no" used in
##  the Clifford-support summary listing.
##
#############################################################################
CliffordSupportText := function(available)
    if available then
        return "yes";;
    fi;;

    return "no";;
end;;

#############################################################################
##
##  PrintSporadicCliffordSupportList()
##
##  Prints a summary table of the 26 sporadics indicating, for each one,
##  whether the Clifford table-shortcut is available in CTblLib (i.e. both
##  the table of S_hat and the table of S_hat.2).
##
#############################################################################
PrintSporadicCliffordSupportList := function()
    local data;;

    Print("\n--- CLIFFORD-SHORTCUT SUPPORT FOR THE 26 SPORADICS ---\n");;
    for data in SPORADIC_DATA_CACHE do
        Print(
            data.cover_name,
            " | simple ", data.simple_name,
            " | M = ", data.schur_multiplier,
            " | Out = ", data.out_cardinal,
            " | Clifford shortcut = ",
            CliffordSupportText(data.clifford_shortcut_available),
            "\n"
        );;
    od;;
end;;
