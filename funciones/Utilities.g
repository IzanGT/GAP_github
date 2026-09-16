#############################################################################
##
##  Utilities.g
##
##  Auxiliary helpers: console progress reporting and the p-part of the
##  conductor.
##
#############################################################################

# Default value: main.g overwrites this from the show_progress parameter.
if not IsBound(SHOW_PROGRESS) then
    SHOW_PROGRESS := false;;
fi;;

#############################################################################
##
##  ProgressMessage( message )
##
##  Print <message> prefixed by "[progress]" if SHOW_PROGRESS = true.
##
#############################################################################
ProgressMessage := function(message)
    if SHOW_PROGRESS = true then
        Print("[progress] ", message, "\n");;
    fi;;
end;;

#############################################################################
##
##  LoopProgress( label, current, total )
##
##  Print loop progress "label: current/total" at 10% milestones and on the
##  last iteration.  Requires SHOW_PROGRESS = true.
##
#############################################################################
LoopProgress := function(label, current, total)
    local step;;

    if SHOW_PROGRESS = true then
        step := Maximum(1, Int(total / 10));;
        if current mod step = 0 or current = total then
            Print("[progress] ", label, ": ", current, "/", total, "\n");;
        fi;;
    fi;;
end;;

#############################################################################
##
##  pPartOfConductor( chi, p )
##
##  Returns p^k where k = v_p(Conductor(chi)).  The conductor of chi is the
##  smallest n such that Q(chi) is contained in Q(zeta_n); its p-part is
##  the invariant that condition (4) of the McKay bijection must preserve.
##
#############################################################################
pPartOfConductor := function(chi, p)
    local n, k;;
    n := Conductor(ValuesOfClassFunction(chi));;
    k := 0;;
    while n mod p = 0 do
        n := n / p;;
        k := k + 1;;
    od;;
    return p ^ k;;
end;;
