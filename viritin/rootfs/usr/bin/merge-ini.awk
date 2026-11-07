# Merges an ini fragment into a generated ini.
#
#   awk -f merge-ini.awk fragment generated
#
# An entry in the fragment replaces the same key in the same section; sections
# and keys the generated file does not have are appended. Names are matched
# without regard to case, which is how the ini reader treats them -- appending
# the fragment instead would leave a second [Configuration] that the reader
# never looks at, so the setting would be silently ignored.

function trim(s) { sub(/^[ \t]+/, "", s); sub(/[ \t]+$/, "", s); return s }

# First file: collect the fragment.
NR == FNR {
    t = trim($0)
    if (t ~ /^\[.*\]$/) {
        sec = tolower(substr(t, 2, length(t) - 2))
        if (!(sec in known)) { known[sec] = 1; order[++nsec] = sec; label[sec] = t }
        next
    }
    if (t == "" || t ~ /^[;#]/ || index(t, "=") == 0) next
    if (sec == "") next                       # a key before any section header
    k = tolower(trim(substr(t, 1, index(t, "=") - 1)))
    if (!((sec SUBSEP k) in value)) { keys[sec] = keys[sec] k "\n" }
    value[sec SUBSEP k] = t
    next
}

# Second file: emit it, substituting and then appending.
{
    t = trim($0)
    if (t ~ /^\[.*\]$/) {
        flush(cur)
        cur = tolower(substr(t, 2, length(t) - 2))
        done[cur] = 1
        print
        next
    }
    if (t != "" && t !~ /^[;#]/ && index(t, "=") > 0 && cur != "") {
        k = tolower(trim(substr(t, 1, index(t, "=") - 1)))
        if ((cur SUBSEP k) in value) {
            print value[cur SUBSEP k]
            delete value[cur SUBSEP k]
            next
        }
    }
    print
}

END {
    flush(cur)
    for (i = 1; i <= nsec; i++) {
        s = order[i]
        if (s in done) continue
        print ""
        print label[s]
        flush(s)
    }
}

# Prints whatever the fragment still has for a section that the generated file
# did not already carry.
function flush(s,   n, j, parts) {
    if (s == "" || !(s in keys)) return
    n = split(keys[s], parts, "\n")
    for (j = 1; j <= n; j++) {
        if (parts[j] == "") continue
        if ((s SUBSEP parts[j]) in value) {
            print value[s SUBSEP parts[j]]
            delete value[s SUBSEP parts[j]]
        }
    }
}
