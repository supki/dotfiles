#!/usr/bin/awk -f

NR == 1 {
	total = $2
}
NR > 1 && NR < 5 {
	rest += $2
}

END {
	printf "%2.0f%%", 100 * (total - rest) / total
}
