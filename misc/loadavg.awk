#!/usr/bin/awk -f

OFS = " → " {
	print $1, $2, $3
}
