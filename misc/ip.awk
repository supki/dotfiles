#!/usr/bin/awk -f

BEGIN {
	ipshow = "ip addr show " ARGV[1]
	res    = 1

	while ((ipshow | getline line) > 0) {
		if (line ~ /inet /) {
			print(substr(line, 10, match(line, "/") - 10))
			res = 0
		}
	}

	exit(res)
}
