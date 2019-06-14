#!/bin/bash
grep "%include;" $(find hd/etc -name '*txt') | sed -e "s/ //g" | sed -e "s/\([^:]*\):\([^:]*\)/\2 <- \1/" | sort
