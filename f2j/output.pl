#!/usr/local/bin/perl -n
next if /array/;
print if /\*\s+\S+\s+\(output\)/i;
print if /\*\s+\S+\s+\(input\/output\)/i;
print if /\*\s+\S+\s+\(output\/input\)/i;
