#!/usr/bin/awk -f

BEGIN {
   i=0
   FS=","
   OFS=","
}
{
   $1=i++
   print $0
}
