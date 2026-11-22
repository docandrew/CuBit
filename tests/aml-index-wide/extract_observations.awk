/^Evaluating \\/ {
 if (waiting) {bad=1; exit 1}
 name=substr($2,2); waiting=1; next
}
/^  \[Integer\] = [0-9A-F]+$/ {
 if (!waiting || length($NF)!=16) {bad=1; exit 1}
 print name "\tInteger\t" $NF; waiting=0; count++; next
}
/\[Buffer\] Length 0A =/ {
 if (!waiting) {bad=1; exit 1}
 start=0; for(i=1;i<=NF;i++)if($i=="0000:")start=i+1
 if(!start || start+9>NF) {bad=1;exit 1}
 bytes=""; for(i=start;i<start+10;i++) {
  if(length($i)!=2 || $i!~/^[0-9A-F]+$/) {bad=1;exit 1}
  bytes=bytes (i==start?"":" ") $i
 }
 print name "\tBuffer\t" bytes; waiting=0; count++; next
}
END {if(bad || waiting || count!=22)exit 1}
