# Preserve query order and distinguish returned Integers from expected errors.
/^Evaluating \\/ {
 if (waiting) {bad=1; exit 1}
 name=$0; sub(/^Evaluating \\/,"",name); waiting=1; next
}
/^  \[Integer\] = [0-9A-F]+$/ {
 if (!waiting || length($NF)!=16) {bad=1; exit 1}
 print name "\tInteger\t" $NF; waiting=0; count++; next
}
/^Evaluation of \\.* failed with status / {
 if (!waiting) {bad=1; exit 1}
 actual=$3; sub(/^\\/,"",actual)
 if(actual!=name) {bad=1; exit 1}
 print name "\tError\t" $NF; waiting=0; count++; next
}
 /^  \[Buffer\] Length 08 =/ {
 if (!waiting) {bad=1; exit 1}
 found=0; bytes=""
 for(i=1;i<=NF;i++)if($i=="0000:"){
  if(i+8>NF){bad=1;exit 1}
  for(j=i+1;j<=i+8;j++){if($j !~ /^[0-9A-F][0-9A-F]$/){bad=1;exit 1};bytes=bytes (j==i+1?"":" ") $j}
  found=1;break
 }
 if(!found){bad=1;exit 1}
 print name "\tBuffer\t" bytes;waiting=0;count++;next
 }
END {if(bad || waiting || !count)exit 1}
