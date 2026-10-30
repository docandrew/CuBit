function hex(s, n,i,p){n=0;for(i=1;i<=length(s);i++){p=index("0123456789ABCDEF",substr(s,i,1));if(!p){bad=1;exit 1}n=n*16+p-1}return n}
/^Evaluating \\/ {if(waiting){bad=1;exit 1}name=substr($2,2);waiting=1;size=0;bytes=0;next}
/^  \[Integer\] = / {if(!waiting||length($NF)!=16){bad=1;exit 1}print name " Integer " $NF;waiting=0;count++;next}
/\[Buffer\] Length/ {if(!waiting || !((name=="R512"&&$3=="200")||(name=="R4K0"&&$3=="1000"))){bad=1;exit 1}size=hex($3);next}
/^[ ]+[0-9A-F]+: / {
 if(!waiting||!size){bad=1;exit 1}
 offset=$1;sub(/:$/,"",offset);if(hex(offset)!=bytes){bad=1;exit 1}
 for(i=2;i<=17;i++){if($i!="EF"){bad=1;exit 1}bytes++}
 if(bytes==size){print name " Buffer " size " EF";waiting=0;count++}else if(bytes>size){bad=1;exit 1}
 next
}
/^Evaluation of \\R16K failed with status AE_BUFFER_OVERFLOW$/ {if(!waiting||name!="R16K"){bad=1;exit 1}print name " Error AE_BUFFER_OVERFLOW";waiting=0;count++;next}
END{if(bad||waiting||count!=5)exit 1}
