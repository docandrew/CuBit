/^Evaluating / { active=($2~/PIRQ[.]_PRT$/); if(active){tables++;root=0;row=-1;field=0} }
active && /\[Package\] Contains/ {
 if(!root){if($3!=4)bad=1;root=1}
 else {if(row>=0 && field!=4)bad=1;row++;field=0}
 next
}
active && /\[Integer\]/ {
 if(field==0)a[row]=$NF;
 else if(field==1)p[row]=$NF;
 else if(field==2){if($NF!="0000000000000000")bad=1;k[row]="GLOBAL";s[row]="-"}
 else if(field==3)x[row]=$NF;else bad=1;
 field++;next
}
active && /\[Object Reference\]/ {
 if(field!=2)bad=1;
 for(i=1;i<=NF;i++)if($i=="Name"){s[row]=$(i+1);if($(i+2)!="Device")bad=1}
 k[row]="DEVICE";field++;next
}
END {
 if(bad || tables!=1 || row!=3 || field!=4)exit 1;
 for(r=0;r<4;r++){if(length(a[r])!=16 || length(p[r])!=16 || length(x[r])!=16 || length(s[r])==0)exit 1; print r "\t" a[r] "\t" p[r] "\t" k[r] "\t" s[r] "\t" x[r]}
}
