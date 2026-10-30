def shape: if startswith("byte-three-") then 0 elif startswith("word-three-") then 1 elif startswith("dword-two-") then 2 elif startswith("any-nine-") then 3 else -1 end;
def rule: if contains("-preserve-") then 0 elif contains("-writeasones-") then 1 else 2 end;
[.[0][] | select((.label|shape)>=0) | . as $case | range(0;3) as $source |
 {key:((((($case.label|split("-r")|last|tonumber)-1)*4+($case.label|shape))*3+($case.label|rule))*5+$source),
 bytes:($case.observations[$source*13+1:$source*13+13]|map(.value))}]
+ [.[1][] | select((.label|shape)>=0) | . as $case | range(0;2) as $source |
 {key:((((($case.label|split("-r")|last|tonumber)-1)*4+($case.label|shape))*3+($case.label|rule))*5+$source+3),
 bytes:($case.observations[$source*13+1:$source*13+13]|map(.value))}]
| sort_by(.key) | if length!=120 or ([.[].key]|unique|length)!=120 or any(.[];(.bytes|length)!=12 or any(.bytes[];. < 0 or . > 255)) then error("bad corpus") else . end
