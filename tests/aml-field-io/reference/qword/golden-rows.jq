def shape: if startswith("aligned-") then 0 elif startswith("shifted-") then 1 else 2 end;
def rule: if contains("-Preserve-") then 0 elif contains("-WriteAsOnes-") then 1 else 2 end;
[.[]|. as $case| (if .label|endswith("-a") then range(1;3) else range(0;3) end) as $block |
 ($case.label|shape) as $shape | ($case.label|rule) as $rule |
 (if $case.label|contains("-r1-") then 0 else 1 end) as $revision |
 (if $case.label|endswith("-a") then $block-1 else $block+2 end) as $source |
 {key:((($revision*3+$shape)*3+$rule)*5+$source),
 bytes:($case.bytes[($block*16):($block*16+16)]|map(.value))}]
|sort_by(.key)|if length==90 and ([.[].key]|unique|length)==90 and all(.[];(.bytes|length)==16 and all(.bytes[];.>=0 and .<=255)) then . else error("oracle cardinality") end
