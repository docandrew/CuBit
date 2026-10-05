"""Import the existing Penny 24px export into Desktop's straight-alpha atlas.

Run in Nix under coordination/build.lock after assets/penny/render.py when
updating the artwork. Existing Bluecurve icons and their ordinals are preserved.
"""
from pathlib import Path
import re
from PIL import Image
root=Path(__file__).resolve().parents[1]
path=root/'userspace/services/desktop/desktop_icons.ads';text=path.read_text()
pixels=Image.open(root/'assets/penny/penny-24.png').convert('RGBA')
assert pixels.size==(24,24)
words=[f'16#{a:02X}{r:02X}{g:02X}{b:02X}#' for r,g,b,a in pixels.getdata()]
entry='      Penny => (\n'+',\n'.join('         '+', '.join(words[i:i+8]) for i in range(0,len(words),8))+'\n      )'
if '      Penny => (' in text:
 text,count=re.subn(r'      Penny => \(.*?\n      \)',lambda _:entry,text,flags=re.S);assert count==1
else:
 assert text.count('      Power\n   );')==1
 text=text.replace('      Power\n   );','      Power,\n      Penny\n   );')
 ending='      )\n   );\nend Desktop_Icons;'
 assert text.count(ending)==1
 text=text.replace(ending,'      ),\n'+entry+'\n   );\nend Desktop_Icons;')
 note='--  Penny: assets/penny/penny-24.png via tools/update_desktop_penny_icon.py\n'
 text=text.replace('with Interfaces;\n',note+'with Interfaces;\n',1)
path.write_text(text)
print('Updated Penny icon; existing atlas entries preserved')
