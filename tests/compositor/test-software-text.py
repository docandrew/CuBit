from pathlib import Path
import tempfile,subprocess,os,json,hashlib
assert os.environ.get('IN_NIX_SHELL')
r=Path(__file__).resolve().parents[2];w=Path(tempfile.mkdtemp(prefix='software-text-',dir=r/'tests/compositor/build'));print(w,flush=True)
names=['compositor_software_text','compositor_glyph_cache','compositor_glyph_storage','compositor_glyph_software','compositor_glyph_placement','compositor_glyph_layout','compositor_glyph_memory','compositor_glyph_arena','compositor_glyph_ffi','compositor_identity','compositor_affine','compositor_transform','compositor_formats','heap_extents','cubit-display_geometry','cubit']
inputs={}
for name in names:
 for ext in ['ads','adb']:
  hits=[p for folder in ['userspace/lib/compositor','userspace/lib/display','userspace/runtime/gnat','userspace/allocator/src'] if (p:=r/folder/(name+'.'+ext)).exists()]
  if not hits:continue
  p=hits[0];data=p.read_bytes();(w/p.name).write_bytes(data);inputs[str(p.relative_to(r))]=hashlib.sha256(data).hexdigest()
(w/'raster.c').write_text('''#include <stdint.h>
#include <string.h>
#include <assert.h>
static unsigned calls, fail;
void set_fault(unsigned f){fail=f;}
unsigned raster_calls(void){return calls;}
struct request {uint32_t font,code,n,d,w,h,pitch,capacity;};
struct metrics {uint32_t advance,height;};
uint32_t cubit_font_raster_mask(const struct request *r,void *pixels,struct metrics *m){
 calls++; if(fail)return 1; assert(r->capacity>=r->pitch*r->h);
 for(unsigned y=0;y<r->h;y++)memset((uint8_t*)pixels+y*r->pitch,127,r->w);
 *m=(struct metrics){r->w,r->h}; return 0;}
''')
(w/'software_text_tests.adb').write_text('''with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Software_Text;
procedure Software_Text_Tests is
 package R renames Compositor_Software_Text;
 package G renames R.G;
 use type R.Software.Word;
 use type G.Logical_Coordinate, R.Software.Pixels;
 procedure Fault (V : Unsigned_32) with Import, Convention => C, External_Name => "set_fault";
 function Calls return Unsigned_32 with Import, Convention => C, External_Name => "raster_calls";
 S : R.State;
 Screen : G.Output := (80, 72, G.Unrotated, (5, 4), -7, -9);
 Pixels : R.Software.Pixels (0 .. 80 * 72 - 1) := (others => 16#FF123456#);
 Before : R.Software.Pixels (Pixels'Range);
 OK : Boolean;
 N : Unsigned_32;
 procedure Draw (Code : Natural; Scale : G.UI_Scale) is
 begin
 Screen.Scale := Scale;
 R.Paint (S, (0, Code, Scale), Screen, (-2, -3), (3, 4, 45, 50), Pixels, 80, 16#FFFFFFFF#, OK);
 end Draw;
begin
 Draw (65, (5, 4)); pragma Assert (OK and Calls = 1);
 N := Calls;
 for I in 1 .. 1000 loop Draw (65, (10, 8)); pragma Assert (OK and Calls = N); end loop;
 for Rotation in G.Orientation loop
 Screen.Rotation := Rotation;
 Before := Pixels; Draw (66, (3, 2)); pragma Assert (OK);
 for I in Pixels'Range loop
 if I mod 80 < 3 or I mod 80 >= 45 or I / 80 < 4 or I / 80 >= 50 then
 pragma Assert (Pixels (I) = Before (I)); end if;
 end loop;
 end loop;
 Screen.Rotation := G.Unrotated;
 for I in 0 .. 299 loop
 Draw (32 + I mod 95, (G.Scale_Component (1 + I / 95), 1));
 pragma Assert (OK and R.Valid (S) and R.Charged (S) <= 524_288);
 end loop;
 Before := Pixels; Fault (1); Draw (90, (7, 4));
 pragma Assert (not OK and Pixels = Before and R.Valid (S));
 Fault (0); Draw (90, (7, 4)); pragma Assert (OK);
 R.Shutdown (S, OK); pragma Assert (OK and R.Charged (S) = 0);
 Ada.Text_IO.Put_Line ("PASS software-only glyph reuse, eviction, rotation clipping, raster fault and retirement");
end Software_Text_Tests;
''')
(w/'test.gpr').write_text('''project Test is
for Source_Dirs use ("."); for Languages use ("Ada", "C");
for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("software_text_tests.adb");
package Compiler is for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2"); end Compiler;
end Test;''')
(w/'inputs.json').write_text(json.dumps(inputs,indent=2))
for cmd in [['gprbuild','-q','-p','-P',str(w/'test.gpr')], ['gnatprove','-P',str(w/'test.gpr'),'-u','compositor_software_text.adb','--level=2','--timeout=30','-j2']]:
 subprocess.run(['alr','exec','--',*cmd],cwd=r/'kernel',check=True)
subprocess.run([str(w/'software_text_tests')],check=True)

report=(w/'obj/gnatprove/gnatprove.out').read_text()
total=next(line for line in report.splitlines() if line.startswith('Total '))
assert total.split()[-2:]==['.','.'],total
for path,digest in inputs.items():
 assert hashlib.sha256((r/path).read_bytes()).hexdigest()==digest,path
(w/'result.json').write_text(json.dumps({'status':'PASS','proof':total,'inputs':inputs},indent=2))
print(total,flush=True)
