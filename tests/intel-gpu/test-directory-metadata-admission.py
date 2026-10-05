from pathlib import Path
import subprocess
import tempfile
source = Path(__file__).resolve().parents[2] / 'userspace/services/intel-gpu/main.adb'
original = source.read_bytes()
text = original.decode()
start = text.index('      Reply_Saved : Boolean := Saved_Reply;')
helpers = text[start:text.index('\n   begin', start)]
start = text.index('                           if Needed.Additional_Tables > Directory_Link_Capacity then')
body = text[start:text.index('                           Update_Table_Pages := Needed.Additional_Tables;', start)]
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
procedure Test is
   Fault, Saves, Direct_Replies, Cap_Replies, Requests : Natural := 0;
   Cap : Boolean := False;
   type Tag is record Label, Length, Flags, Reserved : Natural; end record;
   type Words is array (0 .. 3) of Unsigned_64;
   type Message is record Tag : Test.Tag; Words : Test.Words; end record;
   Msg, Update_Request : Message := ((1,4,0,0), [1,0,0,8192]);
   From : constant Unsigned_64 := 42;
   Application_Reply_Slot : constant := 62;
   function saveReplyCap (Slot : Unsigned_64) return Unsigned_64 is
   begin
      pragma Assert (Slot = 62 and not Cap); Saves := Saves + 1;
      Cap := Fault /= 2; return (if Cap then 1 else 0);
   end;
   function replyCap (Slot : Natural; Value : Message) return Unsigned_64 is
   begin pragma Assert (Cap and Slot = 62); Cap := False;
      Cap_Replies := Cap_Replies + 1; return 1; end;
   function reply (Sender : Unsigned_64; Value : Message) return Unsigned_64 is
   begin pragma Assert (not Cap and Sender = 42);
      Direct_Replies := Direct_Replies + 1; return 1; end;
   In_Place_Inserting, In_Place_Active, Directory_Metadata_Pending, Update_Held : Boolean;
   Directory_Metadata_Epoch : Unsigned_64;
   Update_Index : Natural := 1;
   Stored : constant := 1;
   type Item is record Source : Unsigned_64; end record;
   Private_Contexts : array (1 .. 1) of Item := [(Source => 7)];
   package Application_VM is function Revision (S : Unsigned_64) return Unsigned_64 is (S); end;
   package Application_State is Table_Pages : constant := 64; end;
   package Application_Binding is Update_Label : constant := 2; end;
   package Application_Buffers is Unavailable : constant := 3; end;
   function Directory_Link_Capacity return Positive is (1);
   type Requirements is record Additional_Tables : Positive; end record;
   Needed : constant Requirements := (Additional_Tables => 2);
   Directory_Metadata_Target : Positive := 1;
   package Directory_Metadata_Growth is
      type Phase is (Empty, Idle, Opening);
      type View is record State : Phase; end record;
      function Snapshot (Object : Phase) return View is ((State => Object));
      procedure Configure (Object : in out Phase; Bytes : Unsigned_64; Quota : Positive; OK : out Boolean);
      procedure Request (Object : in out Phase; Count : Positive; OK : out Boolean);
   end;
   use type Directory_Metadata_Growth.Phase;
   package body Directory_Metadata_Growth is
      procedure Configure (Object : in out Phase; Bytes : Unsigned_64; Quota : Positive; OK : out Boolean) is
      begin OK := Fault /= 1; if OK then Object := Idle; end if; end;
      procedure Request (Object : in out Phase; Count : Positive; OK : out Boolean) is
      begin pragma Assert (Cap and Object = Idle and Count = 2);
         Requests := Requests + 1; OK := Fault /= 3;
         if OK then Object := Opening; end if;
      end;
   end;
   Directory_Metadata : array (1 .. 1) of Directory_Metadata_Growth.Phase;
   procedure Admit (Saved_Reply : Boolean := False) is
      Started : Boolean;
      Response : Message;
      Delivered : Unsigned_64;
'''
suffix = '''
   end Admit;
begin
   for F in 0 .. 4 loop
      Fault := F; Saves := 0; Direct_Replies := 0; Cap_Replies := 0; Requests := 0;
      Cap := F = 4; Directory_Metadata (1) := Directory_Metadata_Growth.Empty;
      In_Place_Inserting := True; In_Place_Active := False; Directory_Metadata_Pending := False;
      Update_Held := False; Update_Index := 1;
      Admit (Saved_Reply => F = 4);
      case F is
         when 0 => pragma Assert (Requests = 1 and Saves = 1 and Cap and
           Directory_Metadata_Pending and In_Place_Active and not Update_Held and Directory_Metadata_Epoch = 7);
         when 1 => pragma Assert (Requests = 0 and Saves = 0 and Direct_Replies = 1);
         when 2 =>
            pragma Assert (Requests = 0 and Saves = 1 and Direct_Replies = 1 and
              Directory_Metadata (1) = Directory_Metadata_Growth.Idle);
            -- Next caller can use the same controller after failed capability save.
            Fault := 0; Admit;
            pragma Assert (Requests = 1 and Saves = 2 and Directory_Metadata_Pending);
         when 3 => pragma Assert (Requests = 1 and Saves = 1 and Cap_Replies = 1 and not Cap);
         when 4 => pragma Assert (Requests = 1 and Saves = 0 and Cap_Replies = 0 and Cap and Directory_Metadata_Pending);
         when others => null;
      end case;
   end loop;
   Ada.Text_IO.Put_Line ("Metadata admission PASS5 plus recovery: save before request, saved rejection, no second save");
end Test;
'''
out = Path(tempfile.mkdtemp(prefix='cubit-directory-admission.'))
(out/'test.gpr').write_text('''project P is
for Source_Dirs use (".");
for Object_Dir use "obj";
for Main use ("test.adb");
package Compiler is
for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
end Compiler;
end P;''')
variants = {'native':helpers, 'wrong-reply':helpers.replace('if Reply_Saved then replyCap', 'if Saved_Reply then replyCap')}
for name, helper in variants.items():
    (out/'test.adb').write_text(prefix+helper+'\n   begin\n'+body+suffix)
    build=subprocess.run(['gprbuild','-f','-p','-P',str(out/'test.gpr')],capture_output=True,text=True)
    assert build.returncode == 0, build.stderr
    run=subprocess.run([str(out/'obj/test')],capture_output=True,text=True)
    (out/(name+'.log')).write_text(run.stdout+run.stderr)
    assert (run.returncode == 0) == (name == 'native'), (name,run.stdout,run.stderr)
    print(name,run.returncode,run.stdout.strip())
assert source.read_bytes() == original

print('Evidence:', out)
