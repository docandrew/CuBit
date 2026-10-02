with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Materialize;
with Intel_GPU_PPGTT_Scratch;
procedure Scratch_Update_Tests is
   package V is new Intel_GPU_VM_Image (4);
   package S renames Intel_GPU_PPGTT_Scratch;
   type Page is array (Table_Index) of Unsigned_64;
   type Storage is array (Natural range 0 .. 13) of Page;
   RAM : Storage with Alignment => 4096, Volatile;
   Calls, Fail_Owner, Flushes, Fail_Flush : Natural := 0;
   Updating : Boolean := False;
   function Ready return Boolean is
   begin Calls := Calls + 1; return Calls /= Fail_Owner; end Ready;
   function Flush (CPU : Unsigned_64) return Boolean is
   begin
      Flushes := Flushes + 1;
      if Updating then
         -- Candidate pages followed by stable root; never scratch pages.
         pragma Assert (CPU = Unsigned_64 (To_Integer
           (RAM ((if Flushes <= 4 then 13 - Flushes else 1))'Address)));
      end if;
      return Flushes /= Fail_Flush;
   end Flush;
   package M is new Intel_GPU_VM_Materialize (V, Ready, Flush);
   Source, Candidate : V.Image;
   Old_Backing, New_Backing : M.Mappings;
   Scratch : M.Scratch_Mappings;
   Descriptor : constant S.Backing_Pages := [16#5000#,16#6000#,16#7000#,16#8000#];
   OK : Boolean;
   Total : Natural;
   procedure Run (Expected : Boolean; Corrupt : Natural := 0) is
      Initial, Update_State : M.State;
      Root : Unsigned_64;
      Saved : Storage;
   begin
      RAM := [others => [others => 16#CAFE#]];
      Updating := False; Calls := 0; Flushes := 0;
      -- Fault controls apply to update only.
      declare F : constant Natural := Fail_Owner; G : constant Natural := Fail_Flush; begin
         Fail_Owner := 0; Fail_Flush := 0;
         M.Prepare (Initial, Source, Old_Backing, Root, OK, Scratch);
         pragma Assert (OK);
         Fail_Owner := F; Fail_Flush := G;
      end;
      RAM (5) := [others => 16#BADC0DE#]; -- Simulated GPU stores to private scratch.
      if Corrupt /= 0 then RAM (Corrupt) (1) := 0; end if;
      Saved := RAM;
      Updating := True; Calls := 0; Flushes := 0;
      M.Publish_Update (Update_State, Source, Candidate, New_Backing,
                        Old_Backing (1), OK, Scratch);
      pragma Assert (OK = Expected);
      Total := Calls;
      for P in 5 .. 8 loop pragma Assert (RAM (P) = Saved (P)); end loop;
      pragma Assert (RAM (0) = Saved (0) and RAM (13) = Saved (13));
      if Expected then
         pragma Assert (Flushes = 5);
         for I in Table_Index loop
            pragma Assert (RAM (1) (I) = V.Entry_Value (Candidate, 1, I));
         end loop;
      elsif Corrupt /= 0 then
         pragma Assert (Flushes = 0 and RAM (1) = Saved (1));
      end if;
      M.Publish_Update (Update_State, Source, Candidate, New_Backing,
                        Old_Backing (1), OK, Scratch);
      pragma Assert (not OK and Calls = Total);
   end Run;
begin
   for P in V.Page_Number loop
      Old_Backing (P) := (Unsigned_64 (To_Integer (RAM (P)'Address)), Unsigned_64 (P)*4096);
      New_Backing (P) := (Unsigned_64 (To_Integer (RAM (P+8)'Address)), Unsigned_64 (P+8)*4096);
   end loop;
   for L in S.Level loop
      Scratch (L) := (Unsigned_64 (To_Integer (RAM (L+5)'Address)), Descriptor (L));
   end loop;
   V.Initialize (Source, [4096,8192,12288,16384], OK, Descriptor); pragma Assert (OK);
   V.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
   V.Seal (Source, OK); pragma Assert (OK);
   V.Prepare_Update (Candidate, Source, [16#9000#,16#A000#,16#B000#,16#C000#], OK);
   pragma Assert (OK);
   V.Unmap_Pages (Candidate, 4096, [1 => 16#100000#], OK); pragma Assert (OK);
   V.Seal_Update (Candidate, OK); pragma Assert (OK);
   Run (True);
   declare Bound : constant Natural := Total; begin
      for F in 1 .. Bound loop Fail_Owner := F; Run (False); end loop;
   end;
   Fail_Owner := 0;
   for F in 1 .. 5 loop Fail_Flush := F; Run (False); end loop;
   Fail_Flush := 0;
   for P in 6 .. 8 loop Run (False, P); end loop;
   Run (True);
   Ada.Text_IO.Put_Line ("Scratch update PASS: retained data/tables, root switch, corrupt tables, owner/flush failures, no retry");
end Scratch_Update_Tests;
