with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Materialize;
with Intel_GPU_DMA_Cache;
procedure VM_Materialize_Tests is
   -- The materializer must require mappings only for live tables, not for
   -- unallocated metadata capacity. Guard pages surround the four real pages.
   package VM is new Intel_GPU_VM_Image (8);
   type Page is array (Table_Index) of Unsigned_64;
   type Storage is array (Natural range 0 .. 5) of Page;
   RAM : Storage := [others => [others => 16#A5A5A5A5A5A5A5A5#]]
     with Alignment => 4096, Volatile;
   Owner_Calls, Flush_Calls, Fail_Owner, Fail_Flush : Natural := 0;
   Successful_Owner_Calls : Natural := 0;
   Corrupt, Real_Flush : Boolean := False;
   function Owner_Ready return Boolean is
   begin
      Owner_Calls := Owner_Calls + 1;
      return Owner_Calls /= Fail_Owner;
   end Owner_Ready;
   function Flush (CPU : Unsigned_64) return Boolean is
   begin
      Flush_Calls := Flush_Calls + 1;
      -- Reverse order: children before the root page.
      pragma Assert (CPU = Unsigned_64 (To_Integer (RAM (5 - Flush_Calls)'Address)));
      if Flush_Calls = Fail_Flush then return False; end if;
      if Corrupt and Flush_Calls = 4 then RAM (1) (0) := 16#BAD#; end if;
      return not Real_Flush or else Intel_GPU_DMA_Cache.Flush_Range (CPU, 4096);
   end Flush;
   package Writer is new Intel_GPU_VM_Materialize (VM, Owner_Ready, Flush);
   Source : VM.Image;
   Backing : Writer.Mappings := [others => (0, 0)];
   OK : Boolean;
   Root : Unsigned_64;
   Sentinel : constant Unsigned_64 := 16#A5A5A5A5A5A5A5A5#;
   procedure Guards is
   begin
      for I in Table_Index loop
         pragma Assert (RAM (0) (I) = Sentinel and RAM (5) (I) = Sentinel);
      end loop;
   end Guards;
   procedure Run (Expected : Boolean; First : Positive := 1; Last : Natural := 8) is
      State : Writer.State;
      Calls : Natural;
   begin
      RAM := [others => [others => Sentinel]];
      Owner_Calls := 0; Flush_Calls := 0;
      Writer.Prepare (State, Source, Backing (First .. Last), Root, OK);
      pragma Assert (OK = Expected);
      pragma Assert (Root = (if Expected then 4096 else 0));
      Guards;
      if Expected then
         pragma Assert (Flush_Calls = 4 and Owner_Calls >= 14);
         Successful_Owner_Calls := Owner_Calls;
         for P in 1 .. 4 loop
            for I in Table_Index loop
               pragma Assert (RAM (P) (I) = VM.Entry_Value (Source, P, I));
            end loop;
         end loop;
      end if;
      Calls := Owner_Calls;
      Writer.Prepare (State, Source, Backing (First .. Last), Root, OK);
      pragma Assert (not OK and Root = 0 and Owner_Calls = Calls);
   end Run;
begin
   for P in 1 .. 4 loop
      Backing (P) := (Unsigned_64 (To_Integer (RAM (P)'Address)), Unsigned_64 (P) * 4096);
   end loop;
   VM.Initialize (Source, [1 => 4096, 2 => 8192, 3 => 12288, 4 => 16384, others => 0],
                  OK, Backing_Count => 4); pragma Assert (OK);
   VM.Map_Pages (Source, 4096, [16#100000#, 16#200000#], Write_Back, Read_Write, OK);
   pragma Assert (OK);
   Run (False); -- unsealed: no owner callback or writes
   pragma Assert (Owner_Calls = 0 and Flush_Calls = 0);
   for P in RAM'Range loop
      for I in Table_Index loop pragma Assert (RAM (P) (I) = Sentinel); end loop;
   end loop;
   VM.Seal (Source, OK); pragma Assert (OK);
   Run (True);
   Run (True, 1, 4); -- only the four live mappings, not quota8
   for Case_ID in 1 .. 3 loop
      case Case_ID is
         when 1 => Run (False, 1, 3); -- missing last live page
         when 2 => Run (False, 2, 5); -- enough entries, wrong ordinal origin
         when 3 => Run (False, 1, 0); -- empty view
      end case;
      pragma Assert (Owner_Calls = 0 and Flush_Calls = 0);
      for P in RAM'Range loop
         for I in Table_Index loop pragma Assert (RAM (P) (I) = Sentinel); end loop;
      end loop;
   end loop;
   -- Owner loss at each write/flush/readback stage and final release check.
   for Failure in 1 .. Successful_Owner_Calls loop
      Fail_Owner := Failure; Run (False);
   end loop;
   Fail_Owner := 0;
   for Failure in 1 .. 4 loop
      Fail_Flush := Failure; Run (False);
   end loop;
   Fail_Flush := 0;
   Corrupt := True; Run (False); Corrupt := False;
   -- Invalid destination on the LAST page must reject before touching any.
   for Case_ID in 1 .. 5 loop
      declare Saved : constant Writer.Page_Mapping := Backing (4); begin
         case Case_ID is
            when 1 => Backing (4).CPU := 0;
            when 2 => Backing (4).CPU := Backing (1).CPU;
            when 3 => Backing (4).CPU := Backing (4).CPU + 1;
            when 4 => Backing (4).CPU := 2 ** 47;
            when 5 => Backing (4).DMA := 16#100000#;
         end case;
         Run (False);
         pragma Assert (Flush_Calls = 0);
         for P in RAM'Range loop
            for I in Table_Index loop pragma Assert (RAM (P) (I) = Sentinel); end loop;
         end loop;
         Backing (4) := Saved;
      end;
   end loop;
   Real_Flush := True; Run (True);
   declare
      Unrelated : VM.Image;
      State : Writer.State;
   begin
      VM.Initialize (Unrelated,
        [1 => 16#A00000#, 2 => 16#A01000#, 3 => 16#A02000#, 4 => 16#A03000#, others => 0],
        OK, Backing_Count => 4);
      pragma Assert (OK);
      VM.Map_Page (Unrelated, 4096, 16#B00000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
      VM.Seal (Unrelated, OK); pragma Assert (OK);
      RAM := [others => [others => Sentinel]];
      Owner_Calls := 0; Flush_Calls := 0;
      Writer.Publish_Update
        (State, Source, Unrelated, Backing,
         (Unsigned_64 (To_Integer (RAM (0)'Address)), 4096), OK);
      pragma Assert (not OK and Owner_Calls = 0 and Flush_Calls = 0);
      for P in RAM'Range loop
         for I in Table_Index loop pragma Assert (RAM (P) (I) = Sentinel); end loop;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("VM publication lineage PASS: unrelated sealed image rejected before callbacks or writes");
   Ada.Text_IO.Put_Line
     ("VM materialize PASS: volatile RAM, guards, every ownership-loss point, flush failures, corruption, no retry");
   Ada.Text_IO.Put_Line
     ("Host x86 CLFLUSH PASS; not GPU publication or device-coherence evidence");
end VM_Materialize_Tests;
