with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Materialize;
with Intel_GPU_PPGTT_Scratch;
procedure Scratch_Materialize_Tests is
   package V is new Intel_GPU_VM_Image (4);
   package S renames Intel_GPU_PPGTT_Scratch;
   type Page is array (Table_Index) of Unsigned_64;
   type Storage is array (Natural range 0 .. 9) of Page;
   Sentinel : constant Unsigned_64 := 16#DEAD_BEEF_DEAD_BEEF#;
   RAM : Storage := [others => [others => Sentinel]] with Alignment => 4096, Volatile;
   Calls, Flushes, Fail_Owner, Fail_Flush : Natural := 0;
   Corrupt : Boolean := False;
   function Ready return Boolean is
   begin Calls := Calls + 1; return Calls /= Fail_Owner; end Ready;
   function Flush (CPU : Unsigned_64) return Boolean is
      Expected : constant Natural := (if Flushes < 4 then Flushes + 5 else 8 - Flushes);
   begin
      Flushes := Flushes + 1;
      pragma Assert (CPU = Unsigned_64 (To_Integer (RAM (Expected)'Address)));
      if Corrupt and Flushes = 3 then RAM (7) (5) := 0; end if;
      return Flushes /= Fail_Flush;
   end Flush;
   package M is new Intel_GPU_VM_Materialize (V, Ready, Flush);
   Source : V.Image;
   Backing : M.Mappings;
   Scratch : M.Scratch_Mappings;
   Descriptor : constant S.Backing_Pages := [16#5000#,16#6000#,16#7000#,16#8000#];
   OK : Boolean;
   Root : Unsigned_64;
   Total_Calls : Natural;
   procedure Run (Expected : Boolean; No_Writes : Boolean := False) is
      State : M.State;
   begin
      RAM := [others => [others => Sentinel]];
      Calls := 0; Flushes := 0;
      M.Prepare (State, Source, Backing, Root, OK, Scratch);
      pragma Assert (OK = Expected and Root = (if Expected then 4096 else 0));
      for I in Table_Index loop
         pragma Assert (RAM (0) (I) = Sentinel and RAM (9) (I) = Sentinel);
      end loop;
      if No_Writes then
         for P in RAM'Range loop
            for W of RAM (P) loop pragma Assert (W = Sentinel); end loop;
         end loop;
      end if;
      if Expected then
         pragma Assert (Flushes = 8);
         for P in 1 .. 4 loop
            for I in Table_Index loop
               pragma Assert (RAM (P) (I) = V.Entry_Value (Source, P, I));
            end loop;
         end loop;
         for L in S.Level loop
            for W of RAM (5 + L) loop
               pragma Assert (W = (if L = 0 then 0 else V.Scratch_Entry (Source, L)));
            end loop;
         end loop;
      end if;
      Total_Calls := Calls;
      M.Prepare (State, Source, Backing, Root, OK, Scratch);
      pragma Assert (not OK and Root = 0 and Calls = Total_Calls);
   end Run;
begin
   for P in V.Page_Number loop
      Backing (P) := (Unsigned_64 (To_Integer (RAM (P)'Address)), Unsigned_64 (P) * 4096);
   end loop;
   for L in S.Level loop
      Scratch (L) := (Unsigned_64 (To_Integer (RAM (L + 5)'Address)), Descriptor (L));
   end loop;
   V.Initialize (Source, [4096,8192,12288,16384], OK, Descriptor);
   pragma Assert (OK);
   V.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
   pragma Assert (OK);
   V.Seal (Source, OK); pragma Assert (OK);
   Run (True);
   declare Bound : constant Natural := Total_Calls; begin
      for F in 1 .. Bound loop Fail_Owner := F; Run (False); end loop;
   end;
   Fail_Owner := 0;
   for F in 1 .. 8 loop Fail_Flush := F; Run (False); end loop;
   Fail_Flush := 0; Corrupt := True; Run (False); Corrupt := False;
   for L in S.Level loop
      declare Saved : constant M.Page_Mapping := Scratch (L); begin
         Scratch (L).CPU := Backing (1).CPU; Run (False, True);
         Scratch (L) := Saved;
         Scratch (L).DMA := 4096; Run (False, True);
         Scratch (L) := Saved;
         for K in S.Level loop
            if K /= L then
               Scratch (L).CPU := Scratch (K).CPU; Run (False, True);
               Scratch (L) := Saved;
            end if;
         end loop;
      end;
   end loop;
   Run (True);
   Ada.Text_IO.Put_Line ("Scratch materialize PASS: RAM, guards, owner/flush faults, corruption, aliases, no retry");
end Scratch_Materialize_Tests;
