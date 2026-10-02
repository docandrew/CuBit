with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Snapshots;
with Intel_GPU_PPGTT_Scratch;
with Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Materialize;
procedure VM_Scratch_Tests is
   package V is new Intel_GPU_VM_Image (16);
   package Snapshots is new V.Snapshots;
   package S renames Intel_GPU_PPGTT_Scratch;
   package P renames Intel_GPU_ADLN_PPGTT;
   A, B, Rejected : V.Image;
   Tables, Next_Tables : V.Backing_Pages;
   Scratch : constant S.Backing_Pages := [16#1000#,16#3000#,16#5000#,16#7000#];
   OK : Boolean;
   function Ready return Boolean is (True);
   function Flush (CPU : Unsigned_64) return Boolean is
   begin
      raise Program_Error with "scratch materialization must reject before flush";
      return CPU = 0;
   end Flush;
   package M is new Intel_GPU_VM_Materialize (V, Ready, Flush);
   Materialized : M.State;
   Root : Unsigned_64;
   function Hardware_Walk (Object : V.Image; GPU : Unsigned_64) return Unsigned_64 is
      W : constant P.Walk := P.Locate (GPU);
      type Route is array (Natural range 0 .. 3) of P.Table_Index;
      Indices : constant Route := [W.PML4,W.PDP,W.PD,W.PT];
      DMA : Unsigned_64 := V.Root_DMA (Object);
      Word : Unsigned_64;
      Found : Boolean;
   begin
      for Depth in 0 .. 3 loop
         Found := False;
         Word := 0;
         for Page in 1 .. V.Used (Object) loop
            if DMA = V.Page_DMA (Object, Page) then
               Word := V.Entry_Value (Object, Page, Indices (Depth));
               Found := True;
            end if;
         end loop;
         for L in S.Table_Level loop
            if DMA = V.Scratch_DMA (Object, L) then
               pragma Assert (not Found);
               Word := V.Scratch_Entry (Object, L);
               Found := True;
            end if;
         end loop;
         pragma Assert (Found and then (Word and 1) = 1);
         DMA := Word - Word mod 4096;
      end loop;
      return DMA;
   end Hardware_Walk;
begin
   for Page in V.Page_Number loop
      Tables (Page) := 16#10000# + Unsigned_64 (Page) * 4096;
      Next_Tables (Page) := Tables (Page) + 16#20000#;
   end loop;
   V.Initialize (A, Tables, OK, Scratch);
   pragma Assert (OK);
   for L in S.Level loop
      pragma Assert (not V.DMA_Disjoint (A, Scratch (L), 4096));
      V.Map_Page (A, 16#2000#, Scratch (L), P.Write_Back, P.Read_Write, OK);
      pragma Assert (not OK and V.Used (A) = 1);
   end loop;
   V.Map_Page (A, 16#2000#, 16#800000#, P.Write_Back, P.Read_Write, OK);
   pragma Assert (OK);
   pragma Assert (Hardware_Walk (A, 16#2000#) = 16#800000#);
   for Page in Unsigned_64 range 0 .. 511 loop
      for Depth in 0 .. 3 loop
         declare GPU : constant Unsigned_64 := Shift_Left (Page, 12 + 9 * Depth); begin
            if GPU /= 16#2000# then
               pragma Assert (V.Lookup (A, GPU) = 0);
               pragma Assert (Hardware_Walk (A, GPU) = Scratch (0));
            end if;
         end;
      end loop;
   end loop;
   pragma Assert (Hardware_Walk (A, 2 ** 48 - 1) = Scratch (0));
   V.Seal (A, OK);
   pragma Assert (OK);
   M.Prepare (Materialized, A, [others => (CPU => 4096, DMA => 0)], Root, OK);
   pragma Assert (not OK and Root = 0);
   V.Prepare_Update (B, A, Next_Tables, OK);
   pragma Assert (OK and Hardware_Walk (B, 16#2000#) = 16#800000#);
   V.Unmap_Pages (B, 16#2000#, [1 => 16#800000#], OK);
   pragma Assert (OK and V.Lookup (B, 16#2000#) = 0);
   pragma Assert (Hardware_Walk (B, 16#2000#) = Scratch (0));
   pragma Assert (Hardware_Walk (A, 16#2000#) = 16#800000#);
   V.Seal_Update (B, OK);
   pragma Assert (OK);
   Snapshots.Adopt_Committed (A, B, OK);
   pragma Assert (OK and Hardware_Walk (A, 16#2000#) = Scratch (0));
   Next_Tables (1) := Scratch (0);
   V.Prepare_Update (Rejected, A, Next_Tables, OK);
   pragma Assert (not OK and V.Root_DMA (Rejected) = 0);
   -- Reject initial aliases/partial scratch descriptors, not merely updates.
   for L in S.Level loop
      declare Bad : V.Image; Descriptor : S.Backing_Pages := Scratch; begin
         Descriptor (L) := Tables (1);
         V.Initialize (Bad, Tables, OK, Descriptor);
         pragma Assert (not OK and V.Root_DMA (Bad) = 0);
      end;
      declare Bad : V.Image; Descriptor : S.Backing_Pages := Scratch; begin
         Descriptor (L) := 0;
         V.Initialize (Bad, Tables, OK, Descriptor);
         pragma Assert (not OK);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("VM scratch PASS: exported walks, update/unbind, isolation (hosted)");
end VM_Scratch_Tests;
