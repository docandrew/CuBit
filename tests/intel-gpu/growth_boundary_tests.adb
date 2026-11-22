with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_VM_Image.Growth.Backing;
with Intel_GPU_VM_Image.Growth.Backing.Writer;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure Growth_Boundary_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   package G is new VM.Growth;
   function Owned (DMA : Unsigned_64) return Boolean is (DMA /= 0);
   package B is new G.Backing (Owned);
   Calls : Natural := 0;
   function Held return Boolean is (True);
   procedure Read_Word (DMA : Unsigned_64; Index : Table_Index;
                        Value : out Unsigned_64; OK : out Boolean) is
   begin Calls := Calls + 1; Value := 0; OK := True; end;
   procedure Write_Word (DMA : Unsigned_64; Index : Table_Index;
                         Value : Unsigned_64; OK : out Boolean) is
   begin Calls := Calls + 1; OK := True; end;
   function Flush (DMA : Unsigned_64) return Boolean is
   begin Calls := Calls + 1; return True; end;
   package W is new B.Writer (Held, Read_Word, Write_Word, Flush, Held);
   Source : VM.Image;
   DMA : VM.Backing_Pages;
   OK : Boolean;
begin
   for I in DMA'Range loop DMA (I) := Unsigned_64 (I) * 4096; end loop;
   VM.Initialize (Source, DMA, OK); pragma Assert (OK);
   VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
   pragma Assert (OK);
   VM.Seal (Source, OK); pragma Assert (OK);
   -- Null and over-quota inputs must not turn request-sized scratch into
   -- exceptions, hardware writes, or an accidentally accepted partial plan.
   for Length in 0 .. 10 loop
      declare
         Pages : VM.Data_Pages (5 .. 4 + Length);
         Output : B.Links (7 .. 9);
         State : W.State;
      begin
         for I in Pages'Range loop Pages (I) := 16#40000# + Unsigned_64 (I) * 4096; end loop;
         B.Resolve (Source, 2 ** 39, 4096, 16#90000#, Pages, Output, OK);
         pragma Assert (OK = (Length = 3));
         if not OK then
            pragma Assert (for all Item of Output => Item.Child_DMA = 0);
         end if;
         W.Start (State, Source, 2 ** 39, 4096, 16#90000#, Pages, OK);
         pragma Assert (OK = (Length = 3) and Calls = 0);
         -- Premature commit must also handle an empty receipt's zero-sized
         -- transient arrays, consume the attempt, and forbid later writes.
         W.Commit (State, Source, OK); pragma Assert (not OK);
         W.Step (State, Source); pragma Assert (Calls = 0);
         W.Rearm (State, Source, 16#90000#, OK); pragma Assert (not OK);
      end;
   end loop;
   declare
      State : W.State;
   begin
      W.Commit (State, Source, OK); pragma Assert (not OK);
      W.Start (State, Source, 2 ** 39, 4096, 16#90000#,
               [16#45000#, 16#46000#, 16#47000#], OK);
      pragma Assert (not OK and not W.Pending (State));
      W.Step (State, Source); pragma Assert (Calls = 0);
   end;
   Ada.Text_IO.Put_Line ("Growth boundaries PASS12: null, exact, short, over-quota and idle commit; no hardware callbacks");
end Growth_Boundary_Tests;
