-- Hosted integration gate for adopting CuAlloc for driver CPU metadata.
-- A refused release must remain accounted for; this is not a GPU test.
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CuAlloc;
with Linux_Provider;
procedure Heap_Release_Audit is
   Refuse : Boolean := False;
   function Release (Base, Bytes : Unsigned_64) return Boolean is
   begin
      if Refuse then return False; end if;
      return Linux_Provider.Release (Base, Bytes);
   end Release;
   package Heap is new CuAlloc
     (Linux_Provider.Reserve, Linux_Provider.Commit, Release,
      Linux_Provider.Maximum_Commit);
   Item, Before, Provider_Before : Unsigned_64;
begin
   Item := Heap.Allocate (3_000_000, 4096);
   if Item = 0 then
      Put_Line ("FAIL setup allocation");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   Before := Heap.Committed_Bytes;
   Provider_Before := Linux_Provider.Committed;
   Refuse := True;
   Heap.Free (Item);
   Put_Line ("heap committed before=" & Before'Image &
             " after=" & Heap.Committed_Bytes'Image);
   Put_Line ("provider committed before=" & Provider_Before'Image &
             " after=" & Linux_Provider.Committed'Image);
   if Heap.Committed_Bytes /= Before or else
     Linux_Provider.Committed /= Provider_Before
   then
      Put_Line ("FAIL refused release must retain backing accounting");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("PASS refused release retains backing accounting");
   end if;
end Heap_Release_Audit;
