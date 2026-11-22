with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Metadata_Bundle;
procedure Metadata_Bundle_Revocation_Tests is
   type Table_ID is (Only_Table);
begin
   for Mode in 1 .. 5 loop
      declare
         Owned : Boolean := True;
         Available : Natural := 1;
         Calls : Natural := 0;
         function Owner return Boolean is (Owned);
         function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
         begin
            pragma Assert (Owned and Bytes = 4096);
            Calls := Calls + 1;
            if Mode = 1 then Owned := False; end if;
            return 16#100000#;
         end;
         function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
         begin
            pragma Assert (Owned and Base = 16#100000# and Offset = 0 and Bytes = 4096);
            Calls := Calls + 1;
            if Mode = 2 then Owned := False; end if;
            return True;
         end;
         function Initialize (Base, Bytes : Unsigned_64) return Boolean is (True);
         package Storage is new Intel_GPU_Metadata_Arena (Reserve, Commit, Initialize);
         function Capacity (Table : Table_ID) return Natural is
         begin
            Calls := Calls + 1;
            if Mode = 5 then Owned := False; end if;
            return Available;
         end;
         procedure Extend (Table : Table_ID; Base, Bytes : Unsigned_64; OK : out Boolean) is
         begin
            pragma Assert (Owned and Base = 16#100000# and Bytes = 4096);
            Calls := Calls + 1; Available := 3; OK := True;
            if Mode = 3 then Owned := False; end if;
         end;
         procedure Admit (Count : Positive; OK : out Boolean) is
         begin
            pragma Assert (Owned and Available = 3 and Count = 3);
            Calls := Calls + 1; OK := True;
            if Mode = 4 then Owned := False; end if;
         end;
         package B is new Intel_GPU_Metadata_Bundle
           (Table_ID, Storage, Owner, Capacity, Extend, Admit);
         use type B.Phase;
         Object : B.Bundle;
         OK : Boolean;
         Before : Natural;
      begin
         B.Request (Object, 3, 3, 4096, OK); pragma Assert (OK);
         for Turn in 1 .. 20 loop
            B.Step (Object);
            exit when not Owned;
         end loop;
         pragma Assert (not Owned and B.State (Object) = B.Failed);
         Before := Calls;
         Owned := True;
         for Turn in 1 .. 10 loop B.Step (Object); end loop;
         B.Request (Object, 3, 3, 4096, OK);
         pragma Assert (not OK and Calls = Before and B.State (Object) = B.Failed);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Metadata bundle revocation PASS: capacity/reserve/commit/extend/admit loss, immediate failure, no callback replay");
end Metadata_Bundle_Revocation_Tests;
