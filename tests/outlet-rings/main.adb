--  Outlet ring tables round-trip, measure exactly, and refuse malformed
--  input (every one-byte corruption decodes to a distinct table or none).
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Outlet_Rings; use CuBit.Outlet_Rings;

procedure Main is
   Failures, Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;
   T, Back : Table;
   Item : Bytes (1 .. Maximum_Bytes);
   Length : Table_Length;
   Accepted, Present : Boolean;
   Measured : Table_Length;
begin
   Encode (T, Item, Length);
   Check (Length = 0, "no entries, no table");
   T.Owner := 31;
   T.Count := 2;
   T.Entries (1) := (Outlet => 1, Grant => 16#0000_0001_0000_0005#);
   T.Entries (2) := (Outlet => 3, Grant => 16#FFFF_0000_1234_5678#);
   Encode (T, Item, Length);
   Check (Length = Header_Bytes + 2 * Entry_Bytes, "two entries");
   Measure (Item (1 .. Length + 40), Present, Measured);
   Check (Present and then Measured = Length, "measure finds the table before what follows");
   Decode (Item (1 .. Length), Back, Accepted);
   Check (Accepted and then Back.Owner = 31 and then Back.Count = 2
          and then Back.Entries (2).Grant = T.Entries (2).Grant
          and then Back.Entries (1).Outlet = 1, "round trip");
   Measure (Bytes'[16#50#, 16#44#, 16#53#, 16#43#, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            Present, Measured);
   Check (not Present, "a description is not a ring table");
   declare
      Corrupt : Bytes (1 .. Length);
      Survived : Natural := 0;
   begin
      for I in Corrupt'Range loop
         for B in Unsigned_8 loop
            Corrupt := Item (1 .. Length);
            Corrupt (I) := B;
            Decode (Corrupt, Back, Accepted);
            if Accepted then
               Survived := Survived + 1;
               Check (Back.Count = 2 and then Back.Entries (1).Outlet /= Back.Entries (2).Outlet,
                      "a corrupted table that decodes is distinct");
            else
               Check (Back.Count = 0, "refused tables are empty");
            end if;
         end loop;
      end loop;
      Check (Survived > 0, "grant and owner bytes may vary");
   end;
   T.Entries (2).Outlet := 1;
   Encode (T, Item, Length);
   Decode (Item (1 .. Length), Back, Accepted);
   Check (not Accepted, "a connector twice is refused");
   Put_Line ("outlet-rings:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures /= 0 then
      raise Program_Error;
   end if;
end Main;
