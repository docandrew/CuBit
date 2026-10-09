--  Hosted tests for Process_Identities: encode and decode round-trip at the
--  edges (slot 1 and the last slot, generation 0 and the last), and a
--  reused slot with a newer generation is a different identity.
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Process_Identities; use Process_Identities;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   procedure Round_Trip (S : Slot; G : Generation; Name : String) is
      I : constant Identity := Encode (S, G);
   begin
      Check (Slot_Of (I) = S and then Generation_Of (I) = G, Name);
   end Round_Trip;
begin
   Round_Trip (1, 0, "first slot, first generation");
   Round_Trip (Slot'Last, Generation'Last, "last slot, last generation");
   Round_Trip (255, 1, "today's last slot");
   Check (Encode (5, 1) /= Encode (5, 2), "a reused slot is a new identity");
   Check (Encode (5, 1) /= Encode (6, 1), "another slot is another identity");
   Check (Encode (1, 0) /= No_Identity, "a real slot is never no identity");
   Check (Generation_Of (From_Word (Unsigned_64'Last)) = Generation'Last and then
          Slot_Of (From_Word (Unsigned_64'Last)) = Slot'Last, "every bit is slot or generation");
   Check (To_Word (From_Word (16#1234_5678#)) = 16#1234_5678#, "a word round-trips");

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("process-identities: PASS");
   else
      Ada.Text_IO.Put_Line ("process-identities: FAIL" & Failures'Image);
   end if;
end Main;
