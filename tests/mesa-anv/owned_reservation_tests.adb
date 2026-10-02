with Interfaces; use Interfaces;
with Owned_Reservation_Policy; use Owned_Reservation_Policy;
with Ada.Text_IO;
procedure Owned_Reservation_Tests is
   type Values is array (Positive range <>) of Unsigned_64;
   Boundaries : constant Values :=
     [0, 1, 4095, 4096, 4097, Maximum_Commit_Bytes - 4096,
      Maximum_Commit_Bytes, Maximum_Commit_Bytes + 4096,
      Maximum_Reserved_Bytes - 4096, Maximum_Reserved_Bytes,
      Maximum_Reserved_Bytes + 4096, Unsigned_64'Last];
   Committed : Unsigned_64 := 0;
   Count : Natural := 0;
begin
   for Capacity of Boundaries loop
      for Existing of Boundaries loop
         for Bytes of Boundaries loop
            declare
               Expected : constant Boolean :=
                 Capacity in 4096 .. Maximum_Reserved_Bytes and then
                 Capacity mod 4096 = 0 and then Existing <= Capacity and then
                 Existing mod 4096 = 0 and then
                 Bytes in 4096 .. Maximum_Commit_Bytes and then
                 Bytes mod 4096 = 0 and then Capacity - Existing >= Bytes;
               Next : constant Unsigned_64 := After_Commit (Capacity, Existing, Bytes);
            begin
               pragma Assert (Can_Commit (Capacity, Existing, Bytes) = Expected);
               pragma Assert
                 ((if Expected then Next = Existing + Bytes else Next = Existing));
               Count := Count + 1;
            end;
         end loop;
      end loop;
   end loop;
   for Chunk in 1 .. 128 loop
      pragma Assert (Can_Commit (Maximum_Reserved_Bytes, Committed, Maximum_Commit_Bytes));
      Committed := After_Commit (Maximum_Reserved_Bytes, Committed, Maximum_Commit_Bytes);
      pragma Assert (Committed = Unsigned_64 (Chunk) * Maximum_Commit_Bytes);
   end loop;
   pragma Assert (Committed = Maximum_Reserved_Bytes);
   pragma Assert (not Can_Commit (Maximum_Reserved_Bytes, Committed, 4096));
   Ada.Text_IO.Put_Line ("Reservation arithmetic PASS cases=" & Count'Image &
                        "; NOT a native memory allocator");
end Owned_Reservation_Tests;
