with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with IPC_Request_Ids; use IPC_Request_Ids;

procedure Main is
   State : Sequence := Initial_Sequence;
   Result : Allocation;
begin
   --  In particular, the first two submissions must not both get ID 1.
   for Expected in Unsigned_64 range 1 .. 10_000 loop
      Result := Next (State);
      pragma Assert (Result.Available and then Result.Id = Expected);
      State := Sequence (Result.Id);
   end loop;
   State := Sequence'Last - 2;
   Result := Next (State);
   pragma Assert (Result.Available and then Result.Id = Unsigned_64'Last - 1);
   State := Sequence (Result.Id);
   Result := Next (State);
   pragma Assert (Result.Available and then Result.Id = Unsigned_64'Last);
   State := Sequence (Result.Id);
   for Attempt in 1 .. 100 loop
      Result := Next (State);
      pragma Assert (not Result.Available and State = Sequence'Last);
   end loop;
   Put_Line ("PASS: request IDs start at one, increase, and exhaust without reuse");
end Main;
