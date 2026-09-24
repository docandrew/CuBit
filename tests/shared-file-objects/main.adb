with Ada.Text_IO; use Ada.Text_IO;
with Model; use Model;
procedure Main is
   use Objects;
   S : State;
   R : Attach_Result;
begin
   Attach (S, 0, (NVMe_Volume, 1), 10, R);
   pragma Assert (R = Created);
   Attach (S, 1, (NVMe_Volume, 1), 999, R);
   pragma Assert (R = Shared and then Value (S, 1) = 10);
   Attach (S, 2, (Memory_Volume, 1), 20, R);
   pragma Assert (R = Created and then not Same_Object (S, 0, 2));
   Replace (S, 0, 30);
   pragma Assert (Value (S, 1) = 30 and then Value (S, 2) = 20);
   Attach (S, 0, (NVMe_Volume, 2), 99, R);
   pragma Assert (R = Owner_Busy and then Value (S, 0) = 30);
   Detach (S, 0);
   pragma Assert (Value (S, 1) = 30);
   Attach (S, 0, (NVMe_Volume, 2), 40, R);
   pragma Assert (R = Created and then Value (S, 1) = 30);
   Attach (S, 3, (NVMe_Volume, 3), 50, R);
   pragma Assert (R = Created);
   for Owner in Owner_Index range 4 .. Owner_Index'Last loop
      Attach (S, Owner, (NVMe_Volume, 100 + Owner), Owner, R);
      pragma Assert (R = Created);
   end loop;
   --  Repeated last-close/reuse must not retain old metadata or leak capacity.
   for I in 1 .. 1_000 loop
      Detach (S, 1);
      Attach (S, 1, (NVMe_Volume, 1), I, R);
      pragma Assert (R = Created and then Value (S, 1) = I);
      pragma Assert (Value (S, 0) = 40 and then Value (S, 2) = 20);
   end loop;
   Detach (S, 0);
   Detach (S, 0);
   Attach (S, 0, (NVMe_Volume, 1), 999, R);
   pragma Assert (R = Shared and then Value (S, 0) = 1_000);
   Put_Line ("PASS: shared file metadata lifecycle and volume identity");
end Main;
