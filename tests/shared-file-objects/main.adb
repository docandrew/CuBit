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
   --  Every ordered pair of distinct handle slots, both request modes and
   --  both existing modes. Sharing is independent of the client PID.
   for First in Owner_Index loop
      for Second in Owner_Index loop
         if First /= Second then
            for Existing in Sharing_Mode loop
               for Requested in Sharing_Mode loop
                  declare
                     T : State;
                     Key_A : constant Identity := (NVMe_Volume, 7);
                  begin
                     pragma Assert (Exclusive_Owners_Isolated (T));
                     Attach (T, First, Key_A, 42, R, Existing);
                     pragma Assert (R = Created);
                     Attach (T, Second, Key_A, 99, R, Requested);
                     if Existing = Allow_Sharing and Requested = Allow_Sharing then
                        pragma Assert (R = Shared and Value (T, Second) = 42);
                        Detach (T, Second);
                     else
                        pragma Assert (R = Sharing_Conflict and not Attached (T, Second));
                     end if;
                     pragma Assert (Exclusive_Owners_Isolated (T));
                     pragma Assert (Value (T, First) = 42);
                     pragma Assert (Exclusively_Held (T, Key_A) = (Existing = Deny_Sharing));
                     Attach (T, Second, (Memory_Volume, 7), 19, R, Deny_Sharing);
                     pragma Assert (R = Created); -- same inode, different volume
                     Replace (T, First, 23);
                     pragma Assert (Value (T, Second) = 19 and Exclusive_Owners_Isolated (T));
                     Detach (T, First);
                     pragma Assert (not Exclusively_Held (T, Key_A));
                     Attach (T, First, Key_A, 17, R, Deny_Sharing);
                     pragma Assert (R = Created and Exclusive_Owners_Isolated (T));
                  end;
               end loop;
            end loop;
         end if;
      end loop;
   end loop;
   Put_Line ("PASS: exclusive ownership, 3968 admission/lifecycle scenarios");
end Main;
