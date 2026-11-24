pragma Ada_2022;
with Ada.Text_IO;
with AML_Child_Indices;
procedure Reservation_Tests is
   package Index is new AML_Child_Indices (8);
   package R is new Index.Name_Reservations (Natural, Natural, 0);
   use type R.Node_ID;
   Ticket : R.Ticket;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      if not Condition then raise Program_Error with "reservation invariant"; end if;
      Checks := Checks + 1;
   end Check;
begin
   for Count in R.Node_ID range 0 .. 7 loop
      for Owner in R.Node_ID range 0 .. Count loop
         for Last_Stamp in Natural range 8 .. 15 loop
            declare Owner_Stamp : constant Natural := (if Owner = 0 then 0 else 1); begin
               R.Reserve (Ticket, Count, Owner, Last_Stamp, Owner_Stamp, 7);
               Check (R.Active (Ticket));
               Check (R.Node (Ticket) = Count + 1);
               Check (R.Stamp (Ticket) = Last_Stamp + 1);
               Check (R.Prior_Count (Ticket) = Count);
               Check (R.Prior_Stamp (Ticket) = Last_Stamp);
               Check (R.Matches (Ticket, Count + 1, Owner, Last_Stamp + 1, Owner_Stamp, 7));
               Check (not R.Matches (Ticket, Count + 1, Owner, Last_Stamp, Owner_Stamp, 7));
               Check (not R.Matches (Ticket, Count + 1, Owner, Last_Stamp + 1, Owner_Stamp + 1, 7));
               Check (not R.Matches (Ticket, Count + 1, Owner, Last_Stamp + 1, Owner_Stamp, 8));
               Check (not R.Matches (Ticket, Count, Owner, Last_Stamp + 1, Owner_Stamp, 7));
               R.Release (Ticket, Count + 1, Owner, Last_Stamp + 1, Owner_Stamp, 7);
               Check (not R.Active (Ticket));
               Check (not R.Matches (Ticket, Count + 1, Owner, Last_Stamp + 1, Owner_Stamp, 7));
            end;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Name reservation checks" & Checks'Image);
end Reservation_Tests;
