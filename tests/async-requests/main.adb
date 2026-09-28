with Ada.Text_IO;
with GNAT.Source_Info;
with CuBit.Async_Requests;

procedure Main is
   package A renames CuBit.Async_Requests;
   use type A.Phase;
   use type A.Token;
   use type A.Tracker;
   type Token_Array is array (Positive range <>) of A.Token;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Site : String := GNAT.Source_Info.Source_Location) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Site; end if;
   end Check;
begin
   -- Stop at every lifecycle boundary; rejection and acceptance are distinct.
   for Stop_At in 0 .. 5 loop
      for Submitted in Boolean loop
         declare
            Item, Before : A.Tracker;
            Good : Boolean;
         begin
            Check (A.Drained (Item) and not A.Can_Resume (Item));
            if Stop_At = 1 then A.Stop (Item); end if;
            for ID of Token_Array'(1 => 0, 2 => A.Token'Last) loop
               Before := Item;
               A.Reserve (Item, ID, Good); Check (not Good and Item = Before);
            end loop;
            A.Reserve (Item, 10, Good);
            if Stop_At = 1 then Check (not Good and A.Drained (Item));
            else
               Check (Good and A.State (Item) = A.Reserved);
               Before := Item;
               A.Reserve (Item, 11, Good); Check (not Good and Item = Before);
               A.Capture (Item, 10, True, Good); Check (not Good and Item = Before);
               if Stop_At = 2 then A.Stop (Item); end if;
               A.Submitted (Item, Submitted);
               Check (A.Last_Token (Item) = 10);
               if not Submitted then
                  Check (A.Drained (Item) and not A.Can_Reserve (Item, 10));
               else
                  if Stop_At = 3 then A.Stop (Item); end if;
                  Before := Item;
                  for ID in A.Token range 8 .. 12 loop
                     if ID /= 10 then
                        A.Capture (Item, ID, True, Good); Check (not Good and Item = Before);
                     end if;
                  end loop;
                  A.Capture (Item, 10, False, Good); Check (not Good and Item = Before);
                  A.Capture (Item, 10, True, Good); Check (Good and A.State (Item) = A.Completion_Ready);
                  if Stop_At = 4 then A.Stop (Item); end if;
                  Check (A.Can_Resume (Item) = (Stop_At not in 2 .. 4));
                  Before := Item;
                  A.Capture (Item, 10, True, Good); Check (not Good and Item = Before);
                  A.Reserve (Item, 11, Good); Check (not Good and Item = Before);
                  A.Release (Item); Check (A.Drained (Item) and A.Pending_Token (Item) = 0);
               end if;
               if Stop_At = 5 then A.Stop (Item); end if;
               Check (not A.Can_Reserve (Item, 10));
               A.Reserve (Item, 11, Good);
               Check (Good = not A.Detached (Item));
            end if;
         end;
      end loop;
   end loop;
   -- Last usable token never wraps, even after a rejected submission.
   declare
      Item : A.Tracker;
      Good : Boolean;
   begin
      A.Reserve (Item, A.Token'Last - 1, Good); Check (Good);
      A.Submitted (Item, False);
      Check (not A.Can_Reserve (Item, 1) and not A.Can_Reserve (Item, A.Token'Last));
   end;
   Ada.Text_IO.Put_Line ("Shared async request lifetime: PASS" & Checks'Image & " checks");
end Main;
