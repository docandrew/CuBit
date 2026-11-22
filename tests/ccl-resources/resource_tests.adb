with Ada.Text_IO;
with GNAT.Source_Info;
with CCL.Types;
with CCL.Resources; use CCL.Resources;
with CCL.Resources.Boundaries;

procedure Resource_Tests is
   package T renames CCL.Types;
   use type T.Type_Reference;
   use type T.Definition_Result;
   Types : T.Registry;
   Collection, Window : T.Type_Reference;
   Defined : T.Definition_Result;
   Owner : Registry (1);
   Other : Registry (2);
   Session, Old_Run, Other_Run : Run;
   Item, Old_Item, Other_Item : Reference;
   Factory, Call, Old_Call, Other_Call : Ticket;
   Result : Outcome;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Site : String := GNAT.Source_Info.Source_Location) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Site & " resource check" & Checks'Image; end if;
   end Check;
begin
   CCL.Resources.Boundaries.Check;
   T.Define (Types, (Identifier => T.Named ("Collection"), Form => T.Resource, Count => 1,
                    Parts => [1 => (T.Named ("Value"), T.Integer_Type), others => <>]), Collection, Defined);
   Check (Defined = T.Defined);
   T.Define (Types, (Identifier => T.Named ("Window"), Form => T.Resource, others => <>), Window, Defined);
   Check (Defined = T.Defined);
   Check (Empty (Owner) and not Current (Owner, No_Reference) and not Active (Owner, No_Run));
   Check (not Valid_Ticket (Owner, No_Ticket) and Position_Of (Owner, No_Ticket) = 0);
   Reserve (Owner, No_Run, Collection, Call, Result); Check (Result = Stale_Run and Call = No_Ticket);
   Start (Owner, Types, Session, Result); Check (Result = Succeeded and Active (Owner, Session));
   Start (Owner, Types, Other_Run, Result); Check (Result = Busy and Other_Run = No_Run);
   Reserve (Owner, Session, T.Integer_Type, Call, Result); Check (Result = Invalid_Type and Call = No_Ticket);
   Reserve (Owner, Session, T.Invalid_Type, Call, Result); Check (Result = Invalid_Type and Call = No_Ticket);
   Reserve (Owner, Session, Collection, Factory, Result);
   Check (Result = Succeeded and Valid_Ticket (Owner, Factory) and Position_Of (Owner, Factory) = 1);
   Check (Phase (Owner, 1) = Acquiring and Pending (Owner, 1));
   Reclaim (Owner, Factory, Result); Check (Result = Not_Ready);
   Finish_Use (Owner, Factory, True, Result); Check (Result = Stale_Ticket);
   Publish (Owner, Factory, True, Item, Result);
   Check (Result = Succeeded and Current (Owner, Item) and Type_Of (Owner, Item) = Collection);
   Check (not Valid_Ticket (Owner, Factory) and Phase (Owner, 1) = Available and not Pending (Owner, 1));
   Publish (Owner, Factory, True, Other_Item, Result); Check (Result = Stale_Ticket and Other_Item = No_Reference);
   Begin_Use (Owner, Item, Window, Call, Result); Check (Result = Wrong_Type and Call = No_Ticket);
   Begin_Use (Owner, Item, Collection, Call, Result);
   Check (Result = Succeeded and Phase (Owner, 1) = In_Flight and Current (Owner, Item));
   Begin_Use (Owner, Item, Collection, Other_Call, Result); Check (Result = Busy and Other_Call = No_Ticket);
   Publish (Owner, Call, True, Other_Item, Result); Check (Result = Stale_Ticket and Valid_Ticket (Owner, Call));
   Finish_Use (Owner, Call, True, Result); Check (Result = Succeeded and Current (Owner, Item));
   Old_Call := Call;
   Begin_Use (Owner, Item, Collection, Call, Result); Check (Result = Succeeded and Call /= Old_Call);
   Finish_Use (Owner, Old_Call, False, Result); Check (Result = Stale_Ticket and Valid_Ticket (Owner, Call));
   Retire (Owner, Item, Result); Check (Result = Succeeded and not Current (Owner, Item));
   Check (Type_Of (Owner, Item) = T.Invalid_Type);
   Reclaim (Owner, Factory, Result); Check (Result = Not_Ready); -- still borrowed
   Finish_Use (Owner, Call, True, Result); Check (Result = Succeeded and Phase (Owner, 1) = Retiring);
   Reclaim (Owner, Factory, Result); Check (Result = Succeeded and Empty (Owner));
   Old_Item := Item; Old_Call := Factory;
   Reserve (Owner, Session, Collection, Factory, Result); Check (Result = Succeeded);
   Publish (Owner, Factory, True, Item, Result); Check (Result = Succeeded and Item /= Old_Item);
   Begin_Use (Owner, Old_Item, Collection, Call, Result); Check (Result = Stale_Reference);
   Retire (Owner, Old_Item, Result); Check (Result = Stale_Reference and Current (Owner, Item));
   Retire (Owner, Item, Result); Check (Result = Succeeded);
   Reclaim (Owner, Old_Call, Result); Check (Result = Stale_Ticket and not Empty (Owner));
   Reclaim (Owner, Factory, Result); Check (Result = Succeeded);

   -- Different host contexts cannot collide even with matching local counters.
   Start (Other, Types, Other_Run, Result); Check (Result = Succeeded);
   Reserve (Other, Other_Run, Collection, Other_Call, Result); Check (Result = Succeeded);
   Publish (Other, Other_Call, True, Other_Item, Result); Check (Result = Succeeded);
   Check (not Current (Owner, Other_Item) and not Valid_Ticket (Owner, Other_Call));
   Stop (Owner, Other_Run, Result); Check (Result = Stale_Run and Active (Owner, Session));
   Check (Position_Of (Owner, Other_Call) = 0);

   -- Stop during factory completion: deny publication, drain, then clean up.
   Reserve (Owner, Session, Collection, Factory, Result); Check (Result = Succeeded);
   Old_Run := Session;
   Stop (Owner, Session, Result); Check (Result = Succeeded and not Active (Owner, Session));
   Start (Owner, Types, Session, Result); Check (Result = Busy and Session = No_Run);
   Reclaim (Owner, Factory, Result); Check (Result = Not_Ready);
   Publish (Owner, Factory, True, Item, Result);
   Check (Result = Not_Ready and Item = No_Reference and not Pending (Owner, 1));
   Begin_Cleanup (Owner, Factory, Call, Result); Check (Result = Succeeded and Valid_Ticket (Owner, Call));
   Begin_Cleanup (Owner, Factory, Other_Call, Result); Check (Result = Not_Ready);
   Reclaim (Owner, Factory, Result); Check (Result = Not_Ready);
   Finish_Use (Owner, Call, True, Result); Check (Result = Succeeded and Phase (Owner, 1) = Retiring);
   Reclaim (Owner, Factory, Result); Check (Result = Succeeded);
   Start (Owner, Types, Session, Result); Check (Result = Succeeded and Session /= Old_Run);
   Reserve (Owner, Old_Run, Collection, Call, Result); Check (Result = Stale_Run);
   Stop (Owner, Old_Run, Result); Check (Result = Stale_Run and Active (Owner, Session));

   -- A denied factory also retains its backing slot until host retirement.
   Reserve (Owner, Session, Collection, Factory, Result); Check (Result = Succeeded);
   Publish (Owner, Factory, False, Item, Result); Check (Result = Not_Ready and Item = No_Reference);
   Check (Phase (Owner, 1) = Retiring);
   Reclaim (Owner, Factory, Result); Check (Result = Succeeded);

   -- Fill every slot, stop with an operation on each, then drain in reverse.
   declare
      Factories, Calls : array (Occupied_Slot) of Ticket;
      Items : array (Occupied_Slot) of Reference;
   begin
      for I in Occupied_Slot loop
         Reserve (Owner, Session, Collection, Factories (I), Result);
         Check (Result = Succeeded and Position_Of (Owner, Factories (I)) = I);
         Publish (Owner, Factories (I), True, Items (I), Result); Check (Result = Succeeded);
         Begin_Use (Owner, Items (I), Collection, Calls (I), Result); Check (Result = Succeeded);
      end loop;
      Reserve (Owner, Session, Window, Call, Result); Check (Result = Capacity_Exhausted and Call = No_Ticket);
      Stop (Owner, Session, Result); Check (Result = Succeeded);
      for I in reverse Occupied_Slot loop
         Check (not Current (Owner, Items (I)) and Valid_Ticket (Owner, Calls (I)));
         Reclaim (Owner, Factories (I), Result); Check (Result = Not_Ready);
         Finish_Use (Owner, Calls (I), True, Result); Check (Result = Succeeded and Phase (Owner, I) = Retiring);
         Reclaim (Owner, Factories (I), Result); Check (Result = Succeeded);
         Reclaim (Owner, Calls (I), Result); Check (Result = Stale_Ticket);
      end loop;
      Check (Empty (Owner));
      Start (Owner, Types, Session, Result); Check (Result = Succeeded);
      for I in 1 .. 1_024 loop
         Reserve (Owner, Session, Collection, Factory, Result); Check (Result = Succeeded);
         Publish (Owner, Factory, True, Item, Result); Check (Result = Succeeded);
         for J in Occupied_Slot loop
            Check (not Current (Owner, Items (J)) and not Valid_Ticket (Owner, Calls (J)));
         end loop;
         Begin_Use (Owner, Item, Collection, Call, Result); Check (Result = Succeeded);
         Finish_Use (Owner, Call, False, Result); Check (Result = Succeeded and not Current (Owner, Item));
         Reclaim (Owner, Call, Result); Check (Result = Succeeded);
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Host-owned resource lifetime: PASS" & Checks'Image & " checks");
end Resource_Tests;
