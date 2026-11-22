with Ada.Text_IO;
with CuBit.Display_Outputs;
with CuBit.Output_Registry;
with CuBit.Placement_Tickets;

procedure Main is
   package R renames CuBit.Display_Outputs.Registry;
   package T renames CuBit.Placement_Tickets;
   package W renames R.W;
   use type R.Mutation_Result;
   use type R.Connector_Presence;
   use type R.Power_Policy;
   use type R.Readiness;
   use type R.Output_Reference;
   use type R.State;
   use type R.L.Named_Display_ID;
   use type T.Admission;
   use type W.G.Logical_Coordinate;
   use type W.Work_Area;
   Registry : R.State (1);
   Other : R.State (2);
   Item : R.Description :=
     (Backend => (1, 0), Area => (7, 0, 0, 1_920, 1_040),
      Presence => R.Present, Power => R.Enabled, Stage => R.Ready);
   A, B, C, Old : R.Output_Reference;
   Result : R.Mutation_Result;
   View : R.Snapshot;
   Window : T.Window_Version := (1, 1);
   Desired : constant W.Preference :=
     (Home => 7, Bounds => (100, 50, 640, 480));
   Ticket : T.Ticket;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      if not Condition then
         raise Program_Error with "Check failed after" & Checks'Image;
      end if;
      Checks := Checks + 1;
   end Check;
begin
   Check (not R.Live (Registry, R.No_Output));
   View := R.Capture (Registry);
   Check (R.Current (Registry, View) and R.Areas (View).Count = 0);
   Check (T.Check (Registry, Ticket, Window, W.Idle) = T.No_Proposal);
   R.Register (Registry, Item, A, Result);
   Check (Result = R.Applied and R.Live (Registry, A));
   Check (not R.Current (Registry, View));
   View := R.Capture (Registry);
   Check (R.Areas (View).Count = 1);
   Check (R.Areas (View).Items (1) = Item.Area);
   Check (R.Reference_At (View, 1) = A);
   Check (not R.Current (Other, View));
   R.Register (Other, Item, B, Result);
   Check (Result = R.Applied and not R.Live (Other, A));
   Check (not R.Live (Registry, B));
   R.Update (Other, A, Item, C, Result);
   Check (Result = R.Stale_Reference and C = R.No_Output);

   Ticket := T.Prepare (Registry, Window, Desired);
   Check (T.Check (Registry, Ticket, Window, W.Idle) = T.May_Apply);
   Check (T.Proposal (Ticket).Target.Available);
   Check (T.Proposal (Ticket).Target.Display = Desired.Home);
   Check (T.Check (Registry, Ticket, Window, W.Pointer_Captured) =
          T.Interaction_In_Progress);
   Check (T.Check (Registry, Ticket, Window, W.Modal_Interaction) =
          T.Interaction_In_Progress);
   Window.Intent := 2;
   Check (T.Check (Registry, Ticket, Window, W.Idle) = T.User_Intent_Changed);
   Window := (2, 1);
   Check (T.Check (Registry, Ticket, Window, W.Idle) = T.Window_Replaced);
   Window := (1, 1);
   Ticket := T.Prepare
     (Registry, Window, Desired, Interaction => W.Pointer_Captured);
   Check (T.Check (Registry, Ticket, Window, W.Idle) = T.No_Proposal);
   Ticket := T.Prepare (Registry, Window, Desired);

   -- Conflicts preserve every record and the validity of existing snapshots.
   declare
      Saved : constant R.State := Registry;
   begin
      R.Register (Registry, Item, B, Result);
      Check (Result = R.Identity_Conflict and Registry = Saved);
      Item.Backend.Number := 1; -- duplicate name, different output
      R.Register (Registry, Item, B, Result);
      Check (Result = R.Identity_Conflict and Registry = Saved);
      Item.Area.Display := 8;
      Item.Backend.Number := 0; -- duplicate output, different name
      R.Register (Registry, Item, B, Result);
      Check (Result = R.Identity_Conflict and Registry = Saved);
      R.Update (Registry, A, Item, B, Result);
      Check (Result = R.Identity_Conflict and Registry = Saved);
      Check (R.Current (Registry, View));
   end;
   Item.Backend.Number := 1;
   R.Register (Registry, Item, B, Result);
   Check (Result = R.Applied and R.Live (Registry, A));
   Check (T.Check (Registry, Ticket, Window, W.Idle) = T.Output_State_Changed);
   -- Changes to an unrelated output invalidate topology snapshots, but not
   -- the first output's reference. No session is silently rebound.
   Ticket := T.Prepare (Registry, Window, Desired);
   Old := B;
   Item.Area.X := -1_920;
   R.Update (Registry, B, Item, C, Result);
   Check (Result = R.Applied and not R.Live (Registry, B));
   Check (R.Live (Registry, A) and R.Live (Registry, C));
   B := C;
   Check (T.Check (Registry, Ticket, Window, W.Idle) = T.Output_State_Changed);
   declare
      Saved : constant R.State := Registry;
   begin
      R.Update (Registry, Old, Item, C, Result);
      Check (Result = R.Stale_Reference and Registry = Saved);
      R.Retire (Registry, Old, Result);
      Check (Result = R.Stale_Reference and Registry = Saved);
   end;

   -- Exhaust all independent presence/power/readiness combinations.
   for Presence in R.Connector_Presence loop
      for Power in R.Power_Policy loop
         for Stage in R.Readiness loop
            View := R.Capture (Registry);
            Item.Presence := Presence;
            Item.Power := Power;
            Item.Stage := Stage;
            R.Update (Registry, B, Item, C, Result);
            Check (Result = R.Applied and not R.Live (Registry, B));
            Check (not R.Current (Registry, View));
            B := C;
            View := R.Capture (Registry);
            if Presence = R.Present and Power = R.Enabled and Stage = R.Ready
            then
               Check (R.Areas (View).Count = 2);
               Check (R.Reference_At (View, 2) = B);
            else
               Check (R.Areas (View).Count = 1);
            end if;
            Check (R.Reference_At (View, 1) = A);
         end loop;
      end loop;
   end loop;
   Old := B;
   R.Retire (Registry, B, Result);
   Check (Result = R.Applied and not R.Live (Registry, B));
   Item.Stage := R.Ready;
   Item.Power := R.Enabled;
   Item.Presence := R.Present;
   R.Register (Registry, Item, B, Result); -- same slot/identity, new lifetime
   Check (Result = R.Applied and B /= Old and not R.Live (Registry, Old));

   -- Fill the bounded table, then reuse a retired slot without ABA aliasing.
   for I in R.L.Viewport_Index range 3 .. R.L.Max_Viewports loop
      Item.Backend.Number := R.Output_Number (I);
      Item.Area.Display := R.L.Named_Display_ID (I + 10);
      R.Register (Registry, Item, C, Result);
      Check (Result = R.Applied);
   end loop;
   Item.Backend.Number := 100;
   Item.Area.Display := 100;
   View := R.Capture (Registry);
   R.Register (Registry, Item, C, Result);
   Check (Result = R.Full and R.Current (Registry, View));
   Old := A;
   R.Retire (Registry, A, Result);
   Check (Result = R.Applied);
   R.Register (Registry, Item, A, Result);
   Check (Result = R.Applied and not R.Live (Registry, Old));
   View := R.Capture (Registry);
   Check (R.Areas (View).Count = R.L.Max_Viewports);
   for I in 1 .. R.Areas (View).Count loop
      Check (R.Live (Registry, R.Reference_At (View, I)));
      Check (R.Describe (Registry, R.Reference_At (View, I)).Area =
             R.Areas (View).Items (I));
   end loop;

   -- Counter exhaustion must invalidate, never wrap or preserve a usable view.
   declare
      package Small is new CuBit.Output_Registry (Maximum_Revision => 2);
      use type Small.Mutation_Result;
      use type Small.Output_Reference;
      S : Small.State (1);
      Ref, Next : Small.Output_Reference;
      Status : Small.Mutation_Result;
      Desc : constant Small.Description :=
        (Backend => (1, 0), Area => (7, 0, 0, 1_920, 1_040),
         Presence => Small.Present, Power => Small.Enabled, Stage => Small.Ready);
      Snap : Small.Snapshot;
   begin
      Small.Register (S, Desc, Ref, Status);
      Check (Status = Small.Applied);
      Small.Update (S, Ref, Desc, Next, Status);
      Check (Status = Small.Applied and not Small.Live (S, Ref));
      Ref := Next;
      Snap := Small.Capture (S);
      Small.Retire (S, Ref, Status);
      Check (Status = Small.Exhausted and Small.Is_Closed (S));
      Check (not Small.Live (S, Ref) and not Small.Current (S, Snap));
      Check (Small.Areas (Small.Capture (S)).Count = 0);
      Small.Register (S, Desc, Next, Status);
      Check (Status = Small.Closed and Next = Small.No_Output);
      Small.Update (S, Ref, Desc, Next, Status);
      Check (Status = Small.Closed and Next = Small.No_Output);
      Small.Retire (S, Ref, Status);
      Check (Status = Small.Closed);
   end;
   -- Exhaustion while admitting or updating also closes without wrapping.
   declare
      package Small is new CuBit.Output_Registry (Maximum_Revision => 1);
      use type Small.Mutation_Result;
      use type Small.Output_Reference;
      S : Small.State (1);
      U : Small.State (2);
      Ref, Next : Small.Output_Reference;
      Status : Small.Mutation_Result;
      Desc : Small.Description :=
        (Backend => (1, 0), Area => (7, 0, 0, 1_920, 1_040),
         Presence => Small.Present, Power => Small.Enabled, Stage => Small.Ready);
      Snap : Small.Snapshot;
   begin
      Small.Register (S, Desc, Ref, Status);
      Check (Status = Small.Applied);
      Snap := Small.Capture (S);
      Desc.Backend.Number := 1;
      Desc.Area.Display := 8;
      Small.Register (S, Desc, Next, Status);
      Check (Status = Small.Exhausted and Next = Small.No_Output);
      Check (not Small.Live (S, Ref) and not Small.Current (S, Snap));
      Small.Register (U, Desc, Ref, Status);
      Check (Status = Small.Applied);
      Snap := Small.Capture (U);
      Small.Update (U, Ref, Desc, Next, Status);
      Check (Status = Small.Exhausted and Next = Small.No_Output);
      Check (not Small.Live (U, Ref) and not Small.Current (U, Snap));
   end;
   Ada.Text_IO.Put_Line ("PASS output registry/ticket checks:" & Checks'Image);
end Main;
