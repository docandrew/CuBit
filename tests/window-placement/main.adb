with Ada.Text_IO;
with CuBit.Window_Placement;

procedure Main is
   use CuBit.Window_Placement;
   use type L.Named_Display_ID;
   use type G.Logical_Coordinate;
   use type G.Logical_Rectangle;
   Areas : Ready_Areas;
   Desired : Preference :=
     (Home => 7, Bounds => (X => 100, Y => 50, Width => 640, Height => 480));
   Previous : Display_Choice;
   Result : Plan_Result;
   Episode : Recovery_Episode;
   Cases : Natural := 0;

   procedure Check
     (Reason : Placement_Reason;
      Policy : Fallback_Policy := Allow_Temporary_Fallback;
      Interaction : Interaction_State := Idle)
   is
      Saved : constant Preference := Desired;
   begin
      Result := Plan (Desired, Areas, Previous, Policy, Interaction);
      pragma Assert (Desired = Saved);
      if Result.Reason /= Reason then
         raise Program_Error with "Expected " & Reason'Image &
           " got " & Result.Reason'Image;
      end if;
      if Result.Target.Available then
         declare
            A : constant Work_Area := Areas.Items (Result.Target.Index);
            B : constant G.Logical_Rectangle := Result.Target.Bounds;
         begin
            pragma Assert (Result.Target.Index <= Areas.Count);
            pragma Assert (Result.Target.Display = A.Display);
            pragma Assert (B.Left >= A.X and B.Top >= A.Y);
            pragma Assert (B.Right <= A.X + A.Width);
            pragma Assert (B.Bottom <= A.Y + A.Height);
            pragma Assert (B.Right - B.Left = Desired.Bounds.Width);
            pragma Assert (B.Bottom - B.Top = Desired.Bounds.Height);
         end;
      else
         pragma Assert (Reason not in At_Home | Temporary_Fallback);
      end if;
      Cases := Cases + 1;
   end Check;

   procedure Swap is
      Temp : constant Work_Area := Areas.Items (1);
   begin
      Areas.Items (1) := Areas.Items (2);
      Areas.Items (2) := Temp;
   end Swap;
begin
   Check (Waiting_For_Home, Await_Preferred_Output);
   Check (No_Ready_Output);
   Areas := (Count => 1, Items =>
     [1 => (Display => 3, X => -1_920, Y => -100,
            Width => 1_920, Height => 1_040), others => <>]);
   Begin_Recovery (Episode, Now => 100, Grace => 20);
   for Now in Instant range 100 .. 119 loop
      -- Repeated probes do not postpone the original deadline.
      Begin_Recovery (Episode, Now, Grace => 20);
      pragma Assert (Deadline (Episode) = 120);
      pragma Assert (not Expired (Episode, Now));
      Check (Waiting_For_Home, Await_Preferred_Output);
   end loop;
   pragma Assert (Expired (Episode, 120));
   Check (Temporary_Fallback);
   pragma Assert (Result.Target.Display = 3 and Desired.Home = 7);
   Previous := (True, 3);
   -- Late preferred output: capture and modal interaction defer movement.
   Areas.Count := 2;
   Areas.Items (2) := (Display => 7, X => 0, Y => 0,
                      Width => 1_920, Height => 1_040);
   Check (Deferred_Interaction, Interaction => Pointer_Captured);
   Check (Deferred_Interaction, Interaction => Modal_Interaction);
   Check (At_Home);
   pragma Assert (Result.Target.Display = 7);
   Complete_Recovery (Episode);
   pragma Assert (not Active (Episode) and not Expired (Episode, 999));
   -- Explicit Bring Here changes intent: reconnect cannot undo that move.
   Desired.Home := 3;
   Desired.Bounds.X := 77;
   Check (At_Home);
   pragma Assert (Result.Target.Display = 3);
   pragma Assert (Result.Target.Bounds.Left = -1_843);
   Desired.Home := 7;
   Desired.Bounds.X := 100;
   -- Same preference, new portrait/high-DPI work area: clamp, don't resize.
   Areas.Items (2) := (Display => 7, X => 0, Y => -1_920,
                      Width => 720, Height => 1_280);
   Check (At_Home);
   pragma Assert (Result.Target.Bounds.Left = 80);
   pragma Assert (Result.Target.Bounds.Top = -1_870);
   -- Home is ready but too small; waiting longer won't repair its geometry.
   Areas.Items (2).Width := 600;
   Check (Temporary_Fallback, Await_Preferred_Output);
   Areas.Items (1).Height := 100;
   Check (No_Suitable_Work_Area);
   Areas.Items (1).Height := 1_040;
   -- Enumeration order cannot choose the fallback. Prefer last usable one,
   -- otherwise the smallest stable named ID among suitable work areas.
   Areas.Items (2) := (Display => 2, X => 0, Y => 0,
                      Width => 1_920, Height => 1_040);
   Previous := (Known => False);
   Check (Temporary_Fallback);
   pragma Assert (Result.Target.Display = 2);
   Swap;
   Check (Temporary_Fallback);
   pragma Assert (Result.Target.Display = 2);
   Previous := (True, 3);
   Check (Temporary_Fallback);
   pragma Assert (Result.Target.Display = 3);
   Swap;
   Check (Temporary_Fallback);
   pragma Assert (Result.Target.Display = 3);
   Areas.Items (1).Height := 100;
   Check (Temporary_Fallback);
   pragma Assert (Result.Target.Display = 2);
   Areas.Items (1).Display := 2;
   Check (Ambiguous_Display);
   -- Duplicate unrelated IDs are rejected too, even with a usable home.
   Areas.Count := 3;
   Areas.Items (3) := (Display => 7, X => 1_920, Y => 0,
                      Width => 1_920, Height => 1_040);
   Check (Ambiguous_Display);

   -- Exhaustive small geometry oracle: clamp local coordinates independently.
   Areas.Count := 1;
   Areas.Items (1).Display := 7;
   Previous := (Known => False);
   for Width in Extent range 1 .. 6 loop
      for Height in Extent range 1 .. 6 loop
         Areas.Items (1).Width := Width;
         Areas.Items (1).Height := Height;
         for W in Extent range 1 .. 7 loop
            for H in Extent range 1 .. 7 loop
               Desired.Bounds.Width := W;
               Desired.Bounds.Height := H;
               for X in Position range -2 .. 8 loop
                  for Y in Position range -2 .. 8 loop
                     Desired.Bounds.X := X;
                     Desired.Bounds.Y := Y;
                     Areas.Items (1).X := -20;
                     Areas.Items (1).Y := 30;
                     if W > Width or H > Height then
                        Check (No_Suitable_Work_Area);
                     else
                        Check (At_Home);
                        declare
                           Expected_X : G.Logical_Coordinate := X;
                           Expected_Y : G.Logical_Coordinate := Y;
                        begin
                           if Expected_X < 0 then Expected_X := 0; end if;
                           if Expected_X + W > Width then
                              Expected_X := Width - W;
                           end if;
                           if Expected_Y < 0 then Expected_Y := 0; end if;
                           if Expected_Y + H > Height then
                              Expected_Y := Height - H;
                           end if;
                           pragma Assert
                             (Result.Target.Bounds.Left = -20 + Expected_X);
                           pragma Assert
                             (Result.Target.Bounds.Top = 30 + Expected_Y);
                        end;
                     end if;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;

   -- Numeric extremes are representable and retain full decorated extents.
   Areas.Items (1) := (7, Position'Last, Position'First,
                      Extent'Last, Extent'Last);
   Desired.Bounds := (Position'Last, Position'First,
                      Extent'Last, Extent'Last);
   Check (At_Home);
   pragma Assert (Result.Target.Bounds =
     (Position'Last, Position'First, Position'Last + Extent'Last,
      Position'First + Extent'Last));

   for Start in Instant range 0 .. 20 loop
      for Grace in Instant range 0 .. 20 loop
         Complete_Recovery (Episode);
         Begin_Recovery (Episode, Start, Grace);
         pragma Assert (Deadline (Episode) = Start + Grace);
         for Now in Instant range Start .. 50 loop
            Begin_Recovery (Episode, Now, Grace);
            pragma Assert (Deadline (Episode) = Start + Grace);
            pragma Assert (Expired (Episode, Now) = (Now >= Start + Grace));
            Cases := Cases + 1;
         end loop;
      end loop;
   end loop;
   Complete_Recovery (Episode);
   Begin_Recovery (Episode, Instant'Last - 2, Grace => 10);
   pragma Assert (Deadline (Episode) = Instant'Last);
   pragma Assert (not Expired (Episode, Instant'Last - 1));
   pragma Assert (Expired (Episode, Instant'Last));
   Ada.Text_IO.Put_Line ("PASS window placement/recovery cases:" & Cases'Image);
end Main;
