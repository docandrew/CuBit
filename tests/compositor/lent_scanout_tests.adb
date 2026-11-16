--  UI-020: the compositor composes straight into three lent scanout buffers.
--  A model of one output, run with the units Desktop and display.svc use
--  (Compositor_Pool, Compositor_Repaint, Compositor_Damage, Backend_Targets):
--  the compositor writes the slot it holds, repairs what that slot missed
--  (buffer age), and names the slot as the frame's target; the backend
--  uploads only the frame's area of it to its own copy (virtio-gpu's host
--  resource) and scans that copy out. After every latch the screen must
--  equal a full repaint of the scene the frame showed. With the repair left
--  out of the frame's area the screen goes stale: the rule is checked both
--  ways, so the test fails if the area rule is dropped.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Damage;
with Compositor_Pool;
with Compositor_Repaint;
with CuBit.Backend_Targets;
procedure Lent_Scanout_Tests is
   package BP renames Compositor_Pool;
   package RP renames Compositor_Repaint;
   package D renames Compositor_Damage;
   package BT renames CuBit.Backend_Targets;
   use type BP.Ticket, BT.Phase;

   Width : constant := 64;
   Height : constant := 48;
   type Image is array (0 .. Height - 1, 0 .. Width - 1) of Unsigned_32;
   type Slot_Images is array (BP.Live_Slot) of Image;
   type Host_Images is array (BT.Buffer) of Image;

   Seed : Unsigned_32 := 16#2545_F491#;
   function Next (Limit : Positive) return Natural is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 17);
      Seed := Seed xor Shift_Left (Seed, 5);
      return Natural (Seed mod Unsigned_32 (Limit));
   end Next;

   procedure Fill (Into : in out Image; Area : D.Box; From : Image) is
   begin
      for Y in Area.Top .. Area.Bottom - 1 loop
         for X in Area.Left .. Area.Right - 1 loop
            Into (Y, X) := From (Y, X);
         end loop;
      end loop;
   end Fill;

   --  Frames run; latches whose screen differed from the scene shown.
   procedure Run (Frames : Positive; Repair_In_Area : Boolean;
                  Stale_Latches : out Natural; Latches : out Natural) is
      Scene, Shown : Image := (others => (others => 0));
      Slots : Slot_Images := [others => (others => (others => 16#DEAD#))];
      Host : Host_Images := [others => (others => (others => 0))];
      Pool : BP.State := BP.Open (1);
      Repaint : RP.State := RP.Open ((0, 0, Width, Height));
      Targets : BT.State;
      Writer, Held : BP.Ticket;
      Pending_Token : BT.Identifier := 0;
      Accepted : Boolean;
      Full_Next : Boolean := False;
   begin
      Stale_Latches := 0;
      Latches := 0;
      BT.Set_Count (Targets, 3);
      BT.Cleared (Targets);
      --  The backend shows buffer Active now; its slot starts as the front.
      BP.Seed_Front (Pool, BP.Live_Slot (BT.Active (Targets) + 1));
      BP.Acquire (Pool, Writer);
      pragma Assert (Writer /= BP.None and Writer.Buffer /= BT.Active (Targets) + 1);
      for Frame in 1 .. Frames loop
         --  The scene changes: one rectangle (or all, after a drop).
         declare
            Left : constant Natural := Next (Width - 1);
            Top : constant Natural := Next (Height - 1);
            Change : constant D.Box :=
              (if Full_Next or else Frame = 1 then (0, 0, Width, Height)
               else (Left, Top, Left + 1 + Next (Width - Left), Top + 1 + Next (Height - Top)));
            Repair, Frame_Area : D.State;
         begin
            Full_Next := False;
            for Y in Change.Top .. Change.Bottom - 1 loop
               for X in Change.Left .. Change.Right - 1 loop
                  Scene (Y, X) := Unsigned_32 (Frame);
               end loop;
            end loop;
            --  Desktop invalidates every slot where it draws, then takes
            --  what its writer misses and renders that into it.
            RP.Invalidate (Repaint, Change);
            RP.Take (Repaint, Writer.Buffer, Repair);
            D.Clear (Frame_Area);
            D.Add (Frame_Area, Change);
            for I in 1 .. D.Count (Repair) loop
               Fill (Slots (Writer.Buffer), D.Item (Repair, I), Scene);
               if Repair_In_Area then D.Add (Frame_Area, D.Item (Repair, I)); end if;
            end loop;
            BP.Start_Render (Pool, Writer);
            BP.Finish_Render (Pool, Writer, BP.Completed);
            --  The previous frame's completion arrives (the latch).
            if Pending_Token /= 0 then
               BT.Complete (Targets, Pending_Token, True);
               pragma Assert (BT.Current (Targets) = BT.Idle);
               BP.Latch_Display (Pool, BP.Displayed (Pool), BP.Front (Pool), True);
               pragma Assert (not BP.Faulted (Pool) and
                 BP.Front (Pool).Buffer = BT.Active (Targets) + 1);
               Pending_Token := 0;
               Latches := Latches + 1;
               if Host (BT.Active (Targets)) /= Shown then
                  Stale_Latches := Stale_Latches + 1;
               end if;
            end if;
            BP.Present (Pool, Held);
            pragma Assert (Held = Writer);
            --  display.svc: the frame's own slot is the target, never the
            --  buffer on screen; the backend uploads the frame's area.
            BT.Prepare_Target (Targets, Held.Buffer - 1, Accepted);
            pragma Assert (Accepted and Held.Buffer - 1 /= BT.Active (Targets));
            BT.Seal (Targets, BT.Identifier (Frame));
            Fill (Host (Held.Buffer - 1), D.Bounds (Frame_Area), Slots (Held.Buffer));
            Shown := Scene;
            if Next (11) = 0 then
               --  A certain refusal: nothing shown, the slot returns, the
               --  compositor redraws everything.
               BT.Abandon (Targets, BT.Identifier (Frame));
               BP.Retire_Display (Pool, Held, True);
               Full_Next := True;
               Shown := Host (BT.Active (Targets));
            else
               Pending_Token := BT.Identifier (Frame);
            end if;
            pragma Assert (not BP.Faulted (Pool));
            --  The next writer, held while the frame is pending: the third
            --  slot, never the front or the pending one.
            BP.Acquire (Pool, Writer);
            pragma Assert (Writer /= BP.None and Writer.Buffer /= BP.Front (Pool).Buffer and
                           Writer.Buffer /= BP.Displayed (Pool).Buffer);
         end;
      end loop;
   end Run;

   Stale, Latches : Natural;
begin
   Run (5000, True, Stale, Latches);
   pragma Assert (Stale = 0 and Latches > 4000);
   Ada.Text_IO.Put_Line ("lent-scanout: PASS" & Latches'Image &
     " latches over 3 lent targets, screen = full repaint after each (buffer-age repair, drops)");
   --  Mutation: the area without the repair leaves stale pixels on screen.
   Run (5000, False, Stale, Latches);
   pragma Assert (Stale > 0);
   Ada.Text_IO.Put_Line ("lent-scanout: PASS mutation (repair outside the frame area) caught:" &
     Stale'Image & " stale latches");
end Lent_Scanout_Tests;
