--  Hosted regression for the absolute-pointer codec, mapping, retention and
--  the virtio-input frame assembler. Linux-hosted: no device, no IPC.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Input_Pending;
with Absolute_Pointer; use Absolute_Pointer;
with Absolute_Pending;
with Virtio_Input_Events;

procedure Absolute_Tests is
   Checks, Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   --  Deterministic xorshift for sampled sweeps.
   Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Next return Unsigned_64 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Seed;
   end Next;

   procedure Codec is
      R : Report;
      W : Word;
   begin
      for I in 1 .. 200_000 loop
         R := (Buttons => Button_State (Next mod 256),
               X => Axis_Position (Next mod (Axis_Last + 1)),
               Y => Axis_Position (Next mod (Axis_Last + 1)),
               Wheel => Integer (Next mod 256) - 128,
               Flags => Device_Flags (Next mod 256));
         W := Encode (R);
         Check (Valid_Payload (W) and then Decode (W) = R, "round trip");
      end loop;
      Check (Encode ((others => <>)) = Shift_Left (Word (Wheel_Bias), Wheel_Shift),
             "zero report: biased wheel field only");
      Check (Decode (Encode ((Buttons => 1, X => Axis_Last, Y => 0,
                              Wheel => -1, Flags => 0))).Wheel = -1, "negative wheel");
      --  Axis fields above Axis_Last, and reserved bits, are malformed.
      Check (not Valid_Payload (Shift_Left (Word (Axis_Last + 1), X_Shift)), "X range");
      Check (not Valid_Payload (Shift_Left (Word (Axis_Last + 1), Y_Shift)), "Y range");
      Check (not Valid_Payload (Shift_Left (1, Reserved_Shift)), "reserved bit");
      for Bit in 0 .. 63 loop
         --  Single-bit words: exactly the documented fields are valid.
         W := Shift_Left (1, Bit);
         Check (Valid_Payload (W) =
                  (Bit < Reserved_Shift and then Bit /= X_Shift + Axis_Bits and then
                   Bit /= Y_Shift + Axis_Bits),
                "single bit" & Bit'Image);
         if Valid_Payload (W) then
            Check (Encode (Decode (W)) = W, "decode/encode bit" & Bit'Image);
         end if;
      end loop;
   end Codec;

   procedure Mapping is
      Exact : Boolean;
   begin
      --  Exhaustive exactness for every extent up to 4096 pixels.
      for E in 1 .. 4096 loop
         Exact := True;
         for P in 0 .. E - 1 loop
            if To_Pixel (Host_Position (P, E), E) /= P then
               Exact := False;
            end if;
         end loop;
         Check (Exact, "exact inverse extent" & E'Image);
      end loop;
      --  Sampled larger extents, up to the axis.
      for I in 1 .. 2_000 loop
         declare
            E : constant Pixel_Extent := 4096 + Natural (Next mod Unsigned_64 (Axis_Last - 4095));
            P : constant Natural := Natural (Next mod Unsigned_64 (E));
         begin
            Check (To_Pixel (Host_Position (P, E), E) = P, "sampled exact" & E'Image);
         end;
      end loop;
      --  Edges and monotonicity, including extents beyond the axis.
      for E in Pixel_Extent'(1) .. 70 loop
         Check (To_Pixel (0, E) = 0 and then To_Pixel (Axis_Last, E) = E - 1, "edges");
      end loop;
      Check (To_Pixel (Axis_Last, Maximum_Extent) = Maximum_Extent - 1, "max edge");
      for I in 1 .. 50_000 loop
         declare
            E : constant Pixel_Extent := 1 + Natural (Next mod Maximum_Extent);
            A : constant Axis_Position := Axis_Position (Next mod (Axis_Last + 1));
            B : constant Axis_Position := Axis_Position (Next mod (Axis_Last + 1));
         begin
            Check ((if A <= B then To_Pixel (A, E) <= To_Pixel (B, E)
                    else To_Pixel (B, E) <= To_Pixel (A, E)), "monotone");
         end;
      end loop;
      --  Device ranges: identity for QEMU's range, clamping, edges.
      Check (Normalize (0, (0, Axis_Last)) = 0 and then
             Normalize (Axis_Last, (0, Axis_Last)) = Axis_Last and then
             Normalize (12_345, (0, Axis_Last)) = 12_345, "QEMU identity");
      Check (Normalize (-5, (0, 100)) = 0 and then Normalize (900, (0, 100)) = Axis_Last,
             "clamped");
      Check (Normalize (50, (0, 100)) = Axis_Position (Axis_Last / 2), "midpoint");
      Check (Normalize (-1000, (-1000, 1000)) = 0 and then
             Normalize (1000, (-1000, 1000)) = Axis_Last, "signed range");
   end Mapping;

   procedure Retention is
      package AP renames Absolute_Pending;
      use type AP.Append_Outcome;
      S : AP.State;
      O : AP.Append_Outcome;
      function Count (S : AP.State) return Natural renames AP.Count;
      function Element (S : AP.State; Offset : Input_Pending.Offset_Type)
        return Input_Pending.Item renames AP.Element;
      procedure Append (S : in out AP.State; R : Report; Observed_Ms : Word;
                        Outcome : out AP.Append_Outcome) renames AP.Append;
      function At_Pos (X, Y : Axis_Position; Buttons : Button_State := 0;
                       Wheel : Wheel_Steps := 0) return Report is
        ((Buttons => Buttons, X => X, Y => Y, Wheel => Wheel, Flags => 0));
   begin
      --  A stalled consumer: motion collapses into the newest position.
      Append (S, At_Pos (10, 10), 1, O);
      Check (O = AP.Appended, "first appended");
      Append (S, At_Pos (11, 10), 2, O);
      Check (O = AP.Appended, "second appended (no transition basis yet)");
      for I in 1 .. 1_000 loop
         Append (S, At_Pos (Axis_Position (I), 20), 3, O);
         Check (O = AP.Replaced, "motion replaced");
      end loop;
      Check (Count (S) = 2 and then Decode (Element (S, 1).Payload) = At_Pos (1_000, 20),
             "latest position wins");
      --  A click: press and release are transitions, never replaced.
      Append (S, At_Pos (500, 600, 1), 4, O);
      Check (O = AP.Appended, "press kept");
      Append (S, At_Pos (500, 600, 0), 5, O);
      Check (O = AP.Appended, "release kept");
      Check (Count (S) = 4 and then
             Decode (Element (S, 2).Payload) = At_Pos (500, 600, 1) and then
             Decode (Element (S, 3).Payload) = At_Pos (500, 600, 0),
             "click at its exact position");
      --  Motion after the release replaces the release? No: the release is a
      --  transition, so the move is appended after it.
      Append (S, At_Pos (700, 700), 6, O);
      Check (O = AP.Appended and then Count (S) = 5, "move after release appended");
      Append (S, At_Pos (701, 700), 7, O);
      Check (O = AP.Replaced and then Count (S) = 5, "then motion replaces");
      --  Wheel never moves: a wheel report absorbs only same-position reports.
      Append (S, At_Pos (701, 700, 0, 2), 8, O);
      Check (O = AP.Replaced, "wheel at same position merges");
      Append (S, At_Pos (701, 700, 0, 1), 9, O);
      Check (O = AP.Replaced and then AP.Last (S).Wheel = 3, "wheel steps summed");
      Append (S, At_Pos (900, 700), 10, O);
      Check (O = AP.Appended and then Count (S) = 6, "move after wheel appended");
      Check (Decode (Element (S, 4).Payload) = At_Pos (701, 700, 0, 3),
             "wheel stays at its position");
      --  Publication acknowledges in order.
      for I in 1 .. 6 loop
         AP.Acknowledge (S);
      end loop;
      Check (Count (S) = 0, "drained");
      --  Overflow: Capacity unpublished transitions, then explicit recovery.
      for I in 1 .. Input_Pending.Capacity loop
         Append (S, At_Pos (1, 1, Button_State (I mod 2)), 11, O);
      end loop;
      Check (Count (S) = Input_Pending.Capacity, "full of transitions");
      Append (S, At_Pos (1, 1, 1), 12, O);
      Check (O = AP.Overflowed and then Count (S) = 1 and then Element (S, 0).Recover,
             "overflow explicit");
      --  Upstream loss: retained reports stay, the next report is flagged
      --  and not merged into an older one.
      Append (S, At_Pos (2, 2, 1), 13, O);
      AP.Mark_Lost (S);
      Check (Count (S) = 2 and then not Element (S, 1).Recover, "mark keeps backlog");
      Append (S, At_Pos (3, 3, 1), 14, O);
      Check (O = AP.Appended and then Count (S) = 3 and then Element (S, 2).Recover,
             "next report flagged");
      AP.Reset (S);
      Check (Count (S) = 0 and then not AP.Tail_Mergeable (S), "reset");
   end Retention;

   procedure Frames is
      use Virtio_Input_Events;
      A : Assembler := Start ((0, Axis_Last), (0, Axis_Last));
      R : Report;
      O : Outcome;
      procedure Feed (Kind, Code : Unsigned_16; Value : Unsigned_32) is
      begin
         Virtio_Input_Events.Feed (A, (Kind, Code, Value), O, R);
      end Feed;
   begin
      --  QEMU virtio-tablet motion: ABS_X, ABS_Y, SYN_REPORT.
      Feed (EV_ABS, ABS_X, 1_000);
      Check (O = Absorbed, "abs absorbed");
      Feed (EV_ABS, ABS_Y, 2_000);
      Feed (EV_SYN, SYN_REPORT, 0);
      Check (O = Frame_Report and then R.X = 1_000 and then R.Y = 2_000 and then
             R.Buttons = 0, "motion frame");
      --  A button-only frame keeps the position.
      Feed (EV_KEY, BTN_LEFT, 1);
      Feed (EV_SYN, SYN_REPORT, 0);
      Check (O = Frame_Report and then R.Buttons = 1 and then R.X = 1_000, "press frame");
      Feed (EV_KEY, BTN_RIGHT, 1);
      Feed (EV_KEY, BTN_MIDDLE, 1);
      Feed (EV_SYN, SYN_REPORT, 0);
      Check (O = Frame_Report and then R.Buttons = 7, "right and middle bits");
      Feed (EV_KEY, BTN_LEFT, 0);
      Feed (EV_KEY, BTN_RIGHT, 0);
      Feed (EV_KEY, BTN_MIDDLE, 0);
      Feed (EV_SYN, SYN_REPORT, 0);
      Check (O = Frame_Report and then R.Buttons = 0, "release frame");
      --  Empty and repeated frames are not reports.
      Feed (EV_SYN, SYN_REPORT, 0);
      Check (O = Frame_Unchanged, "empty frame");
      Feed (EV_ABS, ABS_X, 1_000);
      Feed (EV_SYN, SYN_REPORT, 0);
      Check (O = Frame_Unchanged, "same position frame");
      --  Wheel belongs to its frame.
      Feed (EV_REL, REL_WHEEL, 1);
      Feed (EV_SYN, SYN_REPORT, 0);
      Check (O = Frame_Report and then R.Wheel = 1, "wheel up");
      Feed (EV_REL, REL_WHEEL, Unsigned_32'Last);
      Feed (EV_SYN, SYN_REPORT, 0);
      Check (O = Frame_Report and then R.Wheel = -1, "wheel down");
      Feed (EV_SYN, SYN_REPORT, 0);
      Check (O = Frame_Unchanged, "wheel does not persist");
      --  Out-of-range device values clamp to the edges.
      Feed (EV_ABS, ABS_X, Unsigned_32'Last);
      Feed (EV_ABS, ABS_Y, 16#7FFF_FFFF#);
      Feed (EV_SYN, SYN_REPORT, 0);
      Check (O = Frame_Report and then R.X = 0 and then R.Y = Axis_Last, "clamped axes");
      --  SYN_DROPPED discards the partial frame.
      Feed (EV_ABS, ABS_X, 5);
      Feed (EV_SYN, SYN_DROPPED, 0);
      Check (O = Frame_Dropped, "dropped");
      Feed (EV_ABS, ABS_X, 7);
      Check (O = Ignored, "discarding");
      Feed (EV_SYN, SYN_REPORT, 0);
      Check (O = Frame_Unchanged and then Current (A).X = 0, "dropped frame discarded");
      Feed (EV_ABS, ABS_X, 9);
      Feed (EV_SYN, SYN_REPORT, 0);
      Check (O = Frame_Report and then R.X = 9, "next frame after drop");
      --  Unused events.
      Feed (EV_KEY, 16#14A#, 1);
      Check (O = Ignored, "touch key ignored");
      Feed (EV_ABS, 16#35#, 1);
      Check (O = Ignored, "multitouch ignored");
      --  A scaled device range.
      A := Start ((0, 1023), (100, 299));
      Feed (EV_ABS, ABS_X, 1023);
      Feed (EV_ABS, ABS_Y, 100);
      Feed (EV_SYN, SYN_REPORT, 0);
      Check (O = Frame_Report and then R.X = Axis_Last and then R.Y = 0, "scaled range");
   end Frames;
begin
   Codec;
   Mapping;
   Retention;
   Frames;
   Put_Line ("absolute pointer:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures = 0 then
      Put_Line ("ABSOLUTE-POINTER: PASS");
   else
      Put_Line ("ABSOLUTE-POINTER: FAIL");
   end if;
end Absolute_Tests;
