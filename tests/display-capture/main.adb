------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Hosted tests for remote screenshots (REMOTE-001): Desktop's capture
--  policy (Desktop_Capture_Policy) and the display-capture wire
--  (CuBit.Display_Capture).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CuBit.Display_Capture;
with Desktop_Capture_Policy;

procedure Main is
   package P renames Desktop_Capture_Policy;
   package DC renames CuBit.Display_Capture;
   use type P.Decision, P.Session, DC.Area;
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if Condition then
         Put_Line ("PASS: " & What);
      else
         Put_Line ("FAIL: " & What);
         Failures := Failures + 1;
      end if;
   end Check;

   S : P.State := P.Initial;
   D : P.Decision;

   procedure Announce (Holder : P.Session; New_Pairing : Boolean; Now : Unsigned_64 := 0) is
   begin
      P.Step (S, (Kind => P.Announce, Now => Now, Holder => Holder, New_Pairing => New_Pairing), D);
   end Announce;
   procedure Switch (On : Boolean) is
   begin
      P.Step (S, (Kind => P.Set_Allowed, Now => 0, Allowed => On), D);
   end Switch;
   function Ask (Now : Unsigned_64) return P.Decision is
      Result : P.Decision;
   begin
      P.Step (S, (Kind => P.Request, Now => Now), Result);
      return Result;
   end Ask;
   procedure Tick (Now : Unsigned_64) is
   begin
      P.Step (S, (Kind => P.Tick, Now => Now), D);
   end Tick;
begin
   ---------------------------------------------------------------------------
   --  The switch
   ---------------------------------------------------------------------------
   Check (not S.Allowed, "screen capture is off at start");
   Check (Ask (10_000) = P.Refuse_No_Session, "a request before any session is refused (no session)");
   Switch (True);
   Check (not S.Allowed, "the switch cannot be turned on without a session");
   Announce (P.Listening, False);
   Check (Ask (10_000) = P.Refuse_Off, "a listening session's request while off is refused (off)");
   Check (not S.Captured and then not S.Marked, "a refused request changes nothing");
   Announce (P.Paired, True);
   Check (Ask (10_000) = P.Refuse_Off, "a paired session's request while off is refused (off)");
   Switch (True);
   Check (S.Allowed, "the person turns screen capture on for the paired session");
   Check (Ask (10_000) = P.Capture, "with the switch on, a request captures");
   Check (S.Marked and then S.Mark_End = 10_000 + P.Mark_Ms, "a capture shows the Deskbar mark");

   ---------------------------------------------------------------------------
   --  The rate limit
   ---------------------------------------------------------------------------
   Check (Ask (10_000 + P.Minimum_Interval_Ms - 1) = P.Refuse_Rate,
          "a second request sooner than the interval is refused (rate)");
   Check (S.Last_Ms = 10_000, "a rate-limited request does not move the last capture");
   Check (Ask (10_000 + P.Minimum_Interval_Ms) = P.Capture, "a request one interval later captures");
   Check (Ask (5) = P.Refuse_Rate, "a clock that went backwards is refused, not wrapped");

   ---------------------------------------------------------------------------
   --  The mark
   ---------------------------------------------------------------------------
   Tick (10_000 + P.Minimum_Interval_Ms + P.Mark_Ms - 1);
   Check (S.Marked, "the mark stays until its time");
   Tick (10_000 + P.Minimum_Interval_Ms + P.Mark_Ms);
   Check (not S.Marked, "the mark ends at its time");
   Tick (Unsigned_64'Last);
   Check (not S.Marked, "time alone never shows the mark");

   ---------------------------------------------------------------------------
   --  Off again
   ---------------------------------------------------------------------------
   Announce (P.Paired, False);
   Check (S.Allowed, "the same session announcing again keeps the switch");
   Announce (P.Paired, True);
   Check (not S.Allowed, "a new pairing turns screen capture off");
   Switch (True);
   Announce (P.No_Session, False);
   Check (not S.Allowed and then S.Holder = P.No_Session, "the session ending turns it off");
   Announce (P.Listening, False);
   Switch (True);
   Switch (False);
   Check (not S.Allowed, "the person turns it off");
   Check (Ask (Unsigned_64'Last) = P.Refuse_Off, "and the next request is refused (off)");
   S := (P.Initial with delta Holder => P.Paired, Allowed => True);
   Check (Ask (Unsigned_64'Last - 1) = P.Capture and then S.Mark_End = Unsigned_64'Last,
          "a capture near the clock's end saturates the mark's end");

   ---------------------------------------------------------------------------
   --  The wire
   ---------------------------------------------------------------------------
   declare
      A : constant DC.Area := (X => 3, Y => 65_535, Width => 640, Height => 1);
      R : DC.Resolution;
   begin
      Check (DC.Decode_Area (DC.Encode_Area (A)) = A, "an area survives its encoding");
      Check (DC.Decode_Area (0) = DC.Whole_Output, "a zero area is the whole output");
      R := DC.Resolve (DC.Whole_Output, 1024, 768);
      Check (R.Fits and then R.Chosen = (0, 0, 1024, 768), "the whole output resolves to its size");
      R := DC.Resolve ((10, 20, 100, 50), 1024, 768);
      Check (R.Fits and then R.Chosen = (10, 20, 100, 50), "an inside area resolves to itself");
      Check (not DC.Resolve ((1000, 0, 25, 1), 1024, 768).Fits, "an area past the right edge is refused");
      Check (not DC.Resolve ((0, 768, 1, 1), 1024, 768).Fits, "an area below the output is refused");
      Check (not DC.Resolve ((5, 5, 0, 7), 1024, 768).Fits, "an empty area is refused");
      Check (DC.Bytes_Of ((0, 0, 3840, 2160)) = 3840 * 2160 * 4, "a 4K frame's bytes");
      Check (DC.Width_Of (DC.Encode_Size (3840, 2160)) = 3840 and then
             DC.Height_Of (DC.Encode_Size (3840, 2160)) = 2160, "the size word");
   end;
   for St in DC.Status loop
      declare
         Back : constant DC.Status_Decoding := DC.Decode_Status (DC.Status'Enum_Rep (St));
         use type DC.Status;
      begin
         Check (Back.Valid and then Back.Value = St, "status " & DC.Status'Image (St) & " decodes");
      end;
   end loop;
   Check (not DC.Decode_Status (16#F001#).Valid, "an unknown status is not decoded");

   if Failures > 0 then
      Put_Line ("display-capture:" & Failures'Image & " FAILED");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("display-capture: all PASS");
   end if;
end Main;
