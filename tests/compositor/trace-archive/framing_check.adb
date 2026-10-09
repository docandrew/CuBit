with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Trace_Framing;
procedure Framing_Check is
   package F renames Compositor_Trace_Framing;
   package A renames F.A;
   use type F.Phase;
   M : constant F.Metadata := (Unsigned_64'Last, 77, 123, 4096);
   V : A.S.Capture := (True, 77, Unsigned_64'Last, A.S.P.Publisher_Tag (1),
      1, 1, 0, 0, (A.W.Input_Event, 1, (1, 1, 1, 123)));
   S, Prefix, Trial : F.State;
   H, C, Final, Bad : A.Chunk;
   Accepted : Boolean;
   Statistics : A.S.Statistics := (others => 0);
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Reseal (Data : in out A.Chunk) is
   begin
      Data (31) := A.Checksum (A.Prefix (Data));
   end Reseal;
begin
   H := F.Header (M); F.Start (S, H);
   Check (F.Status (S) = F.Reading and F.Events (S) = 0);
   for I in 0 .. 31 loop
      for Bit in 0 .. 63 loop
         Bad := H; Bad (I) := Bad (I) xor Shift_Left (1, Bit);
         F.Start (Trial, Bad); Check (F.Status (Trial) = F.Rejected);
      end loop;
   end loop;
   for Tail in 0 .. 255 loop
      Trial := S; F.End_Of_File (Trial, Tail);
      Check (F.Status (Trial) = F.Incomplete and F.Events (Trial) = 0);
   end loop;
   for I in 1 .. M.Budget loop
      Prefix := S; V.Value.Event_ID := Unsigned_64 (I);
      C := F.Event_Chunk (S, V); F.Feed (S, C, Accepted);
      Check (Accepted and F.Events (S) = I and F.Status (S) = F.Reading);
      Trial := S; F.Feed (Trial, C, Accepted);
      Check (not Accepted and F.Status (Trial) = F.Rejected and F.Events (Trial) = I);
      Bad := C; Bad (3) := 78; Reseal (Bad); Trial := Prefix;
      F.Feed (Trial, Bad, Accepted);
      Check (not Accepted and F.Status (Trial) = F.Rejected);
   end loop;
   Check (not F.Can_Append (S, V));
   Statistics := (Unsigned_64'Last, 7, 2, 4096, 3);
   Final := F.Footer (S, 456, F.Budget_Reached, Statistics);
   for Tail in 0 .. 255 loop
      Trial := S; F.Feed (Trial, Final, Accepted);
      Check (not Accepted and F.Status (Trial) = F.Footer_Seen);
      F.End_Of_File (Trial, Tail);
      Check (F.Status (Trial) = (if Tail = 0 then F.Complete else F.Rejected));
   end loop;
   for I in 0 .. 31 loop
      for Bit in 0 .. 63 loop
         Bad := Final; Bad (I) := Bad (I) xor Shift_Left (1, Bit);
         Trial := S; F.Feed (Trial, Bad, Accepted);
         Check (not Accepted and F.Status (Trial) = F.Rejected);
      end loop;
   end loop;
   for I in 2 .. 7 loop
      Bad := Final; Bad (I) := 0; Reseal (Bad);
      -- Requested_Stop (zero) is a valid alternative termination reason.
      if I /= 6 then
         Trial := S; F.Feed (Trial, Bad, Accepted);
         Check (F.Status (Trial) = F.Rejected);
      end if;
   end loop;
   Trial := S; F.Feed (Trial, Final, Accepted); F.Feed (Trial, Final, Accepted);
   Check (F.Status (Trial) = F.Rejected);
   Trial := S; F.Feed (Trial, Final, Accepted); F.Feed (Trial, C, Accepted);
   Check (F.Status (Trial) = F.Rejected);
   Trial := Prefix; F.Feed (Trial, Final, Accepted);
   Check (F.Status (Trial) = F.Rejected);
   -- Different valid event content at the same last sequence must not match
   -- the original footer even after the individual event is checksummed.
   Bad := C; Bad (18) := Bad (18) xor 1; Reseal (Bad);
   -- Input packet reserves reject this alteration before chaining.
   Trial := Prefix; F.Feed (Trial, Bad, Accepted);
   Check (F.Status (Trial) = F.Rejected);
   V.Value.Event_ID := 9999; Trial := Prefix;
   Bad := F.Event_Chunk (Trial, V); F.Feed (Trial, Bad, Accepted); Check (Accepted);
   F.Feed (Trial, Final, Accepted); Check (F.Status (Trial) = F.Rejected);
   Ada.Text_IO.Put_Line ("PASS archive framing checks=" & Checks'Image);
end Framing_Check;
