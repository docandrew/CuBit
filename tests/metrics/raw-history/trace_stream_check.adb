with Ada.Text_IO;
with Compositor_Trace_Stream; use Compositor_Trace_Stream;
procedure Trace_Stream_Check is
   use type W.Word, W.Event;
   S : State;
   Got : Capture;
   Checks : Natural := 0;
   procedure Check (V : Boolean) is
   begin
      Checks := Checks + 1;
      if not V then raise Program_Error with Checks'Image; end if;
   end Check;
   function Event (N : Positive) return W.Event is
     (W.Render_Event, W.Word'Last - W.Word (N),
      (W.RT.Draw, 1, 3, W.Word'Last, W.Word (N), W.Word'Last - 1,
       W.Word'Last - 2, W.Word (N), 0, 0, W.Word (N)));
   function Row (Sequence : Positive) return P.Raw_Row is
      N : constant Positive := (Sequence - 1) / 4 + 1;
      I : constant R.Trace_Part := (Sequence - 1) mod 4;
      Parts : constant R.Trace_Group := M.Fragment (Event (N));
      Data : constant R.Slot_Words := R.Encode (Parts (I));
      Result : P.Raw_Row := (others => 0);
   begin
      Result (0) := W.Word (Sequence); Result (1) := 99;
      Result (2) := P.Publisher_Tag (1); Result (3) := W.Word (N);
      Result (4) := W.Word (N * 4); Result (5) := W.Word (N);
      for J in R.Slot_Word_Index loop Result (8 + J) := Data (J); end loop;
      return Result;
   end Row;
   Bad : P.Raw_Row;
   Seen, Position, Last : Natural;
begin
   Check (S'Size <= 4096 * 8);
   for Page_Size in 1 .. 32 loop
      for Offset in 0 .. 3 loop
         Start (S, 7, 1); Seen := 0; Position := Offset + 1;
         while Position <= 400 loop
            Last := Natural'Min (400, Position + Page_Size - 1);
            for J in Position .. Last loop
               Feed (S, 7, Row (J), Got);
               Check (Cursor (S) = W.Word (J + 1));
               if Got.Success then
                  Seen := Seen + 1;
                  Check (J mod 4 = 0 and then Got.Value = Event ((J - 1) / 4 + 1));
                  Check (Got.Incarnation = 7 and Got.Pid = 99 and
                    Got.Publisher = P.Publisher_Tag (1) and
                    Got.Batch = W.Word ((J - 1) / 4 + 1));
                  Check (Got.First_Sequence = W.Word (J - 3) and
                    Got.Producer_Dropped = W.Word (J) and
                    Got.Batch_Gaps = W.Word (J / 4));
               end if;
            end loop;
            Position := Last + 1;
         end loop;
         Check (Seen = (if Offset = 0 then 100 else 99));
         Check (Counts (S).Emitted_Events = W.Word (Seen) and
           Counts (S).Skipped_Rows = W.Word (Offset) and Pending (S) = 0);
      end loop;
   end loop;
   --  Each foreign field or missing sequence must prevent an event; next
   --  complete event still recovers with bounded state.
   for Field in 0 .. 7 loop
      Start (S, 7, 1);
      Feed (S, 7, Row (1), Got); Check (not Got.Success);
      Bad := Row (2); Bad (Field) := Bad (Field) + 1;
      Feed (S, 7, Bad, Got); Check (not Got.Success);
      for I in 3 .. 4 loop Feed (S, 7, Row (I), Got); Check (not Got.Success); end loop;
      for I in 5 .. 8 loop Feed (S, 7, Row (I), Got); end loop;
      Check (Got.Success and then Got.Value = Event (2));
      Check (Counts (S).Emitted_Events = 1 and Counts (S).Abandoned_Events = 1);
   end loop;
   Start (S, 7, 1);
   for I in 1 .. 4 loop Feed (S, 7, Row (I), Got); end loop;
   Check (Got.Success);
   for I in 1 .. 4 loop Feed (S, 7, Row (I), Got); Check (not Got.Success); end loop;
   Check (Counts (S).Rejected_Rows = 4 and Counts (S).Emitted_Events = 1 and Cursor (S) = 5);
   Start (S, 7, 1);
   Feed (S, 7, Row (1), Got); Feed (S, 8, Row (2), Got);
   Check (not Got.Success and Pending (S) = 0 and Owner (S) = 7 and Cursor (S) = 2);
   Check (Counts (S).Endpoint_Mismatches = 1 and Counts (S).Abandoned_Events = 1);
   Start (S, 8, 5);
   for I in 5 .. 8 loop Feed (S, 8, Row (I), Got); end loop;
   Check (Got.Success and then Got.Incarnation = 8 and then Counts (S).Skipped_Rows = 0);
   Start (S, 7, 1); Feed (S, 7, Row (1), Got); Discard_Partial (S);
   Check (Pending (S) = 0 and Counts (S).Abandoned_Events = 1);
   Discard_Partial (S); Check (Counts (S).Abandoned_Events = 1);
   Start (S, 7, 1); Bad := Row (1); Bad (0) := W.Word'Last;
   Feed (S, 7, Bad, Got); Check (not Got.Success and Cursor (S) = 1);
   Bad (0) := 0; Feed (S, 7, Bad, Got); Check (not Got.Success and Cursor (S) = 1);
   for I in 1 .. 4 loop Feed (S, 0, Row (I), Got); Check (not Got.Success); end loop;
   Check (Counts (S).Endpoint_Mismatches = 4);
   Ada.Text_IO.Put_Line ("PASS trace stream checks" & Checks'Image);
end Trace_Stream_Check;
