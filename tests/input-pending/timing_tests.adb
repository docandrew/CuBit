with Ada.Text_IO; with Interfaces; with CuBit.Input;
with CuBit.Click_Sequences; with Input_Pending;
procedure Timing_Tests is
   use Interfaces; use CuBit.Click_Sequences;
   package I renames CuBit.Input;
   Q : Input_Pending.Queue;
   Lost : Boolean;
   Clicks : State; Kind : Press_Kind;
   procedure Event (Time : Unsigned_64; Down : Boolean) is
   begin
      if Down then Press (Clicks, 1, (100, 100), Time, Kind);
      else Release (Clicks, (100, 100), Time); end if;
   end Event;
   procedure Check_Time (T : Unsigned_64) is
      Snapshot : constant Unsigned_64 := I.Pointer_Snapshot (16#FFFF_FFFF_FFFF_FFA5#, T);
   begin
      pragma Assert ((Snapshot and 16#FF#) = 16#A5#);
      pragma Assert (I.Pointer_Time (Snapshot) =
        (if T <= I.Maximum_Pointer_Time then T else Unsigned_64'Last));
   end Check_Time;
begin
   Check_Time (0); Check_Time (1); Check_Time (I.Maximum_Pointer_Time);
   Check_Time (I.Maximum_Pointer_Time + 1); Check_Time (Unsigned_64'Last);
   pragma Assert (I.Pointer_Time (16#FF#) = Unsigned_64'Last);
   for Delay_Ms in Unsigned_64 range 0 .. 2000 loop
      Reset (Clicks);
      Event (100, True); Event (170 + Delay_Ms, False); Event (250 + Delay_Ms, True);
      -- Current processing-time call sites lose a real double-click once
      -- compositor work delays delivery beyond the recognition interval.
      pragma Assert ((Kind = Double_Press) = (Delay_Ms <= 350));
      Input_Pending.Reset (Q);
      Input_Pending.Append (Q, 1, Lost, 100); pragma Assert (not Lost);
      Input_Pending.Append (Q, 0, Lost, 170); pragma Assert (not Lost);
      Input_Pending.Append (Q, 1, Lost, 250); pragma Assert (not Lost);
      Reset (Clicks);
      for Index in 1 .. 3 loop
         declare Head : constant Input_Pending.Item := Input_Pending.Element (Q, 0);
            Wire : constant Unsigned_64 := I.Pointer_Snapshot (Head.Payload, Head.Observed_Ms);
         begin
            -- Retrying a refused send must not replace the acquisition time
            -- with retry/handling time, even after an arbitrarily late poll.
            for Retry in 1 .. 3 loop
               pragma Assert (Input_Pending.Element (Q, 0).Observed_Ms = Head.Observed_Ms);
            end loop;
            Event (I.Pointer_Time (Wire), (Wire and 1) = 1);
            Input_Pending.Acknowledge (Q);
         end;
      end loop;
      pragma Assert (Kind = Double_Press);
   end loop;
   Input_Pending.Reset (Q);
   for Index in 1 .. Input_Pending.Capacity + 1 loop
      Input_Pending.Append (Q, Unsigned_64 (Index), Lost, Unsigned_64 (Index) * 17);
   end loop;
   pragma Assert (Lost and Input_Pending.Count (Q) = 1 and
     Input_Pending.Element (Q, 0).Recover and Input_Pending.Element (Q, 0).Observed_Ms = 561);
   Ada.Text_IO.Put_Line ("POINTER-TIME: PASS 2001 delayed click schedules, processing-time negative control, exact retry/overflow timestamp retention and encoding boundaries");
end Timing_Tests;
