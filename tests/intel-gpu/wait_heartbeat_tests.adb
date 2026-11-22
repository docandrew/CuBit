with Ada.Text_IO;
with Intel_GPU_Wait_Heartbeat;
--  The wait-heartbeat bound of intel-gpu's bring-up waits
--  (Intel_GPU_Wait_Heartbeat): one entry line after the threshold,
--  progress at most once a period, at most Lines_Per_Wait lines per wait.
procedure Wait_Heartbeat_Tests is
   package HB renames Intel_GPU_Wait_Heartbeat;
   use type HB.Line_Kind;
   use type HB.Microseconds;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Label : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;

   procedure Heartbeat_Bound is
      M : HB.Monitor;
      Line : HB.Line_Kind;
      Now : HB.Microseconds := 10_000_000;
      Lines, Entries : Natural := 0;
      Last_Line_At : HB.Microseconds := 0;
   begin
      --  A 60 s hang polled every 10 us: one entry, then at most one line per
      --  period, at most Lines_Per_Wait in all.
      for Poll in 1 .. 6_000_000 loop
         HB.Pulse (M, Now, Line);
         if Line /= HB.None then
            if Line = HB.Entry_Line then
               Entries := Entries + 1;
               Check (HB.Elapsed (M, Now) >= HB.Slow_Threshold_Us, "entry after the threshold");
            else
               Check (Now - Last_Line_At >= HB.Period_Us, "progress spaced by the period");
            end if;
            Lines := Lines + 1;
            Last_Line_At := Now;
         end if;
         Now := Now + 10;
      end loop;
      Check (Entries = 1, "one entry line per wait");
      Check (Lines = HB.Lines_Per_Wait, "line bound per wait");
      --  A fast wait (under the threshold) says nothing.
      Now := Now + HB.Wait_Gap_Us;
      Lines := 0;
      for Poll in 1 .. 1_000 loop
         HB.Pulse (M, Now, Line);
         if Line /= HB.None then Lines := Lines + 1; end if;
         Now := Now + 10;
      end loop;
      Check (Lines = 0, "a 10 ms wait is silent");
      --  A pause longer than the gap starts a new wait (its own entry line).
      Now := Now + HB.Wait_Gap_Us;
      Lines := 0;
      for Poll in 1 .. 3_000 loop
         HB.Pulse (M, Now, Line);
         if Line = HB.Entry_Line then Lines := Lines + 1; end if;
         Now := Now + 10;
      end loop;
      Check (Lines = 1, "next slow wait gets its own entry line");
   end Heartbeat_Bound;
begin
   Heartbeat_Bound;
   Ada.Text_IO.Put_Line ("wait_heartbeat PASS:" & Natural'Image (Checks) & " checks (heartbeat bound)");
end Wait_Heartbeat_Tests;
