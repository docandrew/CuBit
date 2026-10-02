with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Metric_Completion;
procedure Metric_Completion_Tests is
   package M renames Compositor_Metric_Completion;
   package P renames M.Protocol;
   use type P.Status;
   R : M.Reply;
begin
   for Sent in 1 .. 63 loop
      for Accepted in 0 .. Sent loop
         R := (True, 0, P.Status'Enum_Rep (P.OK), 4, 0, 0,
               [Unsigned_64 (Accepted), Unsigned_64 (Sent - Accepted), 0, 0]);
         pragma Assert (M.Definitive (R, Sent));
         R.Kernel_Valid := False; pragma Assert (not M.Definitive (R, Sent));
         R.Kernel_Valid := True; R.Words (1) := R.Words (1) + 1;
         pragma Assert (not M.Definitive (R, Sent));
      end loop;
      for Code in P.Status loop
         R := (True, 0, P.Status'Enum_Rep (Code), 4, 0, 0, [others => 0]);
         pragma Assert (M.Definitive (R, Sent) =
           (Code in P.Denied | P.Invalid_Request | P.Exhausted));
      end loop;
      R := (True, 0, P.Status'Enum_Rep (P.OK), 4, 0, 0,
            [Unsigned_64'Last, Unsigned_64'Last, 0, 0]);
      pragma Assert (not M.Definitive (R, Sent));
   end loop;
   R := (True, 0, P.Status'Enum_Rep (P.OK), 4, 0, 0, [others => 0]);
   pragma Assert (not M.Definitive (R, 0));
   Ada.Text_IO.Put_Line ("METRIC-COMPLETION: PASS all valid per-page count splits, refusal codes, invalid envelopes and overflow counts");
end Metric_Completion_Tests;
