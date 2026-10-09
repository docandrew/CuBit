with Ada.Text_IO;
with Interfaces; use Interfaces;
with Page_Admission;
with Process_Memory_Budget; use Process_Memory_Budget;

procedure Quota_Width_Test is
   use type Page_Admission.Decision;
   type Quotas is array (Positive range <>) of Unsigned_64;
   type Counts is array (Positive range <>) of Natural;
   Budget : Ledger;
   OK : Boolean;
   Expected, Actual : Page_Admission.Decision;
begin
   for Quota of Quotas'[0, 1, 2, Unsigned_64 (Natural'Last),
                        Unsigned_64 (Natural'Last) + 1, 2 ** 32,
                        2 ** 40, 2 ** 63, Unsigned_64'Last] loop
      Adopt (Budget, Quota, OK);
      pragma Assert (OK and Limit (Budget) = Quota);
      for Used of Counts'[0, 1, 2, Natural'Last - 1, Natural'Last] loop
         for Capacity of Counts'[1, 2, Natural'Last] loop
            Expected :=
              (if Used >= Capacity then Page_Admission.Tracking_Full
               elsif Quota /= 0 and then Unsigned_64 (Used) >= Quota
               then Page_Admission.Quota_Full
               else Page_Admission.Admitted);
            -- Same bounded adapter as the two Process page-fault paths.
            -- Values above the tracking namespace cannot constrain a Used
            -- value which is still below Capacity; finite policy stays U64.
            Actual := Page_Admission.Check
              (4096, 8192, 16384, 20480, 4096, Used, Capacity,
               Natural (Unsigned_64'Min (Quota, Unsigned_64 (Natural'Last))));
            pragma Assert (Actual = Expected);
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line
     ("PASS quota width: 64-bit policy preserved, zero unlimited, tracking adapter 135 boundary cases");
end Quota_Width_Test;
