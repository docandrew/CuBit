with Ada.Text_IO; use Ada.Text_IO;
with Compositor_Readers;
procedure Reader_Tests is
   Fail_At, Calls, Last : Natural := 0;
   Lease_Expected, Lease_Done : Boolean := False;
   procedure Release_Lease (Confirmed : out Boolean) is
   begin
      pragma Assert (Lease_Expected and not Lease_Done and Calls = 0);
      Calls := Calls + 1;
      Confirmed := Fail_At /= 1;
      Lease_Done := Confirmed;
   end Release_Lease;
   procedure Retire_Grant (Index : Positive; Confirmed : out Boolean) is
   begin
      pragma Assert ((not Lease_Expected or Lease_Done) and Index > Last);
      Calls := Calls + 1;
      Last := Index;
      Confirmed := Fail_At /= Index + 1;
   end Retire_Grant;
   package R is new Compositor_Readers (3, Release_Lease, Retire_Grant);
   Cases : Natural := 0;
begin
   for Has_Lease in Boolean loop
      for Mask in 0 .. 7 loop
         for Failure in 0 .. 4 loop
            declare
               Grants : R.Grant_Set;
               Expected_Calls : Natural := 0;
               Expected_Failure : Boolean := False;
            begin
               for I in Grants'Range loop
                  Grants (I) := (Mask / 2 ** (I - 1)) mod 2 = 1;
               end loop;
               Fail_At := Failure; Calls := 0; Last := 0;
               Lease_Expected := Has_Lease; Lease_Done := False;
               if Has_Lease then
                  Expected_Calls := 1;
                  Expected_Failure := Failure = 1;
               end if;
               if not Expected_Failure then
                  for I in Grants'Range loop
                     if Grants (I) then
                        Expected_Calls := Expected_Calls + 1;
                        if Failure = I + 1 then
                           Expected_Failure := True;
                           exit;
                        end if;
                     end if;
                  end loop;
               end if;
               declare
                  S : R.State := R.Open (Has_Lease, Grants);
               begin
                  R.Retire (S);
                  pragma Assert (Calls = Expected_Calls);
                  pragma Assert (R.Uncertain (S) = Expected_Failure);
                  pragma Assert (R.Clear (S) = not Expected_Failure);
                  pragma Assert (R.Lease_Pending (S) = (Has_Lease and Failure = 1));
                  for I in Grants'Range loop
                     pragma Assert (R.Grant_Pending (S, I) =
                       (Grants (I) and Expected_Failure and
                        (Failure = 1 or I >= Failure - 1)));
                  end loop;
                  if not Expected_Failure then
                     R.Retire (S);
                     pragma Assert (Calls = Expected_Calls and R.Clear (S));
                  end if;
               end;
               Cases := Cases + 1;
            end;
         end loop;
      end loop;
   end loop;
   Put_Line ("COMPOSITOR-READERS: PASS" & Cases'Image &
     " partial setup, ordered retirement, failure retention and idempotence cases");
end Reader_Tests;
