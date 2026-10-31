pragma Ada_2022;
package body ACPI_EC_Backend_Waits is
   use type Progress.Action_Kind, ACPI_EC_Clock_Policy.Pause_Outcome;
   procedure Advance
     (Now : ACPI_EC_Types.Microseconds;
      Remaining : in out ACPI_EC_Types.Work_Budget; Result : out Report) is
      Before : constant Progress.Report := Progress.Next (Now);
   begin
      Result := (others => <>);
      if Before.Action /= Progress.Wait_Until then
         Progress.Advance (Now, Result.Progress_Result); return;
      end if;
      Result.Wait_Checked := True;
      Pause (Before.At_Time, Remaining, Result.Pause_Result);
      if Result.Pause_Result.Status not in ACPI_EC_Clock_Policy.Pause_Returned |
        ACPI_EC_Clock_Policy.Already_Due
      then
         Registers.Request_Stop;
         Result.Stop_Requested := True;
      end if;
      Progress.Advance_Waiter (Now, Result.Progress_Result);
   end Advance;
end ACPI_EC_Backend_Waits;
