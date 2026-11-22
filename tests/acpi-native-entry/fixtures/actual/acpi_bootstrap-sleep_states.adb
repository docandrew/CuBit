pragma Ada_2022;
with AML_Execute;
package body ACPI_Bootstrap.Sleep_States with SPARK_Mode is
   procedure Observe (Session : in out State; Registers : Driver.Register_Model;
      Budget : Natural; Result : out Report) is
      Host_Query_Cost : constant := 1;
      Remaining : Natural;
   begin
      Result := (others => <>);
      for Kind in Driver.System_State loop
         Result.Items (Kind).Kind := Kind;
         Result.Items (Kind).Registers := Registers;
      end loop;
      if Session.Stage /= Complete or else Session.Device_Data.Stage /= Devices_Completed then return; end if;
      for Kind in Driver.System_State loop
         pragma Loop_Invariant (Result.Charged <= Budget);
         if Budget - Result.Charged < Host_Query_Cost then
            Result.Status := Budget_Exhausted; return;
         end if;
         Result.Charged := Result.Charged + Host_Query_Cost;
         Remaining := Budget - Result.Charged;
         Result.Attempted (Kind) := True;
         Result.Examined := Result.Examined + 1;
         Result.Recovery_Needed := True;
         Driver.Query (Session.Core, Kind, Registers, Remaining, Result.Items (Kind));
         if Result.Items (Kind).Charged > Remaining then
            Result.Status := Recovery_Required; return;
         end if;
         Result.Charged := Result.Charged + Result.Items (Kind).Charged;
         Result.Recovery_Needed := Result.Items (Kind).Outcome in Driver.Cleanup_Failed | Driver.Provider_Contract_Error
           or else Result.Items (Kind).Execution in AML_Execute.Field_Indeterminate | AML_Execute.Fatal_Requested;
         if Result.Recovery_Needed then Result.Status := Recovery_Required; return; end if;
         case Result.Items (Kind).Outcome is
            when Driver.Types_Available | Driver.Return_Ignored | Driver.Not_Declared => null;
            when Driver.Budget_Exhausted => Result.Status := Budget_Exhausted; return;
            when others => Result.Status := Object_Failed; return;
         end case;
      end loop;
      Result.Status := Observed;
   end Observe;
end ACPI_Bootstrap.Sleep_States;
