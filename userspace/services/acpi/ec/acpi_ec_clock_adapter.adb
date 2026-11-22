pragma Ada_2022;
package body ACPI_EC_Clock_Adapter with SPARK_Mode is
   package P renames ACPI_EC_Clock_Policy;
   use type P.Sample_Status;
   procedure Read (Value : out ACPI_EC_Types.Microseconds; Available : out Boolean) is
      Raw : Interfaces.Unsigned_64;
      Ready : Boolean;
   begin
      Read_Raw (Raw, Ready);
      declare Sample : constant P.Sample := P.Decode (Raw, Ready); begin
         Status := Sample.Status;
         Available := Sample.Status = P.Sampled;
         Value := (if Available then Sample.Value else 0);
      end;
   end Read;
   function Last_Status return P.Sample_Status is (Status);
   procedure Pause_Until
     (Target : ACPI_EC_Types.Microseconds;
      Remaining : in out ACPI_EC_Types.Work_Budget; Result : out P.Pause_Report) is
      Now : ACPI_EC_Types.Microseconds;
      Available, Accepted : Boolean;
   begin
      Result := (others => <>);
      if Remaining = 0 then return; end if;
      Remaining := Remaining - 1;
      Read (Now, Available);
      if not Available then
         Result.Status := (if Status = P.Representation_Limit then P.Clock_Representation_Limit else P.Clock_Unavailable);
      elsif Target <= Now then Result.Status := P.Already_Due;
      else
         Result.Requested := P.Slice (Now, Target, Maximum_Slice);
         Pause_Raw (Interfaces.Unsigned_64 (Result.Requested), Accepted);
         Result.Status := (if Accepted then P.Pause_Returned else P.Pause_Refused);
      end if;
   end Pause_Until;
end ACPI_EC_Clock_Adapter;
