with ACPI_One_Service;with ACPI_Native_EC_Discovery;
package body ACPI_Clock_Source with SPARK_Mode=>Off is
   procedure Read(Value:out AML_Decode.Integer_Value;Available:out Boolean) is
   begin
      if ACPI_One_Service.Expire_After_Candidate and then
        ACPI_Native_EC_Discovery.Observe.Candidate_Described then
         ACPI_One_Service.Now:=ACPI_One_Service.Current_Event_Deadline+1;
         ACPI_One_Service.Expire_After_Candidate:=False;
      end if;
      if ACPI_One_Service.Delay_After_Candidate /= 0 and then
        ACPI_Native_EC_Discovery.Observe.Candidate_Described then
         ACPI_One_Service.Now := ACPI_One_Service.Now + ACPI_One_Service.Delay_After_Candidate;
         ACPI_One_Service.Delay_After_Candidate := 0;
      end if;
      ACPI_One_Service.Clock_Observations:=ACPI_One_Service.Clock_Observations+1;Value:=ACPI_One_Service.Now;Available:=ACPI_One_Service.Read_Clock_Available;end Read;
end ACPI_Clock_Source;
