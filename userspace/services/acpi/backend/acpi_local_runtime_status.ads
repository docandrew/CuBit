pragma Ada_2022;
package ACPI_Local_Runtime_Status with Pure, SPARK_Mode is
   type Phase is (Fresh, Idle, Active, Stopped);
   type Outcome is (Not_Attempted, Rejected, Owner_Refused, Runtime_Entered,
      Event_Opened, Event_Completed, Delay_Refused, Evaluation_Failed,
      Retained_Results_Pending, IO_Pending, Stop_Pending, Stopped_By_Owner, Already_Stopped);
end ACPI_Local_Runtime_Status;
