pragma Ada_2022;
with Interfaces;
with ACPI_Locked_NVS_Protocol;
with ACPI_Region_Protocol;
-- Passive copied facts. These fields are neither permission nor completion
-- evidence and must not drive replay, deadline renewal or backing release.
package ACPI_Field_Diagnostics with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   type Cause is (No_Failure, Startup_Deadline, Startup_Clock_Unavailable,
      Startup_Clock_Reversed, Lease_Deadline, Lease_Clock_Unavailable,
      Lease_Clock_Reversed, IPC_Deadline, IPC_Clock_Unavailable,
      IPC_Clock_Reversed, Transport_Not_Sent, Transport_Unknown,
      Malformed_Reply, Backend_Refused, Backend_Read_Failed, Backend_Late,
      Invalid_Request, Invalid_Deadline, Peer_Changed, Capacity_Exhausted,
      Reentered, Lease_Unavailable, Scope_Unavailable, Provider_Uncertain,
      Window_Unavailable, Deadline_Overflow);
   type Point is (No_Point, Startup_Window, Lease_Window, Acquire_Lease,
      Validate_Request, Before_Select_Lease, Before_Select_Clock,
      Select_Exchange, Select_Reply, Before_Read_Lease, Before_Read_Clock,
      Read_Exchange, Read_Reply, After_Read, Read_Guard,
      End_Lease, Release_Lease, Cleanup_Lease, Scope_State, Provider_Dispatch);
   type Report is record
      Reason : Cause := No_Failure;
      At_Point : Point := No_Point;
      From_Read : Boolean := False;
      Read_Pending : Boolean := False;
      Remote : ACPI_Locked_NVS_Protocol.Read_Outcome := ACPI_Locked_NVS_Protocol.No_Data;
      Delivery : ACPI_Region_Protocol.Delivery := ACPI_Region_Protocol.Not_Dispatched;
      Record_Number, Byte_Offset, IPC_Until, Last_Known_IPC_Time : Word := 0;
      Seen_IPC_Clock : Boolean := False;
   end record;
   No_Report : constant Report := (others => <>);
   function Present (Item : Report) return Boolean is (Item.Reason /= No_Failure);
   procedure Remember (First : in out Report; Item : Report)
     with Contract_Cases =>
       (Present (First) => First = First'Old,
        others => First = Item);
   function Name (Item : Cause) return String;
   function Name (Item : Point) return String;
   function Format (Item : Report) return String;
   type Microseconds is new Word;
   Microseconds_Per_Millisecond : constant Microseconds := 1_000;
   function Elapsed_Milliseconds (Start, Sample : Microseconds; Available : Boolean) return String;
end ACPI_Field_Diagnostics;
