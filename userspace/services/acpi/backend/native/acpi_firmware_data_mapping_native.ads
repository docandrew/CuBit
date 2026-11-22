pragma Ada_2022;
with Interfaces;
with CuBit.Handles.IPC;
with ACPI_Firmware_Data_Arena;
with ACPI_Firmware_Data_Mapping;
with ACPI_Firmware_Data_Native;
with ACPI_Firmware_Data_Layouts;
generic
   with package Native is new ACPI_Firmware_Data_Native (<>);
   with package Arena is new ACPI_Firmware_Data_Arena (<>);
   Reservations : in out Arena.Context;
   Maximum_Per_Call : Positive;
package ACPI_Firmware_Data_Mapping_Native is
   package R renames Native.R;
   procedure Describe_Reservation (Item : Arena.Reservation; Base, Pages : out Interfaces.Unsigned_64;
      Accepted : out Boolean);
   procedure Map (Held : CuBit.Handles.IPC.Carried; Item : Arena.Reservation;
      Physical, Virtual, Pages : Interfaces.Unsigned_64; Deadline : R.IPC.Time_Instant; Completed : out Boolean);
   -- Private backend callback only. Record/scope policy must precede this raw
   -- access; it is never an IPC-address entry point.
   procedure Transfer (Virtual : Interfaces.Unsigned_64; Width : ACPI_Firmware_Data_Layouts.T.Access_Width;
      For_Write : Boolean; Input : Interfaces.Unsigned_64; Output : out Interfaces.Unsigned_64; Completed : out Boolean);
private
   procedure Map_Chunk (Held : CuBit.Handles.IPC.Carried; Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Completed : out Boolean);
   package Policy is new ACPI_Firmware_Data_Mapping
     (Arena, Reservations, CuBit.Handles.IPC.Carried, Native.Inspect, Map_Chunk, Native.Clock, Maximum_Per_Call);
   Mapping : Policy.Context;
end ACPI_Firmware_Data_Mapping_Native;
