pragma Ada_2022;
with CuBit.Handles.Devices;
with ACPI_Backend_Memory_Access;
package body ACPI_Firmware_Data_Mapping_Native is
   use type Policy.Outcome;
   procedure Describe_Reservation (Item : Arena.Reservation; Base, Pages : out Interfaces.Unsigned_64;
      Accepted : out Boolean) is
   begin Arena.Describe (Reservations, Item, Base, Pages, Accepted); end Describe_Reservation;
   procedure Map_Chunk (Held : CuBit.Handles.IPC.Carried; Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Completed : out Boolean) is
      Taken : constant CuBit.Handles.IPC.Accepted_Device_Memory := CuBit.Handles.IPC.Accept_Device_Memory (Held);
   begin
      Completed := False;
      if Taken.OK then Completed := CuBit.Handles.Devices.Map_Device (Taken.Value, Physical, Virtual, Pages, False); end if;
   end Map_Chunk;
   procedure Map (Held : CuBit.Handles.IPC.Carried; Item : Arena.Reservation;
      Physical, Virtual, Pages : Interfaces.Unsigned_64; Deadline : R.IPC.Time_Instant; Completed : out Boolean) is
      Result : Policy.Report;
   begin
      Policy.Map_Exact (Mapping, Held, Item, Physical, Virtual, Pages, Deadline, Result);
      Completed := Result.Status = Policy.Mapped;
   end Map;
   procedure Transfer (Virtual : Interfaces.Unsigned_64; Width : ACPI_Firmware_Data_Layouts.T.Access_Width;
      For_Write : Boolean; Input : Interfaces.Unsigned_64; Output : out Interfaces.Unsigned_64; Completed : out Boolean) is
   begin ACPI_Backend_Memory_Access.Transfer (Virtual, Width, For_Write, Input, Output, Completed); end Transfer;
end ACPI_Firmware_Data_Mapping_Native;
