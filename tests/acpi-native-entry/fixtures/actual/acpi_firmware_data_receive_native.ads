pragma Ada_2022;
with CuBit.Messages;
with ACPI_Firmware_Data_Replies;
generic
   with procedure Dispatch (Caller : ACPI_Firmware_Data_Replies.Word;
      Request : ACPI_Firmware_Data_Replies.IPC.Packet; Has_Capability : Boolean;
      Response : out ACPI_Firmware_Data_Replies.IPC.Packet; Handled : out Boolean);
package ACPI_Firmware_Data_Receive_Native is
   -- Additional-request callback for the existing single serialized backend
   -- loop. From and authorityTag are kernel receive metadata, not payloads.
   -- Offers are never accepted. A request stamp is never closed as a handle.
   procedure Handle (From : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Response : in out CuBit.Messages.Message;
      Handled : in out Boolean);
end ACPI_Firmware_Data_Receive_Native;
