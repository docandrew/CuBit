pragma Ada_2022;
with CuBit.Handles;
with CuBit.Handles.IPC;
with ACPI_Firmware_Data_Replies;
generic
   with function Close_Carried (Item : CuBit.Handles.IPC.Carried)
      return CuBit.Handles.IPC.Close_Result;
package ACPI_Firmware_Data_Frontend_Native is
   package P renames ACPI_Firmware_Data_Replies;
   package IPC renames P.IPC;
   type Channel is limited private;
   procedure Initialize (Connection : in out Channel; Target : CuBit.Handles.Endpoint;
      Accepted : out Boolean);
   procedure Clock (Now : out IPC.Time_Instant; Available : out Boolean);
   procedure Exchange (Connection : in out Channel; Deadline : IPC.Time_Instant;
      Request : IPC.Packet; Reply : out IPC.Packet;
      Received : out CuBit.Handles.IPC.Carried; Delivered : out IPC.Delivery);
   -- Process-local bounded transport facts; no authority or replay evidence.
   function Diagnostic_Line (Ordinal : Positive) return String;
   function Present (Item : CuBit.Handles.IPC.Carried) return Boolean;
   procedure Dispose (Item : in out CuBit.Handles.IPC.Carried; Closed : out Boolean);
private
   type Channel is limited record
      Consumed, Ready, Working : Boolean := False;
      Endpoint : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   end record;
end ACPI_Firmware_Data_Frontend_Native;
