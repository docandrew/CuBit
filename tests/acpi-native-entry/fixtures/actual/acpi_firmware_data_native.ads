pragma Ada_2022;
with CuBit.Handles;
with CuBit.Handles.IPC;
with ACPI_Firmware_Data_Requests;
generic
   -- Runtime owner supplies arbitrary-carried-capability cleanup. A failed
   -- close must leave the caller's token retained for explicit recovery.
   with function Close_Carried (Item : CuBit.Handles.IPC.Carried)
      return CuBit.Handles.IPC.Close_Result;
package ACPI_Firmware_Data_Native is
   package R renames ACPI_Firmware_Data_Requests;
   package IPC renames R.IPC;
   type State is limited private;
   procedure Initialize (Context : in out State; Destination : CuBit.Handles.Endpoint;
      Accepted : out Boolean);
   procedure Clock (Now : out IPC.Time_Instant; Available : out Boolean);
   procedure Exchange (Channel : in out State; Deadline : IPC.Time_Instant;
      Request : IPC.Packet; Reply : out IPC.Packet;
      Received : out CuBit.Handles.IPC.Carried; Delivered : out IPC.Delivery);
   function Present (Item : CuBit.Handles.IPC.Carried) return Boolean;
   procedure Inspect (Item : CuBit.Handles.IPC.Carried; Evidence : out R.Grant_Evidence);
   procedure Dispose (Item : in out CuBit.Handles.IPC.Carried; Closed : out Boolean);
private
   type State is limited record
      Consumed, Ready, Working : Boolean := False;
      Destination : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   end record;
end ACPI_Firmware_Data_Native;
