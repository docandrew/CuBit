pragma Ada_2022;
with Interfaces;
with CuBit.Handles.IPC;
with CuBit.Messages;
with ACPI_Platform_Pool_Owner;
package ACPI_Platform_Pool_Native is
   package H renames CuBit.Handles;
   package IPC renames H.IPC;
   subtype Word is Interfaces.Unsigned_64;
   function Inspect_Endpoint (Target : H.Endpoint; Expected_Owner : Word) return Boolean;
   function Is_Present (Item : IPC.Carried) return Boolean;
   function Is_Refused (Item : IPC.Carried) return Boolean;
   procedure Inspect_Pool (Item : IPC.Carried; Value : out H.Device_Memory; Accepted : out Boolean);
   procedure Dispose (Item : in out IPC.Carried; Closed : out Boolean);
   procedure Read_Clock (Milliseconds : out Word; Available : out Boolean);
   package Owner is new ACPI_Platform_Pool_Owner
     (H.Endpoint,H.Device_Memory,H.No_Device_Memory,IPC.Carried,(Kind => IPC.Absent),
      Inspect_Endpoint,Is_Present,Is_Refused,Inspect_Pool,Dispose,Read_Clock);
   procedure Exchange (Target : H.Endpoint; Request, Deadline_Milliseconds : Word;
      Reply : out Owner.Response; Received : out IPC.Carried);
   procedure Acquire (State : in out Owner.Context; Target : H.Endpoint;
      Expected_Owner : CuBit.Messages.Process_ID; Request, Deadline_Milliseconds : Word;
      Accepted : out Boolean);
end ACPI_Platform_Pool_Native;
