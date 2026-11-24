pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with ACPI_CMOS_Ports;
with ACPI_Backend_Kernel_Handles;

package body ACPI_Native_CMOS_Ports is
   use type T.I.Word;
   procedure Read_Byte
     (Held : CuBit.Handles.IO_Ports; Port : Interfaces.Unsigned_16;
      Value : out T.I.Word; Outcome : out T.Completion) is
   begin
      Value := CuBit.Messages.portInp8 (Held, Port);
      Outcome := (if Value = T.I.Word'Last then T.Refused else T.Completed);
   end Read_Byte;
   procedure Write_Byte
     (Held : CuBit.Handles.IO_Ports; Port : Interfaces.Unsigned_16;
      Value : T.I.Byte; Outcome : out T.Completion) is
      Result : constant T.I.Word := CuBit.Messages.portOutp8 (Held, Port, Value);
   begin
      Outcome := (if Result = T.I.Word'Last then T.Refused else T.Completed);
   end Write_Byte;
   package Ports is new ACPI_CMOS_Ports
     (CuBit.Handles.IO_Ports, CuBit.Handles.No_IO_Ports,
      ACPI_Backend_Kernel_Handles.Inspect_Ports, Read_Byte, Write_Byte);
   IO : Ports.Context;
   Captured : Boolean := False;
   Saved_Handle : CuBit.Handles.IO_Ports := CuBit.Handles.No_IO_Ports;
   Saved_Profile : T.P.Profile;
   Saved_Known, Saved_NMI : Boolean := False;
   procedure Capture
     (Held : CuBit.Handles.IO_Ports; Profile : T.P.Profile;
      Known_NMI, NMI_Disabled : Boolean) is
   begin
      if Captured then return; end if;
      Captured := True; Saved_Handle := Held; Saved_Profile := Profile;
      Saved_Known := Known_NMI; Saved_NMI := NMI_Disabled;
   end Capture;
   procedure Initialize (Century : T.I.Byte; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Captured then return; end if;
      Ports.Initialize (IO, Saved_Handle, Saved_Profile, Century,
        Saved_Known, Saved_NMI, Accepted);
   end Initialize;
   function Available return Boolean is (Ports.Is_Ready (IO));
   function Pending return Boolean is (Ports.Pending (IO));
   procedure Read_Configuration
     (Profile : out T.P.Profile; Century : out T.I.Byte;
      Known_NMI, NMI_Disabled, Standard_Granted, Extended_Granted : out Boolean) is
   begin
      Ports.Configuration (IO, Profile, Century, Known_NMI, NMI_Disabled,
        Standard_Granted, Extended_Granted);
   end Read_Configuration;
   procedure Transfer_Byte (Plan : T.Transfer; Value : out T.I.Byte; Outcome : out T.Completion) is
   begin Ports.Transfer_Byte (IO, Plan, Value, Outcome); end Transfer_Byte;
end ACPI_Native_CMOS_Ports;
