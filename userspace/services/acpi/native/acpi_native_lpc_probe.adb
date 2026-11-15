pragma Ada_2022;
with Interfaces;
with ACPI_One_Service;
with ACPI_Native_Delays;
with ACPI_Native_PCI_Windows;
with ACPI_Local_PCI_Results;
with ACPI_PCI_Protocol;
with ACPI_Native_Log;
package body ACPI_Native_LPC_Probe is
   package One renames ACPI_One_Service;
   package Q renames ACPI_PCI_Protocol;
   package P renames Q.P;
   package R renames ACPI_Local_PCI_Results;
   subtype Word is Interfaces.Unsigned_64;
   use type Word, R.Outcome, Q.Outcome;
   type Phase is (Not_Captured, Boundary_Unavailable, Window_Unavailable,
      Identity_Unavailable, Different_Device, Register_Unavailable, Captured);
   type Snapshot is record
      Stage : Phase := Not_Captured;
      Window_Status : Q.Outcome := Q.Unavailable;
      Identity, Class_Code, LGMR : R.Result;
   end record;
   Saved : Snapshot;
   Used, Emitted : Boolean := False;
   Original_Deadline : Word := 0;
   LPC_Device : constant P.Device_Number := 31;
   LPC_Function : constant P.Function_Number := 0;
   Standard_Config_Bytes : constant Word := 256;
   Identity_Offset : constant Word := 0;
   Class_Revision_Offset : constant Word := 8;
   LGMR_Offset : constant Word := 16#98#;
   Vendor_Mask : constant Word := 16#FFFF#;
   Intel_Vendor : constant Word := 16#8086#;
   Class_Revision_Base : constant Word := 2 ** 8;
   LPC_Bridge_Class : constant Word := 16#06_01_00#;
   LGMR_Enable : constant Word := 1;
   LGMR_Base_Mask : constant Word := 16#FFFF_0000#;
   LGMR_Window_Bytes : constant Word := 65_536;
   Function_Region : constant P.Region :=
     (Device => (Segment => 0, Bus => 0, Device => LPC_Device, Func => LPC_Function),
      Base => 0, Length => Standard_Config_Bytes);
   function Quiet return Boolean is
     (One.Root.Fields.Control_Idle and then not One.Root.Fields.Uncertain.Present
      and then One.Root.Fields.EC_Commands.Quiescent);
   procedure Capture is
      Window : ACPI_Native_PCI_Windows.Local_Window;
      Available : Boolean;
      procedure Read_Register (Offset : Word; Item : out R.Result) is
      begin
         Item := (others => <>);
         if not Quiet then Item.Status := R.Guard_Refused; return; end if;
         ACPI_Native_PCI_Windows.Read_Local
           (One.Field_Scope, Window.Epoch, Window.ID, Function_Region, Offset,
            P.T.Dword_Access, Original_Deadline, Quiet'Access, Item);
         if Item.Status = R.Read_Complete and then Item.Value > Word (Interfaces.Unsigned_32'Last) then
            Item := (Status => R.Invalid_Geometry, Value => 0);
         end if;
      end Read_Register;
   begin
      if Used then return; end if;
      Used := True; Saved.Stage := Boundary_Unavailable;
      if not Quiet then return; end if;
      ACPI_Native_Delays.Inspect_Window (Original_Deadline, Available);
      if not Available or else Original_Deadline = 0 or else Original_Deadline = Word'Last then return; end if;
      -- Metadata lookup only. A missing bus must NEVER cause broker setup.
      ACPI_Native_PCI_Windows.Prepare_Local
        (One.Field_Scope, (Segment => 0, Bus => 0), Q.Wire.No_Finite_Deadline,
         Allow_Setup => False, Result => Window);
      Saved.Window_Status := Window.Status; Saved.Stage := Window_Unavailable;
      if Window.Status /= Q.Ready then return; end if;
      Saved.Stage := Identity_Unavailable;
      Read_Register (Identity_Offset, Saved.Identity);
      if Saved.Identity.Status /= R.Read_Complete then return; end if;
      Saved.Stage := Different_Device;
      if (Saved.Identity.Value and Vendor_Mask) /= Intel_Vendor then return; end if;
      Read_Register (Class_Revision_Offset, Saved.Class_Code);
      if Saved.Class_Code.Status /= R.Read_Complete then Saved.Stage := Identity_Unavailable; return; end if;
      if Saved.Class_Code.Value / Class_Revision_Base /= LPC_Bridge_Class then return; end if;
      Saved.Stage := Register_Unavailable;
      Read_Register (LGMR_Offset, Saved.LGMR);
      if Saved.LGMR.Status = R.Read_Complete then Saved.Stage := Captured; end if;
   end Capture;
   function Hex (Value : Word) return String is
      Hex_Digits : constant String := "0123456789ABCDEF";
      Radix : constant := 16;
      Text : String (1 .. 16);
      Remaining : Word := Value;
   begin
      for I in reverse Text'Range loop
         Text (I) := Hex_Digits (Natural (Remaining mod Radix) + 1);
         Remaining := Remaining / Radix;
      end loop;
      return Text;
   end Hex;
   procedure Report is
      function Bit (Value : Boolean) return String is (if Value then "1" else "0");
   begin
      if not Used or else Emitted then return; end if;
      Emitted := True;
      ACPI_Native_Log.Copied
        ("acpi: stage lpc-probe status=" & Saved.Stage'Image & " bdf=0000:00:1F.0" &
         " source=EXISTING_READ_ONLY_ECAM setup=0 window=" & Saved.Window_Status'Image &
         " identity=" & Saved.Identity.Status'Image & " ids=0x" & Hex (Saved.Identity.Value) &
         " class=" & Saved.Class_Code.Status'Image & " class-revision=0x" & Hex (Saved.Class_Code.Value),
         Peer_Quiet => False);
      ACPI_Native_Log.Copied
        ("acpi: stage lpc-lgmr offset=0x98 status=" & Saved.LGMR.Status'Image &
         " raw=0x" & Hex (Saved.LGMR.Value) &
         " enabled=" & Bit (Saved.Stage = Captured and then (Saved.LGMR.Value and LGMR_Enable) /= 0) &
         " decoded-base=0x" & Hex (Saved.LGMR.Value and LGMR_Base_Mask) &
         " decoded-bytes=" & LGMR_Window_Bytes'Image & " admission=NONE", Peer_Quiet => False);
   end Report;
end ACPI_Native_LPC_Probe;
