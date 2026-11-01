------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  ACPI PCI configuration window regression (ACPI-PCI-001): the read-only,
--  uncached PCI segment 0 bus 0 window an ACPI hardware grant carries when
--  the startup plan opts in, (acpi-pci-config approve-declared).
--
--  @description
--  With the window: exactly one PCI configuration window; its kernel
--  capability covers exactly bus 0's ECAM slice (1 MiB) at the base this
--  program finds itself in its own MCFG copy (segment 0, starting at bus
--  0); its kind and cache mode are the kernel tag's (PCI_Config,
--  uncached); it is held read-only without grant and cannot be copied on;
--  a writable mapping of it is refused, the pages just past and before it
--  are refused, and a read-only mapping of function 00:00.0 reads the host
--  bridge's vendor and device ID (the headless test compares them with
--  devmgr's enumeration). The rest of the grant is unchanged (the FACS
--  window stays write-back). Last, after TEST: PASS, it writes to the
--  read-only mapping on purpose: the kernel must stop it with a
--  write-protection fault.
--
--  Without the window (the same image, not approved for PCI configuration):
--  it says so and that the rest of the grant is still there, and stops.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles; use CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Control;
with CuBit.Handles.Devices;
with CuBit.ACPI_Windows; use CuBit.ACPI_Windows;
with CuBit.ACPI_Windows.Launch;
with CuBit.ACPI_Memory_Classes;
with CuBit.Firmware_Snapshot;
with Startup;

procedure Main is
   use ASCII;
   package IPC renames CuBit.Handles.IPC;
   package FS renames CuBit.Firmware_Snapshot;
   package AMC renames CuBit.ACPI_Memory_Classes;
   use type ABI.Kind;
   use type FS.Outcome, FS.Byte_Count;
   use type AMC.Memory_Kind, AMC.Cache_Mode;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("acpi-pci-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("acpi-pci-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   Hex_Digits : constant String := "0123456789ABCDEF";
   Nibble_Bits : constant := 4;
   Nibble_Mask : constant := 16#F#;
   --  Value as Count upper-case hex digits (devmgr's "8086:29C0" style).
   function Hex (Value : Unsigned_64; Count : Positive) return String is
      Result : String (1 .. Count);
      V : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digits (Hex_Digits'First + Natural (V and Nibble_Mask));
         V := Shift_Right (V, Nibble_Bits);
      end loop;
      return Result;
   end Hex;

   Page_Bytes     : constant := AMC.Page_Bytes;
   --  Where this program maps the window (its own address space).
   Window_Virtual : constant := 16#6000_0000_0000#;
   --  A second, separate place for the refused writable mapping.
   Probe_Virtual  : constant := 16#6000_0010_0000#;
   --  PCI configuration header (PCI Local Bus 3.0, 6.1): vendor ID at 0,
   --  device ID at 2; an absent function reads all ones.
   Vendor_Offset  : constant := 0;
   Device_Shift   : constant := 16;
   ID_Mask        : constant Unsigned_32 := 16#FFFF#;
   Absent_Vendor  : constant Unsigned_32 := 16#FFFF#;
   ID_Digits      : constant := 4;
   --  MCFG (PCI Firmware 3.2, 4.1.2): allocations from byte 44, 16 bytes
   --  each: base (8), segment (2), start bus (1), end bus (1), reserved (4).
   MCFG_Signature    : constant String := "MCFG";
   MCFG_Fixed_Bytes  : constant := 44;
   Allocation_Bytes  : constant := 16;
   Segment_At        : constant := 8;
   Start_Bus_At      : constant := 10;
   Base_Bytes        : constant := 8;
   Segment_Bytes     : constant := 2;
   Table_Bytes       : constant := 512;
   Byte_Bits         : constant := 8;

   Tables : constant FS.Authority := Startup.Tables;
   Grant_Value : constant CuBit.ACPI_Windows.Grant := Startup.Hardware;
   G : CuBit.ACPI_Windows.Grant renames Grant_Value;

   type Byte_Array is array (Positive range <>) of Unsigned_8;

   --  The first Table_Bytes of the snapshot's table named Signature.
   procedure Read_Table (Signature : String; Into : out Byte_Array; Length : out Natural) is
      Catalog : FS.Catalog_Info;
      D : FS.Table_Description;
      Status : FS.Outcome;
   begin
      Into := [others => 0];
      Length := 0;
      FS.Info (Tables, Catalog, Status);
      if Status /= FS.Ok then
         return;
      end if;
      for T in 1 .. FS.Table_ID (Catalog.Table_Count) loop
         FS.Describe (Tables, T, D, Status);
         if Status = FS.Ok and then
           (for all I in FS.Signature_Index =>
              D.Signature (I) = Character'Pos (Signature (Signature'First + I - 1)))
         then
            declare
               Wanted : constant Natural :=
                 Natural (FS.Byte_Count'Min (D.Extent, FS.Byte_Count (Into'Length)));
               Chunk : FS.Byte_Array (1 .. Wanted);
               Copied : FS.Byte_Count;
            begin
               FS.Copy_Part (Tables, T, 0, Chunk, Copied, Status);
               if Status = FS.Ok and then Copied = FS.Byte_Count (Wanted) then
                  for I in 1 .. Wanted loop
                     Into (Into'First + I - 1) := Unsigned_8 (Chunk (I));
                  end loop;
                  Length := Wanted;
               end if;
            end;
            return;
         end if;
      end loop;
   end Read_Table;

   function Little_Endian (Data : Byte_Array; At_Byte, Count : Natural) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      for I in 0 .. Count - 1 loop
         Value := Value or Shift_Left (Unsigned_64 (Data (Data'First + At_Byte + I)), Byte_Bits * I);
      end loop;
      return Value;
   end Little_Endian;

   --  Segment 0's allocation starting at bus 0 in this program's own MCFG
   --  copy: its base (bus 0's function 00:00.0), or 0.
   function Bus_Zero_Base return Unsigned_64 is
      MCFG : Byte_Array (1 .. Table_Bytes);
      Length : Natural;
      At_Byte : Natural := MCFG_Fixed_Bytes;
   begin
      Read_Table (MCFG_Signature, MCFG, Length);
      while At_Byte + Allocation_Bytes <= Length loop
         if Little_Endian (MCFG, At_Byte + Segment_At, Segment_Bytes) = 0
           and then MCFG (MCFG'First + At_Byte + Start_Bus_At) = 0
         then
            return Little_Endian (MCFG, At_Byte, Base_Bytes);
         end if;
         At_Byte := At_Byte + Allocation_Bytes;
      end loop;
      return 0;
   end Bus_Zero_Base;

   PCI_Index : Natural := 0;
   PCI_Count : Natural := 0;
   Write_Back_Others : Boolean := True;
begin
   debugPrint ("acpi-pci-check: starting" & LF);
   for K in 1 .. G.Memory_Used loop
      if G.Memory (K).Kind = AMC.PCI_Config then
         PCI_Count := PCI_Count + 1;
         PCI_Index := K;
      elsif G.Memory (K).Cache /= AMC.Write_Back then
         Write_Back_Others := False;
      end if;
   end loop;

   if PCI_Count = 0 then
      --  Not approved for PCI configuration: the rest of the grant only.
      debugPrint ("acpi-pci-check: no PCI configuration window; grant has" &
                  Natural'Image (G.IO_Used) & " I/O and" & Natural'Image (G.Memory_Used) &
                  " memory windows" & LF);
      if G.IO_Used > 0 and then G.Memory_Used > 0 and then Write_Back_Others
        and then CuBit.ACPI_Windows.Launch.Well_Formed (G)
      then
         debugPrint ("acpi-pci-check: grant without PCI configuration intact" & LF);
      end if;
      return;
   end if;

   Check (PCI_Count = 1, "exactly one PCI configuration window");
   Check (CuBit.ACPI_Windows.Launch.Well_Formed (G), "windows sorted and disjoint");
   Check (G.IO_Used > 0 and then G.Memory_Used > PCI_Count and then Write_Back_Others,
          "the rest of the grant is unchanged (I/O and write-back firmware windows)");
   declare
      W : constant Memory_Window := G.Memory (PCI_Index);
      D : constant IPC.Description := IPC.Describe (W.Memory);
      First : constant Unsigned_64 := W.First_Page * Page_Bytes;
      Past : constant Unsigned_64 := (W.First_Page + W.Pages) * Page_Bytes;
      Base : constant Unsigned_64 := Bus_Zero_Base;
   begin
      debugPrint ("acpi-pci-check: PCI configuration window " & Hex (First, 16) & " +" &
                  Image (W.Pages) & " pages; MCFG bus 0 at " & Hex (Base, 16) & LF);
      Check (D.Current and then D.Kind = ABI.Device_Memory
             and then D.Object = First and then D.Parameter = AMC.PCI_Bus_Bytes
             and then W.Pages * Page_Bytes = AMC.PCI_Bus_Bytes,
             "handle covers exactly bus 0 (1 MiB)");
      Check (Base /= 0 and then First = Base, "window is the MCFG's segment 0 bus 0 slice");
      Check (D.Current and then AMC.Firmware_Tag (D.Authority_Tag)
             and then AMC.Kind_Of (D.Authority_Tag) = AMC.PCI_Config
             and then W.Cache = AMC.Uncached and then W.Cache = AMC.Cache_Of (W.Kind),
             "kind and cache mode from its kernel tag (PCI_Config, uncached)");
      Check (D.Current and then D.Held.Read and then not D.Held.Write and then not D.Held.Grant,
             "read-only without grant");
      Check (not Valid (Device_Memory'(CuBit.Handles.Control.Derive (Copy (W.Memory, Read_Only))))
             and then not Valid (Device_Memory'(CuBit.Handles.Control.Derive
                                    (Copy (W.Memory, Read_Write))))
             and then not Valid (Device_Memory'(CuBit.Handles.Control.Derive
                                    (Narrowed (W.Memory, Read_Only, First, Page_Bytes)))),
             "cannot be copied on");
      Check (not CuBit.Handles.Devices.Map_Device (W.Memory, First, Probe_Virtual, 1, False),
             "writable mapping refused");
      Check (not CuBit.Handles.Devices.Map_Device (W.Memory, Past, Probe_Virtual, 1, True),
             "bus 1 (the page past the window) refused");
      if First >= Page_Bytes then
         Check (not CuBit.Handles.Devices.Map_Device
                      (W.Memory, First - Page_Bytes, Probe_Virtual, 1, True),
                "the page before the window refused");
      end if;
      declare
         Mapped : constant Boolean :=
           CuBit.Handles.Devices.Map_Device (W.Memory, First, Window_Virtual, 1, True);
      begin
         Check (Mapped, "host bridge function 00:00.0 maps read-only");
         if Mapped then
            declare
               IDs : Unsigned_32 with Import, Volatile,
                 Address => System'To_Address (Window_Virtual + Vendor_Offset);
               Read : constant Unsigned_32 := IDs;
               Vendor : constant Unsigned_32 := Read and ID_Mask;
               Device : constant Unsigned_32 := Shift_Right (Read, Device_Shift) and ID_Mask;
            begin
               debugPrint ("acpi-pci-check: host bridge 00:00.0 " &
                           Hex (Unsigned_64 (Vendor), ID_Digits) & ":" &
                           Hex (Unsigned_64 (Device), ID_Digits) & LF);
               Check (Vendor /= Absent_Vendor and then Vendor /= 0,
                      "host bridge vendor ID read through the window");
            end;
         end if;

         if Failures = 0 then
            debugPrint ("TEST: PASS acpi-pci-config" & LF);
         else
            debugPrint ("TEST: FAIL acpi-pci-config" & Natural'Image (Failures) & LF);
         end if;

         --  The mapping is read-only: this store must fault, and the kernel
         --  stops this program.
         if Mapped then
            debugPrint ("acpi-pci-check: attempting forbidden write at " &
                        Hex (Window_Virtual, 16) & LF);
            declare
               Target : Unsigned_32 with Import, Volatile,
                 Address => System'To_Address (Window_Virtual + Vendor_Offset);
            begin
               Target := Absent_Vendor;
            end;
            debugPrint ("acpi-pci-check: forbidden write was not stopped FAIL" & LF);
         end if;
      end;
   end;
end Main;
