------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  ACPI firmware-data regression (CuBit.Firmware_Data_Exchange; kernel
--  Physical_Ranges Data_Memory, FIRMWARE_DATA_TAG): devmgr's narrowed,
--  uncached, read-write windows over table-named firmware data, for a
--  program the startup plan approves with (acpi-firmware-data
--  approve-declared).
--
--  @description
--  Approved: it asks devmgr, through the "hardware-data" endpoint, for one
--  page of a firmware-reserved span q35 has (the ICH9 root complex register
--  block, then SeaBIOS's other reserved spans), gets it with the kernel's
--  firmware-data tag, exactly that page, read-write without grant, maps it
--  uncached and reads it; the page past it is refused and the capability
--  cannot be copied on. The same request id again is answered the same,
--  never a second grant. Then the named refusals: PCI configuration space
--  (an ECAM page of bus 0 and of bus 1, from its own MCFG copy), a RAM page,
--  a PCI BAR's page (found through its read-only PCI configuration window),
--  the FACS page (from its own FADT copy), a malformed request, and the
--  ordinary "device-manager" endpoint, which is not the firmware-data
--  badge. TEST: PASS acpi-firmware-data when all hold.
--
--  Unapproved (the same image as acpi-data-unapproved.app): it has no
--  "hardware-data" endpoint, and the same request through "device-manager"
--  is refused (Not_Approved) without a capability.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles; use CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Control;
with CuBit.Handles.Devices;
with CuBit.Handles.Startup;
with CuBit.ACPI_Windows; use CuBit.ACPI_Windows;
with CuBit.ACPI_Memory_Classes;
with CuBit.Firmware_Snapshot;
with CuBit.Firmware_Data_Exchange;
with CuBit.Deadlines.Clock;
with Firmware_Data_ABI;
with Startup;

procedure Main is
   use ASCII;
   package IPC renames CuBit.Handles.IPC;
   package HS renames CuBit.Handles.Startup;
   package FS renames CuBit.Firmware_Snapshot;
   package AMC renames CuBit.ACPI_Memory_Classes;
   package FDX renames CuBit.Firmware_Data_Exchange;
   use type ABI.Kind;
   use type FS.Outcome, FS.Byte_Count;
   use type AMC.Memory_Kind;
   use type FDX.Disposition;
   use type IPC.Carried_Kind;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("acpi-data-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("acpi-data-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   Hex_Digits : constant String := "0123456789ABCDEF";
   Nibble_Bits : constant := 4;
   Nibble_Mask : constant := 16#F#;
   function Hex (Value : Unsigned_64; Count : Positive := 16) return String is
      Result : String (1 .. Count);
      V : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digits (Hex_Digits'First + Natural (V and Nibble_Mask));
         V := Shift_Right (V, Nibble_Bits);
      end loop;
      return Result;
   end Hex;

   Page_Bytes : constant := FDX.Page_Bytes;
   --  Where this program maps things (its own address space).
   Data_Virtual  : constant := 16#6000_0000_0000#;
   Probe_Virtual : constant := 16#6000_0010_0000#;
   PCI_Virtual   : constant := 16#6000_1000_0000#;
   --  Each request's own deadline: devmgr answers at once or not at all.
   Request_Deadline_Ms : constant := 2_000;

   --  Firmware-reserved spans q35 (SeaBIOS) reports, most likely first: the
   --  ICH9 root complex register block, KVM's reserved pages, the BIOS.
   type Candidate_Index is range 1 .. 3;
   Reserved_Candidates : constant array (Candidate_Index) of Unsigned_64 :=
     [16#FED1_C000#, 16#FEFF_C000#, 16#FFFC_0000#];
   --  Usable RAM in every headless guest (16 MiB in).
   RAM_Page : constant := 16#0100_0000#;

   --  ACPI table layouts (ACPI 6.5, 5.2.9 FADT; PCI Firmware 3.2, 4.1.2 MCFG).
   FADT_Signature        : constant String := "FACP";
   FADT_Firmware_Ctrl    : constant := 36;
   FADT_X_Firmware_Ctrl  : constant := 132;
   MCFG_Signature        : constant String := "MCFG";
   MCFG_Fixed_Bytes      : constant := 44;
   MCFG_Entry_Bytes      : constant := 16;
   MCFG_Segment_At       : constant := 8;
   MCFG_Start_Bus_At     : constant := 10;
   Table_Bytes           : constant := 512;
   Byte_Bits             : constant := 8;
   PCI_Bus_Bytes         : constant := AMC.PCI_Bus_Bytes;
   --  PCI configuration header (PCI Local Bus 3.0, 6.1).
   PCI_Devices           : constant := AMC.PCI_Devices_Per_Bus;
   PCI_Functions         : constant := AMC.PCI_Functions_Per_Device;
   Vendor_Mask           : constant Unsigned_32 := 16#FFFF#;
   Absent_Vendor         : constant Unsigned_32 := 16#FFFF#;
   Header_Dword          : constant := 16#0C#;
   Header_Shift          : constant := 16;
   Header_Layout_Mask    : constant Unsigned_32 := 16#7F#;
   General_Layout        : constant Unsigned_32 := 0;
   First_BAR             : constant := 16#10#;
   BAR_Count             : constant := 6;
   BAR_Bytes             : constant := 4;
   BAR_IO                : constant Unsigned_32 := 1;
   BAR_Type_Mask         : constant Unsigned_32 := 16#6#;
   BAR_Type_64           : constant Unsigned_32 := 16#4#;
   BAR_Address_Mask      : constant Unsigned_32 := 16#FFFF_FFF0#;

   Tables : constant FS.Authority := Startup.Tables;
   Grant_Value : constant CuBit.ACPI_Windows.Grant := Startup.Hardware;
   G : CuBit.ACPI_Windows.Grant renames Grant_Value;
   Data_Endpoint : constant HS.Optional_Endpoint :=
     HS.Optional_Endpoint_Named (FDX.Data_Label ("hardware"));
   Plain : constant HS.Optional_Endpoint := HS.Optional_Endpoint_Named ("device-manager");

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

   --  Segment 0's allocation starting at bus 0 in its own MCFG copy: its
   --  base (bus 0's first page), or 0.
   function Bus_Zero_Base return Unsigned_64 is
      MCFG : Byte_Array (1 .. Table_Bytes);
      Length : Natural;
      At_Byte : Natural := MCFG_Fixed_Bytes;
   begin
      Read_Table (MCFG_Signature, MCFG, Length);
      while At_Byte + MCFG_Entry_Bytes <= Length loop
         if Little_Endian (MCFG, At_Byte + MCFG_Segment_At, 2) = 0
           and then MCFG (MCFG'First + At_Byte + MCFG_Start_Bus_At) = 0
         then
            return Little_Endian (MCFG, At_Byte, 8);
         end if;
         At_Byte := At_Byte + MCFG_Entry_Bytes;
      end loop;
      return 0;
   end Bus_Zero_Base;

   --  The FACS's address from its own FADT copy (X_FIRMWARE_CTRL when
   --  present), or 0.
   function FACS_Address return Unsigned_64 is
      FADT : Byte_Array (1 .. Table_Bytes);
      Length : Natural;
      Wide : Unsigned_64 := 0;
   begin
      Read_Table (FADT_Signature, FADT, Length);
      if Length >= FADT_X_Firmware_Ctrl + 8 then
         Wide := Little_Endian (FADT, FADT_X_Firmware_Ctrl, 8);
      end if;
      if Wide /= 0 then
         return Wide;
      elsif Length >= FADT_Firmware_Ctrl + 4 then
         return Little_Endian (FADT, FADT_Firmware_Ctrl, 4);
      end if;
      return 0;
   end FACS_Address;

   type Answer is record
      Replied : Boolean := False;
      Words   : MessageWords := [others => 0];
      Got     : IPC.Carried;
   end record;

   function Ask (Target : Endpoint; First_Page, Pages, Request : Unsigned_64) return Answer is
      Msg : Message := NULL_MESSAGE;
      Tag : MessageTag;
      Result : Answer;
   begin
      Msg.tag := (label => FDX.Data_Window_Request, length => FDX.Request_Words,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words (FDX.First_Page_Word) := First_Page;
      Msg.words (FDX.Page_Count_Word) := Pages;
      Msg.words (FDX.Request_Word) := Request;
      Tag := IPC.Call_Accepting
        (Target, Msg, CuBit.Deadlines.Clock.Now + Request_Deadline_Ms, Result.Got);
      Result.Replied := Tag.label = FDX.Data_Window_Request and then Tag.length = FDX.Reply_Words
        and then Msg.words (FDX.Reply_Request_Word) = Request
        and then FDX.Valid_Disposition (Msg.words (FDX.Disposition_Word));
      Result.Words := Msg.words;
      return Result;
   end Ask;

   function Disposition_Of (A : Answer) return FDX.Disposition is
   begin
      for D in FDX.Disposition loop
         if FDX.Disposition'Enum_Rep (D) = A.Words (FDX.Disposition_Word) then
            return D;
         end if;
      end loop;
      return FDX.Malformed;
   end Disposition_Of;

   function Refused_As (A : Answer; Expected : FDX.Disposition) return Boolean is
     (A.Replied and then Disposition_Of (A) = Expected and then A.Got.Kind = IPC.Absent
      and then A.Words (FDX.Reply_First_Page_Word) = 0
      and then A.Words (FDX.Reply_Page_Count_Word) = 0);

   Next_Request : Unsigned_64 := 1;
   function Request_Id return Unsigned_64 is
      Id : constant Unsigned_64 := Next_Request;
   begin
      Next_Request := Next_Request + 1;
      return Id;
   end Request_Id;

   --  The first memory BAR's page of a bus 0 function, through the PCI
   --  configuration window mapped read-only at PCI_Virtual; 0 when none.
   function First_BAR_Page return Unsigned_64 is
      function Config (Device, Offset : Natural) return Unsigned_32 is
         Word : Unsigned_32 with Import, Volatile,
           Address => System'To_Address
             (PCI_Virtual + Unsigned_64 (Device * PCI_Functions) * Page_Bytes + Unsigned_64 (Offset));
      begin
         return Word;
      end Config;
      Low : Unsigned_32;
      Base : Unsigned_64;
      Index : Natural;
   begin
      for Device in 0 .. PCI_Devices - 1 loop
         if (Config (Device, 0) and Vendor_Mask) /= Absent_Vendor
           and then (Shift_Right (Config (Device, Header_Dword), Header_Shift) and Header_Layout_Mask)
                      = General_Layout
         then
            Index := 0;
            while Index < BAR_Count loop
               Low := Config (Device, First_BAR + Index * BAR_Bytes);
               if (Low and BAR_IO) = 0 then
                  Base := Unsigned_64 (Low and BAR_Address_Mask);
                  if (Low and BAR_Type_Mask) = BAR_Type_64 and then Index + 1 < BAR_Count then
                     Base := Base or Shift_Left
                       (Unsigned_64 (Config (Device, First_BAR + (Index + 1) * BAR_Bytes)), 32);
                     Index := Index + 1;
                  end if;
                  if Base /= 0 then
                     debugPrint ("acpi-data-check: device 00:" & Hex (Unsigned_64 (Device), 2) &
                                 ".0 memory BAR at " & Hex (Base) & LF);
                     return Base / Page_Bytes;
                  end if;
               end if;
               Index := Index + 1;
            end loop;
         end if;
      end loop;
      return 0;
   end First_BAR_Page;

   Granted_Page : Unsigned_64 := 0;
   Granted_Request : Unsigned_64 := 0;
   First_Grant : Device_Memory := No_Device_Memory;
begin
   debugPrint ("acpi-data-check: starting" & LF);

   if not Data_Endpoint.Present then
      --  Not approved for firmware data: no endpoint, and the ordinary
      --  devmgr endpoint is not the badge.
      debugPrint ("acpi-data-check: no firmware-data endpoint" & LF);
      if Plain.Present then
         declare
            A : constant Answer := Ask (Plain.Value, Reserved_Candidates (1) / Page_Bytes, 1,
                                        Request_Id);
         begin
            if Refused_As (A, FDX.Not_Approved) then
               debugPrint ("acpi-data-check: unapproved request refused (NOT_APPROVED)" & LF);
            else
               debugPrint ("acpi-data-check: unapproved request answered otherwise FAIL" & LF);
            end if;
         end;
      end if;
      return;
   end if;

   --  The ordinary endpoint is not the badge, even for an approved program.
   if Plain.Present then
      Check (Refused_As (Ask (Plain.Value, Reserved_Candidates (1) / Page_Bytes, 1, Request_Id),
                         FDX.Not_Approved),
             "a request through device-manager refused (not the badge)");
   end if;

   --  One reserved page, granted as firmware data.
   for C of Reserved_Candidates loop
      declare
         Id : constant Unsigned_64 := Request_Id;
         A : constant Answer := Ask (Data_Endpoint.Value, C / Page_Bytes, 1, Id);
      begin
         debugPrint ("acpi-data-check: reserved page " & Hex (C) & ": " &
                     (if A.Replied then FDX.Name (Disposition_Of (A)) else "no reply") & LF);
         if A.Replied and then Disposition_Of (A) = FDX.Granted_Data then
            Granted_Page := C / Page_Bytes;
            Granted_Request := Id;
            Check (A.Words (FDX.Reply_First_Page_Word) = Granted_Page
                   and then A.Words (FDX.Reply_Page_Count_Word) = 1,
                   "the reply names the envelope");
            Check (A.Got.Kind = IPC.Present, "the grant is carried");
            declare
               Taken : constant IPC.Accepted_Device_Memory := IPC.Accept_Device_Memory (A.Got);
            begin
               if Taken.OK then
                  First_Grant := Taken.Value;
               end if;
            end;
            exit;
         end if;
      end;
   end loop;
   Check (Granted_Page /= 0 and then Valid (First_Grant), "a reserved page granted as firmware data");

   if Valid (First_Grant) then
      declare
         D : constant IPC.Description := IPC.Describe (First_Grant);
         First : constant Unsigned_64 := Granted_Page * Page_Bytes;
      begin
         Check (D.Current and then D.Kind = ABI.Device_Memory
                and then D.Object = First and then D.Parameter = Page_Bytes,
                "the handle covers exactly the page");
         Check (D.Current and then D.Authority_Tag = Firmware_Data_ABI.Firmware_Data_Tag,
                "its kernel tag is firmware data (uncached)");
         Check (D.Current and then D.Held.Read and then D.Held.Write and then not D.Held.Grant,
                "read-write without grant");
         Check (not Valid (Device_Memory'(CuBit.Handles.Control.Derive (Copy (First_Grant, Read_Only)))),
                "cannot be copied on");
         Check (not CuBit.Handles.Devices.Map_Device (First_Grant, First + Page_Bytes, Probe_Virtual, 1, True),
                "the page past the envelope refused");
         declare
            Mapped : constant Boolean :=
              CuBit.Handles.Devices.Map_Device (First_Grant, First, Data_Virtual, 1, False);
         begin
            Check (Mapped, "maps read-write (uncached)");
            if Mapped then
               declare
                  Word : Unsigned_32 with Import, Volatile, Address => System'To_Address (Data_Virtual);
                  Read : constant Unsigned_32 := Word;
               begin
                  debugPrint ("acpi-data-check: read " & Hex (Unsigned_64 (Read), 8) & " at " &
                              Hex (First) & LF);
                  Check (True, "reads the page");
               end;
            end if;
         end;
      end;
      --  The same request id again: the original answer, never a second
      --  derivation (the copy names the same envelope and tag).
      declare
         A : constant Answer := Ask (Data_Endpoint.Value, Granted_Page, 1, Granted_Request);
         Taken : constant IPC.Accepted_Device_Memory := IPC.Accept_Device_Memory (A.Got);
      begin
         Check (A.Replied and then Disposition_Of (A) = FDX.Granted_Data
                and then A.Words (FDX.Reply_First_Page_Word) = Granted_Page
                and then Taken.OK
                and then IPC.Describe (Taken.Value).Current
                and then IPC.Describe (Taken.Value).Object = Granted_Page * Page_Bytes
                and then IPC.Describe (Taken.Value).Authority_Tag = Firmware_Data_ABI.Firmware_Data_Tag,
                "the same request id is answered the same");
      end;
   end if;

   --  PCI configuration space: bus 0's ECAM page and bus 1's.
   declare
      Base : constant Unsigned_64 := Bus_Zero_Base;
   begin
      debugPrint ("acpi-data-check: MCFG bus 0 at " & Hex (Base) & LF);
      Check (Base /= 0
             and then Refused_As (Ask (Data_Endpoint.Value, Base / Page_Bytes, 1, Request_Id),
                                  FDX.Overlaps_PCI_Config),
             "an ECAM page of bus 0 refused (Overlaps_PCI_Config)");
      Check (Base /= 0
             and then Refused_As (Ask (Data_Endpoint.Value, (Base + PCI_Bus_Bytes) / Page_Bytes, 1,
                                       Request_Id),
                                  FDX.Overlaps_PCI_Config),
             "an ECAM page of bus 1 refused (Overlaps_PCI_Config)");
   end;

   Check (Refused_As (Ask (Data_Endpoint.Value, RAM_Page / Page_Bytes, 1, Request_Id),
                      FDX.Not_Firmware_Class),
          "a RAM page refused (Not_Firmware_Class)");

   --  A PCI BAR's page, found through the read-only configuration window.
   declare
      PCI_Index : Natural := 0;
      BAR_Page : Unsigned_64 := 0;
   begin
      for K in 1 .. G.Memory_Used loop
         if G.Memory (K).Kind = AMC.PCI_Config then
            PCI_Index := K;
         end if;
      end loop;
      if PCI_Index /= 0
        and then CuBit.Handles.Devices.Map_Device
                   (G.Memory (PCI_Index).Memory, G.Memory (PCI_Index).First_Page * Page_Bytes,
                    PCI_Virtual, G.Memory (PCI_Index).Pages, True)
      then
         BAR_Page := First_BAR_Page;
      end if;
      Check (BAR_Page /= 0
             and then Refused_As (Ask (Data_Endpoint.Value, BAR_Page, 1, Request_Id),
                                  FDX.Overlaps_Device),
             "a device BAR page refused (Overlaps_Device)");
   end;

   declare
      FACS : constant Unsigned_64 := FACS_Address;
   begin
      debugPrint ("acpi-data-check: FACS at " & Hex (FACS) & LF);
      Check (FACS /= 0
             and then Refused_As (Ask (Data_Endpoint.Value, FACS / Page_Bytes, 1, Request_Id),
                                  FDX.Overlaps_Control),
             "the FACS page refused (Overlaps_Control)");
   end;

   Check (Refused_As (Ask (Data_Endpoint.Value, Granted_Page, 0, Request_Id), FDX.Malformed),
          "a request without pages refused (Malformed)");

   if Failures = 0 then
      debugPrint ("TEST: PASS acpi-firmware-data" & LF);
   else
      debugPrint ("TEST: FAIL acpi-firmware-data" & Natural'Image (Failures) & LF);
   end if;
end Main;
