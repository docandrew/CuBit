------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  ACPI hardware windows regression (coordination/acpi-region-boundary.md,
--  slice 1): the grant procmgr gives an approved Request.ACPI_Hardware.
--
--  @description
--  Checks that the grant is non-empty, sorted and disjoint; that each
--  window's kernel capability covers exactly its stated range, read-write,
--  with no grant right; that the windows are exactly the FADT/ECDT fixed
--  blocks this program computes itself from its own firmware snapshot
--  (CuBit.ACPI_Fixed_Windows; on q35 no other holder overlaps them); that
--  the PM timer, read twice through its window, advances; and that a port
--  just past a window is refused. Then, through the role-35 "registers"
--  endpoint: it carries ACPI_Register_Roles.Frontend_Tag
--  (acpi-registers-standin reports the kernel-stamped tag), and no copy of
--  it can be re-badged or derived at all.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles; use CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Control;
with CuBit.Handles.Startup;
with CuBit.Handles.Devices;
with CuBit.ACPI_Windows; use CuBit.ACPI_Windows;
with CuBit.ACPI_Windows.Launch;
with CuBit.ACPI_Fixed_Windows;
with CuBit.Firmware_Snapshot;
with ACPI_Register_Roles;
with Startup;

procedure Main is
   use ASCII;
   package IPC renames CuBit.Handles.IPC;
   package FS renames CuBit.Firmware_Snapshot;
   package AF renames CuBit.ACPI_Fixed_Windows;
   use type ABI.Kind;
   use type FS.Outcome, FS.Byte_Count;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("acpi-hardware-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("acpi-hardware-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   --  The FADT's 32-bit PM timer block address (ACPI 6.5, 5.2.9).
   PM_Timer_Offset : constant := AF.PM_Timer_Offset;
   PM_Timer_Mask   : constant Unsigned_64 := 16#FF_FFFF#;   --  24-bit counter
   Timer_Gap_Ms    : constant := 2;
   Table_Bytes     : constant := 512;
   Probe_Label     : constant Unsigned_32 := 16#AC35#;     --  the stand-in's probe
   REPLY_OK        : constant Unsigned_32 := 16#F000#;
   Probe_Deadline_Ms : constant := 2_000;
   --  Any other tag a holder might try to stamp on its copy.
   Forged_Tag      : constant Unsigned_64 := 16#5245_4241_4447_4521#;

   Tables : constant FS.Authority := Startup.Tables;
   Grant_Value : constant CuBit.ACPI_Windows.Grant := Startup.Hardware;

   --  The first Table_Bytes of the snapshot's table named Signature.
   procedure Read_Table (Signature : String; Into : out AF.Byte_Array; Length : out Natural) is
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
               Wanted : constant Natural := Natural (FS.Byte_Count'Min (D.Extent, Table_Bytes));
               Chunk : FS.Byte_Array (1 .. Wanted);
               Copied : FS.Byte_Count;
            begin
               FS.Copy_Part (Tables, T, 0, Chunk, Copied, Status);
               if Status = FS.Ok and then Copied = FS.Byte_Count (Wanted) then
                  for I in 1 .. Wanted loop
                     Into (I) := Chunk (I);
                  end loop;
                  Length := Wanted;
               end if;
            end;
            return;
         end if;
      end loop;
   end Read_Table;

   FADT, ECDT : AF.Byte_Array (1 .. Table_Bytes);
   FADT_Length, ECDT_Length : Natural;
   Expected : AF.Window_List;
   G : CuBit.ACPI_Windows.Grant renames Grant_Value;
begin
   debugPrint ("acpi-hardware-check: starting" & LF);
   Check (G.IO_Used > 0, "grant has I/O windows");
   Check (G.Memory_Used = 0, "slice 1 grants no memory windows");
   Check (CuBit.ACPI_Windows.Launch.Well_Formed (G), "windows sorted and disjoint");

   for K in 1 .. G.IO_Used loop
      declare
         W : constant IO_Window := G.IO (K);
         D : constant IPC.Description := IPC.Describe (W.Ports);
      begin
         debugPrint ("acpi-hardware-check: window " & Image (Unsigned_64 (W.First)) & " +" &
                     Image (Unsigned_64 (W.Count)) & LF);
         Check (D.Current and then D.Kind = ABI.IO_Ports
                and then D.Object = Unsigned_64 (W.First)
                and then D.Parameter = Unsigned_64 (W.Count),
                "window " & Image (Unsigned_64 (K)) & " handle covers exactly its range");
         Check (D.Current and then D.Held.Read and then D.Held.Write and then not D.Held.Grant,
                "window " & Image (Unsigned_64 (K)) & " read-write without grant");
         --  The port just past the window is not the handle's to touch.
         if Unsigned_32 (W.First) + W.Count < Port_Count'Last then
            Check (CuBit.Handles.Devices.In_8
                     (W.Ports, Unsigned_16 (Unsigned_32 (W.First) + W.Count)) = Unsigned_64'Last,
                   "window " & Image (Unsigned_64 (K)) & " refuses the next port");
         end if;
      end;
   end loop;

   --  Independently: the fixed blocks of this program's own FADT/ECDT copy.
   Read_Table (AF.FADT_Signature, FADT, FADT_Length);
   Read_Table (AF.ECDT_Signature, ECDT, ECDT_Length);
   Check (FADT_Length > 0, "FADT read");
   Check (ECDT_Length = 0, "q35 has no ECDT");
   AF.Compute (FADT (1 .. FADT_Length), ECDT (1 .. ECDT_Length), Expected);
   Check (Expected.Count = G.IO_Used
          and then (for all K in 1 .. Expected.Count =>
                      Expected.Windows (K).First = G.IO (K).First
                      and then Expected.Windows (K).Count = G.IO (K).Count),
          "windows are exactly the FADT fixed blocks");

   --  The PM timer advances when read twice through its window.
   declare
      Timer : Unsigned_64 := 0;
      Found : Natural := 0;
      First_Read, Second_Read : Unsigned_64;
      Ignore : Unsigned_64;
   begin
      for I in 0 .. 3 loop
         Timer := Timer or Shift_Left (Unsigned_64 (FADT (PM_Timer_Offset + 1 + I)), 8 * I);
      end loop;
      for K in 1 .. G.IO_Used loop
         if Timer >= Unsigned_64 (G.IO (K).First)
           and then Timer + 4 <= Unsigned_64 (G.IO (K).First) + Unsigned_64 (G.IO (K).Count)
         then
            Found := K;
         end if;
      end loop;
      Check (Timer /= 0 and then Found /= 0, "PM timer inside a window");
      if Found /= 0 then
         First_Read := CuBit.Handles.Devices.In_32 (G.IO (Found).Ports, Unsigned_16 (Timer));
         Ignore := syscall (SYSCALL_SLEEP, Timer_Gap_Ms);
         Second_Read := CuBit.Handles.Devices.In_32 (G.IO (Found).Ports, Unsigned_16 (Timer));
         debugPrint ("acpi-hardware-check: PM timer " & Image (First_Read and PM_Timer_Mask) &
                     " then " & Image (Second_Read and PM_Timer_Mask) & LF);
         Check (First_Read /= Unsigned_64'Last and then Second_Read /= Unsigned_64'Last
                and then (First_Read and PM_Timer_Mask) /= (Second_Read and PM_Timer_Mask),
                "PM timer advances through its window");
      end if;
   end;

   --  The role-35 endpoint: frontend badge, and no re-badging.
   declare
      Registers : constant CuBit.Handles.Startup.Optional_Endpoint := Startup.Registers;
   begin
      Check (Registers.Present, "registers endpoint granted");
      if Registers.Present then
         declare
            Probe : Message := NULL_MESSAGE;
            D : constant IPC.Description := IPC.Describe (Registers.Value);
         begin
            Probe.tag.label := Probe_Label;
            Probe.tag := capCall (Registers.Value, Probe, Deadline_After (Probe_Deadline_Ms));
            Check (Probe.tag.label = REPLY_OK
                   and then ACPI_Register_Roles.Is_Frontend (ACPI_Register_Roles.Role_Tag (Probe.words (0))),
                   "registers endpoint carries the frontend badge");
            Check (D.Current and then not D.Held.Grant
                   and then D.Authority_Tag = Unsigned_64 (ACPI_Register_Roles.Frontend_Tag),
                   "registers endpoint holds no grant right");
            Check (not Valid (Endpoint'(CuBit.Handles.Control.Derive
                                          (Badged (Registers.Value, Read_Write, Forged_Tag))))
                   and then not Valid (Endpoint'(CuBit.Handles.Control.Derive
                                          (Copy (Registers.Value, Read_Only))))
                   and then not Valid (Endpoint'(CuBit.Handles.Control.Derive
                                          (Copy (Registers.Value,
                                                 (Read | Write | Grant => True, others => False))))),
                   "registers endpoint cannot be re-badged or derived");
            Check (IPC.Describe (Registers.Value).Authority_Tag =
                     Unsigned_64 (ACPI_Register_Roles.Frontend_Tag),
                   "refused re-badging leaves the frontend badge");
         end;
      end if;
   end;

   if Failures = 0 then
      debugPrint ("TEST: PASS acpi-hardware" & LF);
   else
      debugPrint ("TEST: FAIL acpi-hardware" & Natural'Image (Failures) & LF);
   end if;
end Main;
