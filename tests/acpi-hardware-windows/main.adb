------------------------------------------------------------------------------
--  Hosted tests: the ACPI fixed-hardware I/O windows procmgr computes
--  (CuBit.ACPI_Fixed_Windows), the window startup names
--  (CuBit.ACPI_Hardware_Authority), devmgr's port-claim ledger
--  (Devmgr_Port_Claims), its ACPI memory-claim ledger
--  (Devmgr_Memory_Claims) and the memory windows' kernel-tag classes
--  (CuBit.ACPI_Memory_Classes). Synthetic FADT/ECDT bytes, laid out per ACPI 6.5.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.ACPI_Fixed_Windows; use CuBit.ACPI_Fixed_Windows;
with CuBit.ACPI_Hardware_Authority;
with CuBit.Startup_Tables;
with Devmgr_Port_Claims;
with Devmgr_Memory_Claims;
with CuBit.ACPI_Memory_Classes;
with Capability_ABI;
with CuBit.Authority_Policy;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Put_Line ("PASS " & Name);
      else
         Failures := Failures + 1;
         Put_Line ("FAIL " & Name);
      end if;
   end Check;

   FADT_Bytes : constant := 276;
   ECDT_Bytes : constant := 66;
   SystemIO : constant := System_IO_Space;
   System_Memory : constant := 0;

   procedure Put (T : in out Byte_Array; Offset : Natural; Value : Unsigned_64; Bytes : Positive) is
   begin
      for I in 0 .. Bytes - 1 loop
         T (Offset + 1 + I) := Unsigned_8 (Shift_Right (Value, 8 * I) and 16#FF#);
      end loop;
   end Put;

   procedure Header (T : in out Byte_Array; Signature : String; Length : Natural) is
   begin
      for I in Signature'Range loop
         T (I - Signature'First + 1) := Character'Pos (Signature (I));
      end loop;
      Put (T, Length_Offset, Unsigned_64 (Length), 4);
   end Header;

   procedure Put_GAS (T : in out Byte_Array; Offset : Natural; Space : Unsigned_8; Address : Unsigned_64) is
   begin
      T (Offset + 1) := Space;
      Put (T, Offset + GAS_Address_Offset, Address, 8);
   end Put_GAS;

   --  QEMU q35's FADT blocks: PM1a event 0x600 (4), PM1a control 0x604 (2),
   --  PM timer 0x608 (4), GPE0 0x620 (16); no PM1b, no GPE1.
   function Q35 return Byte_Array is
      T : Byte_Array (1 .. FADT_Bytes) := [others => 0];
   begin
      Header (T, FADT_Signature, FADT_Bytes);
      Put (T, PM1a_Event_Offset, 16#600#, 4);
      Put (T, PM1a_Control_Offset, 16#604#, 4);
      Put (T, PM_Timer_Offset, 16#608#, 4);
      Put (T, GPE0_Offset, 16#620#, 4);
      T (PM1_Event_Length_Offset + 1) := 4;
      T (PM1_Control_Length_Offset + 1) := 2;
      T (PM_Timer_Length_Offset + 1) := 4;
      T (GPE0_Length_Offset + 1) := 16;
      return T;
   end Q35;

   No_Table : constant Byte_Array (1 .. 0) := [];

   function Equals (L : Window_List; Expected : Window_Array; Count : Window_Count) return Boolean is
     (L.Count = Count and then
      (for all K in 1 .. Count =>
         L.Windows (K).First = Expected (K).First and then L.Windows (K).Count = Expected (K).Count));

   L : Window_List;
   Q35_Windows : constant Window_Array :=
     [(16#600#, 4), (16#604#, 2), (16#608#, 4), (16#620#, 16), others => (0, 1)];
begin
   ---------------------------------------------------------------------------
   --  Windows from the tables.
   ---------------------------------------------------------------------------
   Compute (Q35, No_Table, L);
   Check (Equals (L, Q35_Windows, 4) and then Well_Formed (L),
          "q35 FADT: exactly its four blocks, adjacent blocks kept apart");

   declare
      T : Byte_Array := Q35;
   begin
      --  The 64-bit timer block wins when present and in the I/O space.
      Put_GAS (T, X_PM_Timer_Offset, SystemIO, 16#B008#);
      Compute (T, No_Table, L);
      Check (L.Count = 4 and then L.Windows (4) = (16#B008#, 4), "X_PM_TMR in I/O replaces the 32-bit timer");
      --  ... and a 64-bit block in memory is no I/O window at all.
      Put_GAS (T, X_PM_Timer_Offset, System_Memory, 16#FED0_0000#);
      Compute (T, No_Table, L);
      Check (L.Count = 3 and then (for all K in 1 .. 3 => L.Windows (K).First /= 16#608#),
             "X_PM_TMR in memory gives no timer window (no fallback to 32-bit)");
   end;

   declare
      T : Byte_Array := Q35;
   begin
      --  Overlapping blocks become their union; nothing outside them.
      Put (T, GPE1_Offset, 16#628#, 4);
      T (GPE1_Length_Offset + 1) := 16;
      Compute (T, No_Table, L);
      Check (L.Count = 4 and then L.Windows (4) = (16#620#, 24) and then Well_Formed (L),
             "overlapping GPE blocks merge to their exact union");
      --  A block running past the port space is left out, never clipped.
      Put (T, GPE1_Offset, 16#FFF8#, 4);
      Compute (T, No_Table, L);
      Check (L.Count = 4 and then L.Windows (4) = (16#620#, 16), "a block past port 65535 is left out");
      --  Address zero or length zero: absent.
      Put (T, GPE1_Offset, 16#700#, 4);
      T (GPE1_Length_Offset + 1) := 0;
      Compute (T, No_Table, L);
      Check (Equals (L, Q35_Windows, 4), "a zero-length block is absent");
   end;

   declare
      T : Byte_Array := Q35;
   begin
      --  The table's own Length bounds what is read.
      Put (T, Length_Offset, Unsigned_64 (PM1_Event_Length_Offset), 4);
      Compute (T, No_Table, L);
      Check (L.Count = 0, "a FADT ending before its length fields gives nothing");
      Put (T, Length_Offset, Unsigned_64 (PM1_Control_Length_Offset + 1), 4);
      Compute (T, No_Table, L);
      Check (L.Count = 2 and then L.Windows (1) = (16#600#, 4) and then L.Windows (2) = (16#604#, 2),
             "only blocks whose length field the FADT holds");
      Put (T, Length_Offset, 116, 4);
      Put_GAS (T, X_PM_Timer_Offset, SystemIO, 16#B008#);
      Compute (T, No_Table, L);
      Check (Equals (L, Q35_Windows, 4), "fields past the stated length are not read");
      T := Q35;
      T (1) := Character'Pos ('X');
      Compute (T, No_Table, L);
      Check (L.Count = 0, "a table that is not the FADT gives nothing");
      Compute (Q35 (1 .. 40), No_Table, L);
      Check (L.Count = 0, "a buffer cut short gives only what it holds");
   end;

   declare
      E : Byte_Array (1 .. ECDT_Bytes) := [others => 0];
   begin
      Header (E, ECDT_Signature, ECDT_Bytes);
      Put_GAS (E, EC_Control_Offset, SystemIO, 16#66#);
      Put_GAS (E, EC_Data_Offset, SystemIO, 16#62#);
      Compute (Q35, E, L);
      Check (L.Count = 6 and then L.Windows (1) = (16#62#, 1) and then L.Windows (2) = (16#66#, 1)
             and then Well_Formed (L),
             "ECDT EC data and command ports: one port each, sorted");
      Put_GAS (E, EC_Data_Offset, System_Memory, 16#FE00_0000#);
      Compute (Q35, E, L);
      Check (L.Count = 5 and then L.Windows (1) = (16#66#, 1), "an EC register in memory is no window");
   end;

   ---------------------------------------------------------------------------
   --  Window startup names.
   ---------------------------------------------------------------------------
   declare
      package AHA renames CuBit.ACPI_Hardware_Authority;
      package ST renames CuBit.Startup_Tables;
      use type AHA.Declared_Source;
      Longest : constant String (1 .. AHA.Maximum_Binding_Length) := [others => 'h'];
   begin
      Check (AHA.IO_Label ("hardware", 1) = "hardware-io-1"
             and then AHA.IO_Label ("hardware", 16) = "hardware-io-16",
             "window labels");
      Check (ST.Valid_Name_Text (AHA.IO_Label (Longest, AHA.Maximum_IO_Windows)),
             "the longest binding's labels are startup names");
      Check (AHA.Memory_Label ("hardware", 1) = "hardware-mem-1"
             and then AHA.Memory_Label ("hardware", 16) = "hardware-mem-16",
             "memory window labels");
      Check (ST.Valid_Name_Text (AHA.Memory_Label (Longest, AHA.Maximum_Memory_Windows)),
             "the longest binding's memory labels are startup names");
      --  A grant is admitted with a window of either kind: memory-only
      --  (a hardware-reduced platform, no FADT fixed I/O blocks) too; with
      --  neither it is refused. The declared entry holds the first window.
      Check (AHA.Declared_Holds (0, 1) = AHA.First_Memory_Window
             and then AHA.Declared_Holds (0, AHA.Maximum_Memory_Windows) = AHA.First_Memory_Window,
             "a memory-only grant is admitted, declared on its first memory window");
      Check (AHA.Declared_Holds (0, 0) = AHA.No_Window, "a grant with no window is refused");
      Check (AHA.Declared_Holds (3, 0) = AHA.First_IO_Window
             and then AHA.Declared_Holds (1, 2) = AHA.First_IO_Window,
             "with I/O windows the declared entry holds the first I/O window");
      Check ((for all K in 1 .. AHA.Maximum_IO_Windows =>
                (for all J in 1 .. AHA.Maximum_Memory_Windows =>
                   AHA.IO_Label ("hardware", K) /= AHA.Memory_Label ("hardware", J))),
             "I/O and memory labels never collide");
   end;

   ---------------------------------------------------------------------------
   --  A memory window's kind and cache mode come from its kernel tag; any
   --  other capability (device memory, an unknown tag, partial pages) is
   --  no firmware window, whatever a startup entry calls it.
   ---------------------------------------------------------------------------
   declare
      use CuBit.ACPI_Memory_Classes;
      NVS     : constant Unsigned_64 := Capability_ABI.Firmware_NVS_Tag;
      Control : constant Unsigned_64 := Capability_ABI.Firmware_Control_Tag;
      Page    : constant := Page_Bytes;
   begin
      Check (Firmware_Tag (NVS) and then Kind_Of (NVS) = ACPI_NVS
             and then Cache_Of (Kind_Of (NVS)) = Write_Back,
             "the NVS tag is an NVS window, write-back");
      Check (Firmware_Tag (Control) and then Kind_Of (Control) = Firmware_Control
             and then Cache_Of (Kind_Of (Control)) = Write_Back,
             "the control tag is the FACS page, write-back");
      Check (not Firmware_Tag (0) and then not Firmware_Tag (NVS + 2)
             and then not Firmware_Tag (NVS - 1) and then not Firmware_Tag (Unsigned_64'Last),
             "no other tag is firmware memory");
      Check (Admits (NVS, 16#3EB7_F000#, 12_208 * Page)
             and then Admits (Control, 16#0FFE_0000#, Page),
             "the laptop's NVS run and the q35 FACS page admitted");
      Check (not Admits (0, 16#3EB7_F000#, Page), "untagged device memory refused");
      Check (not Admits (NVS + 7, 16#3EB7_F000#, Page), "an unknown tag refused");
      Check (not Admits (NVS, 16#3EB7_F018#, Page)
             and then not Admits (NVS, 16#3EB7_F000#, Page + 1)
             and then not Admits (NVS, 16#3EB7_F000#, 0),
             "partial or empty pages refused");
      Check (not Admits (NVS, Page_Limit * Page, Page)
             and then not Admits (NVS, (Page_Limit - 1) * Page, 2 * Page),
             "pages past the physical limit refused");
   end;

   ---------------------------------------------------------------------------
   --  devmgr's ACPI memory claims (the firmware-memory windows).
   ---------------------------------------------------------------------------
   declare
      use Devmgr_Memory_Claims;
      Claims : Claim_Array (1 .. 16) := [others => <>];
      Count : Natural := 0;
      Result : Claim_Result;
      Old_Window_Limit : constant Positive := 8;
      --  The laptop's NVS run and the q35 FACS page.
      NVS_First : constant := 16#3EB7_F000#;
      NVS_Bytes : constant := 16#41B2_F000# - 16#3EB7_F000#;
      FACS_Page : constant := 16#0FFE_0000#;
   begin
      Claim_Pages (Claims, Count, NVS_First, NVS_Bytes, ACPI_Window, Result);
      Check (Result = Granted, "an NVS run claimed for ACPI");
      Claim_Pages (Claims, Count, FACS_Page, Page_Bytes, ACPI_Window, Result);
      Check (Result = Granted, "the FACS page claimed for ACPI");
      Claim_Pages (Claims, Count, NVS_First, NVS_Bytes, ACPI_Window, Result);
      Check (Result = Granted and then Count = 2, "the same window again granted, not duplicated");
      Claim_Pages (Claims, Count, NVS_First + Page_Bytes, Page_Bytes, ACPI_Window, Result);
      Check (Result /= Granted, "a different window inside an ACPI claim refused");
      Claim_Pages (Claims, Count, NVS_First - Page_Bytes, 2 * Page_Bytes, ACPI_Window, Result);
      Check (Result /= Granted, "a window overlapping an ACPI claim's first page refused");
      Claim_Pages (Claims, Count, 16#5000_0800#, Page_Bytes, ACPI_Window, Result);
      Check (Result /= Granted, "an unaligned window refused");
      Claim_Pages (Claims, Count, 16#5000_0000#, 16#800#, ACPI_Window, Result);
      Check (Result /= Granted, "a partial page refused");
      Claim_Pages (Claims, Count, 16#5000_0000#, 0, ACPI_Window, Result);
      Check (Result /= Granted, "an empty window refused");
      Claim_Pages (Claims, Count, Byte_Limit - Page_Bytes, 2 * Page_Bytes, ACPI_Window, Result);
      Check (Result /= Granted, "a window past the physical limit refused");
      Check (Clear_Of_ACPI (Claims, Count, 16#FEB0_0000#, 16#4000#), "a BAR elsewhere is clear");
      Check (Clear_Of_ACPI (Claims, Count, NVS_First - Page_Bytes, Page_Bytes),
             "the page just below an ACPI claim is clear");
      Check (Clear_Of_ACPI (Claims, Count, NVS_First + NVS_Bytes, Page_Bytes),
             "the page just above an ACPI claim is clear");
      Check (not Clear_Of_ACPI (Claims, Count, NVS_First - 1, 2),
             "a range one byte into an ACPI claim is not clear");
      Check (not Clear_Of_ACPI (Claims, Count, FACS_Page + 16#40#, 4),
             "bytes inside the FACS page are not clear");
      Check (not Clear_Of_ACPI (Claims, Count, 0, 16#1_0000_0000#),
             "a root spanning an ACPI claim is not clear");
      Check (not Clear_Of_ACPI (Claims, Count, 16#FEB0_0000#, 0)
             and then not Clear_Of_ACPI (Claims, Count, Unsigned_64'Last, 2),
             "empty and wrapping ranges are not clear");
      for K in 1 .. Old_Window_Limit + 1 loop
         Claim_Pages (Claims, Count, Unsigned_64 (16#6000_0000# + K * 16#10_0000#), Page_Bytes, ACPI_Window, Result);
         Check (Result = Granted, "memory claims past the old eight-window ceiling granted");
      end loop;
      Check (Count > Old_Window_Limit, "more than eight ACPI windows recorded");
      for K in 1 .. Claims'Length loop
         Claim_Pages (Claims, Count, Unsigned_64 (16#6000_0000# + K * 16#10_0000#), Page_Bytes, ACPI_Window, Result);
      end loop;
      Check (Count = Claims'Length, "the ledger fills");
      Claim_Pages (Claims, Count, 16#7000_0000#, Page_Bytes, ACPI_Window, Result);
      Check (Result = Needs_Room, "a full caller array requests room for a new window");
      Claim_Pages (Claims, Count, FACS_Page, Page_Bytes, ACPI_Window, Result);
      Check (Result = Granted, "a full ledger still re-grants a held window");
   end;

   ---------------------------------------------------------------------------
   --  The role-35 frontend badge's approval: the trusted plan AND its entry.
   ---------------------------------------------------------------------------
   declare
      use CuBit.Authority_Policy;
   begin
      Check (Startup_Approves (ACPI_Register_Frontend, True, True)
             and then not Startup_Approves (ACPI_Register_Frontend, True, False)
             and then not Startup_Approves (ACPI_Register_Frontend, False, True)
             and then not Bootstrap_Approves (ACPI_Register_Frontend, True),
             "frontend badge needs the plan's entry approval, not just the plan");
      Check (Startup_Approves (Disk_Installation, True, False)
             and then not Startup_Approves (Disk_Installation, False, True),
             "other startup-only authorities unchanged");
   end;

   ---------------------------------------------------------------------------
   --  devmgr's port claims.
   ---------------------------------------------------------------------------
   declare
      use Devmgr_Port_Claims;
      Claims : Claim_Array (1 .. 64) := [others => <>];
      Count : Natural := 0;
      Result : Take_Result;
      Driver_A : constant Owner_Word := 16#1_0000_0021#;
      Driver_B : constant Owner_Word := 16#2_0000_0021#;
      Old_Port_Limit : constant Positive := 48;
   begin
      Initialize (Claims, Count);
      Take (Claims, Count, 16#600#, 4, ACPI, No_Owner, Result);
      Check (Result = Granted, "ACPI window on free ports granted");
      Take (Claims, Count, 16#600#, 4, ACPI, No_Owner, Result);
      Check (Result = Granted, "the same ACPI window again (restarted backend) granted");
      Take (Claims, Count, 16#602#, 4, ACPI, No_Owner, Result);
      Check (Result /= Granted, "a different overlapping ACPI window refused");
      Take (Claims, Count, 16#603#, 1, Driver, Driver_A, Result);
      Check (Result /= Granted, "a driver claim inside an ACPI window refused");
      Take (Claims, Count, 16#70#, 2, ACPI, No_Owner, Result);
      Check (Result /= Granted, "an ACPI window over devmgr's CMOS ports refused");
      Take (Claims, Count, 16#3F8#, 1, ACPI, No_Owner, Result);
      Check (Result /= Granted, "an ACPI window over the kernel's console refused");
      Take (Claims, Count, 16#1F0#, 8, Driver, Driver_A, Result);
      Check (Result = Granted, "a driver claim elsewhere granted");
      Take (Claims, Count, 16#1F0#, 8, Driver, Driver_A, Result);
      Check (Result = Granted, "the same driver claim again granted");
      Take (Claims, Count, 16#1F4#, 1, ACPI, No_Owner, Result);
      Check (Result /= Granted, "an ACPI window over a driver's ports refused");
      Take (Claims, Count, 16#FFFF#, 2, ACPI, No_Owner, Result);
      Check (Result /= Granted, "a range past the port space refused");
      Take (Claims, Count, 16#800#, 0, ACPI, No_Owner, Result);
      Check (Result /= Granted, "an empty range refused");
      Take (Claims, Count, 16#2F0#, 8, Driver, Driver_B, Result);
      Check (Result = Granted, "another full-width driver identity recorded");
      for K in 1 .. Old_Port_Limit + 1 loop
         Take (Claims, Count, Unsigned_64 (16#2000# + 4 * K), 1, Driver, Driver_A, Result);
         Check (Result = Granted, "port claims past the old forty-eight ceiling granted");
      end loop;
      Check (Count > Old_Port_Limit, "more than forty-eight port claims recorded");
      for K in 1 .. Claims'Length loop
         Take (Claims, Count, Unsigned_64 (16#2000# + 4 * K), 1, Driver, Driver_A, Result);
      end loop;
      Take (Claims, Count, 16#9000#, 1, ACPI, No_Owner, Result);
      Check (Result = Needs_Room, "a full caller array requests room for a new port claim");
      Take (Claims, Count, 16#600#, 4, ACPI, No_Owner, Result);
      Check (Result = Granted, "a full ledger still re-grants a held ACPI window");
      Release_Owner (Claims, Count, Driver_A);
      Check ((for all K in 1 .. Count => Claims (K).Owner /= Driver_A)
        and then (for some K in 1 .. Count => Claims (K).Owner = Driver_B),
        "release compares the full opaque owner, including high bits");
      Take (Claims, Count, 16#2F0#, 1, ACPI, No_Owner, Result);
      Check (Result = Refused, "the other driver's ports remain claimed after release");
      Take (Claims, Count, 16#1F0#, 8, ACPI, No_Owner, Result);
      Check (Result = Granted, "an exited driver's ports can be claimed again");
      Take (Claims, Count, 16#600#, 1, Driver, Driver_A, Result);
      Check (Result = Refused, "owner release retains the ACPI window");
   end;

   if Failures = 0 then
      Put_Line ("acpi-hardware-windows: all checks passed");
   else
      Put_Line ("acpi-hardware-windows:" & Natural'Image (Failures) & " FAILED");
      raise Program_Error;
   end if;
end Main;
