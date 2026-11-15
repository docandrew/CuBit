------------------------------------------------------------------------------
--  Hosted tests of legacy interrupt lines:
--  * Interrupt_Line_ABI: route and acknowledgement words;
--  * Interrupt_Line_Policy: the kernel's owners and mask-until-acknowledged
--    state machine (level lines held masked until every owner acknowledges;
--    edge lines never held; a level line left ownerless is masked);
--  * CuBit.ACPI_SCI_Route: the SCI's and ISA IRQs' GSI, trigger and
--    polarity from synthetic FADT/MADT bytes (q35's override, the ACPI
--    default, reserved flags, ambiguous and stolen overrides, several I/O
--    APICs, GSI 16+, hardware-reduced and malformed tables).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Interrupt_Line_ABI; use Interrupt_Line_ABI;
with Interrupt_Line_Policy; use Interrupt_Line_Policy;
with CuBit.ACPI_SCI_Route;
with ACPI_FADT;
with ACPI_SCI_Registers;
with ACPI_SCI_Quiesce;

procedure Main is
   package SR renames CuBit.ACPI_SCI_Route;
   use type SR.Refusal;

   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL " & Name);
      end if;
   end Check;

   ---------------------------------------------------------------------------
   --  Table builders
   ---------------------------------------------------------------------------
   Capacity : constant := 1024;
   type Table is record
      Bytes  : SR.Byte_Array (1 .. Capacity) := [others => 0];
      Length : Natural := 0;
   end record;

   procedure Put (T : in out Table; Offset : Natural; Value : Unsigned_64; Width : Positive) is
   begin
      for I in 0 .. Width - 1 loop
         T.Bytes (Offset + 1 + I) := Unsigned_8 (Shift_Right (Value, 8 * I) and 16#FF#);
      end loop;
   end Put;

   procedure Header (T : in out Table; Signature : String; Length : Natural) is
   begin
      for I in 0 .. 3 loop
         T.Bytes (1 + I) := Character'Pos (Signature (Signature'First + I));
      end loop;
      T.Length := Length;
      Put (T, SR.Length_Offset, Unsigned_64 (Length), 4);
   end Header;

   function FADT (SCI : Unsigned_16; Flags : Unsigned_32 := 0; Length : Natural := 276) return Table is
      T : Table;
   begin
      Header (T, SR.FADT_Signature, Length);
      if Length >= SR.SCI_INT_Offset + 2 then
         Put (T, SR.SCI_INT_Offset, Unsigned_64 (SCI), 2);
      end if;
      if Length >= SR.FADT_Flags_Offset + 4 then
         Put (T, SR.FADT_Flags_Offset, Unsigned_64 (Flags), 4);
      end if;
      return T;
   end FADT;

   function New_MADT return Table is
      T : Table;
   begin
      Header (T, SR.MADT_Signature, SR.MADT_Entries_Offset);
      return T;
   end New_MADT;

   procedure Grow (T : in out Table; By : Natural) is
   begin
      T.Length := T.Length + By;
      Put (T, SR.Length_Offset, Unsigned_64 (T.Length), 4);
   end Grow;

   procedure Add_LAPIC (T : in out Table; ID : Unsigned_8) is
      At_Byte : constant Natural := T.Length;
   begin
      Put (T, At_Byte, 0, 1);
      Put (T, At_Byte + 1, 8, 1);
      Put (T, At_Byte + 3, Unsigned_64 (ID), 1);
      Put (T, At_Byte + 4, 1, 4);
      Grow (T, 8);
   end Add_LAPIC;

   procedure Add_IO_APIC (T : in out Table; Base : Unsigned_32; Length : Natural := SR.IO_APIC_Bytes) is
      At_Byte : constant Natural := T.Length;
   begin
      Put (T, At_Byte, SR.IO_APIC_Type, 1);
      Put (T, At_Byte + 1, Unsigned_64 (Length), 1);
      Put (T, At_Byte + 4, 16#FEC0_0000#, 4);
      if Length >= SR.IO_APIC_Bytes then
         Put (T, At_Byte + SR.IO_APIC_GSI_Offset, Unsigned_64 (Base), 4);
      end if;
      Grow (T, Length);
   end Add_IO_APIC;

   procedure Add_Override
     (T : in out Table; Source : Unsigned_8; GSI : Unsigned_32; Flags : Unsigned_16;
      Bus : Unsigned_8 := SR.ISA_Bus)
   is
      At_Byte : constant Natural := T.Length;
   begin
      Put (T, At_Byte, SR.Override_Type, 1);
      Put (T, At_Byte + 1, SR.Override_Bytes, 1);
      Put (T, At_Byte + SR.Override_Bus_Offset, Unsigned_64 (Bus), 1);
      Put (T, At_Byte + SR.Override_Source_Offset, Unsigned_64 (Source), 1);
      Put (T, At_Byte + SR.Override_GSI_Offset, Unsigned_64 (GSI), 4);
      Put (T, At_Byte + SR.Override_Flags_Offset, Unsigned_64 (Flags), 2);
      Grow (T, SR.Override_Bytes);
   end Add_Override;

   --  q35 (QEMU's MADT): one I/O APIC at GSI 0, IRQ0 -> GSI 2, and IRQs 5,
   --  9, 10, 11 level / active high (flags 16#000D#).
   QEMU_Level_High : constant Unsigned_16 := 16#000D#;
   function Q35_MADT return Table is
      T : Table := New_MADT;
   begin
      Add_LAPIC (T, 0);
      Add_IO_APIC (T, 0);
      Add_Override (T, 0, 2, 0);
      Add_Override (T, 5, 5, QEMU_Level_High);
      Add_Override (T, 9, 9, QEMU_Level_High);
      Add_Override (T, 10, 10, QEMU_Level_High);
      Add_Override (T, 11, 11, QEMU_Level_High);
      return T;
   end Q35_MADT;

   function SCI_Route (F, M : Table) return SR.Resolution is
     (SR.Resolve_SCI (F.Bytes (1 .. F.Length), M.Bytes (1 .. M.Length)));

   function Refused (R : SR.Resolution; Why : SR.Refusal) return Boolean is
     (not R.Routed and then R.Why = Why);

   ---------------------------------------------------------------------------
   --  The ABI
   ---------------------------------------------------------------------------
   procedure Test_ABI is
      W : constant Unsigned_64 := Route_Word (3, Level, Active_Low);
   begin
      Check (CPU_Of (W) = 3 and then Trigger_Of (W) = Level and then Polarity_Of (W) = Active_Low
             and then Defined_Route (W), "route word round trip (level, low)");
      Check (Trigger_Of (Route_Word (0, Edge, Active_High)) = Edge
             and then Polarity_Of (Route_Word (0, Edge, Active_High)) = Active_High,
             "route word round trip (edge, high)");
      Check (not Defined_Route (16#800#), "undefined route bit refused");
      --  The owner form of ENABLE_IRQ (the acknowledgement): exactly the
      --  acknowledge bit, never a route, never mixed with one.
      Check (Is_Acknowledge_Route (Route_Acknowledge)
             and then not Defined_Route (Route_Acknowledge),
             "acknowledge word is the owner form, not a route");
      Check (not Is_Acknowledge_Route (Route_Acknowledge or Route_Level)
             and then not Defined_Route (Route_Acknowledge or Route_Level)
             and then not Is_Acknowledge_Route (Route_Word (0, Level, Active_Low)),
             "acknowledge mixed with route bits is neither");
      Check (not Is_Acknowledged (Outcome_Word (Route_Outcome'Last)),
             "refusal codes never carry the acknowledged flag");
      for O in Route_Outcome loop
         Check (Is_Outcome_Word (Outcome_Word (O)) and then To_Outcome (Outcome_Word (O)) = O,
                "outcome word round trip " & Route_Outcome'Image (O));
      end loop;
      Check (Outcome_Word (Routed) = 0, "Routed is 0");
      Check (Is_Acknowledged (Acknowledged_Word (0)) and then Count_Of (Acknowledged_Word (7)) = 7,
             "acknowledged word carries the count");
      Check (not Is_Acknowledged (15), "a refusal code is not an acknowledgement");
      Check (Next_Count (Delivery_Count'Last) = Delivery_Count'Last, "delivery count saturates");
      Check (Vector_Of (9) = 41 and then Input_Of (41) = 9, "GSI 9 is vector 41");
   end Test_ABI;

   ---------------------------------------------------------------------------
   --  The policy
   ---------------------------------------------------------------------------
   procedure Test_Policy is
      S : Line_State;
      Result : Route_Result;
      Action : Hardware_Action;
      Dispatch, Accepted : Boolean;
      A : constant Unsigned_64 := 11;
      B : constant Unsigned_64 := 12;
      C : constant Unsigned_64 := 13;
   begin
      --  A level line, one owner.
      Route (S, A, Level, Active_High, Result, Action);
      Check (Result = Routed and then Action = Program_Unmasked, "level route programs unmasked");
      Deliver (S, Action, Dispatch);
      Check (Action = Mask_Input and then Dispatch and then S.Held_Masked and then Is_Waiting (S, A),
             "level delivery masks and the owner waits");
      Deliver (S, Action, Dispatch);   --  a spurious repeat while masked
      Check (Action = Mask_Input and then S.Delivered = 2, "a second delivery keeps it masked");
      Acknowledge (S, B, Accepted, Action);
      Check (not Accepted and then Action = No_Change and then S.Held_Masked,
             "a non-owner's acknowledgement is refused");
      Acknowledge (S, A, Accepted, Action);
      Check (Accepted and then Action = Unmask_Input and then not S.Held_Masked,
             "the owner's acknowledgement unmasks");
      Acknowledge (S, A, Accepted, Action);
      Check (Accepted and then Action = No_Change, "acknowledging an open line changes nothing");

      --  A mode conflict.
      Route (S, B, Edge, Active_High, Result, Action);
      Check (Result = Mode_Conflict and then Action = No_Change and then not Is_Owner (S, B),
             "another trigger mode is a conflict");
      Route (S, B, Level, Active_Low, Result, Action);
      Check (Result = Mode_Conflict, "another polarity is a conflict");

      --  Shared: two owners; unmasked only when both acknowledged.
      Route (S, B, Level, Active_High, Result, Action);
      Check (Result = Routed and then Is_Owner (S, A) and then Is_Owner (S, B), "shared level line");
      Route (S, B, Level, Active_High, Result, Action);
      Check (Result = Routed, "routing an owner again is idempotent");
      Deliver (S, Action, Dispatch);
      Acknowledge (S, A, Accepted, Action);
      Check (Accepted and then Action = No_Change and then S.Held_Masked,
             "one of two acknowledgements keeps it masked");
      --  A third owner joining while masked does not unmask it.
      Route (S, C, Level, Active_High, Result, Action);
      Check (Result = Routed and then Action = Program_Masked and then not Is_Waiting (S, C),
             "an owner joining a held line programs it masked");
      Acknowledge (S, B, Accepted, Action);
      Check (Accepted and then Action = Unmask_Input, "the last waiter's acknowledgement unmasks");

      --  An owner leaving while waited on.
      Deliver (S, Action, Dispatch);
      Release (S, A, Action);
      Check (Action = No_Change and then S.Held_Masked, "others still waiting: stays masked");
      Release (S, B, Action);
      Check (Action = No_Change and then S.Held_Masked, "still one waiter");
      Acknowledge (S, C, Accepted, Action);
      Check (Accepted and then Action = Unmask_Input, "the remaining owner unmasks it");
      Deliver (S, Action, Dispatch);
      Release (S, C, Action);
      Check (Action = Mask_Input and then S.Held_Masked and then not Has_Owner (S),
             "an ownerless level line stays masked");
      Deliver (S, Action, Dispatch);
      Check (not Dispatch and then Action = Mask_Input, "nobody is told on an ownerless line");
      --  A new owner, even in another mode, opens it.
      Route (S, A, Edge, Active_High, Result, Action);
      Check (Result = Routed and then Action = Program_Unmasked and then not S.Held_Masked,
             "a new owner of an ownerless line reprograms it open");

      --  Edge: never held.
      Deliver (S, Action, Dispatch);
      Check (Action = No_Change and then Dispatch and then not S.Held_Masked
             and then not Is_Waiting (S, A), "edge delivery is not held");
      Acknowledge (S, A, Accepted, Action);
      Check (Accepted and then Action = No_Change, "edge acknowledgement changes nothing");
      Release (S, A, Action);
      Check (Action = No_Change and then not S.Held_Masked, "an ownerless edge line is left alone");

      --  The bounded owner set.
      declare
         T : Line_State;
      begin
         for I in 1 .. Maximum_Owners loop
            Route (T, Unsigned_64 (100 + I), Level, Active_Low, Result, Action);
         end loop;
         Check (Result = Routed, "Maximum_Owners owners fit");
         Route (T, 999, Level, Active_Low, Result, Action);
         Check (Result = Owners_Full and then Action = No_Change, "one more is Owners_Full");
      end;
   end Test_Policy;

   ---------------------------------------------------------------------------
   --  The SCI and ISA routes
   ---------------------------------------------------------------------------
   procedure Test_Routes is
      R : SR.Resolution;
      M : Table;
   begin
      --  q35: SCI_INT 9, overridden to GSI 9 level / active high.
      R := SCI_Route (FADT (9), Q35_MADT);
      Check (R.Routed and then R.Route.GSI = 9 and then R.Route.Overridden
             and then R.Route.Trigger = Level and then R.Route.Polarity = Active_High
             and then R.Vector = 41, "q35 SCI: GSI 9 level active high, vector 41");

      --  No override: GSI = SCI_INT, the SCI's default level / active low.
      M := New_MADT;
      Add_IO_APIC (M, 0);
      R := SCI_Route (FADT (9), M);
      Check (R.Routed and then R.Route.GSI = 9 and then not R.Route.Overridden
             and then R.Route.Trigger = Level and then R.Route.Polarity = Active_Low,
             "SCI without override: level active low");

      --  Conforming flags: the SCI's own default.
      M := New_MADT;
      Add_IO_APIC (M, 0);
      Add_Override (M, 9, 9, 0);
      R := SCI_Route (FADT (9), M);
      Check (R.Routed and then R.Route.Trigger = Level and then R.Route.Polarity = Active_Low,
             "SCI conforming flags: level active low");

      --  An override that moves the SCI to another GSI.
      M := New_MADT;
      Add_IO_APIC (M, 0);
      Add_Override (M, 9, 11, 16#000F#);
      R := SCI_Route (FADT (9), M);
      Check (R.Routed and then R.Route.GSI = 11 and then R.Vector = 43
             and then R.Route.Polarity = Active_Low, "SCI moved to GSI 11");

      --  ISA IRQ 1 (keyboard) on q35: identity, edge, high; IRQ 0 is moved.
      R := SR.Resolve_ISA (Q35_MADT.Bytes (1 .. Q35_MADT.Length), 1, SCI => False);
      Check (R.Routed and then R.Route.GSI = 1 and then R.Route.Trigger = Edge
             and then R.Route.Polarity = Active_High, "ISA IRQ 1 identity edge high");
      R := SR.Resolve_ISA (Q35_MADT.Bytes (1 .. Q35_MADT.Length), 2, SCI => False);
      Check (Refused (R, SR.GSI_Overridden_Away), "ISA IRQ 2's GSI belongs to IRQ 0");
      R := SR.Resolve_ISA (Q35_MADT.Bytes (1 .. Q35_MADT.Length), 0, SCI => False);
      Check (R.Routed and then R.Route.GSI = 2 and then R.Vector = 34, "ISA IRQ 0 is GSI 2");
      R := SR.Resolve_ISA (Q35_MADT.Bytes (1 .. Q35_MADT.Length), 12, SCI => False);
      Check (R.Routed and then R.Route.GSI = 12 and then R.Route.Trigger = Edge, "ISA IRQ 12 identity");
      --  A conforming ISA override is edge / active high.
      M := New_MADT;
      Add_IO_APIC (M, 0);
      Add_Override (M, 1, 1, 0);
      R := SR.Resolve_ISA (M.Bytes (1 .. M.Length), 1, SCI => False);
      Check (R.Routed and then R.Route.Trigger = Edge and then R.Route.Polarity = Active_High,
             "ISA conforming override: edge active high");

      --  Refusals.
      M := New_MADT;
      Add_IO_APIC (M, 0);
      Add_Override (M, 9, 9, 2#10#);
      Check (Refused (SCI_Route (FADT (9), M), SR.Reserved_Override_Flags), "reserved polarity");
      M := New_MADT;
      Add_IO_APIC (M, 0);
      Add_Override (M, 9, 9, 2#1000#);
      Check (Refused (SCI_Route (FADT (9), M), SR.Reserved_Override_Flags), "reserved trigger");
      M := New_MADT;
      Add_IO_APIC (M, 0);
      Add_Override (M, 9, 9, QEMU_Level_High);
      Add_Override (M, 9, 10, QEMU_Level_High);
      Check (Refused (SCI_Route (FADT (9), M), SR.Ambiguous_Override), "two overrides for the SCI");
      M := New_MADT;
      Add_IO_APIC (M, 0);
      Add_Override (M, 9, 20, QEMU_Level_High);
      R := SCI_Route (FADT (9), M);
      Check (Refused (R, SR.Beyond_Legacy_Lines) and then R.Seen.GSI = 20,
             "SCI on GSI 20 is past the legacy lines");
      M := New_MADT;
      Add_IO_APIC (M, 0);
      Add_IO_APIC (M, 8);
      Check (Refused (SCI_Route (FADT (9), M), SR.Other_IO_APIC), "a second I/O APIC serves GSI 9");
      M := New_MADT;
      Add_IO_APIC (M, 0);
      Add_IO_APIC (M, 24);
      Check (SCI_Route (FADT (9), M).Routed, "a second I/O APIC above the lines is fine");
      M := New_MADT;
      Add_IO_APIC (M, 24);
      Add_IO_APIC (M, 0);
      Check (Refused (SCI_Route (FADT (9), M), SR.First_IO_APIC_Not_GSI_0),
             "the driven (first) I/O APIC does not start at GSI 0");
      M := New_MADT;
      Add_LAPIC (M, 0);
      Check (Refused (SCI_Route (FADT (9), M), SR.No_IO_APIC), "no I/O APIC");
      M := New_MADT;
      Add_IO_APIC (M, 0);
      Add_Override (M, 9, 0, QEMU_Level_High);
      Check (Refused (SCI_Route (FADT (9), M), SR.Timer_Input), "GSI 0 is the timer input");
      Check (Refused (SCI_Route (FADT (9, Flags => 2 ** SR.HW_Reduced_ACPI_Bit), Q35_MADT),
                      SR.Hardware_Reduced), "hardware-reduced: no SCI");
      Check (Refused (SCI_Route (FADT (0), Q35_MADT), SR.No_SCI_Line), "SCI_INT 0");
      Check (Refused (SCI_Route (FADT (300), Q35_MADT), SR.Beyond_Legacy_Lines), "SCI_INT 300");
      Check (Refused (SCI_Route (FADT (9, Length => 40), Q35_MADT), SR.No_FADT), "short FADT");
      --  A revision-1 FADT (no flags field) still has its SCI.
      Check (SCI_Route (FADT (9, Length => 116), Q35_MADT).Routed, "a 116-byte FADT has SCI_INT");
      Check (SCI_Route (FADT (9, Length => 100), Q35_MADT).Routed, "an FADT without flags has SCI_INT");
      declare
         Empty : constant Table := (others => <>);
      begin
         Check (Refused (SCI_Route (FADT (9), Empty), SR.No_MADT), "no MADT");
         Check (Refused (SCI_Route (Empty, Q35_MADT), SR.No_FADT), "no FADT");
      end;

      --  Malformed MADTs.
      M := New_MADT;
      Add_IO_APIC (M, 0, Length => 6);
      Check (Refused (SCI_Route (FADT (9), M), SR.Malformed_MADT), "short I/O APIC entry");
      M := New_MADT;
      Add_IO_APIC (M, 0);
      Add_Override (M, 9, 9, QEMU_Level_High, Bus => 1);
      Check (Refused (SCI_Route (FADT (9), M), SR.Malformed_MADT), "override on a non-ISA bus");
      M := Q35_MADT;
      M.Bytes (SR.MADT_Entries_Offset + 2) := 0;   --  first entry length 0
      Check (Refused (SCI_Route (FADT (9), M), SR.Malformed_MADT), "zero-length entry");
      M := Q35_MADT;
      M.Bytes (SR.MADT_Entries_Offset + 2) := 200;   --  past the table
      Check (Refused (SCI_Route (FADT (9), M), SR.Malformed_MADT), "entry past the table");
      M := Q35_MADT;
      Check (Refused (SR.Resolve_SCI (FADT (9).Bytes (1 .. 276), M.Bytes (1 .. M.Length - 1)),
                      SR.Malformed_MADT), "a MADT cut short of its Length");
      M := Q35_MADT;
      M.Length := M.Length + 1;   --  a trailing byte: half an entry header
      Put (M, SR.Length_Offset, Unsigned_64 (M.Length), 4);
      Check (Refused (SCI_Route (FADT (9), M), SR.Malformed_MADT), "a trailing half entry");
      M := Q35_MADT;
      Put (M, SR.Length_Offset, 16#0100_0000#, 4);
      Check (Refused (SCI_Route (FADT (9), M), SR.Too_Large), "a MADT stating 16 MiB");
   end Test_Routes;

   ---------------------------------------------------------------------------
   --  Decode_Flags, exhaustively over the four bits.
   ---------------------------------------------------------------------------
   procedure Test_Flags is
      D : SR.Flag_Decoding;
   begin
      for Flags in Unsigned_16 range 0 .. 15 loop
         for SCI in Boolean loop
            D := SR.Decode_Flags (Flags, SCI);
            declare
               P : constant Unsigned_16 := Flags and 3;
               T : constant Unsigned_16 := Shift_Right (Flags, 2) and 3;
            begin
               Check (D.Defined = (P /= 2 and then T /= 2), "flags defined" & Flags'Image);
               if D.Defined then
                  Check ((D.Polarity = Active_Low) = (P = 3 or else (SCI and then P = 0)),
                         "polarity" & Flags'Image & SCI'Image);
                  Check ((D.Trigger = Level) = (T = 3 or else (SCI and then T = 0)),
                         "trigger" & Flags'Image & SCI'Image);
               end if;
            end;
         end loop;
      end loop;
   end Test_Flags;

   ---------------------------------------------------------------------------
   --  acpi.svc's SCI register layout (ACPI_SCI_Registers) from a decoded FADT
   ---------------------------------------------------------------------------
   procedure Test_Registers is
      package F renames ACPI_FADT;
      package RG renames ACPI_SCI_Registers;
      use type RG.Refusal, RG.GPE_Kind;
      SLP_Control_Method : constant Unsigned_32 := RG.Sleep_Button_Control_Method_Flag;
      HW_Reduced_Flag    : constant Unsigned_32 := 2 ** 20;
      Memory_Space       : constant Unsigned_8 := 0;

      --  q35: PM1a event 0x600+4, control 0x604+2, GPE0 0x620+16, no GPE1,
      --  the sleep button a control method.
      function Q35 return F.Description is
         D : F.Description;
      begin
         D.Revision := 3;
         D.Legacy (F.PM1A_Event) := 16#600#;   D.Lengths (F.PM1A_Event) := 4;
         D.Legacy (F.PM1A_Control) := 16#604#; D.Lengths (F.PM1A_Control) := 2;
         D.Legacy (F.GPE0) := 16#620#;         D.Lengths (F.GPE0) := 16;
         D.Flags := SLP_Control_Method;
         return D;
      end Q35;

      function Refused_With (D : F.Description; Why : RG.Refusal) return Boolean is
         R : constant RG.Layout_Result := RG.Resolve (D);
      begin
         return not R.Valid and then R.Why = Why;
      end Refused_With;

      D : F.Description := Q35;
      R : RG.Layout_Result := RG.Resolve (D);
   begin
      Check (R.Valid, "q35 layout resolves");
      if R.Valid then
         Check (R.Value.Events (RG.PM1a).Status = 16#600# and then R.Value.Events (RG.PM1a).Enable = 16#602#
                and then not R.Value.Events (RG.PM1b).Present, "PM1a status/enable halves");
         Check (R.Value.GPEs (RG.GPE0).Status = 16#620# and then R.Value.GPEs (RG.GPE0).Enable = 16#628#
                and then R.Value.GPEs (RG.GPE0).Registers = 8 and then not R.Value.GPEs (RG.GPE1).Present,
                "GPE0 status/enable halves, 8 registers");
         Check (R.Value.Control_Present and then R.Value.Control = 16#604#, "PM1a control (SCI_EN)");
         Check (R.Value.Fixed_Hardware (RG.Power_Button) and then not R.Value.Fixed_Hardware (RG.Sleep_Button),
                "fixed power button; control-method sleep button");
         declare
            L : constant RG.GPE_Location := RG.Locate (R.Value, 16#11#);
         begin
            Check (L.Present and then L.Block = RG.GPE0 and then L.Register = 2 and then L.Bit = 1,
                   "GPE 0x11 is GPE0 byte 2 bit 1");
         end;
         Check (not RG.Locate (R.Value, 64).Present, "GPE past GPE0 and no GPE1 is nowhere");
      end if;

      --  The IdeaPad's PCH shape: GPE0 0x1860+32 (128 GPEs), GPE1 at 128.
      D := Q35;
      D.Legacy (F.PM1A_Event) := 16#1800#; D.Legacy (F.PM1A_Control) := 16#1804#;
      D.Legacy (F.GPE0) := 16#1860#; D.Lengths (F.GPE0) := 32;
      D.Legacy (F.GPE1) := 16#1880#; D.Lengths (F.GPE1) := 4; D.GPE1_Base := 128;
      D.Flags := 0;
      R := RG.Resolve (D);
      Check (R.Valid and then R.Value.GPEs (RG.GPE0).Registers = 16
             and then R.Value.GPEs (RG.GPE1).Present and then R.Value.GPEs (RG.GPE1).First_GPE = 128
             and then R.Value.Fixed_Hardware (RG.Sleep_Button), "PCH layout with GPE1 at 128");
      if R.Valid then
         declare
            L : constant RG.GPE_Location := RG.Locate (R.Value, 134);
         begin
            Check (L.Present and then L.Block = RG.GPE1 and then L.Register = 0 and then L.Bit = 6,
                   "GPE 134 is GPE1 byte 0 bit 6");
         end;
         Check (RG.Locate (R.Value, 127).Present and then not RG.Locate (R.Value, 144).Present,
                "GPE0's last and past GPE1's last");
      end if;
      D.GPE1_Base := 100;
      R := RG.Resolve (D);
      Check (R.Valid and then not R.Value.GPEs (RG.GPE1).Present
             and then R.Value.GPE1_Refused = RG.GPE1_Overlaps_GPE0,
             "GPE1 overlapping GPE0 refused alone");
      D.GPE1_Base := 250;
      R := RG.Resolve (D);
      Check (R.Valid and then R.Value.GPE1_Refused = RG.GPE1_Past_Last_GPE, "GPE1 past GPE 255 refused alone");

      --  Refusals.
      D := Q35; D.Revision := 5; D.Flags := HW_Reduced_Flag;
      Check (Refused_With (D, RG.Hardware_Reduced), "hardware-reduced ACPI has no SCI");
      D := Q35; D.Legacy (F.PM1A_Event) := 0;
      Check (Refused_With (D, RG.No_PM1a_Event_Block), "no PM1a event block refused");
      D := Q35; D.Lengths (F.PM1A_Event) := 3;
      Check (Refused_With (D, RG.Malformed_Event_Block), "odd PM1 event length refused");
      D := Q35; D.Lengths (F.GPE0) := 15;
      Check (Refused_With (D, RG.Malformed_GPE_Block), "odd GPE0 length refused");
      D := Q35; D.Legacy (F.GPE0) := 16#FFF8#;
      Check (Refused_With (D, RG.Malformed_GPE_Block), "GPE0 past the port space refused");
      D := Q35;
      D.Extended (F.PM1A_Event) := (Present => True,
        Value => (Space => Memory_Space, Address => 16#FED0_0000#, others => 0));
      Check (Refused_With (D, RG.Block_Not_In_IO_Space), "PM1a in memory space refused");
      D := Q35;
      D.Extended (F.PM1A_Event) := (Present => True,
        Value => (Space => RG.System_IO_Space, Address => 16#B000#, others => 0));
      R := RG.Resolve (D);
      Check (R.Valid and then R.Value.Events (RG.PM1a).Status = 16#B000#, "X_PM1a in I/O space wins");
      --  The ACPI-mode decision (4.8.2.1): q35's SMI_CMD 0xB2, ACPI_ENABLE 2.
      declare
         use type RG.Mode_Decision;
         SCI_On  : constant Unsigned_16 := RG.SCI_Enable_Mask;
         SCI_Off : constant Unsigned_16 := 0;
      begin
         D := Q35; D.SMI_Command := 16#B2#; D.Enable := 2; D.Disable := 3;
         R := RG.Resolve (D);
         Check (R.Valid and then R.Value.SMI_Command_Present and then R.Value.SMI_Command = 16#B2#
                and then R.Value.ACPI_Enable = 2, "SMI_CMD and ACPI_ENABLE kept (never ACPI_DISABLE)");
         if R.Valid then
            Check (RG.Decide_Mode (R.Value, True, SCI_Off) = RG.Switch_To_ACPI_Mode,
                   "SCI_EN=0 with a transition: switch");
            Check (RG.Decide_Mode (R.Value, True, SCI_On) = RG.Already_ACPI_Mode,
                   "SCI_EN=1: no write");
            Check (RG.Decide_Mode (R.Value, True, SCI_On or 16#FFFE#) = RG.Already_ACPI_Mode
                   and then RG.Decide_Mode (R.Value, True, 16#FFFE#) = RG.Switch_To_ACPI_Mode,
                   "only SCI_EN (bit 0) decides");
            Check (RG.Decide_Mode (R.Value, False, SCI_Off) = RG.Control_Unreadable,
                   "unreadable PM1 control: no write");
         end if;
         D.Enable := 0;
         R := RG.Resolve (D);
         Check (R.Valid and then not R.Value.SMI_Command_Present
                and then RG.Decide_Mode (R.Value, True, SCI_Off) = RG.No_Mode_Transition,
                "ACPI_ENABLE zero: no transition");
         D.Enable := 2; D.SMI_Command := 0;
         R := RG.Resolve (D);
         Check (R.Valid and then RG.Decide_Mode (R.Value, True, SCI_Off) = RG.No_Mode_Transition,
                "SMI_CMD zero: no transition");
         D.SMI_Command := 16#1_0000#;
         R := RG.Resolve (D);
         Check (R.Valid and then not R.Value.SMI_Command_Present, "SMI_CMD past the port space: no transition");
         D := Q35; D.SMI_Command := 16#B2#; D.Enable := 2; D.Revision := 5; D.Flags := HW_Reduced_Flag;
         Check (Refused_With (D, RG.Hardware_Reduced), "hardware-reduced: no layout, so never a switch");
         D := Q35; D.SMI_Command := 16#B2#; D.Enable := 2; D.Legacy (F.PM1A_Control) := 0;
         R := RG.Resolve (D);
         Check (R.Valid and then RG.Decide_Mode (R.Value, True, SCI_Off) = RG.Control_Unreadable,
                "no PM1a control block: no write");
      end;
      Check (RG.Fixed_Mask (RG.Power_Button) = 2 ** 8 and then RG.Fixed_Mask (RG.Global_Lock_Release) = 2 ** 5,
             "PWRBTN bit 8, GBL bit 5");
   end Test_Registers;

   ---------------------------------------------------------------------------
   --  acpi.svc's Disarm (ACPI_SCI_Quiesce): what it writes and reports
   ---------------------------------------------------------------------------
   procedure Test_Quiesce is
      package Q renames ACPI_SCI_Quiesce;
      package RG renames ACPI_SCI_Registers;
      use type Q.Outcome, Q.Pending_Reason;
      PCIEXP_Wake_Disable : constant Unsigned_16 := 2 ** 14;
      Every_Fixed : constant Unsigned_16 := Q.Fixed_Mask_Of (Q.All_Fixed);
   begin
      Check (Every_Fixed = 2#0000_0111_0010_0001#, "fixed events are bits 0, 5, 8, 9, 10");
      Check (Q.Fixed_Mask_Of ([RG.Power_Button => True, others => False]) = 2 ** 8,
             "power button alone is bit 8");
      Check (Q.Disarmed_PM1_Enable (16#FFFF#) = (16#FFFF# and not Every_Fixed),
             "every fixed event disabled");
      Check ((Q.Disarmed_PM1_Enable (PCIEXP_Wake_Disable or 2 ** 8) and PCIEXP_Wake_Disable) /= 0,
             "PCIEXP_WAKE_DIS kept as found");
      Check (Q.Disarmed_PM1_Enable (0) = 0, "nothing enabled stays nothing");
      Check (Q.Report (True, True, True).State = Q.Quiesced, "all steps done: quiesced");
      declare
         R1 : constant Q.Result := Q.Report (False, False, False);
         R2 : constant Q.Result := Q.Report (True, False, True);
         R3 : constant Q.Result := Q.Report (True, True, False);
      begin
         Check (R1.State = Q.Pending and then R1.Why = Q.Fixed_Disable_Refused,
                "first failing step reported (fixed)");
         Check (R2.State = Q.Pending and then R2.Why = Q.GPE_Mask_Refused, "GPE mask refusal pending");
         Check (R3.State = Q.Pending and then R3.Why = Q.Acknowledge_Refused,
                "unacknowledged line is pending, never quiesced");
      end;
      --  Acknowledge_Only's classification: only the kernel's doorbell with
      --  the SCI line held is acknowledged; anything else is left alone.
      declare
         use type Q.Doorbell_Class;
      begin
         Check (Q.Classify (Doorbell_Label => True, From_Kernel => True, Line_Held => True)
                  = Q.SCI_Doorbell, "authenticated SCI doorbell is acknowledged");
         Check (Q.Classify (Doorbell_Label => False, From_Kernel => True, Line_Held => True)
                  = Q.Foreign, "another kernel event is foreign (untouched)");
         Check (Q.Classify (Doorbell_Label => True, From_Kernel => False, Line_Held => True)
                  = Q.Foreign, "a doorbell label from a process is foreign (untouched)");
         Check (Q.Classify (Doorbell_Label => True, From_Kernel => True, Line_Held => False)
                  = Q.No_Line, "a doorbell with no SCI line held is not acknowledged");
      end;
   end Test_Quiesce;

begin
   Test_ABI;
   Test_Policy;
   Test_Routes;
   Test_Flags;
   Test_Registers;
   Test_Quiesce;
   Put_Line ("interrupt-lines:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
