pragma Ada_2022;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with ACPI;
with Firmware_Tables.MCFG;
with Firmware_Tables.Snapshots;
with Physical_Ranges;
with Physical_Range_Map;
with TextIO;
procedure Adapter_Tests is
   use type Firmware_Tables.Snapshots.Phase;
   package P renames Physical_Ranges;
   package MC renames Firmware_Tables.MCFG;
   Checks : Natural := 0;
   Buffer_Bytes : constant := MC.Fixed_Size + P.Max_PCI_Allocations * MC.Allocation_Size;
   procedure Check (Good : Boolean; Reason : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Reason; end if;
   end Check;
   procedure Reset (Allocations : Natural) is
   begin
      Physical_Range_Map.Kernel_Map := P.Empty_Map;
      TextIO.Captured := Ada.Strings.Unbounded.Null_Unbounded_String;
      ACPI.Configure (Allocations);
   end Reset;
   procedure Run (Complete : Boolean; Allocations, Copies : Natural;
                  Failure : String := "") is
      Cursor : P.Firmware_Cursor;
      Found : Boolean;
      Item : P.Firmware_Run;
      Omitted : Natural := 0;
      Log : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Physical_Range_Map.Seal_From_Boot;
      Check (Physical_Range_Map.Kernel_Map.Sealed, "always sealed");
      Check (Physical_Range_Map.Kernel_Map.Complete = Complete, "classification status");
      Check (Natural (Physical_Range_Map.Kernel_Map.PCI_Used) = Allocations,
             "stored allocations");
      Check (ACPI.Copy_Calls = Copies, "copy calls");
      if Copies /= 0 then
         Check (ACPI.Copy_First = 1 and ACPI.Copy_Length = ACPI.Advertised_Bytes,
                "exact copy bounds");
      end if;
      if Failure /= "" then
         Check (Ada.Strings.Fixed.Index
           (Ada.Strings.Unbounded.To_String (TextIO.Captured),
            "PCI metadata refused " & Failure & ASCII.LF) /= 0,
            "named refusal");
      end if;
      if not Complete then
         Cursor := P.First_Firmware_Cursor (Physical_Range_Map.Kernel_Map);
         P.Next_Firmware_Run (Physical_Range_Map.Kernel_Map, Cursor, Found, Item, Omitted);
         Check (not Found, "no root from an incomplete inventory");
      end if;
      Log := TextIO.Captured;
      Physical_Range_Map.Seal_From_Boot;
      Check (ACPI.Copy_Calls = Copies, "sealed adapter never copies again");
      Check (Ada.Strings.Unbounded.To_String (Log) =
               Ada.Strings.Unbounded.To_String (TextIO.Captured), "no duplicate seal log");
   end Run;
begin
   Reset (1); Run (True, 1, 1);
   Reset (P.Max_PCI_Allocations - 1); Run (True, P.Max_PCI_Allocations - 1, 1);
   Reset (P.Max_PCI_Allocations); Run (True, P.Max_PCI_Allocations, 1);
   Check (ACPI.Copy_Length = Buffer_Bytes, "exact static capacity accepted");
   Reset (P.Max_PCI_Allocations);
   ACPI.Advertised_Bytes := Buffer_Bytes + 1;
   Run (False, 0, 0, "METADATA_CAPACITY");
   Reset (P.Max_PCI_Allocations + 1);
   Run (False, 0, 0, "METADATA_CAPACITY");
   Reset (1); ACPI.Copy_Failure := True; Run (False, 0, 1, "COPY_REFUSED");
   Reset (1); ACPI.Corrupt_Data := True; Run (False, 0, 1, "POLICY_REFUSED");
   Reset (1); ACPI.Descriptors := 2; Run (False, 1, 1, "DUPLICATE_MCFG");
   Reset (1); ACPI.Metadata_Missing := True;
   Run (False, 0, 0, "DESCRIPTOR_UNAVAILABLE");
   for Mode in Firmware_Tables.Snapshots.Phase loop
      if Mode /= Firmware_Tables.Snapshots.Ready then
         Reset (1); ACPI.Snapshot_Mode := Mode;
         Run (False, 0, 0, "SNAPSHOT_UNAVAILABLE");
         Check (ACPI.Describe_Calls = 0, "unready snapshot never queried");
      end if;
   end loop;
   Reset (1); ACPI.Descriptors := 0; Run (True, 0, 0);
   Reset (1); ACPI.First_Name := "TEST"; Run (True, 0, 0);
   Reset (1); ACPI.First_Name := "TEST"; ACPI.Descriptors := 2; Run (True, 1, 1);
   Check (ACPI.Copy_Index = 2, "MCFG need not be first snapshot table");
   Ada.Text_IO.Put_Line ("static boot adapter checks:" & Checks'Image);
end Adapter_Tests;
