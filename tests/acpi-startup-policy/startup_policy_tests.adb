pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with AML_Events;
with ACPI_Device_Policy;
procedure Startup_Policy_Tests is
   use type Interfaces.Unsigned_64;
   use type ACPI_Device_Policy.Status_Decision;
   use type AML_Events.Timeout_Outcome;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   type Case_Record is record Value : Interfaces.Unsigned_64; Expected : AML_Events.Timeout; end record;
   Cases : constant array (Positive range <>) of Case_Record :=
     [(0,0),(1,1),(65534,65534),(65535,65535),(65536,0),(65537,1),
      (131071,65535),(16#FFFF_FFFF#,65535),(16#1_0000_0000#,0),
      (Interfaces.Unsigned_64'Last,65535)];
   Outcome : AML_Events.Timeout_Outcome;
begin
   for High in Boolean loop
      declare
         Extra : constant Interfaces.Unsigned_64 := (if High then 16#FFFF_FFFF_FFFF_FFF6# else 0);
      begin
         Check (ACPI_Device_Policy.Decide (Extra) = ACPI_Device_Policy.Prune_Subtree);
         Check (ACPI_Device_Policy.Decide (Extra or 1) = ACPI_Device_Policy.Initialize_And_Descend);
         Check (ACPI_Device_Policy.Decide (Extra or 8) = ACPI_Device_Policy.Descend_Only);
         Check (ACPI_Device_Policy.Decide (Extra or 9) = ACPI_Device_Policy.Initialize_And_Descend);
      end;
   end loop;
   Check (ACPI_Device_Policy.Decide (ACPI_Device_Policy.Missing_Status_Default) =
     ACPI_Device_Policy.Initialize_And_Descend);
   for Item of Cases loop
      Check (AML_Events.Normalize_Timeout (Item.Value) = Item.Expected);
   end loop;
   AML_Events.Unavailable_Timeout (AML_Events.Poll_Only, Outcome); Check (Outcome = AML_Events.Unavailable);
   AML_Events.Unavailable_Timeout (1, Outcome); Check (Outcome = AML_Events.Unavailable);
   AML_Events.Unavailable_Timeout (AML_Events.Wait_Forever, Outcome); Check (Outcome = AML_Events.Unavailable);
   Ada.Text_IO.Put_Line ("Startup policy checks" & Checks'Image);
end Startup_Policy_Tests;
