pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_FADT.Transactions;
with ACPI_Region_Policy;
with ACPI_Field_Hosted_Provider;
with AML_Field_Access;
procedure Confirmation_History_Tests is
   use type AML_Field_Access.Completion;
   type Bus is record
      Value : Unsigned_64 := 0;
      Calls : Natural := 0;
   end record;
   procedure Transact
     (Hardware : in out Bus; Space : ACPI_Region_Policy.Address_Space;
      Address : Unsigned_64; Width : ACPI_Region_Policy.Access_Width;
      For_Write : Boolean; Input : Unsigned_64; Output : out Unsigned_64;
      Completed : out Boolean) is
      pragma Unreferenced (Space, Width);
   begin
      if Address /= 16#4000# or else not For_Write then
         raise Program_Error with "unexpected diagnostic transaction";
      end if;
      Hardware.Value := Input; Hardware.Calls := Hardware.Calls + 1;
      Output := 0; Completed := False;
   end Transact;
   package P is new ACPI_Field_Hosted_Provider (Bus, Transact);
   IO : P.Context;
   C : constant ACPI_Region_Policy.Configuration :=
     (Tag => 3, Base => 16#4000#, Length => 1,
      Space => ACPI_Region_Policy.Memory_Space, Readable => True, Writable => True,
      Widths => [ACPI_FADT.Transactions.Byte_Access => True, others => False],
      Register_Authority => (Resource_ID => 16#8000_0000_0000_0001#,
        Readable => True, Writable => True, Delegable => False, Write_Mask => 16#FF#),
      Register_Offset => 0, Register_Width => ACPI_FADT.Transactions.Byte_Access);
   T : AML_Field_Access.Target;
   Reply : AML_Field_Access.Response;
   First, Second : AML_Field_Access.Observation;
   OK : Boolean;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Label_Text : String) is
   begin
      if not Condition then raise Program_Error with Label_Text; end if;
      Checks := Checks + 1;
   end Check;
begin
   P.Install (IO, 7, C, True, OK); Check (OK, "install first epoch");
   T := (Provider => P.Identity (IO), Endpoint => 7, Epoch => 1, Register => 16#8000_0000_0000_0001#);
   P.Dispatch (IO, (Destination => T, For_Write => True, Value => 1), Reply);
   Check (Reply.Status = AML_Field_Access.Indeterminate, "first unknown");
   First := P.Uncertain (IO);
   P.Confirm_Completion (IO, First, True, OK); Check (OK, "confirm first");
   P.Reconciled (IO, First, OK); Check (OK, "first initially reconciliable");
   P.Install (IO, 7, C, True, OK); Check (OK, "install second epoch");
   T.Epoch := 2;
   P.Dispatch (IO, (Destination => T, For_Write => True, Value => 2), Reply);
   Check (Reply.Status = AML_Field_Access.Indeterminate, "second unknown");
   Second := P.Uncertain (IO);
   P.Confirm_Completion (IO, Second, True, OK); Check (OK, "confirm second");
   -- This assertion is deliberately unchanged between exact old q9 and the
   -- new history provider. Old latest-only storage is expected to fail here.
   P.Reconciled (IO, First, OK); Check (OK, "older confirmation remains attributable");
   P.Reconciled (IO, Second, OK); Check (OK, "second confirmation remains attributable");
   Check (not P.Pending (IO), "provider pending clear");
   Check (P.Observe_Hardware (IO).Calls = 2 and then P.Observe_Hardware (IO).Value = 2,
          "effects retained without replay");
   Ada.Text_IO.Put_Line ("Confirmation history checks" & Checks'Image);
end Confirmation_History_Tests;
