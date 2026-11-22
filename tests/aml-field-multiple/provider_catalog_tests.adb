pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_FADT.Transactions;
with ACPI_Region_Policy;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_Field_Hosted_Provider;
procedure Provider_Catalog_Tests is
   use type AML_Field_Access.Completion;
   use type AML_Field_Access.Operation_ID;
   use type AML_Field_Access.Target;
   use type AML_Field_Access.Observation;
   type Values is array (Natural range 0 .. 1) of Unsigned_64;
   type Bus is record
      Registers : Values := [others => 0];
      Calls : Natural := 0;
   end record;
   type Test_Mode is (Normal, Unknown_Write, Wide_Read, Reenter);
   Mode : Test_Mode := Normal;
   Reentry_Denied : Boolean := False;
   procedure Transact
     (Hardware : in out Bus; Space : ACPI_Region_Policy.Address_Space;
      Address : Unsigned_64; Width : ACPI_Region_Policy.Access_Width;
      For_Write : Boolean; Input : Unsigned_64; Output : out Unsigned_64;
      Completed : out Boolean);
   package P is new ACPI_Field_Hosted_Provider (Bus, Transact, Maximum_Registers => 2, Maximum_Confirmations => 2);
   IO, Other, Collision : P.Context;
   First_ID : constant AML_Field_Access.Register_ID := 16#8000_0000_0000_0001#;
   Second_ID : constant AML_Field_Access.Register_ID := 16#FFFF_FFFF_FFFF_FFFE#;
   function Config (ID : AML_Field_Access.Register_ID; Offset : Unsigned_64)
     return ACPI_Region_Policy.Configuration is
     ((Tag => 9, Base => 16#1000#, Length => 2,
       Space => ACPI_Region_Policy.Memory_Space, Readable => True, Writable => True,
       Widths => [ACPI_FADT.Transactions.Byte_Access => True, others => False],
       Register_Authority => (Resource_ID => Unsigned_64 (ID), Readable => True,
         Writable => True, Delegable => False, Write_Mask => 16#FF#),
       Register_Offset => Offset, Register_Width => ACPI_FADT.Transactions.Byte_Access));
   function Target (C : P.Context; ID : AML_Field_Access.Register_ID; Epoch : Unsigned_64 := 1)
     return AML_Field_Access.Target is
     ((Provider => P.Identity (C), Endpoint => 7, Epoch => Epoch, Register => ID));
   A, B, Stale : AML_Field_Access.Target;
   Reply : AML_Field_Access.Response;
   Geometry : AML_Field_Layout.Register_Geometry;
   Saved, Wrong, First_Receipt : AML_Field_Access.Observation;
   C : ACPI_Region_Policy.Configuration;
   OK : Boolean;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Label_Text : String) is
   begin
      if not Condition then raise Program_Error with Label_Text; end if;
      Checks := Checks + 1;
   end Check;
   procedure Transact
     (Hardware : in out Bus; Space : ACPI_Region_Policy.Address_Space;
      Address : Unsigned_64; Width : ACPI_Region_Policy.Access_Width;
      For_Write : Boolean; Input : Unsigned_64; Output : out Unsigned_64;
      Completed : out Boolean) is
      pragma Unreferenced (Space, Width);
      Slot : Natural range 0 .. 1;
      Nested : AML_Field_Access.Response;
   begin
      -- The named ID never becomes an address or an array index.
      case Address is
         when 16#1000# => Slot := 0;
         when 16#1001# => Slot := 1;
         when others => raise Program_Error with "unexpected bus address";
      end case;
      Hardware.Calls := Hardware.Calls + 1;
      if Mode = Reenter then
         P.Dispatch (IO, (Destination => B, For_Write => True, Value => 16#EE#), Nested);
         Reentry_Denied := Nested.Status = AML_Field_Access.Denied;
      end if;
      if For_Write then Hardware.Registers (Slot) := Input; end if;
      Output := (if Mode = Wide_Read then 16#100# else Hardware.Registers (Slot));
      Completed := Mode /= Unknown_Write;
   end Transact;
begin
   C := Config (0, 0);
   P.Install (IO, 7, C, True, OK); Check (not OK, "zero ID rejected");
   P.Install (IO, 7, Config (First_ID, 0), True, OK); Check (OK, "first install");
   P.Install (IO, 7, Config (Second_ID, 1), True, OK); Check (OK, "second install");
   A := Target (IO, First_ID); B := Target (IO, Second_ID);
   P.Describe (IO, A, Geometry, OK); Check (OK and then Geometry.Byte_Offset = 0, "first lookup");
   P.Describe (IO, B, Geometry, OK); Check (OK and then Geometry.Byte_Offset = 1, "second high ID lookup");
   P.Install (IO, 7, Config (First_ID, 0), True, OK); Check (not OK, "active duplicate");
   P.Install (IO, 7, Config (3, 0), True, OK); Check (not OK, "capacity");
   P.Dispatch (IO, (Destination => A, For_Write => True, Value => 16#31#), Reply);
   Check (Reply.Status = AML_Field_Access.Completed, "first write");
   P.Dispatch (IO, (Destination => B, For_Write => True, Value => 16#72#), Reply);
   Check (Reply.Status = AML_Field_Access.Completed, "second write");
   Check (P.Observe_Hardware (IO).Registers = [16#31#, 16#72#], "one shared bus");
   P.Install (Other, 7, Config (First_ID, 0), True, OK); Check (OK, "foreign context");
   P.Describe (Other, A, Geometry, OK); Check (not OK, "foreign identity");
   Stale := B; Stale.Endpoint := 8;
   P.Describe (IO, Stale, Geometry, OK); Check (not OK, "wrong endpoint");
   Stale := B; Stale.Register := 2;
   P.Describe (IO, Stale, Geometry, OK); Check (not OK, "ID is not slot");
   Stale := A; Stale.Epoch := Unsigned_64'Last;
   P.Describe (IO, Stale, Geometry, OK); Check (not OK, "forged maximum epoch");
   P.Revoke_Register (IO, A, OK); Check (OK, "trusted revoke");
   P.Describe (IO, A, Geometry, OK); Check (not OK, "revoked");
   P.Install (IO, 7, Config (3, 0), True, OK); Check (not OK, "revoked slot not recycled");
   P.Install (IO, 8, Config (First_ID, 0), True, OK); Check (not OK, "endpoint fixed");
   P.Install (IO, 7, Config (First_ID, 0), True, OK); Check (OK, "same ID reinstall");
   P.Describe (IO, A, Geometry, OK); Check (not OK, "old epoch");
   A := Target (IO, First_ID, 2);
   P.Describe (IO, A, Geometry, OK); Check (OK, "new epoch");
   Mode := Reenter;
   P.Dispatch (IO, (Destination => A, For_Write => False, Value => 0), Reply);
   Check (Reply.Status = AML_Field_Access.Completed and then Reentry_Denied, "reentry guard");
   Check (P.Observe_Hardware (IO).Calls = 3, "no nested transaction");
   Mode := Unknown_Write;
   P.Dispatch (IO, (Destination => A, For_Write => True, Value => 16#44#), Reply);
   Check (Reply.Status = AML_Field_Access.Indeterminate and then P.Pending (IO), "unknown write");
   Saved := P.Uncertain (IO);
   Check (Saved.Item.Destination = A and then Saved.Operation = 3, "same ID ticket high water");
   Check (P.Observe_Hardware (IO).Registers = [16#44#, 16#72#], "effect retained");
   P.Dispatch (IO, (Destination => B, For_Write => True, Value => 16#99#), Reply);
   Check (Reply.Status = AML_Field_Access.Denied and then P.Observe_Hardware (IO).Calls = 4, "global no replay");
   P.Install (IO, 7, Config (3, 0), True, OK); Check (not OK, "pending install denied");
   P.Revoke_Register (IO, B, OK); Check (not OK, "pending revoke denied");
   Wrong := Saved; Wrong.Item.Destination := B;
   P.Confirm_Completion (IO, Wrong, True, OK); Check (not OK and then P.Pending (IO), "wrong register receipt");
   P.Confirm_Completion (IO, Saved, False, OK); Check (not OK, "no terminal evidence");
   P.Confirm_Completion (IO, Saved, True, OK); Check (OK and then not P.Pending (IO), "confirm exact");
   P.Confirm_Completion (IO, Saved, True, OK); Check (not OK, "confirmation replay");
   P.Reconciled (IO, Saved, OK); Check (OK, "provider confirmation fact");
   P.Describe (IO, A, Geometry, OK); Check (not OK, "unknown policy remains revoked");
   Mode := Normal;
   P.Dispatch (IO, (Destination => B, For_Write => False, Value => 0), Reply);
   Check (Reply.Status = AML_Field_Access.Completed and then Reply.Value = 16#72#, "unaffected register resumes");
   P.Install (IO, 7, Config (First_ID, 0), True, OK); Check (OK, "confirmed ID reinstall");
   A := Target (IO, First_ID, 3);
   Mode := Wide_Read;
   P.Dispatch (IO, (Destination => B, For_Write => False, Value => 0), Reply);
   Check (Reply.Status = AML_Field_Access.Failed_Completed and then not P.Pending (IO), "terminal fault distinct");
   Check (P.Completed_Fault (IO).Present, "fault observation");
   P.Describe (IO, A, Geometry, OK); Check (OK, "fault leaves other policy active");
   P.Describe (IO, B, Geometry, OK); Check (not OK, "fault selected revoke");
   Mode := Normal;
   C := Config (Second_ID, 1); C.Register_Authority.Writable := False; C.Register_Authority.Write_Mask := 0;
   P.Install (IO, 7, C, False, OK); Check (OK, "read-only reinstall");
   B := Target (IO, Second_ID, 2);
   P.Dispatch (IO, (Destination => B, For_Write => True, Value => 1), Reply);
   Check (Reply.Status = AML_Field_Access.Denied, "fresh rights enforced");
   P.Dispatch (IO, (Destination => B, For_Write => False, Value => 0), Reply);
   Check (Reply.Status = AML_Field_Access.Completed, "fresh read permitted");
   -- Two fresh entries issue equal numeric tickets; full observation is decisive.
   P.Install (Collision, 7, Config (First_ID, 0), True, OK); Check (OK, "collision first install");
   P.Install (Collision, 7, Config (Second_ID, 1), True, OK); Check (OK, "collision second install");
   Mode := Unknown_Write;
   P.Dispatch (Collision, (Destination => Target (Collision, First_ID), For_Write => True, Value => 1), Reply);
   First_Receipt := P.Uncertain (Collision); Check (P.Pending (Collision), "first collision receipt");
   P.Confirm_Completion (Collision, First_Receipt, True, OK); Check (OK, "first collision confirm");
   P.Dispatch (Collision, (Destination => Target (Collision, Second_ID), For_Write => True, Value => 2), Reply);
   Saved := P.Uncertain (Collision);
   Check (Saved.Operation = First_Receipt.Operation, "equal numeric tickets");
   P.Confirm_Completion (Collision, First_Receipt, True, OK); Check (not OK and then P.Pending (Collision), "ticket alone insufficient");
   P.Confirm_Completion (Other, Saved, True, OK); Check (not OK, "foreign confirmation");
   P.Confirm_Completion (Collision, Saved, True, OK); Check (OK and then not P.Pending (Collision), "selected slot confirmation");
   Check (P.Observe_Hardware (Collision).Registers = [1, 2], "both effects retained");
   P.Reconciled (Collision, First_Receipt, OK); Check (OK, "first arena receipt retained");
   P.Reconciled (Collision, Saved, OK); Check (OK, "second arena receipt retained");
   P.Install (Collision, 7, Config (First_ID, 0), True, OK); Check (OK, "reinstall with full history");
   Mode := Normal;
   P.Dispatch (Collision, (Destination => Target (Collision, First_ID, 2), For_Write => True, Value => 3), Reply);
   Check (Reply.Status = AML_Field_Access.Denied and then P.Observe_Hardware (Collision).Calls = 2,
          "history capacity denies before effects");
   P.Retire_Confirmation (Collision, First_Receipt, False, OK); Check (not OK, "false arena retirement");
   Wrong := Saved; Wrong.Item.Value := 99;
   P.Retire_Confirmation (Collision, Wrong, True, OK); Check (not OK, "wrong retirement");
   P.Reconciled (Collision, First_Receipt, OK); Check (OK, "failed retirement preserves first");
   P.Reconciled (Collision, Saved, OK); Check (OK, "failed retirement preserves second");
   P.Retire_Confirmation (Collision, First_Receipt, True, OK); Check (OK, "trusted first retirement");
   P.Reconciled (Collision, First_Receipt, OK); Check (not OK, "retired fact removed");
   P.Reconciled (Collision, Saved, OK); Check (OK, "other fact retained");
   Mode := Unknown_Write;
   P.Dispatch (Collision, (Destination => Target (Collision, First_ID, 2), For_Write => True, Value => 3), Reply);
   Check (Reply.Status = AML_Field_Access.Indeterminate and then P.Observe_Hardware (Collision).Calls = 3,
          "explicit later request uses released capacity");
   First_Receipt := P.Uncertain (Collision);
   P.Reconciled (Collision, Saved, OK); Check (OK, "historical fact while later pending");
   P.Retire_Confirmation (Collision, Saved, True, OK);
   Check (OK and then P.Pending (Collision) and then P.Uncertain (Collision) = First_Receipt,
          "retirement does not clear pending");
   P.Confirm_Completion (Collision, First_Receipt, True, OK); Check (OK, "reserved history survived retirement");
   P.Describe (Collision, Target (Collision, First_ID, 2), Geometry, OK);
   Check (not OK, "confirmation never reenables policy");
   Check (P.Observe_Hardware (Collision).Registers = [3, 2], "no implicit replay");

   Ada.Text_IO.Put_Line ("Provider catalog checks" & Checks'Image);
end Provider_Catalog_Tests;
