pragma Ada_2022;
with ACPI_FADT.Registers;
package ACPI_FADT.Sleep_Registers with SPARK_Mode, Pure is
   type Register_Model is (No_Sleep_Registers, PM1A_Only, PM1A_And_B, Hardware_Reduced);
   type Selection_Status is (Selected, Invalid_Fixed_Blocks, Inconsistent_Fixed_Blocks, Invalid_Sleep_Control);
   type Selection is record
      Code : Selection_Status := Invalid_Fixed_Blocks;
      Model : Register_Model := No_Sleep_Registers;
      A, B : Registers.Status := Registers.Absent;
      Legacy_Fallback_A, Legacy_Fallback_B : Boolean := False;
      Low_Power_Idle : Boolean := False;
      Control : Optional_Address;
   end record;
   use type Registers.Status;
   Low_Power_Idle_Flag : constant Unsigned_32 := 16#20_0000#;
   ACPI_5_Revision : constant := 5;
   Byte_Width : constant := 8;
   -- Declarative relation for this classifier under Registers.Describe's
   -- interface. No byte parser,physical backing or backend authority proof.
   function Descriptions_Agree (Item : Description; R : Selection) return Boolean is
     (R.Low_Power_Idle = (Item.Revision >= ACPI_5_Revision and then (Item.Flags and Low_Power_Idle_Flag) /= 0)
      and then (if ACPI_FADT.Hardware_Reduced (Item) then
         R.A = Registers.Hardware_Reduced and then R.B = Registers.Hardware_Reduced
         and then R.Control = Item.Sleep_Control
         and then not R.Legacy_Fallback_A and then not R.Legacy_Fallback_B
         and then R.Code =
           (if not Item.Sleep_Control.Present or else Item.Sleep_Control.Value.Address = 0 then Selected
            elsif Item.Sleep_Control.Value.Space in 0 | 1 | 2
              and then Item.Sleep_Control.Value.Width = Byte_Width
              and then Item.Sleep_Control.Value.Bit_Offset = 0
              and then Item.Sleep_Control.Value.Access_Size in 0 | 1 then Selected
            else Invalid_Sleep_Control)
         and then R.Model =
           (if R.Code = Selected and then Item.Sleep_Control.Present
              and then Item.Sleep_Control.Value.Address /= 0 then Hardware_Reduced else No_Sleep_Registers)
       else
         R.A = Registers.Describe (Item, PM1A_Control, Unsigned_64'Last).Code
         and then R.B = Registers.Describe (Item, PM1B_Control, Unsigned_64'Last).Code
         and then not R.Control.Present
         and then R.Code =
           (if R.A not in Registers.Described | Registers.Absent
              or else R.B not in Registers.Described | Registers.Absent then Invalid_Fixed_Blocks
            elsif R.A = Registers.Absent and then R.B = Registers.Described then Inconsistent_Fixed_Blocks
            else Selected)
         and then R.Model =
           (if R.Code = Selected and then R.A = Registers.Described then
              (if R.B = Registers.Described then PM1A_And_B else PM1A_Only)
            else No_Sleep_Registers)
         and then (if Registers.Describe (Item, PM1A_Control, Unsigned_64'Last).Code = Registers.Described then
            R.Legacy_Fallback_A = Registers.Describe (Item, PM1A_Control, Unsigned_64'Last).Extended_Rejected
            else not R.Legacy_Fallback_A)
         and then (if Registers.Describe (Item, PM1B_Control, Unsigned_64'Last).Code = Registers.Described then
            R.Legacy_Fallback_B = Registers.Describe (Item, PM1B_Control, Unsigned_64'Last).Extended_Rejected
            else not R.Legacy_Fallback_B)))
     with Ghost, Global => null;
   -- Only the descriptive used-field model,not a backend,PCI target decode,
   -- physical reservation,address admission,register access or transition.
   -- HW-reduced mode ignores legacy PM1 blocks. An absent sleep control means
   -- unused Sx return data; present control must describe natural byte geometry.
   function Describe (Item : Description) return Selection
     with Global => null, Post => Descriptions_Agree (Item, Describe'Result);
   type Source_Status is (Not_Ready, Missing_FADT, Multiple_FADT, Invalid_FADT, Classified);
   type Source_Report is record
      Status : Source_Status := Not_Ready;
      Table : Natural := 0;
      Matches : Natural := 0;
      Data : Selection;
   end record;
end ACPI_FADT.Sleep_Registers;
