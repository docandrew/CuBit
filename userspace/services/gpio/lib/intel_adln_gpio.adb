pragma Ada_2022;
package body Intel_ADLN_GPIO with SPARK_Mode is
   use type Word;
   use type Span;
   Pins_Per_Owner_Word : constant := 8;
   Owner_Field_Bits : constant := 4;
   Owner_Field_Mask : constant Word := 16#F#;
   Lock_Register_Pair_Bytes : constant Register_Offset := 2 * Register_Bytes;
   Debounce_Revision : constant Word := 16#94#;
   Revision_Shift : constant := 16;
   Two_Word_Bytes : constant Register_Offset := 2 * Register_Bytes;
   Four_Word_Bytes : constant Register_Offset := 4 * Register_Bytes;
   Metadata_End : constant array (Community) of Register_Offset :=
     [Community_0 => GPI_Enable_Base + 3 * Register_Bytes,
      Community_1 => GPI_Enable_Base + 5 * Register_Bytes,
      Community_4 => GPI_Enable_Base + 4 * Register_Bytes,
      Community_5 => GPI_Enable_Base + Register_Bytes];
   function First_At_Or_After (From : ACPI_Pin) return Pin_Search is
      Best : ACPI_Pin := ACPI_Pin'Last;
      Found : Boolean := False;
      Candidate : ACPI_Pin;
   begin
      for D of Groups loop
         if From < D.GPIO_First or else From - D.GPIO_First < ACPI_Pin (D.Size) then
            Candidate := (if From < D.GPIO_First then D.GPIO_First else From);
            if not Found or else Candidate < Best then Best := Candidate; Found := True; end if;
         end if;
         pragma Loop_Invariant (not Found or else (Best >= From and then Mapped_Pin (Best)));
      end loop;
      return (if Found then (True, Best) else (Found => False));
   end First_At_Or_After;
   function Locate (Pin : ACPI_Pin) return Location is
      Local : Group_Bit;
      Pad : Pad_Number;
      Slot_Offset : Register_Offset;
   begin
      for Group in Group_ID loop
         if Pin >= Groups (Group).GPIO_First and then Pin - Groups (Group).GPIO_First < ACPI_Pin (Groups (Group).Size) then
            Local := Group_Bit (Pin - Groups (Group).GPIO_First);
            Pad := Groups (Group).Pad_First + Pad_Number (Local);
            Slot_Offset := Register_Offset (Groups (Group).Slot) * Register_Bytes;
            return (Found => True, Pad => Pad, In_Community => Groups (Group).In_Community,
               Resource => Groups (Group).Resource, Group => Group, Bit_In_Group => Local,
               Community_Pad => Pad - Groups (Group).Community_First,
               Interrupt_Mask => Interfaces.Shift_Left (Word'(1), Natural (Local)),
               Owner_Register => PAD_OWN_Base + Register_Bytes *
                 (Register_Offset (Groups (Group).Owner_Prefix) + Register_Offset (Local) / Pins_Per_Owner_Word),
               Host_Register => HOSTSW_OWN_Base + Slot_Offset,
               Lock_Register => PADCFGLOCK_Base + Register_Offset (Groups (Group).Slot) * Lock_Register_Pair_Bytes,
               TX_Lock_Register => PADCFGLOCK_Base + Register_Offset (Groups (Group).Slot) * Lock_Register_Pair_Bytes + Register_Bytes,
               Status_Register => GPI_Status_Base + Slot_Offset,
               Enable_Register => GPI_Enable_Base + Slot_Offset,
               Owner_Field_Shift => Owner_Shift (Local mod Pins_Per_Owner_Word) * Owner_Field_Bits);
         end if;
         pragma Loop_Invariant
           (for all Prior in Group_ID'First .. Group =>
              not (Pin >= Groups (Prior).GPIO_First and then
                   Pin - Groups (Prior).GPIO_First < ACPI_Pin (Groups (Prior).Size)));
      end loop;
      return (Found => False);
   end Locate;
   function Plan
     (Controller_HID : String; Pin : ACPI_Pin; Revision, PADBAR : Word;
      Window_Bytes : Span) return Plan_Result is
      Selected : constant Location := Locate (Pin);
      Stride : Pad_Stride;
      subtype Stride_Bytes is Register_Offset range Two_Word_Bytes .. Four_Word_Bytes;
      Step : Stride_Bytes;
      Pad_Delta, Base : Register_Offset;
      Last : Span;
   begin
      if Controller_HID /= Target_HID then return (Status => Unsupported_Controller); end if;
      if not Selected.Found then return (Status => No_GPIO_Pin); end if;
      if Revision = Word'Last then return (Status => Community_Absent); end if;
      Base := Register_Offset (PADBAR);
      if Base mod Register_Bytes /= 0 or else Base < Metadata_End (Selected.In_Community)
      then return (Status => Invalid_PADBAR); end if;
      Stride := (if Interfaces.Shift_Right (Revision, Revision_Shift) >= Debounce_Revision
         then Four_Words else Two_Words);
      Step := (if Stride = Four_Words then Four_Word_Bytes else Two_Word_Bytes);
      Pad_Delta := Register_Offset (Selected.Community_Pad) * Step;
      if Base > Register_Offset'Last - Pad_Delta or else
        Base + Pad_Delta > Register_Offset'Last - (Step - Register_Bytes)
      then return (Status => Offset_Overflow); end if;
      Base := Base + Pad_Delta;
      Last := Span (Base) + Span (Step);
      if Window_Bytes < Span (Metadata_End (Selected.In_Community)) or else Last > Window_Bytes
      then return (Status => Window_Too_Small); end if;
      return (Status => Ready, Selected => Selected, Stride => Stride,
         PADCFG0 => Base, PADCFG1 => Base + Register_Bytes,
         PADCFG2 => (if Stride = Four_Words then Base + 2 * Register_Bytes else 0),
         Has_PADCFG2 => Stride = Four_Words, End_Exclusive => Last);
   end Plan;
   function Interrupt_Eligible
     (Selected : Location; Owner_Value, Host_Value, Lock_Value : Word) return Boolean is
   begin
      return Selected.Found and then
        (Interfaces.Shift_Right (Owner_Value, Natural (Selected.Owner_Field_Shift)) and Owner_Field_Mask) = 0
        and then (Host_Value and Selected.Interrupt_Mask) /= 0
        and then (Lock_Value and Selected.Interrupt_Mask) = 0;
   end Interrupt_Eligible;
end Intel_ADLN_GPIO;
