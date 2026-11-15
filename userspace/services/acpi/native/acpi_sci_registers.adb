pragma Ada_2022;
package body ACPI_SCI_Registers with SPARK_Mode is
   package F renames ACPI_FADT;

   -- One FADT block in the I/O space: its 64-bit address when present and
   -- nonzero (refused outside the I/O space), else its 32-bit one; the
   -- FADT's length field for it. Absent when address or length is zero.
   procedure Select_Block
     (Item : F.Description; Kind : F.Block_Kind;
      Present : out Boolean; First : out Port_Number; Length : out Natural;
      Why : out Refusal)
     with Post => (if Present then Why = No_Refusal and then Length > 0
                     and then Natural (First) + Length <= Port_Space)
   is
      Address : Unsigned_64;
      Wide : constant F.Optional_Address := Item.Extended (Kind);
   begin
      Present := False; First := 0; Why := No_Refusal;
      Length := Natural (Item.Lengths (Kind));
      if Wide.Present and then Wide.Value.Address /= 0 then
         if Wide.Value.Space /= System_IO_Space then
            Why := Block_Not_In_IO_Space; return;
         end if;
         Address := Wide.Value.Address;
      else
         Address := Unsigned_64 (Item.Legacy (Kind));
      end if;
      if Address = 0 or else Length = 0 then
         return;
      elsif Address >= Port_Space or else Unsigned_64 (Length) > Port_Space - Address then
         Why := (if Kind in F.GPE0 | F.GPE1 then Malformed_GPE_Block else Malformed_Event_Block);
         return;
      end if;
      First := Port_Number (Address);
      Present := True;
   end Select_Block;

   procedure Event_Block
     (Item : F.Description; Kind : F.Block_Kind; Block : out PM1_Block; Why : out Refusal)
   is
      Present : Boolean;
      First : Port_Number;
      Length : Natural;
   begin
      Block := (others => <>);
      Select_Block (Item, Kind, Present, First, Length, Why);
      if not Present then
         return;
      elsif Length mod 2 /= 0 or else Length < Minimum_PM1_Event_Bytes then
         Why := Malformed_Event_Block; return;
      end if;
      Block := (Present => True, Status => First,
                Enable => First + Port_Number (Length / 2));
   end Event_Block;

   procedure GPE_Register_Block
     (Item : F.Description; Kind : F.Block_Kind; Block : out GPE_Block; Why : out Refusal)
   is
      Present : Boolean;
      First : Port_Number;
      Length : Natural;
   begin
      Block := (others => <>);
      Select_Block (Item, Kind, Present, First, Length, Why);
      if not Present then
         return;
      elsif Length mod 2 /= 0 or else Length / 2 > Maximum_GPE_Registers then
         Why := Malformed_GPE_Block; return;
      end if;
      Block := (Present => True, Status => First,
                Enable => First + Port_Number (Length / 2),
                Registers => Length / 2, First_GPE => 0);
   end GPE_Register_Block;

   function Resolve (Item : F.Description) return Layout_Result is
      Result : Layout;
      Why : Refusal;
      Control_Present : Boolean;
      Control_Length : Natural;
      Control_Why : Refusal;
      GPE0_Numbers : Natural;
   begin
      if F.Hardware_Reduced (Item) then
         return (Valid => False, Why => Hardware_Reduced);
      end if;
      Event_Block (Item, F.PM1A_Event, Result.Events (PM1a), Why);
      if Why /= No_Refusal then
         return (Valid => False, Why => Why);
      elsif not Result.Events (PM1a).Present then
         return (Valid => False, Why => No_PM1a_Event_Block);
      end if;
      -- PM1b is optional; a malformed one is refused like PM1a.
      Event_Block (Item, F.PM1B_Event, Result.Events (PM1b), Why);
      if Why /= No_Refusal then
         return (Valid => False, Why => Why);
      end if;
      GPE_Register_Block (Item, F.GPE0, Result.GPEs (GPE0), Why);
      if Why /= No_Refusal then
         return (Valid => False, Why => Why);
      end if;
      GPE0_Numbers := Result.GPEs (GPE0).Registers * GPE_Register_Bits;
      GPE_Register_Block (Item, F.GPE1, Result.GPEs (GPE1), Why);
      if Why /= No_Refusal then
         Result.GPE1_Refused := Why;
         Result.GPEs (GPE1) := (others => <>);
      elsif Result.GPEs (GPE1).Present then
         if Natural (Item.GPE1_Base) < GPE0_Numbers then
            Result.GPE1_Refused := GPE1_Overlaps_GPE0;
            Result.GPEs (GPE1) := (others => <>);
         elsif Natural (Item.GPE1_Base) + Result.GPEs (GPE1).Registers * GPE_Register_Bits
                 > GPE_Count
         then
            Result.GPE1_Refused := GPE1_Past_Last_GPE;
            Result.GPEs (GPE1) := (others => <>);
         else
            Result.GPEs (GPE1).First_GPE := GPE_Number (Item.GPE1_Base);
         end if;
      end if;
      Select_Block (Item, F.PM1A_Control, Control_Present, Result.Control, Control_Length, Control_Why);
      Result.Control_Present := Control_Present and then Control_Length >= PM1_Register_Bytes;
      -- Never on a hardware-reduced platform: refused above, before this.
      if Item.SMI_Command /= 0 and then Item.SMI_Command < Port_Space
        and then Item.Enable /= 0
      then
         Result.SMI_Command_Present := True;
         Result.SMI_Command := Port_Number (Item.SMI_Command);
         Result.ACPI_Enable := Item.Enable;
      end if;
      Result.Fixed_Hardware :=
        [Power_Button => (Item.Flags and Power_Button_Control_Method_Flag) = 0,
         Sleep_Button => (Item.Flags and Sleep_Button_Control_Method_Flag) = 0,
         Timer_Overflow | Global_Lock_Release | RTC_Alarm => True];
      return (Valid => True, Value => Result);
   end Resolve;

   function Locate (Item : Layout; GPE : GPE_Number) return GPE_Location is
   begin
      for Kind in GPE_Kind loop
         declare
            Block : constant GPE_Block := Item.GPEs (Kind);
         begin
            if Block.Present and then GPE >= Block.First_GPE
              and then GPE - Block.First_GPE < Block.Registers * GPE_Register_Bits
            then
               return (Present => True, Block => Kind,
                       Register => (GPE - Block.First_GPE) / GPE_Register_Bits,
                       Bit => (GPE - Block.First_GPE) mod GPE_Register_Bits);
            end if;
         end;
      end loop;
      return (Present => False);
   end Locate;
end ACPI_SCI_Registers;
