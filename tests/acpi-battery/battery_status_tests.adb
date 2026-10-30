pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with ACPI_Battery_Status;
procedure Battery_Status_Tests is
   package B renames ACPI_Battery_Status;
   use type B.Raw_Integer;
   use type B.Outcome;
   use type B.Field;
   use type B.Direction;
   use type B.Sample;
   use type B.Quantity_Value;
   Checks : Natural := 0;
   R : B.Decode_Result;
   procedure Check (OK : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Label_Text; end if;
   end Check;
   procedure Rejected (Code : B.Outcome; F : B.Field) is
   begin
      Check (R.Status = Code, "refusal kind");
      Check (R.Failure_Field = F, "refusal field");
      Check (R.Value = B.Empty_Sample, "no partial sample");
   end Rejected;
   Boundary : constant array (Positive range <>) of B.Raw_Integer :=
     [0, 1, 100, 16#7FFF_FFFE#, 16#7FFF_FFFF#, 16#8000_0000#,
      16#FFFF_FFFE#, 16#FFFF_FFFF#, 16#1_0000_0000#, B.Raw_Integer'Last];
   Field_Name : constant array (Positive range 1 .. 4) of B.Field :=
     [B.State_Field, B.Rate_Field, B.Capacity_Field, B.Voltage_Field];
begin
   --  Values returned by the checked-in ASL fixture under ACPICA. This checks
   --  the decoder against the fixture's semantics, not AML execution here.
   R := B.Decode ([10, 1234, 5678, 11000]);
   Check (R.Status = B.Decoded and R.Value.Flow = B.Charging
     and R.Value.Charge_Limiting and not R.Value.Critical, "ACPICA BAT0 flags");
   Check (R.Value.Rate.Value = 1234 and R.Value.Remaining_Capacity.Value = 5678
     and R.Value.Voltage.Value = 11000, "ACPICA BAT0 measurements");
   --  All low-byte flags, including charge limiting, reserved bits and both
   --  contradictory direction bits. Numeric samples remain distinct and known.
   for Flags in B.Raw_Integer range 0 .. 255 loop
      R := B.Decode ([Flags, 101, 202, 303]);
      if Flags mod 4 = 3 then
         Rejected (B.Conflicting_Directions, B.State_Field);
      else
         Check (R.Status = B.Decoded and R.Failure_Field = B.No_Field, "sampled flags");
         Check (R.Value.Flow = (case Flags mod 4 is
           when 1 => B.Discharging, when 2 => B.Charging, when others => B.Neither), "direction");
         Check (R.Value.Critical = ((Flags / 4) mod 2 = 1), "critical flag");
         Check (R.Value.Charge_Limiting = ((Flags / 8) mod 2 = 1), "charge limit flag");
         Check (R.Value.Reserved_State_Bits = Interfaces.Unsigned_32 (Flags / 16 * 16), "reserved retained");
         Check (R.Value.Rate.Known and R.Value.Rate.Value = 101, "known rate");
         Check (R.Value.Remaining_Capacity.Known and R.Value.Remaining_Capacity.Value = 202, "known capacity");
         Check (R.Value.Voltage.Known and R.Value.Voltage.Value = 303, "known voltage");
      end if;
   end loop;
   for F in Positive range 2 .. 4 loop
      for V of Boundary loop
         declare
            Raw : B.Raw_Package := [0, 10, 20, 30];
            M : B.Measurement;
         begin
            Raw (F) := V;
            R := B.Decode (Raw);
            if V > B.Unknown_Quantity then
               Rejected (B.Integer_Too_Wide, Field_Name (F));
            elsif V > B.Maximum_Quantity and V /= B.Unknown_Quantity then
               Rejected (B.Invalid_Measurement, Field_Name (F));
            else
               Check (R.Status = B.Decoded, "boundary sample");
               M := (case F is when 2 => R.Value.Rate,
                 when 3 => R.Value.Remaining_Capacity, when others => R.Value.Voltage);
               Check (M.Known = (V /= B.Unknown_Quantity), "unknown distinct");
               Check (M.Value = (if V = B.Unknown_Quantity then 0 else B.Quantity_Value (V)), "boundary exact");
            end if;
         end;
      end loop;
   end loop;
   --  Every declared flag independently at both DWORD ends; high bits must
   --  survive as diagnostics and cannot accidentally change known low flags.
   for Bit in 4 .. 31 loop
      R := B.Decode ([2 ** Bit, 0, 0, 0]);
      Check (R.Status = B.Decoded, "upper state bit accepted");
      Check (R.Value.Reserved_State_Bits = 2 ** Bit, "upper state bit retained");
      Check (R.Value.Flow = B.Neither and not R.Value.Critical
        and not R.Value.Charge_Limiting, "upper state bit independent");
   end loop;
   R := B.Decode ([16#FFFF_FFFC#, 0, 0, 0]);
   Check (R.Status = B.Decoded and R.Value.Critical and R.Value.Charge_Limiting
     and R.Value.Reserved_State_Bits = 16#FFFF_FFF0#, "maximum nonconflicting flags");
   R := B.Decode ([16#1_0000_0000#, 0, 0, 0]);
   Rejected (B.Integer_Too_Wide, B.State_Field);
   R := B.Decode ([B.Raw_Integer'Last, 0, 0, 0]);
   Rejected (B.Integer_Too_Wide, B.State_Field);
   --  Unknown quantities are valid untyped observations until _BIX supplies
   --  technology and units. Do not calculate percentage, presence or full.
   R := B.Decode ([8, B.Unknown_Quantity, B.Unknown_Quantity, B.Unknown_Quantity]);
   Check (R.Status = B.Decoded and R.Value.Charge_Limiting
     and R.Value.Flow = B.Neither, "steady charge limit");
   Check (not R.Value.Rate.Known and not R.Value.Remaining_Capacity.Known
     and not R.Value.Voltage.Known, "all unknown");
   R := B.Decode ([0, 0, 0, 0]);
   Check (R.Status = B.Decoded and R.Value.Rate.Known
     and R.Value.Remaining_Capacity.Known and R.Value.Voltage.Known, "zero is known");
   for Count in Natural range 0 .. 7 loop
      if Count /= B.Expected_Elements then
         declare V : constant B.Raw_Package (1 .. Count) := (others => 0); begin
            R := B.Decode (V); Rejected (B.Wrong_Element_Count, B.No_Field);
         end;
      end if;
   end loop;
   declare
      High : constant B.Raw_Package (Positive'Last - 3 .. Positive'Last) := [1, 4, 5, 6];
      Shifted : constant B.Raw_Package (50 .. 53) := [1, 4, 5, 6];
   begin
      R := B.Decode (High);
      Check (R.Status = B.Decoded and R.Value.Rate.Value = 4, "high bound exact package");
      Check (B.Decode (Shifted).Value = R.Value, "shifted bounds same sample");
   end;
   --  Dense independent sweep across the quantity boundary in every slot.
   for Delta_Value in B.Raw_Integer range 0 .. 1024 loop
      for F in Positive range 2 .. 4 loop
         declare Raw : B.Raw_Package := [0, 0, 0, 0]; begin
            Raw (F) := B.Maximum_Quantity - Delta_Value;
            R := B.Decode (Raw);
            Check (R.Status = B.Decoded, "below boundary accepted");
            Raw (F) := B.Maximum_Quantity + 1 + Delta_Value;
            R := B.Decode (Raw);
            Rejected (B.Invalid_Measurement, Field_Name (F));
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("battery status checks" & Natural'Image (Checks));
end Battery_Status_Tests;
