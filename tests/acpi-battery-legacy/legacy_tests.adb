with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Battery_Legacy_Information;
procedure Legacy_Tests is
   package F is new ACPI_Battery_Legacy_Information (8);
   use type F.Outcome, F.Field, F.Information, F.Unit_Kind;
   Numbers : F.Raw_Numbers :=
     [F.Power_Unit => 1, F.Design_Capacity => 42000, F.Last_Full_Charge => 40000,
      F.Technology => 1, F.Design_Voltage => 11000, F.Warning_Capacity => 2000,
      F.Low_Capacity => 1000, F.Granularity_Low => 1, F.Granularity_High => 10];
   Text : F.Texts := F.Empty_Texts;
   R : F.Decode_Result;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Label_Text : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Label_Text; end if; end Check;
   procedure Rejected (Code : F.Outcome; Item : F.Field) is
   begin
      R := F.Decode (Numbers, Text);
      Check (R.Status = Code and then R.Failure_Field = Item and then
        R.Value = F.Empty_Information, "exact failure and no partial output");
   end Rejected;
begin
   R := F.Decode (Numbers, Text);
   Check (R.Status = F.Decoded and then R.Value.Units = F.Charge_And_Current
     and then R.Value.Design.Value = 42000 and then R.Value.Voltage.Value = 11000,
     "legacy wire positions and units");
   for Item in F.Numeric_Field loop
      declare Saved : constant Unsigned_64 := Numbers (Item); begin
         Numbers (Item) := 16#1_0000_0000#; Rejected (F.Integer_Too_Wide, Item);
         Numbers (Item) := Saved;
      end;
   end loop;
   Numbers (F.Power_Unit) := 2; Rejected (F.Invalid_Units, F.Power_Unit);
   Numbers (F.Power_Unit) := 0; R := F.Decode (Numbers, Text);
   Check (R.Status = F.Decoded and then R.Value.Units = F.Energy_And_Power, "energy units");
   Numbers (F.Power_Unit) := 1;
   Numbers (F.Technology) := 2; Rejected (F.Invalid_Technology, F.Technology);
   Numbers (F.Technology) := 1;
   for Item in F.Design_Capacity .. F.Low_Capacity loop
      if Item /= F.Technology then
         declare Saved : constant Unsigned_64 := Numbers (Item); begin
            Numbers (Item) := F.Maximum_Capacity; R := F.Decode (Numbers, Text);
            Check (R.Status = F.Decoded, "maximum capacity/voltage");
            Numbers (Item) := F.Maximum_Capacity + 1; Rejected (F.Invalid_Capacity, Item);
            Numbers (Item) := F.Unknown_Word;
            if Item in F.Design_Capacity | F.Last_Full_Charge | F.Design_Voltage then
               R := F.Decode (Numbers, Text); Check (R.Status = F.Decoded, "optional unknown");
            else Rejected (F.Invalid_Capacity, Item); end if;
            Numbers (Item) := Saved;
         end;
      end if;
   end loop;
   Numbers (F.Design_Capacity) := F.Unknown_Word;
   R := F.Decode (Numbers, Text);
   Check (not R.Value.Design.Known and then R.Value.Design.Value = 0, "unknown distinct");
   Numbers (F.Design_Capacity) := 0; R := F.Decode (Numbers, Text);
   Check (R.Value.Design.Known and then R.Value.Design.Value = 0, "known zero");
   Numbers (F.Granularity_Low) := F.Unknown_Word;
   Numbers (F.Granularity_High) := F.Unknown_Word;
   R := F.Decode (Numbers, Text);
   Check (R.Status = F.Decoded and then R.Value.Granularity_One = Unsigned_32'Last
     and then R.Value.Granularity_Two = Unsigned_32'Last, "raw DWORD granularities");
   for Item in F.Text_Field loop
      Text (Item).Length := 1;
      for Code in Unsigned_8 loop
         Text (Item).Data (1) := Code; R := F.Decode (Numbers, Text);
         Check ((if Code in 1 .. 127 then R.Status = F.Decoded and then
           R.Value.Identifiers (Item).Data (1) = Code else
           R.Status = F.Invalid_Text and then R.Failure_Field = Item and then
           R.Value = F.Empty_Information), "ASCII boundary");
      end loop;
      Text (Item).Length := 0;
   end loop;
   Put_Line ("Legacy battery format checks" & Checks'Image);
end Legacy_Tests;
