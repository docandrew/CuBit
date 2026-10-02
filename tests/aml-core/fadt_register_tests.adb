with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_FADT; use ACPI_FADT;
with ACPI_FADT.Registers; use ACPI_FADT.Registers;
procedure FADT_Register_Tests is
   Checks : Natural := 0;
   Item : Description;
   R : Block_Result;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "FADT register" & Checks'Image; end if;
   end Check;
   Addresses : constant array (Positive range <>) of Unsigned_64 :=
     [0, 1, 16#FFFC#, 16#FFFD#, 16#FFFF#, 16#1_0000#, Unsigned_64'Last - 3, Unsigned_64'Last];
   Limits : constant array (Positive range <>) of Unsigned_64 := [0, 3, 16#FFFF#, Unsigned_64'Last];
   Spaces : constant array (Positive range <>) of Unsigned_8 := [0, 1, 2, 3, 127, 255];
   function Fits_Four (Base, Last : Unsigned_64) return Boolean is
     (Last >= 3 and then Base > 0 and then Base <= Last - 3);
begin
   Item.Revision := 6;
   for Kind in Block_Kind loop
      for Length in Unsigned_8 loop
         Item.Lengths (Kind) := Length;
         Item.Legacy (Kind) := 16#100#;
         Item.Extended (Kind) := (True, (Space => 0, Address => 16#200#, others => 255));
         declare
            Good : constant Boolean :=
              (case Kind is
                 when PM1A_Event | PM1B_Event => Length in 4 .. 254 and Length mod 2 = 0,
                 when PM1A_Control | PM1B_Control => Length in 2 .. 255,
                 when PM2_Control => Length in 1 .. 255,
                 when PM_Timer => Length = 4,
                 when GPE0 | GPE1 => Length in 2 .. 254 and Length mod 2 = 0);
         begin
            R := Describe (Item, Kind, Unsigned_64'Last);
            Check (R.Code = (if Good then Described else Bad_Length));
            if R.Code = Described then
               Check (R.Kind = Kind and R.Space = System_Memory and R.Base = 16#200#);
               Check (R.Length = Natural (Length) and R.Source = Extended_Address and not R.Extended_Rejected);
               if Is_Paired (Kind) then
                  Check (Enable_Base (R) = 16#200# + Unsigned_64 (Length) / 2);
               end if;
            end if;
            Item.Flags := 16#10_0000#;
            Check (Describe (Item, Kind, Unsigned_64'Last).Code = Hardware_Reduced);
            Item.Flags := 0;
            Item.Legacy (Kind) := 0; Item.Extended (Kind).Present := False;
            Check (Describe (Item, Kind, Unsigned_64'Last).Code = Absent);
         end;
      end loop;
   end loop;
   Item := (Revision => 6, others => <>);
   for Kind in Block_Kind loop
      Item.Lengths (Kind) := 4;
      for Space of Spaces loop
         for Base of Addresses loop
            for Last of Limits loop
               for Present in Boolean loop
                  for Legacy in 0 .. 1 loop
                     Item.Legacy (Kind) := Unsigned_32 (Legacy * 256);
                     Item.Extended (Kind) := (Present, (Space => Space, Address => Base, others => 255));
                     R := Describe (Item, Kind, Last);
                     declare
                        Extended_OK : constant Boolean := Present and Base /= 0 and
                          (if Space = 0 then Fits_Four (Base, Last)
                           elsif Space = 1 then Fits_Four (Base, 16#FFFF#) else False);
                     begin
                        if Extended_OK then
                           Check (R.Code = Described and then R.Source = Extended_Address and then R.Base = Base);
                        elsif Legacy /= 0 then
                           Check (R.Code = Described and then R.Source = Legacy_Address and then R.Base = 256);
                           Check (R.Extended_Rejected = (Present and Base /= 0));
                        else
                           Check (R.Code = (if Present and Base /= 0 then No_Usable_Address else Absent));
                        end if;
                        if R.Code = Described then
                           Check (R.Kind = Kind and R.Length = 4);
                           Check (Fits_Four (R.Base, (if R.Space = System_IO then 16#FFFF# else Last)));
                           if Is_Paired (Kind) then Check (Enable_Base (R) = R.Base + 2); end if;
                        end if;
                     end;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Item := (Revision => 4, Flags => 16#10_0000#, others => <>);
   Item.Legacy (PM_Timer) := 16#FFFC#; Item.Lengths (PM_Timer) := 4;
   Check (Describe (Item, PM_Timer, 0).Code = Described);
   Item.Legacy (PM_Timer) := 16#FFFD#;
   Check (Describe (Item, PM_Timer, Unsigned_64'Last).Code = No_Usable_Address);
   Item.Legacy (PM_Timer) := 1;
   Check (Describe (Item, PM_Timer, Unsigned_64'Last, 3).Code = No_Usable_Address);
   Check (Describe (Item, PM_Timer, Unsigned_64'Last, 4).Code = Described);
   Ada.Text_IO.Put_Line ("ACPI-FADT-REGISTERS: PASS" & Checks'Image);
end FADT_Register_Tests;
