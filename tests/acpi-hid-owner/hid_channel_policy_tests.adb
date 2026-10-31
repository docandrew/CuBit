pragma Ada_2022;
with ACPI_HID_I2C_Candidate;
with ACPI_HID_I2C_Channels;
with I2C_Channel_Badges;
with GPIO_Channel_Badges;
procedure HID_Channel_Policy_Tests (Checks : out Natural) is
   package C is new ACPI_HID_I2C_Candidate;
   package P is new ACPI_HID_I2C_Channels (C);
   package I renames I2C_Channel_Badges;
   package G renames GPIO_Channel_Badges;
   use type P.Outcome, I.Target, G.Interrupt_Input, C.Candidate;
   use type I.Encode_Status, I.Decode_Status, G.Encode_Status, G.Decode_Status;
   use type C.S.Address_Mode, C.S.Slave_Address, C.G.Interrupt_Mode, C.G.Interrupt_Polarity;
   Facts : C.Candidate;
   procedure Check (Good : Boolean; Why : String) is
   begin
      if not Good then raise Program_Error with Why; end if;
      Checks := Checks + 1;
   end Check;
   procedure Expect (Wanted : P.Outcome) is
      Result : constant P.Preparation := P.Prepare (Facts);
   begin
      Check (Result.Status = Wanted, "typed channel preparation outcome");
      if Result.Status = P.Prepared then
         Check (Result.Facts = Facts, "all copied metadata retained without normalization");
         Check (I.Valid (Result.I2C) and then G.Valid (Result.GPIO), "channel codec domains");
         declare
            Bus : constant I.Encoding := I.Encode (Result.I2C);
            Pin : constant G.Encoding := G.Encode (Result.GPIO);
         begin
            Check (Bus.Status = I.Encoded and then Pin.Status = G.Encoded, "typed channel descriptions encode");
            declare
               B : constant I.Decoding := I.Decode (Bus.Badge);
               N : constant G.Decoding := G.Decode (Pin.Badge);
            begin
               Check (B.Status = I.Decoded and then B.Value = Result.I2C,
                 "exact I2C mode/address/full speed roundtrip");
               Check (N.Status = G.Decoded and then N.Value = Result.GPIO,
                 "exact GPIO pin/trigger/polarity roundtrip");
            end;
         end;
      end if;
   end Expect;
   procedure Normal is
   begin
      Facts := (others => <>);
      Facts.I2C.Address := 16#15#; Facts.I2C.Connection_Speed := 400_000;
      Facts.Pin := 16#2A#; Facts.Interrupt.Polarity := C.G.Active_Low;
      Facts.Descriptor_Register := 16#FFFF#;
      Facts.I2C_Shared := True; Facts.Interrupt_Shared := True;
      Facts.Interrupt.Wake_Capable := True;
      Facts.I2C_Vendor.Length := 3; Facts.I2C_Vendor.Data (1 .. 3) := [1,2,3];
      Facts.GPIO_Vendor.Length := 2; Facts.GPIO_Vendor.Data (1 .. 2) := [16#A5#,16#5A#];
      Facts.Configuration := 2; Facts.Drive := 500; Facts.Debounce := 250;
   end Normal;
   type Pins is array (Positive range <>) of C.G.Pin_Number;
   Pin_Cases : constant Pins := [0,1,16#2A#,16#FFFE#];
   type Addresses is array (Positive range <>) of C.S.Slave_Address;
   Address_Cases : constant Addresses := [0,127,128,1023];
   type Speeds is array (Positive range <>) of C.S.Rate;
   Speed_Cases : constant Speeds := [1,100_000,400_000,1_000_000,C.S.Rate'Last];
begin
   Checks := 0; Normal; Expect (P.Prepared);
   for Addressing in C.S.Address_Mode loop
      for Address of Address_Cases loop
         for Speed of Speed_Cases loop
            Normal; Facts.I2C.Addressing := Addressing;
            Facts.I2C.Address := Address; Facts.I2C.Connection_Speed := Speed;
            Expect (if Addressing = C.S.Seven_Bit and then Address > 127
              then P.Invalid_I2C_Target else P.Prepared);
         end loop;
      end loop;
   end loop;
   for Pin of Pin_Cases loop
      for Trigger in C.G.Interrupt_Mode loop
         for Active in C.G.Interrupt_Polarity loop
            Normal; Facts.Pin := Pin; Facts.Interrupt.Mode := Trigger;
            Facts.Interrupt.Polarity := Active;
            Expect (if Trigger = C.G.Level_Triggered and then Active = C.G.Active_Both
              then P.Invalid_GPIO_Input else P.Prepared);
         end loop;
      end loop;
   end loop;
   Normal; Facts.Pin := C.G.No_Pin; Expect (P.Invalid_GPIO_Input);
   Normal; Facts.I2C.Connection_Speed := 0; Expect (P.Invalid_I2C_Target);
   Normal; Facts.I2C_Role := C.S.Produces_And_Consumes; Expect (P.Unsupported_I2C_Role);
   Normal; Facts.Initiated_By := C.S.Device_Initiated; Expect (P.Unsupported_Initiator);
   Normal; Facts.Interrupt_Role := C.G.Produces_And_Consumes; Expect (P.Unsupported_GPIO_Role);
end HID_Channel_Policy_Tests;
