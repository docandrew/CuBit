pragma Ada_2022;
with Ada.Text_IO;
with HID_I2C_Descriptors;
with Interfaces;
procedure Descriptor_Tests is
   package D renames HID_I2C_Descriptors;
   use type D.Decode_Status, D.Capacity_Status, D.Register_Number, D.Byte_Count;
   use type Interfaces.Unsigned_16, Interfaces.Unsigned_32;
   Checks : Natural := 0;
   procedure Check (Pass : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Pass then raise Program_Error with Name; end if;
   end Check;
   -- Deliberately distinct high/low bytes and nonzero reserved bytes.
   Golden : constant D.Bytes :=
     [16#1E#, 0, 0, 1, 16#34#, 16#12#, 16#78#, 16#56#,
      16#BC#, 16#9A#, 16#F0#, 16#DE#, 16#57#, 16#13#,
      16#68#, 16#24#, 16#9A#, 16#35#, 16#BC#, 16#46#,
      16#DE#, 16#57#, 16#F0#, 16#68#, 16#12#, 16#79#,
      16#34#, 16#8A#, 16#56#, 16#9B#];
   High : D.Bytes (Natural'Last - 29 .. Natural'Last) := Golden;
   Data : D.Bytes (0 .. 29) := Golden;
   R : constant D.Decode_Result := D.Decode (Golden);
   V : D.Descriptor := R.Value;
   procedure Put_Word (Offset, Value : Natural) is
   begin
      Data (Offset) := D.Byte (Value mod 256);
      Data (Offset + 1) := D.Byte (Value / 256);
   end Put_Word;
begin
   Check (R.Status = D.Decoded, "golden");
   Check (V.Report_Length = 16#1234# and V.Report_Register = 16#5678#
     and V.Input_Register = 16#9ABC# and V.Maximum_Input = 16#DEF0#
     and V.Output_Register = 16#1357# and V.Maximum_Output = 16#2468#
     and V.Command_Register = 16#359A# and V.Data_Register = 16#46BC#
     and V.Vendor_ID = 16#57DE# and V.Product_ID = 16#68F0#
     and V.Firmware_Version = 16#7912# and V.Reserved = 16#9B568A34#,
     "all fields independently specified");
   Check (D.Decode (High).Value.Reserved = V.Reserved, "highest index");
   for Size in 0 .. 29 loop
      declare
         Short : constant D.Bytes (17 .. 16 + Size) := [others => 0];
      begin
         Check (D.Decode (Short).Status = D.Short_Transfer, "short length");
      end;
   end loop;
   declare
      Long : constant D.Bytes (0 .. 30) := [others => 0];
   begin
      Check (D.Decode (Long).Status = D.Trailing_Data, "trailing");
   end;
   -- Exhaust every 16-bit header length/version and a register field; no
   -- assertion-only checks, so release mode actually evaluates every case.
   for N in 0 .. 65_535 loop
      Data := Golden;
      Put_Word (0, N);
      Check (D.Decode (Data).Status =
        (if N = 30 then D.Decoded else D.Invalid_Length), "header length");
      Data := Golden;
      Put_Word (2, N);
      Check (D.Decode (Data).Status =
        (if N = 256 then D.Decoded else D.Unsupported_Version), "version");
      Data := Golden;
      Put_Word (6, N);
      Check (D.Decode (Data).Value.Report_Register = D.Register_Number (N),
        "full register range");
   end loop;
   V.Report_Length := 1;
   V.Maximum_Input := 2;
   V.Maximum_Output := 0;
   Check (D.Check_Capacities (V, 1, 2, 0) = D.Fits, "optional output");
   V.Report_Length := 0;
   Check (D.Check_Capacities (V, 0, 2, 0) = D.Empty_Report, "empty report");
   V.Report_Length := 1;
   for N in 0 .. 1 loop
      V.Maximum_Input := D.Byte_Count (N);
      Check (D.Check_Capacities (V, 1, 2, 0) = D.Invalid_Input_Length,
        "missing input prefix");
   end loop;
   V.Maximum_Input := 2;
   V.Maximum_Output := 1;
   Check (D.Check_Capacities (V, 1, 2, 2) = D.Invalid_Output_Length,
     "short output prefix");
   V.Maximum_Output := 2;
   Check (D.Check_Capacities (V, 1, 2, 2) = D.Fits, "exact prefixes");
   for N in D.Byte_Count range 2 .. D.Byte_Count'Last loop
      V.Report_Length := N;
      V.Maximum_Input := N;
      V.Maximum_Output := N;
      Check (D.Check_Capacities (V, N, N, N) = D.Fits, "exact capacities");
      Check (D.Check_Capacities (V, N - 1, N, N) = D.Report_Too_Large,
        "report refuses truncation");
      Check (D.Check_Capacities (V, N, N - 1, N) = D.Input_Too_Large,
        "input refuses truncation");
      Check (D.Check_Capacities (V, N, N, N - 1) = D.Output_Too_Large,
        "output refuses truncation");
   end loop;
   High := [others => 0];
   Check (D.Decode (High).Status = D.Invalid_Length, "high failure");
   Ada.Text_IO.Put_Line ("HID-I2C descriptor checks:" & Natural'Image (Checks));
end Descriptor_Tests;
