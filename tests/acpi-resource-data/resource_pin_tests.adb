pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_Resource_Data; use ACPI_Resource_Data;
with ACPI_Resource_Data.Pin_Descriptors;
procedure Resource_Pin_Tests is
   package P renames ACPI_Resource_Data.Pin_Descriptors;
   use type P.Decode_Status;
   use type P.Descriptor_Kind;
   use type P.Resource_Role;
   use type P.Pin_Number;
   use type P.Pull_Configuration;
   use type P.Function_Number;
   use type P.Configuration_Type;
   use type P.Configuration_Value;
   subtype Bytes is Firmware_Tables.Bytes;
   subtype Kind_Number is Positive range 1 .. 5;
   type Positions is array (Kind_Number) of Natural;
   Fixed : constant Positions := [18,20,14,17,20];
   Pins_Field : constant Positions := [9,11,6,0,0];
   Source_Field : constant Positions := [12,14,0,9,12];
   Label_Field : constant Positions := [0,0,8,11,14];
   Vendor_Field : constant Positions := [14,16,10,13,16];
   Length_Field : constant Positions := [16,18,12,15,18];
   Index_Field : constant Positions := [11,13,0,8,11];
   Source_First : constant Positions := [24,26,0,17,20];
   Label_First : constant Positions := [0,0,20,22,25];
   Vendor_First : constant Positions := [29,31,24,26,29];
   Tag : constant Positions := [16#8D#,16#8F#,16#90#,16#91#,16#92#];
   Checks : Natural := 0;
   procedure Verify (Good : Boolean; Name : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Name; end if; end Verify;
   procedure Put_Word (Data : in out Bytes; Offset, Value : Natural) is
   begin
      Data (Data'First + Offset) := Firmware_Tables.Byte (Value mod 256);
      Data (Data'First + Offset + 1) := Firmware_Tables.Byte (Value / 256);
   end Put_Word;
   procedure Put_Dword (Data : in out Bytes; Offset : Natural; Value : Unsigned_32) is
   begin
      for I in Natural range 0 .. 3 loop
         Data (Data'First + Offset + I) := Firmware_Tables.Byte (Shift_Right (Value, I * 8) and 255);
      end loop;
   end Put_Dword;
   function Example (Kind : Kind_Number) return Bytes is
      Data : Bytes (1 .. Vendor_First (Kind) + 2) := [others => 0];
   begin
      Data (1) := Firmware_Tables.Byte (Tag (Kind)); Put_Word (Data, 1, Data'Length - 3);
      Data (4) := 1; Data (5) := (if Kind in 1 | 3 then 1 else 3);
      Put_Word (Data, Vendor_Field (Kind), Vendor_First (Kind)); Put_Word (Data, Length_Field (Kind), 2);
      if Kind <= 3 then
         Put_Word (Data, Pins_Field (Kind), Fixed (Kind));
         Put_Word (Data, Fixed (Kind), 0); Put_Word (Data, Fixed (Kind) + 2, 16#1234#);
         Put_Word (Data, Fixed (Kind) + 4, 65_535);
      end if;
      if Kind /= 3 then
         Put_Word (Data, Source_Field (Kind), Source_First (Kind));
         Data (Source_First (Kind) + 1 .. Source_First (Kind) + 5) := [71,80,73,79,0];
      end if;
      if Kind >= 3 then
         Put_Word (Data, Label_Field (Kind), Label_First (Kind));
         Data (Label_First (Kind) + 1 .. Label_First (Kind) + 4) := [71,82,80,0];
      end if;
      case Kind is
         when 1 => Data (7) := 2; Put_Word (Data, 7, 16#BEEF#);
         when 2 | 5 => Data (7) := 10; Put_Dword (Data, 7, 16#FEDC_BA98#);
         when 4 => Put_Word (Data, 6, 16#1234#);
         when others => null;
      end case;
      Data (Data'Last - 1 .. Data'Last) := [16#DA#,16#DB#];
      return Data;
   end Example;
begin
   Verify (P.Decode ([], 0).Status = P.Malformed_Frame, "empty");
   Verify (P.Decode ([16#79#,0], 0).Status = P.Unsupported_Descriptor, "other descriptor");
   for Kind in Kind_Number loop
      declare Data : constant Bytes := Example (Kind); R : constant P.Decode_Result := P.Decode (Data, 0); begin
         Verify (R.Status = P.Decoded and then R.Information.Kind = P.Descriptor_Kind'Val (Kind - 1)
           and then R.Count = (if Kind <= 3 then 3 else 0)
           and then R.Shared = (Kind /= 3) and then R.Role = P.Consumes_Resource
           and then R.Vendor.First = Byte_Count (Vendor_First (Kind)) and then R.Vendor.Length = 2
           and then R.Source.First = Byte_Count (Source_First (Kind))
           and then R.Source.Length = (if Kind = 3 then 0 else 5)
           and then R.Label.First = Byte_Count (Label_First (Kind))
           and then R.Label.Length = (if Kind <= 2 then 0 else 4)
           and then R.Next_Offset = Byte_Count (Data'Length), "complete descriptor fields/spans");
         Verify (not P.Read_Pin (Data, 0, 4).Available and then not P.Read_Pin (Data, 0, Positive'Last).Available, "index limits");
         for Size in Natural range 0 .. Data'Length - 1 loop
            Verify (P.Decode (Data (1 .. Size), 0).Status = P.Malformed_Frame, "all truncated prefixes");
         end loop;
         Verify (P.Decode (Data, Byte_Offset'Last).Status = P.Malformed_Frame, "offset beyond input");
         for I in Positive range 1 .. 3 loop
            declare Pin : constant P.Pin_Result := P.Read_Pin (Data, 0, I); begin
               Verify (Pin.Available = (Kind <= 3), "pin presence for direct/reference kinds");
               if Pin.Available then Verify (Pin.Value = (case I is when 1 => 0, when 2 => 16#1234#, when others => 65_535),
                 "pin order and full-width value"); end if;
            end;
         end loop;
      end;
   end loop;
   declare R : constant P.Decode_Result := P.Decode (Example (1), 0); begin
      Verify (R.Information.Pull = 2 and then R.Information.Function_ID = 16#BEEF#, "function settings");
   end;
   declare R : constant P.Decode_Result := P.Decode (Example (4), 0); begin
      Verify (R.Information.Group_Function_ID = 16#1234#, "group function settings");
   end;
   for Kind in Kind_Number loop
      if Kind in 2 | 5 then
         declare R : constant P.Decode_Result := P.Decode (Example (Kind), 0); begin
            Verify (R.Information.Configuration = 10 and then R.Information.Value = 16#FEDC_BA98#, "configuration fields");
         end;
      end if;
   end loop;
   for Value in Natural range 0 .. 65_535 loop
      for Kind in Kind_Number loop
         declare Data : Bytes := Example (Kind); begin
            Put_Word (Data, 4, Value);
            declare R : constant P.Decode_Result := P.Decode (Data, 0); begin
               Verify (R.Status = (if Value < (if Kind in 1 | 3 then 2 else 4)
                 then P.Decoded else P.Reserved_Bits_Set), "all flag words");
               if R.Status = P.Decoded then
                  Verify (R.Shared = (Kind /= 3 and then Value mod 2 = 1)
                    and then R.Role = (if Kind = 1 then P.Consumes_Resource
                      elsif (Value / (if Kind = 3 then 1 else 2)) mod 2 = 1 then P.Consumes_Resource
                      else P.Produces_And_Consumes), "role and sharing by descriptor");
               end if;
            end;
            Data := Example (Kind); Put_Word (Data, Vendor_Field (Kind), Value);
            Verify (P.Decode (Data, 0).Status = (if Value = Vendor_First (Kind) then P.Decoded else P.Bad_Offsets),
              "every vendor offset and exact terminal extent");
            Data := Example (Kind); Put_Word (Data, Length_Field (Kind), Value);
            Verify (P.Decode (Data, 0).Status = (if Value = 2 then P.Decoded else P.Bad_Offsets), "every vendor length");
            if Kind <= 3 then
               Data := Example (Kind); Put_Word (Data, Pins_Field (Kind), Value);
               declare
                  Limit : constant Natural := (if Kind = 3 then Label_First (Kind) else Source_First (Kind));
                  R : constant P.Decode_Result := P.Decode (Data, 0);
                  Expected : constant P.Decode_Status :=
                    (if Value < Fixed (Kind) or else Value > Limit then P.Bad_Offsets
                     elsif Value = Limit or else Value mod 2 /= Limit mod 2 then P.Bad_Pin_Count else P.Decoded);
               begin
                  Verify (R.Status = Expected, "every pin offset partition");
                  if R.Status = P.Decoded then Verify (R.Count = (Limit - Value) / 2, "pin count follows partition"); end if;
               end;
               Data := Example (Kind); Put_Word (Data, Fixed (Kind), Value);
               declare Pin : constant P.Pin_Result := P.Read_Pin (Data, 0, 1); begin
                  Verify (Pin.Available and then Pin.Value = P.Pin_Number (Value), "all raw pin numbers");
               end;
            end if;
            if Kind /= 3 then
               Data := Example (Kind); Put_Word (Data, Source_Field (Kind), Value);
               declare
                  Limit : constant Natural := (if Kind <= 2 then Vendor_First (Kind) else Label_First (Kind));
                  R : constant P.Decode_Result := P.Decode (Data, 0);
                  Expected : constant P.Decode_Status :=
                    (if Value < Fixed (Kind) or else Value > Limit then P.Bad_Offsets
                     elsif Kind <= 2 and then (Value < Fixed (Kind) + 2 or else Value mod 2 /= Fixed (Kind) mod 2)
                       then P.Bad_Pin_Count else P.Decoded);
               begin
                  Verify (R.Status = Expected, "every source offset partition");
                  if R.Status = P.Decoded then
                     Verify (R.Source.First = Byte_Count (Value) and then R.Source.Length = Byte_Count (Limit - Value),
                       "source span follows encoded partition");
                  end if;
               end;
            end if;
            if Kind >= 3 then
               Data := Example (Kind); Put_Word (Data, Label_Field (Kind), Value);
               declare
                  R : constant P.Decode_Result := P.Decode (Data, 0);
                  Expected : constant P.Decode_Status :=
                    (if Value < Fixed (Kind) or else Value > Vendor_First (Kind) then P.Bad_Offsets
                     elsif Value > Vendor_First (Kind) - 2 or else Value < (if Kind = 3 then 16 else Label_First (Kind))
                       then P.Bad_Label
                     elsif Kind = 3 and then Value mod 2 /= 0 then P.Bad_Pin_Count else P.Decoded);
               begin
                  Verify (R.Status = Expected, "every label offset and independent byte partition");
               end;
            end if;
            if Kind in 1 | 4 then
               Data := Example (Kind); Put_Word (Data, (if Kind = 1 then 7 else 6), Value);
               declare R : constant P.Decode_Result := P.Decode (Data, 0); begin
                  Verify (R.Status = P.Decoded and then (if Kind = 1 then R.Information.Function_ID = P.Function_Number (Value)
                    else R.Information.Group_Function_ID = P.Function_Number (Value)), "every function ID");
               end;
            end if;
         end;
      end loop;
   end loop;
   for Value in Firmware_Tables.Byte loop
      for Kind in Kind_Number loop
         declare Data : Bytes := Example (Kind); begin
            Data (4) := Value;
            Verify (P.Decode (Data, 0).Status = (if Value = 1 then P.Decoded else P.Unsupported_Revision), "all revisions");
            if Kind /= 3 then
               Data := Example (Kind); Data (Index_Field (Kind) + 1) := Value;
               Verify (P.Decode (Data, 0).Status = (if Value = 0 then P.Decoded else P.Bad_Source_Index), "all source indices");
            end if;
            if Kind = 1 then
               Data := Example (Kind); Data (7) := Value;
               Verify (P.Decode (Data, 0).Status = (if Value <= 3 or else Value >= 128 then P.Decoded else P.Reserved_Pull),
                 "all pull encodings");
            elsif Kind in 2 | 5 then
               Data := Example (Kind); Data (7) := Value;
               Verify (P.Decode (Data, 0).Status = (if Value <= 13 or else Value >= 128 then P.Decoded else P.Reserved_Configuration_Type),
                 "all configuration types");
            end if;
         end;
      end loop;
   end loop;
   for Kind in Kind_Number loop
      if Kind in 2 | 5 then
         for Bit in Natural range 0 .. 31 loop
            for Invert in Boolean loop
               declare
                  Data : Bytes := Example (Kind);
                  Value : constant Unsigned_32 := (if Invert then not Shift_Left (1, Bit) else Shift_Left (1, Bit));
               begin
                  Put_Dword (Data, 7, Value);
                  declare R : constant P.Decode_Result := P.Decode (Data, 0); begin
                     Verify (R.Status = P.Decoded and then R.Information.Value = P.Configuration_Value (Value), "all value bits");
                  end;
               end;
            end loop;
         end loop;
      end if;
      declare Base : constant Bytes := Example (Kind); begin
         if Kind >= 3 then
            for Position in Label_First (Kind) + 1 .. Vendor_First (Kind) loop
               declare Data : Bytes := Base; begin
                  Data (Position) := (if Position = Vendor_First (Kind) then 1 else 0);
                  Verify (P.Decode (Data, 0).Status = P.Bad_Label, "missing and early label termination");
               end;
            end loop;
         end if;
         if Kind /= 3 then
            declare Data : Bytes := Base; begin
               Data (Source_First (Kind) + 5) := 255;
               Verify (P.Decode (Data, 0).Status = P.Decoded, "source is opaque rather than label syntax");
            end;
         end if;
         declare Data : Bytes (Positive'Last - Base'Length - 2 .. Positive'Last) := [others => 16#AA#]; begin
            Data (Data'First + 2 .. Data'Last - 1) := Base;
            declare R : constant P.Decode_Result := P.Decode (Data, 2); begin
               Verify (R.Status = P.Decoded and then R.Vendor.First = Byte_Count (Vendor_First (Kind) + 2)
                 and then R.Next_Offset = Byte_Count (Base'Length + 2), "high array bounds and descriptor origin");
               Verify (P.Read_Pin (Data, 2, 2).Available = (Kind <= 3), "high bound pin lookup");
            end;
         end;
      end;
   end loop;
   -- Maximum payload, variable pin capacity, padding and nonzero descriptor
   -- origins. Source names remain opaque; group labels stay nonempty/terminated.
   for Kind in Kind_Number loop
      declare
         Large : Bytes (1 .. 65_538) := [others => 0];
         Name_First : constant Natural := (if Kind <= 2 then 65_534 else 65_532);
         Pin_Limit : constant Natural := (if Kind <= 2 then 65_534 else 65_532);
      begin
         Large (1) := Firmware_Tables.Byte (Tag (Kind)); Put_Word (Large, 1, 65_535);
         Large (4) := 1; Put_Word (Large, Vendor_Field (Kind), 65_535);
         Put_Word (Large, Length_Field (Kind), 3);
         if Kind /= 3 then Put_Word (Large, Source_Field (Kind), (if Kind <= 2 then Name_First else Fixed (Kind))); end if;
         if Kind >= 3 then
            Put_Word (Large, Label_Field (Kind), Name_First);
            Large (Name_First + 1 .. Name_First + 3) := [71,80,0];
         end if;
         if Kind <= 3 then
            Put_Word (Large, Pins_Field (Kind), Fixed (Kind));
            Put_Word (Large, Pin_Limit - 2, 16#ABCD#);
         end if;
         declare R : constant P.Decode_Result := P.Decode (Large, 0); begin
            Verify (R.Status = P.Decoded and then R.Next_Offset = 65_538
              and then R.Vendor.First = 65_535 and then R.Vendor.Length = 3,
              "maximum pin descriptor extent");
            if Kind <= 3 then
               Verify (R.Count = (Pin_Limit - Fixed (Kind)) / 2, "maximum-size decoded pin capacity");
               Verify (P.Read_Pin (Large, 0, R.Count).Available
                 and then P.Read_Pin (Large, 0, R.Count).Value = 16#ABCD#
                 and then not P.Read_Pin (Large, 0, R.Count + 1).Available, "last large-list pin and limit");
               Put_Word (Large, Pins_Field (Kind), Fixed (Kind) + 2);
               declare Padded : constant P.Decode_Result := P.Decode (Large, 0); begin
                  Verify (Padded.Status = P.Decoded and then Padded.Count = R.Count - 1
                    and then Padded.Pins.First = Byte_Count (Fixed (Kind) + 2), "padding before pin list");
               end;
            end if;
            if Kind > 3 then
               Verify (R.Count = 0 and then not P.Read_Pin (Large, 0, 1).Available, "large group reference has no inline pins");
            end if;
         end;
         declare Wide : Bytes (Positive'Last - Large'Length - 2 .. Positive'Last) := [others => 16#AA#]; begin
            Wide (Wide'First + 3 .. Wide'Last) := Large;
            declare R : constant P.Decode_Result := P.Decode (Wide, 3); begin
               Verify (R.Status = P.Decoded and then R.Next_Offset = 65_541
                 and then R.Vendor.First = 65_538, "maximum descriptor at high array bound and nonzero origin");
               if Kind <= 3 then Verify (P.Read_Pin (Wide, 3, R.Count).Available
                 and then P.Read_Pin (Wide, 3, R.Count).Value = 16#ABCD#, "high-bound last large-list pin"); end if;
            end;
         end;
         Verify (P.Decode (Large (1 .. Large'Last - 1), 0).Status = P.Malformed_Frame, "maximum descriptor truncation");
         Put_Word (Large, Length_Field (Kind), 4);
         Verify (P.Decode (Large, 0).Status = P.Bad_Offsets, "maximum vendor extent cannot escape frame");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Resource pin checks" & Checks'Image);
end Resource_Pin_Tests;
