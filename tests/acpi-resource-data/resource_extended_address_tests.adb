pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_Resource_Data; use ACPI_Resource_Data;
with ACPI_Resource_Data.Address_Spaces;
with ACPI_Resource_Data.Address_Spaces.Geometry;
with ACPI_Resource_Data.Address_Spaces.Extended_Descriptors;
procedure Resource_Extended_Address_Tests is
   package A renames ACPI_Resource_Data.Address_Spaces;
   package E renames A.Extended_Descriptors;
   use type A.Address_Value;
   use type A.Address_Width;
   use type A.Decode_Mode;
   use type A.Memory_Range_Claim;
   use type A.IO_Range_Claim;
   use type E.Decode_Status;
   use type E.Extended_Kind;
   use type E.Attribute_Value;
   use type E.Resource_Role;
   use type E.Cache_Bits;
   use type Firmware_Tables.Bytes;
   subtype Bytes is Firmware_Tables.Bytes;
   Checks : Natural:=0;
   procedure Verify(Good : Boolean; Label : String) is
   begin Checks:=Checks+1; if not Good then raise Program_Error with Label; end if; end Verify;
   function Make return Bytes is
      Data : Bytes(1 .. 56):=[others=>0];
   begin Data(1):=16#8B#; Data(2):=53; Data(7):=1; return Data; end Make;
   procedure Put(Data : in out Bytes; First : Positive; Value : Unsigned_64) is
      Remaining : Unsigned_64:=Value;
   begin
      for I in Natural range 0 .. 7 loop
         Data(First+I):=Firmware_Tables.Byte(Remaining mod 256); Remaining:=Remaining/256;
      end loop;
   end Put;
   procedure Expect(Data : Bytes; Expected : E.Decode_Status) is
   begin Verify(E.Decode(Data,0).Status=Expected,"status"); end Expect;
begin
   Expect([],E.Malformed_Frame); Expect([16#8B#],E.Malformed_Frame);
   Expect([16#79#,0],E.Unsupported_Descriptor); Expect([16#8A#,0,0],E.Unsupported_Descriptor);
   for General in Firmware_Tables.Byte loop
      declare Data : Bytes:=Make; begin
         Data(5):=General;
         declare R : constant E.Decode_Result:=E.Decode(Data,0); begin
            if General>=16 then Verify(R.Status=E.Reserved_Bits_Set,"general reserved");
            else
               Verify(R.Status=E.Decoded and then R.General_Flags=General
                 and then R.Role=(if General mod 2=1 then E.Consumes_Resource else E.Produces_And_Consumes)
                 and then R.Mode=(if (General/2) mod 2=1 then A.Subtractive_Decode else A.Positive_Decode)
                 and then R.Minimum_Fixed=((General/4) mod 2=1)
                 and then R.Maximum_Fixed=((General/8) mod 2=1),"extended consumer bit is meaningful");
            end if;
         end;
      end;
   end loop;
   for Code in Firmware_Tables.Byte loop
      declare Data : Bytes:=Make; begin
         Data(4):=Code; if Code=1 then Data(6):=3; end if;
         declare R : constant E.Decode_Result:=E.Decode(Data,0); begin
            if Code in 0 .. 2 | 192 .. 255 then
               Verify(R.Status=E.Decoded,"defined extended type");
               Verify(R.Information.Kind=(case Code is when 0=>E.Memory_Range,when 1=>E.IO_Range,
                 when 2=>E.Bus_Range,when others=>E.Vendor_Range),"extended type classification");
               if Code>=192 then Verify(R.Information.Vendor_Type=Code,"vendor type retained"); end if;
            else Verify(R.Status=E.Reserved_Resource_Type,"PCC and reserved types rejected"); end if;
         end;
      end;
   end loop;
   for Kind in Firmware_Tables.Byte range 0 .. 2 loop
      for Info in Firmware_Tables.Byte loop
         declare Data : Bytes:=Make; begin
            Data(4):=Kind; Data(6):=Info;
            declare R : constant E.Decode_Result:=E.Decode(Data,0); begin
               if (Kind=0 and then Info>=64) or else (Kind=1 and then (Info and 16#CC#)/=0)
                 or else (Kind=2 and then Info/=0) then Verify(R.Status=E.Reserved_Bits_Set,"specific reserved bits");
               elsif Kind=1 and then Info mod 4=0 then Verify(R.Status=E.Reserved_IO_Coverage,"reserved IO coverage");
               else
                  Verify(R.Status=E.Decoded and then R.Information.Raw=Info,"raw type flags");
                  if Kind=0 then
                     Verify(R.Information.Declared_Writeable=(Info mod 2=1)
                       and then R.Information.Memory_To_IO=((Info/32) mod 2=1)
                       and then R.Information.Memory_Type=A.Memory_Range_Claim'Val(Natural((Info/8) mod 4))
                       and then R.Information.Ignored_Cache_Bits=E.Cache_Bits((Info/2) mod 4),"ignored cache bits remain distinct");
                  elsif Kind=1 then
                     Verify(R.Information.IO_To_Memory=((Info/16) mod 2=1)
                       and then R.Information.Sparse_Bit=((Info/32) mod 2=1)
                       and then R.Information.Coverage=A.IO_Range_Claim'Val(Natural(Info mod 4)-1),"IO flags");
                  end if;
               end if;
            end;
         end;
      end loop;
   end loop;
   for Info in Firmware_Tables.Byte loop
      declare Data : Bytes:=Make; begin
         Data(4):=192; Data(6):=Info;
         declare R : constant E.Decode_Result:=E.Decode(Data,0); begin
            Verify(R.Status=E.Decoded and then R.Information.Kind=E.Vendor_Range
              and then R.Information.Raw=Info and then R.Information.Vendor_Type=192,"vendor flags retained");
         end;
      end;
   end loop;
   for Byte_Value in Firmware_Tables.Byte loop
      declare Data : Bytes:=Make; begin
         Data(7):=Byte_Value;
         Verify(E.Decode(Data,0).Status=(if Byte_Value=1 then E.Decoded else E.Unsupported_Revision),"all revisions");
         Data(7):=1; Data(8):=Byte_Value;
         Verify(E.Decode(Data,0).Status=(if Byte_Value=0 then E.Decoded else E.Reserved_Byte_Set),"reserved byte");
      end;
   end loop;
   for Field in Natural range 0 .. 5 loop
      for Bit in Natural range 0 .. 63 loop
         declare
            Data : Bytes:=Make;
            Value : constant Unsigned_64:=Shift_Left(Unsigned_64'(1),Bit);
            V : constant A.Address_Value:=A.Address_Value(Value);
         begin
            Put(Data,9+Field*8,Value);
            declare R : constant E.Decode_Result:=E.Decode(Data,0); begin
               Verify(R.Status=E.Decoded and then R.Next_Offset=56,"full numeric bit decode");
               Verify(R.Granularity=(if Field=0 then V else 0) and then R.Minimum=(if Field=1 then V else 0)
                 and then R.Maximum=(if Field=2 then V else 0) and then R.Translation=(if Field=3 then V else 0)
                 and then R.Length=(if Field=4 then V else 0)
                 and then R.Attributes=(if Field=5 then E.Attribute_Value(Value) else 0),"six field positions/endian");
               declare P : constant A.Geometry.Parameters:=E.Geometry_Of(R); begin
                  Verify(P.Width=A.QWord_Width and then P.Granularity=R.Granularity and then P.Minimum=R.Minimum
                    and then P.Maximum=R.Maximum and then P.Length=R.Length
                    and then P.Minimum_Fixed=R.Minimum_Fixed and then P.Maximum_Fixed=R.Maximum_Fixed,"geometry adapter is descriptive");
               end;
               if Field=5 then
                  for Code in Firmware_Tables.Byte loop
                     if Code in 1 .. 2 | 192 .. 255 then
                        Data(4):=Code; Data(6):=(if Code=1 then 3 else 0);
                        Expect(Data,E.Reserved_Attributes_Set);
                     end if;
                  end loop;
               end if;
            end;
         end;
      end loop;
   end loop;
   declare Data : Bytes:=Make; begin
      Data(9 .. Data'Last):=[others=>255];
      declare R : constant E.Decode_Result:=E.Decode(Data,0); begin
         Verify(R.Status=E.Decoded and then R.Granularity=A.Address_Value'Last and then R.Minimum=A.Address_Value'Last
           and then R.Maximum=A.Address_Value'Last and then R.Translation=A.Address_Value'Last
           and then R.Length=A.Address_Value'Last and then R.Attributes=E.Attribute_Value'Last,"all ones memory quantities/attributes retained");
      end;
      for Cut in Natural range 0 .. Data'Length-1 loop
         Verify(E.Decode(Data(1 .. Cut),0).Status=E.Malformed_Frame,"truncated prefix");
      end loop;
      declare High : constant Bytes(Positive'Last-Data'Length+1 .. Positive'Last):=Data;
        Padded : constant Bytes:=[0] & Data; begin
         Verify(E.Decode(High,0).Status=E.Decoded,"high lower bound");
         Verify(E.Decode(Padded,1).Status=E.Decoded,"nonzero offset");
         Verify(E.Decode(Data,Byte_Offset'Last).Status=E.Malformed_Frame,"oversized offset");
      end;
   end;
   for Size in Natural range 0 .. 64 loop
      declare Data : Bytes(1 .. Size+3):=[others=>0]; begin
         Data(1):=16#8B#; Data(2):=Firmware_Tables.Byte(Size);
         if Size>=4 then Data(7):=1; end if;
         Verify(E.Decode(Data,0).Status=(if Size=53 then E.Decoded else E.Bad_Length),"exact length, no optional tail");
      end;
   end loop;
   Ada.Text_IO.Put_Line("Resource extended address checks" & Natural'Image(Checks));
end Resource_Extended_Address_Tests;
