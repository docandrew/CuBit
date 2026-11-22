pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_Resource_Data; use ACPI_Resource_Data;
with ACPI_Resource_Data.Memory_Ranges;
with ACPI_Resource_Data.Generic_Registers;
procedure Resource_Memory_Register_Tests is
   package M renames ACPI_Resource_Data.Memory_Ranges;
   package R renames ACPI_Resource_Data.Generic_Registers;
   use type M.Decode_Status;
   use type M.Memory_Encoding;
   use type M.Memory_Value;
   use type R.Decode_Status;
   use type Firmware_Tables.Bytes;
   subtype Bytes is Firmware_Tables.Bytes;
   Checks : Natural:=0;
   procedure Verify(Good : Boolean; Label : String) is
   begin Checks:=Checks+1; if not Good then raise Program_Error with Label; end if; end Verify;
   function Memory(Encoding : M.Memory_Encoding) return Bytes is
      Count : constant Positive:=(if Encoding=M.Memory_24 then 9 else 17);
      Data : Bytes(1 .. Count+3):=[others=>0];
   begin
      Data(1):=(if Encoding=M.Memory_24 then 16#81# else 16#85#);
      Data(2):=Firmware_Tables.Byte(Count); return Data;
   end Memory;
   function Register_Data return Bytes is
     ([16#82#,12,0,0,0,0,0,0,0,0,0,0,0,0,0]);
   procedure Put(Data : in out Bytes; First, Count : Positive; Value : Unsigned_64) is
      Remaining : Unsigned_64:=Value;
   begin
      for I in Natural range 0 .. Count-1 loop
         Data(First+I):=Firmware_Tables.Byte(Remaining mod 256); Remaining:=Remaining/256;
      end loop;
   end Put;
begin
   Verify(M.Decode([],0).Status=M.Malformed_Frame and then R.Decode([],0).Status=R.Malformed_Frame,"empty");
   Verify(M.Decode([16#79#,0],0).Status=M.Unsupported_Descriptor
     and then R.Decode([16#79#,0],0).Status=R.Unsupported_Descriptor,"other families");
   for Field in Natural range 0 .. 3 loop
      for Encoded in Unsigned_16 loop
         declare
            Data : Bytes:=Memory(M.Memory_24);
            Value : constant M.Memory_Value:=M.Memory_Value(Encoded);
         begin
            Put(Data,5+Field*2,2,Unsigned_64(Encoded));
            declare Decoded : constant M.Decode_Result:=M.Decode(Data,0); begin
               Verify(Decoded.Status=M.Decoded and then Decoded.Encoding=M.Memory_24
                 and then Decoded.Next_Offset=12,"all Memory24 field encodings");
               Verify(Decoded.Minimum_Base=(if Field=0 then Value*256 else 0)
                 and then Decoded.Maximum_Base=(if Field=1 then Value*256 else 0)
                 and then Decoded.Length=(if Field=3 then Value*256 else 0)
                 and then Decoded.Encoded_Alignment=(if Field=2 then Value else 0)
                 and then Decoded.Alignment=(if Field=2 and then Value/=0 then Value else 65_536),
                 "Memory24 block scaling and zero alignment");
            end;
         end;
      end loop;
   end loop;
   for Bit in Natural range 0 .. 31 loop
      for Field in Natural range 0 .. 3 loop
         declare Data : Bytes:=Memory(M.Memory_32);
           Value : constant M.Memory_Value:=M.Memory_Value(Shift_Left(Unsigned_32'(1),Bit)); begin
            Put(Data,5+Field*4,4,Unsigned_64(Value));
            declare Decoded : constant M.Decode_Result:=M.Decode(Data,0); begin
               Verify(Decoded.Status=M.Decoded and then Decoded.Encoding=M.Memory_32
                 and then Decoded.Minimum_Base=(if Field=0 then Value else 0)
                 and then Decoded.Maximum_Base=(if Field=1 then Value else 0)
                 and then Decoded.Length=(if Field=3 then Value else 0)
                 and then Decoded.Encoded_Alignment=(if Field=2 then Value else 0)
                 and then Decoded.Alignment=Decoded.Encoded_Alignment,"Memory32 field bit positions");
            end;
         end;
      end loop;
   end loop;
   for Encoding in M.Memory_Encoding loop
      for Info in Firmware_Tables.Byte loop
         declare Data : Bytes:=Memory(Encoding); begin
            Data(4):=Info;
            declare Decoded : constant M.Decode_Result:=M.Decode(Data,0); begin
               Verify(Decoded.Status=M.Decoded and then Decoded.Information=Info
                 and then Decoded.Declared_Writeable=(Info mod 2=1),"ignored memory info retained");
            end;
         end;
      end loop;
      declare Data : Bytes:=Memory(Encoding); begin
         Data(5 .. Data'Last):=[others=>255];
         declare Decoded : constant M.Decode_Result:=M.Decode(Data,0);
           Value : constant M.Memory_Value:=(if Encoding=M.Memory_24 then 16#FFFF00# else M.Memory_Value'Last); begin
            Verify(Decoded.Status=M.Decoded and then Decoded.Minimum_Base=Value
              and then Decoded.Maximum_Base=Value and then Decoded.Length=Value,"all ones memory fields");
         end;
         for Cut in Natural range 0 .. Data'Length-1 loop
            Verify(M.Decode(Data(1 .. Cut),0).Status=M.Malformed_Frame,"memory truncated prefix");
         end loop;
         declare High : constant Bytes(Positive'Last-Data'Length+1 .. Positive'Last):=Data;
           Padded : constant Bytes:=[0] & Data; begin
            Verify(M.Decode(High,0).Status=M.Decoded,"memory high bounds");
            Verify(M.Decode(Padded,1).Status=M.Decoded,"memory nonzero offset");
            Verify(M.Decode(Data,Byte_Offset'Last).Status=M.Malformed_Frame,"memory oversized offset");
         end;
      end;
   end loop;
   for Field in Natural range 0 .. 3 loop
      for Value in Firmware_Tables.Byte loop
         declare Data : Bytes:=Register_Data; begin
            Data(4+Field):=Value;
            declare Decoded : constant R.Decode_Result:=R.Decode(Data,0); begin
               if Field=3 and then Value>4 then Verify(Decoded.Status=R.Reserved_Access_Size,"reserved access sizes");
               else
                  Verify(Decoded.Status=R.Decoded and then Decoded.Next_Offset=15,"register raw bytes");
                  Verify(Decoded.Value.Space=(if Field=0 then Value else 0)
                    and then Decoded.Value.Width=(if Field=1 then Value else 0)
                    and then Decoded.Value.Bit_Offset=(if Field=2 then Value else 0)
                    and then Decoded.Value.Access_Size=(if Field=3 then Value else 0)
                    and then Decoded.Value.Address=0,"space/bit fields retained without admission");
               end if;
            end;
         end;
      end loop;
   end loop;
   for Bit in Natural range 0 .. 63 loop
      declare Data : Bytes:=Register_Data;
        Value : constant Unsigned_64:=Shift_Left(Unsigned_64'(1),Bit); begin
         Put(Data,8,8,Value);
         declare Decoded : constant R.Decode_Result:=R.Decode(Data,0); begin
            Verify(Decoded.Status=R.Decoded and then Decoded.Value.Address=Value,"full register address bit");
         end;
      end;
   end loop;
   declare Data : Bytes:=Register_Data; begin
      Data(8 .. Data'Last):=[others=>255];
      Verify(R.Decode(Data,0).Value.Address=Unsigned_64'Last,"all ones register address");
      for Cut in Natural range 0 .. Data'Length-1 loop
         Verify(R.Decode(Data(1 .. Cut),0).Status=R.Malformed_Frame,"register prefix");
      end loop;
      declare High : constant Bytes(Positive'Last-Data'Length+1 .. Positive'Last):=Data;
        Padded : constant Bytes:=[0] & Data; begin
         Verify(R.Decode(High,0).Status=R.Decoded,"register high bounds");
         Verify(R.Decode(Padded,1).Status=R.Decoded,"register nonzero offset");
         Verify(R.Decode(Data,Byte_Offset'Last).Status=R.Malformed_Frame,"register oversized offset");
      end;
   end;
   for Size in Natural range 0 .. 24 loop
      declare Data : Bytes(1 .. Size+3):=[others=>0]; begin
         Data(2):=Firmware_Tables.Byte(Size);
         Data(1):=16#81#;
         Verify(M.Decode(Data,0).Status=(if Size=9 then M.Decoded else M.Bad_Length),"Memory24 exact length");
         Data(1):=16#85#;
         Verify(M.Decode(Data,0).Status=(if Size=17 then M.Decoded else M.Bad_Length),"Memory32 exact length");
         Data(1):=16#82#;
         Verify(R.Decode(Data,0).Status=(if Size=12 then R.Decoded else R.Bad_Length),"register exact length");
      end;
   end loop;
   Ada.Text_IO.Put_Line("Resource memory/register checks" & Natural'Image(Checks));
end Resource_Memory_Register_Tests;
