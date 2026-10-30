pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_Resource_Data; use ACPI_Resource_Data;
with ACPI_Resource_Data.Address_Spaces;
procedure Resource_Address_Tests is
   package A renames ACPI_Resource_Data.Address_Spaces;
   use type Firmware_Tables.Bytes;
   use type A.Decode_Status;
   use type A.Address_Width;
   use type A.Address_Value;
   use type A.Resource_Kind;
   use type A.Decode_Mode;
   use type A.Memory_Cache_Claim;
   use type A.Memory_Range_Claim;
   use type A.IO_Range_Claim;
   subtype Bytes is Firmware_Tables.Bytes;
   Checks : Natural:=0;
   procedure Check(Good : Boolean; Label : String) is
   begin Checks:=Checks+1; if not Good then raise Program_Error with Label; end if; end Check;
   function Make(Width : A.Address_Width; Tail : Bytes:=[]) return Bytes is
      N : constant Positive:=(case Width is when A.Word_Width=>2,when A.DWord_Width=>4,when A.QWord_Width=>8);
      Payload : constant Natural:=3+5*N+Tail'Length;
      Data : Bytes(1 .. 3+Payload):=[others=>0];
   begin
      Data(1):=(case Width is when A.Word_Width=>16#88#,when A.DWord_Width=>16#87#,when A.QWord_Width=>16#8A#);
      Data(2):=Firmware_Tables.Byte(Payload mod 256); Data(3):=Firmware_Tables.Byte(Payload/256);
      if Tail'Length>0 then Data(Data'Last-Tail'Length+1 .. Data'Last):=Tail; end if;
      return Data;
   end Make;
   procedure Put(Data : in out Bytes; Position : Positive; N : Positive; Value : Unsigned_64) is
      V : Unsigned_64:=Value;
   begin
      for J in Natural range 0 .. N-1 loop
         Data(Position+J):=Firmware_Tables.Byte(V and 255); V:=Shift_Right(V,8);
      end loop;
   end Put;
   procedure Expect(Data : Bytes; Status : A.Decode_Status) is
   begin Check(A.Decode(Data,0).Status=Status,"expected status"); end Expect;
begin
   Expect([],A.Malformed_Frame); Expect([16#8A#],A.Malformed_Frame);
   Expect([16#79#,0],A.Unsupported_Descriptor); Expect([16#89#,0,0],A.Unsupported_Descriptor);
   for Width in A.Address_Width loop
      declare
         Data : Bytes:=Make(Width);
         N : constant Positive:=(case Width is when A.Word_Width=>2,when A.DWord_Width=>4,when A.QWord_Width=>8);
      begin
         for General in Firmware_Tables.Byte loop
            Data(5):=General;
            declare R : constant A.Decode_Result:=A.Decode(Data,0); begin
               if General>=16 then Check(R.Status=A.Reserved_Bits_Set,"general reserved bits");
               else
                  Check(R.Status=A.Decoded,"general flags decoded");
                  Check(R.General_Flags=General and then R.Minimum_Fixed=((General/4) mod 2=1)
                    and then R.Maximum_Fixed=((General/8) mod 2=1)
                    and then R.Mode=(if (General/2) mod 2=1 then A.Subtractive_Decode else A.Positive_Decode),
                    "general bit zero preserved, not role");
               end if;
            end;
         end loop;
         Data(5):=0;
         for Kind in Firmware_Tables.Byte loop
            Data(4):=Kind; Data(6):=0;
            declare R : constant A.Decode_Result:=A.Decode(Data,0); begin
               if Kind=1 then Check(R.Status=A.Reserved_IO_Coverage,"IO zero coverage");
               elsif Kind in 0|2|10|192 .. 255 then
                  Check(R.Status=A.Decoded,"defined resource type");
                  if Kind>=192 then Check(R.Information.Kind=A.Vendor_Range
                    and then R.Information.Vendor_Type=Kind,"vendor type retained"); end if;
               else Check(R.Status=A.Reserved_Resource_Type,"reserved resource types"); end if;
            end;
         end loop;
         for Kind in Firmware_Tables.Byte range 0 .. 2 loop
            Data(4):=Kind;
            for Specific in Firmware_Tables.Byte loop
               Data(6):=Specific;
               declare R : constant A.Decode_Result:=A.Decode(Data,0); begin
                  if (Kind=0 and then Specific>=64)
                    or else (Kind=1 and then (Specific and 16#CC#)/=0)
                    or else (Kind=2 and then Specific/=0) then
                     Check(R.Status=A.Reserved_Bits_Set,"type reserved bits");
                  elsif Kind=1 and then (Specific and 3)=0 then
                     Check(R.Status=A.Reserved_IO_Coverage,"reserved IO range");
                  else
                     Check(R.Status=A.Decoded and then R.Information.Raw=Specific,"type flags retained");
                     if Kind=0 then
                        Check(R.Information.Declared_Writeable=(Specific mod 2=1)
                          and then R.Information.Memory_To_IO=((Specific/32) mod 2=1)
                          and then R.Information.Cache=A.Memory_Cache_Claim'Val(Natural((Specific/2) and 3))
                          and then R.Information.Memory_Type=A.Memory_Range_Claim'Val(Natural((Specific/8) and 3)),"memory claims");
                     elsif Kind=1 then
                        Check(R.Information.IO_To_Memory=((Specific/16) mod 2=1)
                          and then R.Information.Sparse_Bit=((Specific/32) mod 2=1)
                          and then R.Information.Coverage=A.IO_Range_Claim'Val(Natural(Specific and 3)-1),"IO claims");
                     end if;
                  end if;
               end;
            end loop;
         end loop;
         Data(4):=0; Data(6):=0;
         for Bit in Natural range 0 .. N*8-1 loop
            for Field in Natural range 0 .. 4 loop
               Data(7 .. Data'Last):=[others=>0];
               Put(Data,7+Field*N,N,Shift_Left(Unsigned_64'(1),Bit));
               declare R : constant A.Decode_Result:=A.Decode(Data,0);
                 V : constant A.Address_Value:=A.Address_Value(Shift_Left(Unsigned_64'(1),Bit)); begin
                  Check(R.Status=A.Decoded and then R.Width=Width and then R.Next_Offset=Byte_Offset(Data'Length),"width and bounds");
                  Check(R.Granularity=(if Field=0 then V else 0) and then R.Minimum=(if Field=1 then V else 0)
                    and then R.Maximum=(if Field=2 then V else 0) and then R.Translation=(if Field=3 then V else 0)
                    and then R.Length=(if Field=4 then V else 0),"field bit position and endian");
               end;
            end loop;
         end loop;
         Data(7 .. Data'Last):=[others=>255];
         declare R : constant A.Decode_Result:=A.Decode(Data,0);
           Max : constant A.Address_Value:=A.Address_Value(Shift_Right(Unsigned_64'Last,64-N*8)); begin
            Check(R.Status=A.Decoded and then R.Granularity=Max and then R.Minimum=Max and then R.Maximum=Max
              and then R.Translation=Max and then R.Length=Max,"full unsigned fields do not imply valid geometry");
         end;
         for Cut in Natural range 0 .. Data'Length-1 loop
            Check(A.Decode(Data(Data'First .. Data'First+Cut-1),0).Status/=A.Decoded,"every truncated prefix");
         end loop;
         declare High : Bytes(Positive'Last-Data'Length+1 .. Positive'Last):=Data;
           Padded : constant Bytes:=[0] & Data; begin
            Check(A.Decode(High,0).Status=A.Decoded,"high lower bound");
            Check(A.Decode(Padded,1).Status=A.Decoded,"explicit nonzero offset");
            Check(A.Decode(Data,Byte_Offset'Last).Status=A.Malformed_Frame,"oversized offset");
            High(High'First+1):=0; High(High'First+2):=0;
            Check(A.Decode(High,0).Status=A.Bad_Length,"short encoded payload");
         end;
      end;
      for Index in Firmware_Tables.Byte loop
         declare Data : constant Bytes:=Make(Width,[Index,16#5C#,16#58#,0]);
           R : constant A.Decode_Result:=A.Decode(Data,0);
           Strict : constant A.Decode_Result:=A.Decode(Data,0,A.Require_Zero); begin
            Check(R.Status=A.Decoded and then R.Source.Present and then R.Source.Index=Index
              and then R.Source.Name_Length=2 and then R.Source.Name_Offset=Byte_Offset(Data'Length-3),"source bytes and offsets");
            Check(Strict.Status=(if Width=A.QWord_Width and then Index/=0 then A.Bad_Source_Index else A.Decoded),"width-specific source policy");
         end;
      end loop;
      Expect(Make(Width,[7]),A.Bad_Source);
      Expect(Make(Width,[0,16#58#]),A.Bad_Source);
      Expect(Make(Width,[0,0,16#58#,0]),A.Bad_Source);
      Expect(Make(Width,[0,16#58#,0,16#79#,0]),A.Bad_Source);
      declare R : constant A.Decode_Result:=A.Decode(Make(Width,[0,0]),0); begin
         Check(R.Status=A.Decoded and then R.Source.Present and then R.Source.Name_Length=0,"empty source retained");
      end;
      declare Tail : Bytes(1 .. 60_000):=[others=>16#58#]; begin
         Tail(1):=0; Tail(Tail'Last):=0;
         declare R : constant A.Decode_Result:=A.Decode(Make(Width,Tail),0); begin
            Check(R.Status=A.Decoded and then R.Source.Name_Length=59_998,"large bounded source");
         end;
      end;
   end loop;
   Ada.Text_IO.Put_Line("Resource address checks" & Natural'Image(Checks));
end Resource_Address_Tests;
