pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_Resource_Data; use ACPI_Resource_Data;
with ACPI_Resource_Data.DMA_Descriptors;
procedure Resource_DMA_Tests is
   package D renames ACPI_Resource_Data.DMA_Descriptors;
   use type D.Decode_Status;
   use type D.Resource_Kind;
   use type D.Channel_Mask;
   use type D.Request_Line;
   use type D.Channel_Number;
   use type D.Channel_Speed;
   use type D.Transfer_Preference;
   use type D.Fixed_Transfer_Width;
   use type D.Resource_Use;
   use type Firmware_Tables.Bytes;
   subtype Bytes is Firmware_Tables.Bytes;
   Checks : Natural:=0;
   procedure Verify(Good : Boolean; Label : String) is
   begin Checks:=Checks+1; if not Good then raise Program_Error with Label; end if; end Verify;
begin
   Verify(D.Decode([],0).Status=D.Malformed_Frame,"empty");
   Verify(D.Decode([16#79#,0],0).Status=D.Unsupported_Descriptor,"EndTag unsupported");
   Verify(D.Decode([16#85#,0,0],0).Status=D.Unsupported_Descriptor,"large name not small DMA");
   for Mask in Firmware_Tables.Byte loop
      for Info in Firmware_Tables.Byte loop
         declare R : constant D.Decode_Result:=D.Decode([16#2A#,Mask,Info],0); begin
            if Info>=128 then Verify(R.Status=D.Reserved_Bits_Set,"reserved legacy bit");
            elsif Info mod 4=3 then Verify(R.Status=D.Reserved_Transfer_Preference,"reserved preference");
            else
               Verify(R.Status=D.Decoded and then R.Value.Kind=D.Legacy_DMA and then R.Next_Offset=3,"legacy shape");
               Verify(R.Value.Mask=D.Channel_Mask(Mask) and then R.Value.Information=Info
                 and then R.Value.Speed=D.Channel_Speed'Val(Natural(Info)/32)
                 and then R.Value.Bus_Master_Claim=((Info/4) mod 2=1)
                 and then R.Value.Preference=D.Transfer_Preference'Val(Natural(Info mod 4)),"all masks, speeds, ignored and master bits");
            end if;
         end;
      end loop;
   end loop;
   for Value in Unsigned_16 loop
      declare
         Low : constant Firmware_Tables.Byte:=Firmware_Tables.Byte(Value mod 256);
         High : constant Firmware_Tables.Byte:=Firmware_Tables.Byte(Value/256);
         Request : constant D.Decode_Result:=D.Decode([16#55#,Low,High,16#A5#,16#5A#,5],0);
         Channel : constant D.Decode_Result:=D.Decode([16#55#,16#5A#,16#A5#,Low,High,0],0);
      begin
         Verify(Request.Status=D.Decoded and then Request.Value.Kind=D.Fixed_DMA
           and then Request.Value.Request=D.Request_Line(Value) and then Request.Value.Channel=16#5AA5#
           and then Request.Value.Width=D.Width_256 and then Request.Next_Offset=6,"all request lines");
         Verify(Channel.Status=D.Decoded and then Channel.Value.Kind=D.Fixed_DMA
           and then Channel.Value.Request=16#A55A# and then Channel.Value.Channel=D.Channel_Number(Value)
           and then Channel.Value.Width=D.Width_8,"all channels");
      end;
   end loop;
   for Width in Firmware_Tables.Byte loop
      declare R : constant D.Decode_Result:=D.Decode([16#55#,255,255,255,255,Width],0); begin
         if Width<=5 then
            Verify(R.Status=D.Decoded and then R.Value.Width=D.Fixed_Transfer_Width'Val(Natural(Width)),"fixed width codes");
         else Verify(R.Status=D.Reserved_Transfer_Width,"reserved fixed width"); end if;
      end;
   end loop;
   for Purpose in D.Resource_Use loop
      Verify(D.Decode([16#55#,0,0,0,0,2],0,Purpose).Status=
        (if Purpose=D.Current_Resources then D.Decoded else D.Wrong_Use),"fixed DMA only _CRS");
      Verify(D.Decode([16#2A#,0,0],0,Purpose).Status=D.Decoded,"legacy use preserved");
   end loop;
   for Size in Natural range 0 .. 7 loop
      declare Data : Bytes(1 .. Size+1):=[others=>0]; begin
         Data(1):=16#28#+Firmware_Tables.Byte(Size);
         Verify(D.Decode(Data,0).Status=(if Size=2 then D.Decoded else D.Bad_Length),"legacy exact length");
         Data(1):=16#50#+Firmware_Tables.Byte(Size);
         Verify(D.Decode(Data,0).Status=(if Size=5 then D.Decoded else D.Bad_Length),"fixed exact length");
      end;
   end loop;
   for Form in Boolean loop
      declare Data : constant Bytes:=(if Form then [16#55#,0,0,0,0,0] else [16#2A#,255,16#7E#]);
        High : constant Bytes(Positive'Last-Data'Length+1 .. Positive'Last):=Data;
        Padded : constant Bytes:=[0] & Data; begin
         Verify(D.Decode(High,0).Status=D.Decoded,"high array lower bound");
         Verify(D.Decode(Padded,1).Status=D.Decoded,"nonzero offset");
         Verify(D.Decode(Data,Byte_Offset'Last).Status=D.Malformed_Frame,"oversized offset");
         for Cut in Natural range 0 .. Data'Length-1 loop
            Verify(D.Decode(Data(1 .. Cut),0).Status=D.Malformed_Frame,"all truncated prefixes");
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line("Resource DMA checks" & Natural'Image(Checks));
end Resource_DMA_Tests;
