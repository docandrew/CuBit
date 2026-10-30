pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_Resource_Data;
with ACPI_Resource_Data.Basic_Descriptors;
procedure Resource_Basic_Tests is
   package B renames ACPI_Resource_Data.Basic_Descriptors;
   use type B.Decode_Status;
   use type B.Resource_Kind;
   use type B.IRQ_Mask;
   use type B.IRQ_Description;
   use type B.Interrupt_Mode;
   use type B.Interrupt_Polarity;
   use type B.IO_Decoding;
   use type B.Port_Address;
   use type B.Memory_Base_32;
   use type B.Memory_Length_32;
   use type ACPI_Resource_Data.Byte_Offset;
   subtype Bytes is Firmware_Tables.Bytes;
   Checks : Natural:=0;
   procedure Check(Good : Boolean; Label : String) is
   begin Checks:=Checks+1; if not Good then raise Program_Error with Label; end if; end Check;
   procedure Status(Data : Bytes; Expected : B.Decode_Status) is
   begin Check(B.Decode(Data,0).Status=Expected,"decode status"); end Status;
begin
   Status([],B.Malformed_Frame);
   Status([16#80#],B.Malformed_Frame);
   Status([16#22#,0],B.Malformed_Frame);
   Status([16#79#,0],B.Unsupported_Descriptor);
   Status([0],B.Unsupported_Descriptor);
   for Mask in Unsigned_16 loop
      declare
         R : constant B.Decode_Result:=B.Decode([16#22#,Firmware_Tables.Byte(Mask mod 256),Firmware_Tables.Byte(Mask/256)],0);
      begin
         Check(R.Status=B.Decoded,"all IRQ masks");
         Check(R.Value.Kind=B.IRQ_Resource and then R.Value.IRQ.Mask=B.IRQ_Mask(Mask)
           and then R.Value.IRQ.Mode=B.Edge_Triggered and then R.Value.IRQ.Polarity=B.Active_High
           and then not R.Value.IRQ.Shared and then not R.Value.IRQ.Wake_Capable
           and then not R.Value.IRQ.Information_Present and then R.Next_Offset=3,"IRQ defaults and endian");
      end;
   end loop;
   for Info in Firmware_Tables.Byte loop
      declare
         R : constant B.Decode_Result:=B.Decode([16#23#,1,16#80#,Info],0);
         Valid : constant Boolean:=Info in 1|3|5|7|8|10|12|14|17|19|21|23|24|26|28|30|
           33|35|37|39|40|42|44|46|49|51|53|55|56|58|60|62;
      begin
         if Info>=64 then Check(R.Status=B.Reserved_Bits_Set,"IRQ reserved bits");
         elsif not Valid then Check(R.Status=B.Invalid_IRQ_Combination,"PIC trigger/polarity");
         else
            Check(R.Status=B.Decoded,"valid IRQ information");
            Check(R.Value.Kind=B.IRQ_Resource and then R.Value.IRQ.Mask=16#8001#
              and then R.Value.IRQ.Information_Present and then R.Next_Offset=4,"IRQ flags shape");
            Check(R.Value.IRQ.Shared=(Info in 16 .. 31 | 48 .. 63)
              and then R.Value.IRQ.Wake_Capable=(Info in 32 .. 63),"IRQ sharing and wake");
         end if;
         declare
            IO : constant B.Decode_Result:=B.Decode([16#47#,Info,0,16#12#,0,16#FE#,16#10#,8],0);
            Mem : constant B.Decode_Result:=B.Decode([16#86#,9,0,Info,16#EF#,16#CD#,16#AB#,16#89#,16#98#,16#BA#,16#DC#,16#FE#],0);
         begin
            if Info in 0 .. 1 then
               Check(IO.Status=B.Decoded,"IO decoding flag");
               Check(IO.Value.Kind=B.IO_Resource and then IO.Value.IO.Minimum=16#1200#
                 and then IO.Value.IO.Maximum=16#FE00# and then IO.Value.IO.Alignment=16#10#
                 and then IO.Value.IO.Length=8 and then
                 IO.Value.IO.Decoding=(if Info=0 then B.Decode_10 else B.Decode_16),"IO fields");
            else Check(IO.Status=B.Reserved_Bits_Set,"IO reserved information"); end if;
            Check(Mem.Status=B.Decoded,"memory ignored high information bits");
            Check(Mem.Value.Kind=B.Fixed_Memory_32_Resource and then Mem.Value.Memory.Base=16#89ABCDEF#
              and then Mem.Value.Memory.Length=16#FEDCBA98# and then Mem.Value.Memory.Information=Info
              and then Mem.Value.Memory.Declared_Writeable=(Info mod 2=1),"full unsigned memory fields");
         end;
      end;
   end loop;
   declare
      Base : constant B.Decode_Result:=B.Decode([16#23#,16#20#,0,16#31#],0);
   begin
      for Info in Firmware_Tables.Byte range 16#32# .. 16#37# loop
         if Info mod 2=1 then
            declare R : constant B.Decode_Result:=B.Decode([16#23#,16#20#,0,Info],0); begin
               Check(R.Status=B.Decoded and then R.Value.IRQ=Base.Value.IRQ,"IRQ ignored bits retain semantics");
            end;
         end if;
      end loop;
   end;
   for Size in Natural range 0 .. 7 loop
      declare
         Data : Bytes(1 .. 1+Size):=[others=>0];
      begin
         Data(1):=16#20#+Firmware_Tables.Byte(Size);
         if Size in 2 .. 3 then null; else Status(Data,B.Bad_Length); end if;
         Data(1):=16#40#+Firmware_Tables.Byte(Size);
         if Size/=7 then Status(Data,B.Bad_Length); end if;
         Data(1):=16#48#+Firmware_Tables.Byte(Size);
         if Size/=3 then Status(Data,B.Bad_Length); end if;
      end;
   end loop;
   for Size in Natural range 0 .. 12 loop
      declare Data : Bytes(1 .. 3+Size):=[others=>0]; begin
         Data(1):=16#86#; Data(2):=Firmware_Tables.Byte(Size);
         if Size/=9 then Status(Data,B.Bad_Length); end if;
      end;
   end loop;
   declare
      High : constant Bytes(Positive'Last-3 .. Positive'Last):=[16#4B#,16#FF#,16#FF#,16#FF#];
      R : constant B.Decode_Result:=B.Decode(High,0);
      Offset_Data : constant Bytes:=[16#A5#,16#4B#,16#F8#,3,8];
   begin
      Check(R.Status=B.Decoded and then R.Value.Fixed_IO.Base=65_535
        and then R.Value.Fixed_IO.Length=255,"raw FixedIO upper bits preserved");
      Check(B.Decode(Offset_Data,1).Status=B.Decoded and then
        B.Decode(Offset_Data,1).Value.Fixed_IO.Base=16#3F8#,"explicit input offset");
      Check(B.Decode(High,ACPI_Resource_Data.Byte_Offset'Last).Status=B.Malformed_Frame,"oversized offset");
      for Cut in 0 .. High'Length-1 loop Status(High(High'First .. High'First+Cut-1),B.Malformed_Frame); end loop;
   end;
   Ada.Text_IO.Put_Line("Resource basic checks" & Checks'Image);
end Resource_Basic_Tests;
