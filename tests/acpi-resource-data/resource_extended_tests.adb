pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_Resource_Data;
with ACPI_Resource_Data.Extended_Interrupts;
procedure Resource_Extended_Tests is
   package E renames ACPI_Resource_Data.Extended_Interrupts;
   use type E.Decode_Status;
   use type E.Interrupt_Number;
   use type E.Interrupt_Mode;
   use type E.Interrupt_Polarity;
   use type E.Resource_Role;
   use type E.Descriptor_Use;
   use type ACPI_Resource_Data.Byte_Count;
   subtype Bytes is Firmware_Tables.Bytes;
   Checks : Natural:=0;
   procedure Check(Good : Boolean; Label : String) is
   begin Checks:=Checks+1; if not Good then raise Program_Error with Label; end if; end Check;
   procedure Put_Number(Data : in out Bytes; Start : Positive; Value : Unsigned_32) is
   begin
      for I in Natural range 0 .. 3 loop
         Data(Start+I):=Firmware_Tables.Byte(Shift_Right(Value,8*I) and 255);
      end loop;
   end Put_Number;
   procedure Status(Data : Bytes; Expected : E.Decode_Status) is
   begin Check(E.Decode(Data,0).Status=Expected,"extended decode status"); end Status;
   type Lengths is array(Positive range <>) of Natural;
   Name_Lengths : constant Lengths:=[0,1,4,16,256,60_000];
begin
   Status([],E.Malformed_Frame);
   Status([16#89#],E.Malformed_Frame);
   Status([16#22#,0,0],E.Unsupported_Descriptor);
   Status([16#8A#,6,0,1,1,0,0,0,0],E.Unsupported_Descriptor);
   Status([16#89#,5,0,1,1,0,0,0],E.Bad_Length);
   Status([16#89#,6,0,1,0,0,0,0,0],E.Bad_Interrupt_Count);
   Status([16#89#,6,0,1,2,0,0,0,0],E.Bad_Length);
   for Flags in Firmware_Tables.Byte loop
      declare
         R : constant E.Decode_Result:=E.Decode([16#89#,6,0,Flags,1,16#EF#,16#CD#,16#AB#,16#89#],0);
      begin
         if Flags>=32 then Check(R.Status=E.Reserved_Bits_Set,"extended reserved flags");
         else
            Check(R.Status=E.Decoded,"all extended trigger/polarity combinations");
            Check(R.Count=1 and then R.Interrupts(1)=16#89ABCDEF# and then R.Next_Offset=9,"extended full unsigned number");
            Check(R.Role=(if Flags mod 2=1 then E.Consumer else E.Producer),"producer/consumer");
            Check(R.Mode=(if (Flags/2) mod 2=1 then E.Edge_Triggered else E.Level_Triggered),"extended trigger");
            Check(R.Polarity=(if (Flags/4) mod 2=1 then E.Active_Low else E.Active_High),"extended polarity");
            Check(R.Shared=((Flags/8) mod 2=1) and then R.Wake_Capable=((Flags/16) mod 2=1),"extended sharing/wake");
            Check(not R.Source.Present,"absent source");
         end if;
      end;
   end loop;
   for Count in E.Interrupt_Count loop
      declare
         Payload : constant Natural:=2+4*Count;
         Data : Bytes(1 .. 3+Payload):=[others=>0];
      begin
         Data(1):=16#89#; Data(2):=Firmware_Tables.Byte(Payload mod 256); Data(3):=Firmware_Tables.Byte(Payload/256);
         Data(4):=1; Data(5):=Firmware_Tables.Byte(Count);
         for I in 1 .. Count loop Put_Number(Data,6+4*(I-1),Unsigned_32(I)*16#010203#); end loop;
         declare R : constant E.Decode_Result:=E.Decode(Data,0,E.Possible_Resources); begin
            Check(R.Status=E.Decoded and then R.Count=Count and then R.Next_Offset=ACPI_Resource_Data.Byte_Count(Data'Length),"all possible interrupt counts");
            for I in 1 .. Count loop Check(R.Interrupts(I)=E.Interrupt_Number(Unsigned_32(I)*16#010203#),"entire list endian/order"); end loop;
            for I in Count+1 .. E.Maximum_Interrupts loop Check(R.Interrupts(I)=0,"unused entries zero"); end loop;
         end;
         for Purpose in E.Descriptor_Use range E.Current_Resources .. E.Set_Resources loop
            if Purpose/=E.Possible_Resources then
               declare R : constant E.Decode_Result:=E.Decode(Data,0,Purpose); begin
                  Check(R.Status=(if Count=1 then E.Decoded else E.Wrong_Usage_Count),"_CRS/_SRS count constraint");
               end;
            end if;
         end loop;
         Check(E.Decode(Data(1 .. Data'Last-1),0,E.Possible_Resources).Status=E.Malformed_Frame,"truncated list frame");
      end;
   end loop;
   for Length of Name_Lengths loop
      declare
         Payload : constant Natural:=8+Length;
         Data : Bytes(1 .. 3+Payload):=[others=>Character'Pos('A')];
      begin
         Data(1):=16#89#; Data(2):=Firmware_Tables.Byte(Payload mod 256); Data(3):=Firmware_Tables.Byte(Payload/256);
         Data(4):=1; Data(5):=1; Put_Number(Data,6,16#FFFFFFFF#); Data(10):=0; Data(Data'Last):=0;
         declare R : constant E.Decode_Result:=E.Decode(Data,0,E.Current_Resources,E.Require_Zero); begin
            Check(R.Status=E.Decoded and then R.Source.Present,"optional source lengths");
            Check(R.Source.Index=0 and then R.Source.Name_Offset=10
              and then R.Source.Name_Length=ACPI_Resource_Data.Byte_Count(Length)
              and then R.Interrupts(1)=16#FFFFFFFF#,"bounded raw source span");
         end;
      end;
   end loop;
   for Index in Firmware_Tables.Byte loop
      declare
         Data : constant Bytes:=[16#89#,9,0,1,1,0,0,0,0,Index,Character'Pos('X'),0];
         Legacy : constant E.Decode_Result:=E.Decode(Data,0);
         Negotiated : constant E.Decode_Result:=E.Decode(Data,0,E.Current_Resources,E.Require_Zero);
      begin
         Check(Legacy.Status=E.Decoded and then Legacy.Source.Index=Index,"unnegotiated raw source index");
         Check(Negotiated.Status=(if Index=0 then E.Decoded else E.Bad_Source_Index),"negotiated zero index");
      end;
   end loop;
   Status([16#89#,7,0,1,1,0,0,0,0,7],E.Bad_Source);
   Status([16#89#,8,0,1,1,0,0,0,0,7,Character'Pos('X')],E.Bad_Source);
   Status([16#89#,8,0,1,1,0,0,0,0,7,Character'Pos('X'),16#79#,0],E.Bad_Source);
   Status([16#89#,10,0,1,1,0,0,0,0,0,Character'Pos('A'),0,Character'Pos('B')],E.Bad_Source);
   Status([16#89#,10,0,1,1,0,0,0,0,0,Character'Pos('A'),0,0],E.Bad_Source);
   declare
      High : constant Bytes(Positive'Last-8 .. Positive'Last):=[16#89#,6,0,1,1,16#78#,16#56#,16#34#,16#12#];
      R : constant E.Decode_Result:=E.Decode(High,0);
      Offset_Data : constant Bytes:=[16#A5#,16#89#,6,0,1,1,16#FF#,16#FF#,16#FF#,16#FF#];
   begin
      Check(R.Status=E.Decoded and then R.Interrupts(1)=16#12345678#,"high array bound");
      Check(E.Decode(Offset_Data,1).Status=E.Decoded and then E.Decode(Offset_Data,1).Interrupts(1)=16#FFFFFFFF#,"nonzero byte offset");
      Check(E.Decode(High,ACPI_Resource_Data.Byte_Offset'Last).Status=E.Malformed_Frame,"oversized offset");
      for Cut in 0 .. High'Length-1 loop Status(High(High'First .. High'First+Cut-1),E.Malformed_Frame); end loop;
   end;
   Ada.Text_IO.Put_Line("Resource extended checks" & Checks'Image);
end Resource_Extended_Tests;
