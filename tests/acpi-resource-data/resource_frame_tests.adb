pragma Ada_2022;
with Ada.Text_IO;
with Firmware_Tables;
with ACPI_Resource_Data; use ACPI_Resource_Data;
with Resource_Oracle;
procedure Resource_Frame_Tests is
   use type Firmware_Tables.Byte;
   subtype Bytes is Firmware_Tables.Bytes;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Check;
   procedure Status (Data : Bytes; Expected : Template_Status; Limit : Item_Count := Default_Item_Limit) is
      R : constant Scan_Result := Scan (Data,Limit);
   begin
      Check (R.Status=Expected,"template status");
      Check (R.Position<=Byte_Count(Data'Length) and then R.Items<=Limit,"bounded report");
   end Status;
   type Lengths is array (Positive range <>) of Payload_Length;
   Sizes : constant Lengths := [0,1,7,255,256,65_535];
begin
   Status ([],Missing_End_Tag);
   Status ([16#79#,0],Complete,0);
   Status ([16#79#,16#87#],Complete,0);
   Status ([16#79#,1],Bad_Checksum);
   Status ([16#78#],Bad_End_Tag);
   Status ([16#7A#,0,0],Bad_End_Tag);
   Status ([16#79#],Malformed_Frame);
   Status ([16#79#,0,0],Trailing_Data);
   Status ([16#30#,16#79#,0],Complete);
   Status ([16#30#,16#79#,16#57#],Complete);
   Status ([16#30#,16#79#,16#58#],Bad_Checksum);
   Status ([16#30#,16#79#,0],Item_Limit,0);
   Status ([16#30#,16#38#,16#79#,0],Item_Limit,1);
   Status ([16#30#,16#38#,16#79#,0],Complete,2);
   -- Names remain descriptive, including reserved encodings. They do not
   -- become valid resources merely because their frames fit in the buffer.
   Status ([0,16#79#,0],Complete);
   for Tag in Firmware_Tables.Byte range 0 .. 16#7F# loop
      declare
         Size : constant Natural := Natural(Tag mod 8);
         Data : Bytes (17 .. 17+Size) := [others => 16#A5#];
      begin
         Data(Data'First):=Tag;
         declare R : constant Frame := Read_Frame(Data,0); begin
            Check(R.Status=Decoded,"all small headers");
            Check(R.Encoding=Small_Item and then R.Name=Natural(Tag/8) and then R.Header=1
              and then R.Length=Size and then R.Payload=1 and then R.Next_Offset=Byte_Count(Data'Length),"small framing fields");
         end;
         if Size>0 then
            Check(Read_Frame(Data(Data'First .. Data'Last-1),0).Status=Truncated_Payload,"small payload truncation");
         end if;
         Check(Read_Frame(Data,Byte_Count(Data'Length)).Status=No_Header,"exact end offset");
         Check(Read_Frame(Data,Byte_Offset'Last).Status=No_Header,"oversized offset");
      end;
   end loop;
   for Name in Item_Name loop
      for Size of Sizes loop
         declare
            Data : Bytes (1 .. 3+Size) := [others => 0];
         begin
            Data(1):=16#80#+Firmware_Tables.Byte(Name);
            Data(2):=Firmware_Tables.Byte(Size mod 256);
            Data(3):=Firmware_Tables.Byte(Size/256);
            declare R : constant Frame := Read_Frame(Data,0); begin
               Check(R.Status=Decoded,"large header bounds");
               Check(R.Encoding=Large_Item and then R.Name=Name and then R.Header=3
                 and then R.Length=Size and then R.Payload=3 and then R.Next_Offset=Byte_Count(Data'Length),"large framing fields");
            end;
            Check(Read_Frame(Data(1 .. 1),0).Status=Truncated_Header,"large short header1");
            Check(Read_Frame(Data(1 .. 2),0).Status=Truncated_Header,"large short header2");
            if Size>0 then Check(Read_Frame(Data(1 .. Data'Last-1),0).Status=Truncated_Payload,"large short payload"); end if;
         end;
      end loop;
   end loop;
   for Index in Resource_Oracle.Case_Index loop
      declare
         Data : constant Bytes := Resource_Oracle.Template(Index);
         R : constant Scan_Result := Scan(Data,Item_Count(Resource_Oracle.Items(Index)));
      begin
         Check(R.Status=Complete and then R.Items=Item_Count(Resource_Oracle.Items(Index))
           and then R.Position=Byte_Count(Data'Length),"normal ACPICA template");
         if Index/=0 then Status(Data,Item_Limit,Item_Count(Resource_Oracle.Items(Index)-1)); end if;
         for Cut in 0 .. Data'Length-1 loop
            Check(Scan(Data(Data'First .. Data'First+Cut-1)).Status/=Complete,"every prefix incomplete");
         end loop;
      end;
   end loop;
   declare
      High : constant Bytes (Positive'Last-1 .. Positive'Last) := [16#79#,0];
      Header : constant Bytes (Positive'Last-2 .. Positive'Last) := [16#84#,16#FF#,16#FF#];
   begin
      Status(High,Complete);
      Check(Read_Frame(Header,0).Status=Truncated_Payload,"high lower bound maximum encoded payload");
   end;
   Ada.Text_IO.Put_Line ("Resource framing checks" & Checks'Image);
end Resource_Frame_Tests;
