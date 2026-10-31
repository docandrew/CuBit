with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces;
with Firmware_Tables;
with ACPI_EC_Boot_Table;
procedure EC_Boot_Table_Tests is
   use Interfaces;
   use ACPI_EC_Boot_Table;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with "ECDT check" & Checks'Image; end if;
   end Check;
   function Load (Name : String) return Firmware_Tables.Bytes is
      File : Ada.Streams.Stream_IO.File_Type;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Name);
      Ada.Streams.Stream_IO.Read (File, Buffer, Last);
      Ada.Streams.Stream_IO.Close (File);
      return Result : Firmware_Tables.Bytes (1 .. Natural (Last)) do
         for I in Result'Range loop Result (I) := Firmware_Tables.Byte (Buffer (Ada.Streams.Stream_Element_Offset (I))); end loop;
      end return;
   end Load;
   procedure Fix (Data : in out Firmware_Tables.Bytes) is
      Sum : Firmware_Tables.Byte := 0;
   begin
      Data (Data'First + 9) := 0;
      for B of Data loop Sum := Sum + B; end loop;
      Data (Data'First + 9) := -Sum;
   end Fix;
   procedure Set_Length (Data : in out Firmware_Tables.Bytes) is
      Length : Unsigned_32 := Unsigned_32 (Data'Length);
   begin
      for I in 0 .. 3 loop
         Data (Data'First + 4 + I) := Firmware_Tables.Byte (Length and 16#FF#);
         Length := Shift_Right (Length, 8);
      end loop;
   end Set_Length;
   procedure Exercise (Wire : Firmware_Tables.Bytes) is
      Item : Description := Decode (Wire);
      Path : Path_Result := Read_Path (Wire);
      function With_Path (Name : String; Suffix : Natural := 0) return Firmware_Tables.Bytes is
      begin
         return Result : Firmware_Tables.Bytes (1 .. Fixed_Size + Name'Length + 1 + Suffix) := [others => 0] do
            Result (1 .. Fixed_Size) := Wire (Wire'First .. Wire'First + Fixed_Size - 1);
            for I in Name'Range loop Result (Fixed_Size + 1 + I - Name'First) := Character'Pos (Name (I)); end loop;
            if Suffix > 0 then Result (Result'Last) := 16#FF#; end if;
            Set_Length (Result); Fix (Result);
         end return;
      end With_Path;
      procedure Invalid_Name (Name : String; Expected : Path_Status := Invalid_Path) is
         Data : constant Firmware_Tables.Bytes := With_Path (Name);
      begin Check (Read_Path (Data).Status = Expected); end Invalid_Name;
   begin
      Check (Item.Status = Decoded);
      Check (Item.Revision = 1 and then Item.Control.Space = 1 and then Item.Control.Width = 8
             and then Item.Control.Bit_Offset = 0 and then Item.Control.Access_Size = 0
             and then Item.Control.Address = 16#66# and then Item.Data.Address = 16#62#
             and then Item.UID = 0 and then Item.GPE_Bit = 9 and then Item.Suffix_Length = 0);
      Check (Path.Status = Parsed and then Path.Count = 4 and then
             Path.Parts (1) = "_SB_" and then Path.Parts (2) = "PCI0"
             and then Path.Parts (3) = "LPCB" and then Path.Parts (4) = "EC0_");
      Check (Check_Registers (Wire) = Byte_Registers_Described);
      for Length in 0 .. Wire'Length - 1 loop
         Check (Decode (Wire (Wire'First .. Wire'First + Length - 1)).Status /= Decoded);
      end loop;
      for Length in Firmware_Tables.Table_Header_Size .. Fixed_Size loop
         declare Data : Firmware_Tables.Bytes := Wire (Wire'First .. Wire'First + Length - 1); begin
            Set_Length (Data); Fix (Data);
            Check (Decode (Data).Status = Truncated_Fixed_Fields);
         end;
      end loop;
      for I in Wire'Range loop
         declare Data : Firmware_Tables.Bytes := Wire; begin
            Data (I) := Data (I) xor 1;
            Check (Decode (Data).Status = Invalid_Table);
         end;
      end loop;
      for First in 1 .. 3 loop
         declare
            Base : constant Positive := (case First is when 1 => 1, when 2 => 37, when 3 => Positive'Last - Wire'Length + 1);
            Data : Firmware_Tables.Bytes (Base .. Base + (Wire'Length - 1));
         begin
            for I in Wire'Range loop Data (Base + (I - Wire'First)) := Wire (I); end loop;
            Item := Decode (Data); Path := Read_Path (Data);
            Check (Item.Status = Decoded and then Item.Control.Address = 16#66#);
            Check (Path.Status = Parsed and then Path.Parts (4) = "EC0_");
         end;
      end loop;
      Invalid_Name (""); Invalid_Name ("\"); Invalid_Name ("EC0"); Invalid_Name ("^EC0");
      Invalid_Name ("\\EC0"); Invalid_Name ("\.A"); Invalid_Name ("\A."); Invalid_Name ("\A..B");
      Invalid_Name ("\ABCDE"); Invalid_Name ("\1A"); Invalid_Name ("\Ab"); Invalid_Name ("\A/B"); Invalid_Name ("\A B");
      declare Data : constant Firmware_Tables.Bytes := With_Path ("\_SB.PCI0.LPCB.EC0", 1); begin
         Item := Decode (Data); Check (Item.Status = Decoded and then Item.Suffix_Length = 1);
         Check (Read_Path (Data).Status = Parsed);
         Check (Check_Registers (Data) = Unrecognized_Suffix);
      end;
      declare Data : Firmware_Tables.Bytes := With_Path ("\A"); begin
         Data (Data'Last) := Character'Pos ('B'); Fix (Data);
         Check (Decode (Data).Status = Missing_Terminator);
         Data (Fixed_Size + 2) := 16#80#; Fix (Data);
         Check (Decode (Data).Status = Non_ASCII_Identifier);
      end;
      for Count in 255 .. 256 loop
         declare Name : String (1 .. Count * 2) := [others => '.']; begin
            Name (1) := '\';
            for I in 1 .. Count loop Name (I * 2) := 'A'; end loop;
            declare Data : constant Firmware_Tables.Bytes := With_Path (Name); begin
               Path := Read_Path (Data);
               Check (Path.Status = (if Count = 255 then Parsed else Path_Too_Deep));
               if Path.Status = Parsed then
                  Check (Path.Count = 255 and then (for all I in Path.Parts'Range => Path.Parts (I) = "A___"));
               end if;
            end;
         end;
      end loop;
      for Mutation in 0 .. 10 loop
         declare Data : Firmware_Tables.Bytes := Wire; Expected : Register_Status; begin
            case Mutation is
               when 0 => Data (9) := 2; Expected := Unsupported_Revision;
               when 1 => Data (37) := 2; Expected := Unsupported_Space;
               when 2 => Data (38) := 16; Expected := Unsupported_Access;
               when 3 => Data (39) := 1; Expected := Unsupported_Access;
               when 4 => Data (40) := 2; Expected := Unsupported_Access;
               when 5 => Data (43) := 1; Expected := IO_Address_Too_Wide;
               when 6 => Data (41) := 16#62#; Expected := Aliased_Registers;
               when 7 => Data (41) := 0; Expected := Byte_Registers_Described;
               when 8 => Data (40) := 1; Expected := Byte_Registers_Described;
               when 9 => Data (49) := 2; Expected := Unsupported_Space;
               when 10 => Data (52) := 2; Expected := Unsupported_Access;
            end case;
            Fix (Data); Check (Decode (Data).Status = Decoded);
            Check (Check_Registers (Data) = Expected);
         end;
      end loop;
   end Exercise;
begin
   Check (Ada.Command_Line.Argument_Count = 3);
   Exercise (Load (Ada.Command_Line.Argument (1)));
   declare
      Data : constant Firmware_Tables.Bytes := Load (Ada.Command_Line.Argument (2));
      Item : constant Description := Decode (Data);
   begin
      Check (Item.Status = Decoded and then Item.Control.Space = 0
             and then Item.Control.Address = 16#FEDC_BA98_7654_3267#
             and then Item.Data.Space = 0 and then Item.Data.Address = 16#0123_4567_89AB_CDE3#
             and then Item.UID = 16#FEED_BEEF# and then Item.GPE_Bit = 255);
      Check (Check_Registers (Data) = Byte_Registers_Described);
   end;
   declare
      Data : constant Firmware_Tables.Bytes := Load (Ada.Command_Line.Argument (3));
      Item : constant Description := Decode (Data);
   begin
      Check (Item.Status = Decoded and then Item.Path_Length = 0);
      Check (Read_Path (Data).Status = Invalid_Path);
   end;
   Ada.Text_IO.Put_Line ("ECDT checks:" & Checks'Image);
end EC_Boot_Table_Tests;
