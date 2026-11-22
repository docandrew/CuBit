pragma Ada_2022;
package body Firmware_Tables.Power_Registers with SPARK_Mode is
   Bits_Per_Byte : constant := 8;

   --  FADT field offsets (ACPI 6.5, Table 5.9).
   PM1A_Control_Offset   : constant := 64;
   PM1B_Control_Offset   : constant := 68;
   PM1_Control_Length    : constant := 89;
   SMI_Command_Offset    : constant := 48;
   Flags_Offset          : constant := 112;
   Reset_Register_Offset : constant := 116;
   Reset_Value_Offset    : constant := 128;
   X_PM1A_Control_Offset : constant := 172;
   X_PM1B_Control_Offset : constant := 184;
   Sleep_Control_Offset  : constant := 244;
   Flags_Bytes   : constant := 4;
   Port_Bytes    : constant := 4;
   Address_Bytes : constant := 8;

   --  FADT flags.
   Reset_Register_Supported : constant Unsigned_32 := 2 ** 10;
   Hardware_Reduced_ACPI    : constant Unsigned_32 := 2 ** 20;

   --  Generic Address Structure (ACPI 6.5, 5.2.3.2).
   GAS_Bytes          : constant := 12;
   GAS_Space          : constant := 0;
   GAS_Bit_Width      : constant := 1;
   GAS_Bit_Offset     : constant := 2;
   GAS_Access_Size    : constant := 3;
   GAS_Address        : constant := 4;
   Space_System_Memory : constant Byte := 0;
   Space_System_IO     : constant Byte := 1;
   Access_Undefined    : constant Byte := 0;
   Access_Byte         : constant Byte := 1;
   Access_Word         : constant Byte := 2;

   function Read_LE (Data : Bytes; Offset : Natural; Width : Positive)
     return Unsigned_64
     with Pre => Width <= Address_Bytes and then Data'Length >= Width
       and then Offset <= Data'Length - Width
   is
      Value : Unsigned_64 := 0;
   begin
      for I in 0 .. Width - 1 loop
         Value := Value or Shift_Left
           (Unsigned_64 (Data (Data'First + Offset + I)), Bits_Per_Byte * I);
      end loop;
      return Value;
   end Read_LE;

   function Read_DWord (Data : Bytes; Offset : Natural) return Unsigned_32 is
     (Unsigned_32 (Data (Data'First + Offset))
      or Shift_Left (Unsigned_32 (Data (Data'First + Offset + 1)), Bits_Per_Byte)
      or Shift_Left (Unsigned_32 (Data (Data'First + Offset + 2)), 2 * Bits_Per_Byte)
      or Shift_Left (Unsigned_32 (Data (Data'First + Offset + 3)), 3 * Bits_Per_Byte))
     with Pre => Data'Length >= Flags_Bytes
       and then Offset <= Data'Length - Flags_Bytes;

   --  The register a GAS at Offset names, if it is a whole Wanted-byte
   --  register at bit offset 0.
   function GAS_At (Data : Bytes; Offset : Natural; Wanted : Register_Bytes)
     return Register
     with Pre  => Wanted in Byte_Register_Bytes .. PM1_Control_Bytes
                  and then Data'Length >= GAS_Bytes
                  and then Offset <= Data'Length - GAS_Bytes,
          Post => Well_Formed (GAS_At'Result)
                  and then (if GAS_At'Result.Present then GAS_At'Result.Bytes = Wanted)
   is
      Space   : constant Byte := Data (Data'First + Offset + GAS_Space);
      Width   : constant Byte := Data (Data'First + Offset + GAS_Bit_Width);
      Shift   : constant Byte := Data (Data'First + Offset + GAS_Bit_Offset);
      Access_Size : constant Byte := Data (Data'First + Offset + GAS_Access_Size);
      Address : constant Unsigned_64 := Read_LE (Data, Offset + GAS_Address, Address_Bytes);
      Kind    : constant Register_Space :=
        (if Space = Space_System_IO then System_IO
         elsif Space = Space_System_Memory then System_Memory
         else Other_Space);
      Natural_Access : constant Byte :=
        (if Wanted = PM1_Control_Bytes then Access_Word else Access_Byte);
   begin
      if Address = 0 or else Shift /= 0
        or else Natural (Width) /= Wanted * Bits_Per_Byte
        or else Access_Size not in Access_Undefined | Natural_Access
      then
         return No_Register;
      end if;
      if Kind = System_IO
        and then Address > Port_Space_Last - Address_Value (Wanted - 1)
      then
         return No_Register;
      end if;
      return (Present => True, Space => Kind, Address => Address, Bytes => Wanted);
   end GAS_At;

   --  A legacy PM1x_CNT_BLK: a port, with PM1_CNT_LEN at least two bytes.
   function Legacy_Port (Data : Bytes; Offset : Natural; Length : Byte)
     return Register
     with Pre  => Data'Length >= Port_Bytes
                  and then Offset <= Data'Length - Port_Bytes,
          Post => Well_Formed (Legacy_Port'Result)
                  and then (if Legacy_Port'Result.Present then
                              Legacy_Port'Result.Bytes = PM1_Control_Bytes
                              and then Legacy_Port'Result.Space = System_IO)
   is
      Port : constant Unsigned_64 := Read_LE (Data, Offset, Port_Bytes);
   begin
      if Port = 0 or else Natural (Length) < PM1_Control_Bytes
        or else Port > Port_Space_Last - Address_Value (PM1_Control_Bytes - 1)
      then
         return No_Register;
      end if;
      return (Present => True, Space => System_IO, Address => Port,
              Bytes => PM1_Control_Bytes);
   end Legacy_Port;

   --  A PM1 control block: the extended form when the table has it and it
   --  names a usable register, else the legacy port.
   function PM1_Control
     (Data : Bytes; Extent : Positive; X_Offset, Legacy_Offset : Natural;
      Length : Byte) return Register
     with Pre  => Extent <= Data'Length
                  and then Legacy_Offset <= Extent - Port_Bytes
                  and then Extent >= Port_Bytes,
          Post => Well_Formed (PM1_Control'Result)
                  and then (if PM1_Control'Result.Present then
                              PM1_Control'Result.Bytes = PM1_Control_Bytes)
   is
      Extended : Register := No_Register;
   begin
      if Extent >= GAS_Bytes and then X_Offset <= Extent - GAS_Bytes then
         Extended := GAS_At (Data, X_Offset, PM1_Control_Bytes);
      end if;
      if Extended.Present then
         return Extended;
      end if;
      return Legacy_Port (Data, Legacy_Offset, Length);
   end PM1_Control;

   function Decode (Data : Bytes) return Description is
      Table  : constant Table_Result := Read_Table (Data, "FACP");
      Result : Description := No_Description;
      Flags  : Unsigned_32;
      Length : Byte;
   begin
      if Table.Status /= Accepted or else Table.Extent < Flags_Offset + Flags_Bytes then
         return No_Description;
      end if;
      Flags := Read_DWord (Data, Flags_Offset);
      Length := Data (Data'First + PM1_Control_Length);
      --  SMI_CMD is a 32-bit System I/O port; a value past the port space
      --  is not one, and hardware-reduced firmware has none.
      if (Flags and Hardware_Reduced_ACPI) = 0 then
         declare
            Port : constant Unsigned_32 := Read_DWord (Data, SMI_Command_Offset);
         begin
            if Address_Value (Port) <= Port_Space_Last then
               Result.SMI_Command := Address_Value (Port);
            end if;
         end;
      end if;

      if (Flags and Hardware_Reduced_ACPI) /= 0 then
         if Table.Extent >= Sleep_Control_Offset + GAS_Bytes then
            Result.Sleep_Control := GAS_At (Data, Sleep_Control_Offset, Byte_Register_Bytes);
         end if;
         Result.Model :=
           (if Result.Sleep_Control.Present then Hardware_Reduced else No_Sleep_Registers);
      else
         Result.PM1A_Control :=
           PM1_Control (Data, Table.Extent, X_PM1A_Control_Offset, PM1A_Control_Offset, Length);
         if Result.PM1A_Control.Present then
            Result.PM1B_Control :=
              PM1_Control (Data, Table.Extent, X_PM1B_Control_Offset, PM1B_Control_Offset, Length);
            Result.Model :=
              (if Result.PM1B_Control.Present then PM1A_And_B else PM1A_Only);
         end if;
      end if;

      if (Flags and Reset_Register_Supported) /= 0
        and then Table.Extent > Reset_Value_Offset
      then
         Result.Reset := GAS_At (Data, Reset_Register_Offset, Byte_Register_Bytes);
         if Result.Reset.Present then
            Result.Reset_Value := Data (Data'First + Reset_Value_Offset);
         end if;
      end if;
      return Result;
   end Decode;
end Firmware_Tables.Power_Registers;
