pragma Ada_2022;
package body ACPI_FADT with SPARK_Mode is
   use type Firmware_Tables.Admission;
   function Decode (Data : Firmware_Tables.Bytes) return Result is
      Header : constant Firmware_Tables.Table_Result := Firmware_Tables.Read_Table (Data, "FACP");
      Item : Description;
      function Fits (Offset, Count : Natural) return Boolean is
        (Offset <= Data'Length and then Count <= Data'Length - Offset);
      function B (Offset : Natural) return Unsigned_8 is
        (Data (Data'First + Offset)) with Pre => Fits (Offset, 1);
      function Word (Offset : Natural; Count : Positive) return Unsigned_64
        with Pre => Count <= 8 and then Fits (Offset, Count)
      is
         Value : Unsigned_64 := 0;
      begin
         for I in 0 .. Count - 1 loop
            Value := Value or Shift_Left (Unsigned_64 (B (Offset + I)), 8 * I);
         end loop;
         return Value;
      end Word;
      function W16 (Offset : Natural) return Unsigned_16 is
        (Unsigned_16 (Word (Offset, 2) and 16#FFFF#)) with Pre => Fits (Offset, 2);
      function W32 (Offset : Natural) return Unsigned_32 is
        (Unsigned_32 (Word (Offset, 4) and 16#FFFF_FFFF#)) with Pre => Fits (Offset, 4);
      function GAS (Offset : Natural) return Optional_Address is
      begin
         if not Fits (Offset, 12) then return (others => <>); end if;
         return (True, (B (Offset), B (Offset + 1), B (Offset + 2),
                        B (Offset + 3), Word (Offset + 4, 8)));
      end GAS;
      function Pointer (Offset : Natural) return Optional_Pointer is
      begin
         if not Fits (Offset, 8) then return (others => <>); end if;
         return (True, Word (Offset, 8));
      end Pointer;
      Length_Offsets : constant array (Block_Kind) of Natural := [88, 88, 89, 89, 90, 91, 92, 93];
   begin
      if Header.Status /= Firmware_Tables.Accepted or else
        Header.Extent /= Data'Length or else Data'Length < 116
      then return (Valid => False); end if;
      Item.Revision := Header.Revision;
      Item.FACS := W32 (36); Item.DSDT := W32 (40); Item.Profile := B (45);
      Item.SCI := W16 (46); Item.SMI_Command := W32 (48);
      Item.Enable := B (52); Item.Disable := B (53);
      Item.S4_Request := B (54); Item.P_State_Control := B (55);
      for Kind in Block_Kind loop
         Item.Legacy (Kind) := W32 (56 + Block_Kind'Pos (Kind) * 4);
         Item.Lengths (Kind) := B (Length_Offsets (Kind));
         Item.Extended (Kind) := GAS (148 + Block_Kind'Pos (Kind) * 12);
      end loop;
      Item.GPE1_Base := B (94); Item.C_State_Control := B (95);
      Item.C2_Latency := W16 (96); Item.C3_Latency := W16 (98);
      Item.Flush_Size := W16 (100); Item.Flush_Stride := W16 (102);
      Item.Duty_Offset := B (104); Item.Duty_Width := B (105);
      Item.Day_Alarm := B (106); Item.Month_Alarm := B (107); Item.Century := B (108);
      Item.IA_PC_Boot := W16 (109); Item.Flags := W32 (112);
      Item.Reset := GAS (116);
      if Fits (128, 1) then Item.Reset_Value_Present := True; Item.Reset_Value := B (128); end if;
      if Fits (129, 2) then Item.ARM_Boot_Present := True; Item.ARM_Boot := W16 (129); end if;
      if Fits (131, 1) then Item.Minor_Present := True; Item.Minor := B (131); end if;
      Item.X_FACS := Pointer (132); Item.X_DSDT := Pointer (140);
      Item.Sleep_Control := GAS (244); Item.Sleep_Status := GAS (256);
      Item.Hypervisor := Pointer (268);
      return (True, Item);
   end Decode;
end ACPI_FADT;
