package body AML_Resource_Templates with SPARK_Mode is
   Small_Header : constant Positive := 1;
   Large_Header : constant Positive := 3;
   End_Tag_Size : constant Positive := 2;
   Serial_Type_Offset : constant Natural := 5;
   Large_Compatibility_Window : constant Positive := Serial_Type_Offset + 1;
   Large_Bit : constant AML_Decode.Byte := 16#80#;
   End_Tag : constant AML_Decode.Byte := 16#79#;
   Large_Name_Mask : constant AML_Decode.Byte := 16#7F#;
   Small_Length_Mask : constant AML_Decode.Byte := 7;
   Small_Name_Divisor : constant AML_Decode.Byte := 8;
   Byte_Radix : constant Positive := 256;
   Length_Low_Offset : constant Positive := 1;
   Length_High_Offset : constant Positive := 2;
   Serial_First_Type : constant AML_Decode.Byte := 1;
   Serial_Last_Type : constant AML_Decode.Byte := 4;
   subtype Small_Name is Natural range 0 .. 15;
   subtype Large_Name is Natural range 0 .. 127;
   subtype Payload_Length is Natural range 0 .. 65_535;
   type Length_Policy is (Reserved, Exact, Minimum, Optional_Last);
   type Rule is record
      Policy : Length_Policy;
      Size : Payload_Length;
   end record;
   IRQ_Code : constant Small_Name := 4;
   IRQ_Payload : constant Payload_Length := 3;
   DMA_Code : constant Small_Name := 5;
   DMA_Payload : constant Payload_Length := 2;
   Start_Dependent_Code : constant Small_Name := 6;
   Start_Dependent_Payload : constant Payload_Length := 1;
   End_Dependent_Code : constant Small_Name := 7;
   End_Dependent_Payload : constant Payload_Length := 0;
   IO_Code : constant Small_Name := 8;
   IO_Payload : constant Payload_Length := 7;
   Fixed_IO_Code : constant Small_Name := 9;
   Fixed_IO_Payload : constant Payload_Length := 3;
   Fixed_DMA_Code : constant Small_Name := 10;
   Fixed_DMA_Payload : constant Payload_Length := 5;
   Vendor_Small_Code : constant Small_Name := 14;
   Vendor_Small_Payload : constant Payload_Length := 0;
   End_Tag_Code : constant Small_Name := 15;
   End_Tag_Payload : constant Payload_Length := 1;
   Memory24_Code : constant Large_Name := 1;
   Memory24_Payload : constant Payload_Length := 9;
   Register_Code : constant Large_Name := 2;
   Register_Payload : constant Payload_Length := 12;
   Vendor_Large_Code : constant Large_Name := 4;
   Vendor_Large_Payload : constant Payload_Length := 0;
   Memory32_Code : constant Large_Name := 5;
   Memory32_Payload : constant Payload_Length := 17;
   Fixed_Memory32_Code : constant Large_Name := 6;
   Fixed_Memory32_Payload : constant Payload_Length := 9;
   Address32_Code : constant Large_Name := 7;
   Address32_Payload : constant Payload_Length := 23;
   Address16_Code : constant Large_Name := 8;
   Address16_Payload : constant Payload_Length := 13;
   Extended_IRQ_Code : constant Large_Name := 9;
   Extended_IRQ_Payload : constant Payload_Length := 6;
   Address64_Code : constant Large_Name := 10;
   Address64_Payload : constant Payload_Length := 43;
   Extended_Address64_Code : constant Large_Name := 11;
   Extended_Address64_Payload : constant Payload_Length := 53;
   GPIO_Code : constant Large_Name := 12;
   GPIO_Payload : constant Payload_Length := 20;
   Pin_Function_Code : constant Large_Name := 13;
   Pin_Function_Payload : constant Payload_Length := 15;
   Serial_Code : constant Large_Name := 14;
   Serial_Payload : constant Payload_Length := 9;
   Pin_Config_Code : constant Large_Name := 15;
   Pin_Config_Payload : constant Payload_Length := 17;
   Pin_Group_Code : constant Large_Name := 16;
   Pin_Group_Payload : constant Payload_Length := 11;
   Pin_Group_Function_Code : constant Large_Name := 17;
   Pin_Group_Function_Payload : constant Payload_Length := 14;
   Pin_Group_Config_Code : constant Large_Name := 18;
   Pin_Group_Config_Payload : constant Payload_Length := 17;
   Clock_Input_Code : constant Large_Name := 19;
   Clock_Input_Payload : constant Payload_Length := 9;
   -- Packed ACPICA sizes; Vendor_Small zero is a compatibility allowance.
   Small_Rules : constant array (Small_Name) of Rule :=
     [IRQ_Code => (Optional_Last, IRQ_Payload),
      DMA_Code => (Exact, DMA_Payload),
      Start_Dependent_Code => (Optional_Last, Start_Dependent_Payload),
      End_Dependent_Code => (Exact, End_Dependent_Payload),
      IO_Code => (Exact, IO_Payload),
      Fixed_IO_Code => (Exact, Fixed_IO_Payload),
      Fixed_DMA_Code => (Exact, Fixed_DMA_Payload),
      Vendor_Small_Code => (Minimum, Vendor_Small_Payload),
      End_Tag_Code => (Exact, End_Tag_Payload),
      others => (Reserved, 0)];
   Large_Rules : constant array (Large_Name) of Rule :=
     [Memory24_Code => (Exact, Memory24_Payload),
      Register_Code => (Exact, Register_Payload),
      Vendor_Large_Code => (Minimum, Vendor_Large_Payload),
      Memory32_Code => (Exact, Memory32_Payload),
      Fixed_Memory32_Code => (Exact, Fixed_Memory32_Payload),
      Address32_Code => (Minimum, Address32_Payload),
      Address16_Code => (Minimum, Address16_Payload),
      Extended_IRQ_Code => (Minimum, Extended_IRQ_Payload),
      Address64_Code => (Minimum, Address64_Payload),
      Extended_Address64_Code => (Exact, Extended_Address64_Payload),
      GPIO_Code => (Minimum, GPIO_Payload),
      Pin_Function_Code => (Minimum, Pin_Function_Payload),
      Serial_Code => (Minimum, Serial_Payload),
      Pin_Config_Code => (Minimum, Pin_Config_Payload),
      Pin_Group_Code => (Minimum, Pin_Group_Payload),
      Pin_Group_Function_Code => (Minimum, Pin_Group_Function_Payload),
      Pin_Group_Config_Code => (Minimum, Pin_Group_Config_Payload),
      Clock_Input_Code => (Minimum, Clock_Input_Payload),
      others => (Reserved, 0)];
   function Length_Valid (Item : Rule; Size : Payload_Length) return Boolean is
     (case Item.Policy is
       when Reserved => False,
       when Exact => Size = Item.Size,
       when Minimum => Size >= Item.Size,
       when Optional_Last => Size = Item.Size or else Size + 1 = Item.Size);
   function Locate_End (Data : AML_Decode.Bytes) return Location is
      Cursor : Natural := 0;
      Remaining : Natural;
      Header : Positive;
      Name : Large_Name;
      Size : Payload_Length;
      Item : Rule;
      Is_Large : Boolean;
      Tag : AML_Decode.Byte;
   begin
      if Data'Length = 0 then
         return (Located, 0);
      end if;
      if Data'Length < End_Tag_Size then
         return (Status => No_End_Tag);
      end if;
      while Cursor < Data'Length loop
         Remaining := Data'Length - Cursor;
         if Remaining < End_Tag_Size then
            return (Status => Buffer_Length);
         end if;
         Tag := Data (Data'First + Cursor);
         Is_Large := (Tag and Large_Bit) /= 0;
         if Is_Large then
            if Remaining < Large_Compatibility_Window then
               return (Status => Buffer_Length);
            end if;
            Header := Large_Header;
            Name := Natural (Tag and Large_Name_Mask);
            Item := Large_Rules (Name);
            Size := Natural
              (Data (Data'First + (Cursor + Length_Low_Offset)))
              + Byte_Radix * Natural
                (Data (Data'First + (Cursor + Length_High_Offset)));
         else
            Header := Small_Header;
            Name := Natural (Tag / Small_Name_Divisor);
            Item := Small_Rules (Name);
            Size := Natural (Tag and Small_Length_Mask);
         end if;
         if Item.Policy = Reserved then
            return (Status => Invalid_Resource_Type);
         end if;
         if not Length_Valid (Item, Size) then
            return (Status => Bad_Resource_Length);
         end if;
         if Is_Large and then Name = Serial_Code and then
           Data (Data'First + (Cursor + Serial_Type_Offset))
             not in Serial_First_Type .. Serial_Last_Type
         then
            return (Status => Invalid_Resource_Type);
         end if;
         if Size > Remaining - Header then
            return (Status => Buffer_Length);
         end if;
         if not Is_Large and then Name = End_Tag_Code then
            return (Located, Cursor);
         end if;
         Cursor := Cursor + (Header + Size);
      end loop;
      return (Status => No_End_Tag);
   end Locate_End;

   function Build (Left, Right : AML_Decode.Bytes) return Build_Result is
      L : constant Location := Locate_End (Left);
      function Failure (Status : Scan_Status) return Build_Result is
      begin
         case Status is
            when Invalid_Resource_Type =>
               return (Status => Invalid_Resource_Type);
            when Bad_Resource_Length =>
               return (Status => Bad_Resource_Length);
            when Buffer_Length =>
               return (Status => Buffer_Length);
            when No_End_Tag =>
               return (Status => No_End_Tag);
            when Located =>
               return (Status => Output_Limit);
         end case;
      end Failure;
   begin
      if L.Status /= Located then
         return Failure (L.Status);
      end if;
      declare
         R : constant Location := Locate_End (Right);
      begin
         if R.Status /= Located then
            return Failure (R.Status);
         end if;
         if Max_Result_Length < End_Tag_Size
           or else L.Prefix_Length > Max_Result_Length - End_Tag_Size
           or else R.Prefix_Length > Max_Result_Length - End_Tag_Size - L.Prefix_Length
         then
            return (Status => Output_Limit);
         end if;
         return Result : Build_Result (Built) do
            Result.Length := L.Prefix_Length + R.Prefix_Length + End_Tag_Size;
            for I in 1 .. L.Prefix_Length loop
               Result.Data (I) := Left (Left'First + (I - 1));
            end loop;
            for I in 1 .. R.Prefix_Length loop
               Result.Data (L.Prefix_Length + I) := Right (Right'First + (I - 1));
            end loop;
            Result.Data (Result.Length - 1) := End_Tag;
            -- Default zero initialization supplies EndTag checksum and suffix.
         end return;
      end;
   end Build;
end AML_Resource_Templates;
