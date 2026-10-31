pragma Ada_2022;
with Interfaces;
with ACPI_Resource_Data.Basic_Descriptors;
with ACPI_Resource_Data.Address_Spaces;
with ACPI_Resource_Data.Memory_Ranges;
with ACPI_Resource_Data.Extended_Interrupts;
with ACPI_Resource_Data.GPIO_Connections;
package body ACPI_EC_Resources with SPARK_Mode is
   package R renames ACPI_Resource_Data;
   package B renames R.Basic_Descriptors;
   package A renames R.Address_Spaces;
   package M renames R.Memory_Ranges;
   package E renames R.Extended_Interrupts;
   package G renames R.GPIO_Connections;
   use Interfaces;
   use type R.Template_Status, R.Frame_Status, R.Item_Encoding, R.Byte_Offset;
   use type B.Decode_Status, B.IO_Decoding, B.Port_Address, B.Memory_Length_32, B.Resource_Kind;
   use type A.Decode_Status, A.Resource_Kind, A.Address_Value, A.Decode_Mode;
   use type M.Decode_Status, M.Memory_Value;
   use type E.Decode_Status, G.Decode_Status, G.Connection_Kind;
   IRQ_Name : constant R.Item_Name := 4;
   IO_Name : constant R.Item_Name := 8;
   Fixed_IO_Name : constant R.Item_Name := 9;
   End_Name : constant R.Item_Name := 15;
   Memory24_Name : constant R.Item_Name := 1;
   Memory32_Name : constant R.Item_Name := 5;
   Fixed_Memory_Name : constant R.Item_Name := 6;
   Dword_Name : constant R.Item_Name := 7;
   Word_Name : constant R.Item_Name := 8;
   Extended_IRQ_Name : constant R.Item_Name := 9;
   Qword_Name : constant R.Item_Name := 10;
   GPIO_Name : constant R.Item_Name := 12;
   System_Memory : constant Unsigned_8 := 0;
   System_IO : constant Unsigned_8 := 1;
   Register_Bits : constant Unsigned_8 := 8;
   Byte_Access : constant Unsigned_8 := 1;
   function Describe (Data : Firmware_Tables.Bytes;
      Maximum_Items : R.Item_Count := R.Default_Item_Limit) return Result
   is
      Scan : constant R.Scan_Result := R.Scan (Data, Maximum_Items);
      Offset : R.Byte_Offset := 0;
      Count : Natural range 0 .. Register_Role'Pos (Register_Role'Last) + 1 := 0;
      Registers : Claims;
      Has_GPIO : Boolean := False;
      GPIO_Offset : R.Byte_Offset := 0;
      Claim : Register_Claim;
      Is_Register : Boolean;
      Frame : R.Frame;
   begin
      if Scan.Status /= R.Complete then return (Outcome => Bad_Template); end if;
      while Offset < R.Byte_Offset (Data'Length) loop
         Frame := R.Read_Frame (Data, Offset);
         if Frame.Status /= R.Decoded then return (Outcome => Bad_Template); end if;
         Claim := (others => <>);
         Claim.Address.Width := Register_Bits;
         Claim.Address.Access_Size := Byte_Access;
         Is_Register := False;
         if Frame.Encoding = R.Small_Item then
            if Frame.Name = End_Name then exit; end if;
            if Frame.Name not in IRQ_Name | IO_Name | Fixed_IO_Name then
               return (Outcome => Unsupported_Descriptor);
            end if;
            declare Item : constant B.Decode_Result := B.Decode (Data, Offset); begin
               if Item.Status /= B.Decoded then return (Outcome => Malformed_Descriptor); end if;
               case Item.Value.Kind is
                  when B.IRQ_Resource => null;
                  when B.IO_Resource =>
                     if Item.Value.IO.Minimum /= Item.Value.IO.Maximum or Item.Value.IO.Length /= 1 then
                        return (Outcome => Unsupported_Layout);
                     end if;
                     Claim.Address.Space := System_IO;
                     Claim.Address.Address := Unsigned_64 (Item.Value.IO.Minimum);
                     Claim.IO_Decode_10 := Item.Value.IO.Decoding = B.Decode_10;
                     Is_Register := True;
                  when B.Fixed_IO_Resource =>
                     if Item.Value.Fixed_IO.Length /= 1 then return (Outcome => Unsupported_Layout); end if;
                     Claim.Address.Space := System_IO;
                     Claim.Address.Address := Unsigned_64 (Item.Value.Fixed_IO.Base);
                     Claim.IO_Decode_10 := True;
                     Is_Register := True;
                  when B.Fixed_Memory_32_Resource => return (Outcome => Malformed_Descriptor);
               end case;
            end;
         else
            case Frame.Name is
               when Fixed_Memory_Name =>
                  declare Item : constant B.Decode_Result := B.Decode (Data, Offset); begin
                     if Item.Status /= B.Decoded or else Item.Value.Kind /= B.Fixed_Memory_32_Resource then
                        return (Outcome => Malformed_Descriptor);
                     elsif Item.Value.Memory.Length /= 1 then return (Outcome => Unsupported_Layout);
                     end if;
                     Claim.Address.Space := System_Memory;
                     Claim.Address.Address := Unsigned_64 (Item.Value.Memory.Base);
                     Claim.Writeability_Declared := True;
                     Claim.Declared_Writeable := Item.Value.Memory.Declared_Writeable;
                     Is_Register := True;
                  end;
               when Memory24_Name | Memory32_Name =>
                  declare Item : constant M.Decode_Result := M.Decode (Data, Offset); begin
                     if Item.Status /= M.Decoded then return (Outcome => Malformed_Descriptor);
                     elsif Item.Minimum_Base /= Item.Maximum_Base or Item.Length /= 1 then
                        return (Outcome => Unsupported_Layout);
                     end if;
                     Claim.Address.Space := System_Memory;
                     Claim.Address.Address := Unsigned_64 (Item.Minimum_Base);
                     Claim.Writeability_Declared := True;
                     Claim.Declared_Writeable := Item.Declared_Writeable;
                     Is_Register := True;
                  end;
               when Word_Name | Dword_Name | Qword_Name =>
                  declare Item : constant A.Decode_Result := A.Decode (Data, Offset); begin
                     if Item.Status /= A.Decoded then return (Outcome => Malformed_Descriptor);
                     elsif Item.Information.Kind not in A.Memory_Range | A.IO_Range
                       or else not Item.Minimum_Fixed or else not Item.Maximum_Fixed
                       or else Item.Minimum /= Item.Maximum or else Item.Length /= 1
                       or else Item.Translation /= 0 or else Item.Granularity /= 0
                       or else Item.Mode /= A.Positive_Decode or else Item.Source.Present
                     then return (Outcome => Unsupported_Layout);
                     end if;
                     if Item.Information.Kind = A.Memory_Range then
                        if Item.Information.Memory_To_IO then return (Outcome => Unsupported_Layout); end if;
                        Claim.Address.Space := System_Memory;
                        Claim.Writeability_Declared := True;
                        Claim.Declared_Writeable := Item.Information.Declared_Writeable;
                     else
                        if Item.Information.IO_To_Memory or Item.Information.Sparse_Bit then
                           return (Outcome => Unsupported_Layout);
                        end if;
                        if Item.Minimum > A.Address_Value (Unsigned_16'Last) then
                           return (Outcome => Unsupported_Layout);
                        end if;
                        Claim.Address.Space := System_IO;
                     end if;
                     Claim.Address.Address := Unsigned_64 (Item.Minimum);
                     Is_Register := True;
                  end;
               when Extended_IRQ_Name =>
                  declare Item : constant E.Decode_Result := E.Decode (Data, Offset); begin
                     if Item.Status /= E.Decoded then return (Outcome => Malformed_Descriptor); end if;
                  end;
               when GPIO_Name =>
                  declare Item : constant G.Decode_Result := G.Decode (Data, Offset); begin
                     if Item.Status /= G.Decoded then return (Outcome => Malformed_Descriptor);
                     elsif Item.Information.Kind /= G.Interrupt_Connection then return (Outcome => Unsupported_Layout);
                     elsif Has_GPIO then return (Outcome => Ambiguous_Interrupt);
                     end if;
                     Has_GPIO := True;
                     GPIO_Offset := Offset;
                  end;
               when others => return (Outcome => Unsupported_Descriptor);
            end case;
         end if;
         if Is_Register then
            if Count = Claims'Length then return (Outcome => Extra_Registers); end if;
            Registers (Register_Role'Val (Count)) := Claim;
            Count := Count + 1;
         end if;
         Offset := Frame.Next_Offset;
      end loop;
      if Count /= Claims'Length then return (Outcome => Missing_Registers); end if;
      if Registers (Data_Register).Address.Space = Registers (Control_Register).Address.Space
        and Registers (Data_Register).Address.Address = Registers (Control_Register).Address.Address
      then return (Outcome => Aliased_Registers); end if;
      return (Outcome => Described, Registers => Registers,
              GPIO_Interrupt_Present => Has_GPIO, GPIO_Offset => GPIO_Offset);
   end Describe;
end ACPI_EC_Resources;
