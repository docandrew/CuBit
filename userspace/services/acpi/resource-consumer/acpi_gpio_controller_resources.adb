pragma Ada_2022;
with ACPI_Resource_Data.Basic_Descriptors;
package body ACPI_GPIO_Controller_Resources with SPARK_Mode is
   package Basic renames R.Basic_Descriptors;
   use type R.Template_Status,R.Frame_Status,R.Item_Encoding;
   use type Basic.Decode_Status,Basic.Resource_Kind,IRQ.Decode_Status;
   use type R.Byte_Offset;
   Fixed_Memory_Item:constant R.Item_Name:=6;
   Extended_Interrupt_Item:constant R.Item_Name:=9;
   function Overlap(Left,Right:Memory_Window) return Boolean is
     (Left.Base<Right.Base+Right.Bytes and then Right.Base<Left.Base+Left.Bytes);
   function Valid(Item:Description) return Boolean is
   begin
      if not Item.Interrupt.Present then return False;end if;
      for I in Resource_Slot loop
         if not Valid_Span(Item.Memory(I)) then return False;end if;
         for J in Resource_Slot loop
            if J<I and then Valid_Span(Item.Memory(J)) and then Overlap(Item.Memory(I),Item.Memory(J)) then return False;end if;
         end loop;
      end loop;
      return True;
   end Valid;
   procedure Describe(Data:Firmware_Tables.Bytes;Result:out Report) is
      Scan:constant R.Scan_Result:=R.Scan(Data);
      Cursor:R.Byte_Offset:=0;
      Temp:Description;
      subtype Count is Natural range 0..Community_Resources;
      Memories:Count:=0;
   begin
      Result:=(others=><>);if Scan.Status/=R.Complete then return;end if;
      for Step in 1..Scan.Items loop
         Result.At_Offset:=Cursor;
         declare Frame:constant R.Frame:=R.Read_Frame(Data,Cursor);begin
            if Frame.Status/=R.Decoded then Result.Status:=Invalid_Descriptor;return;end if;
            if Frame.Encoding/=R.Large_Item then Result.Status:=Unsupported_Descriptor;return;end if;
            if Frame.Name=Fixed_Memory_Item then
               if Memories=Count'Last then Result.Status:=Wrong_Memory_Count;return;end if;
               declare Decoded:constant Basic.Decode_Result:=Basic.Decode(Data,Cursor);Item:Memory_Window;begin
                  if Decoded.Status/=Basic.Decoded or else Decoded.Value.Kind/=Basic.Fixed_Memory_32_Resource
                    or else Decoded.Next_Offset/=Frame.Next_Offset then Result.Status:=Invalid_Descriptor;return;end if;
                  Item:=(Base=>Address(Decoded.Value.Memory.Base),Bytes=>Address(Decoded.Value.Memory.Length),
                     Declared_Writeable=>Decoded.Value.Memory.Declared_Writeable,Raw_Information=>Decoded.Value.Memory.Information);
                  if not Valid_Span(Item) then Result.Status:=Invalid_Memory;return;end if;
                  for I in Resource_Slot loop
                     if I<=Memories and then Valid_Span(Temp.Memory(I)) and then Overlap(Item,Temp.Memory(I)) then
                        Result.Status:=Overlapping_Memory;return;
                     end if;
                  end loop;
                  Memories:=Memories+1;Temp.Memory(Resource_Slot(Memories)):=Item;
               end;
            elsif Frame.Name=Extended_Interrupt_Item then
               if Temp.Interrupt.Present then Result.Status:=Wrong_Interrupt_Count;return;end if;
               declare Decoded:constant IRQ.Decode_Result:=IRQ.Decode(Data,Cursor);begin
                  if Decoded.Status/=IRQ.Decoded or else Decoded.Count/=1 or else Decoded.Next_Offset/=Frame.Next_Offset then
                     Result.Status:=Invalid_Descriptor;return;
                  end if;
                  Temp.Interrupt:=(Present=>True,Number=>Decoded.Interrupts(1),Mode=>Decoded.Mode,Polarity=>Decoded.Polarity,
                     Role=>Decoded.Role,Shared=>Decoded.Shared,Wake_Capable=>Decoded.Wake_Capable,Source=>Decoded.Source);
               end;
            else Result.Status:=Unsupported_Descriptor;return;end if;
            Cursor:=Frame.Next_Offset;
         end;
      end loop;
      if Memories/=Count'Last then Result.Status:=Wrong_Memory_Count;return;end if;
      if not Temp.Interrupt.Present then Result.Status:=Wrong_Interrupt_Count;return;end if;
      -- Final whole-description check keeps success atomic and the modular
      -- postcondition explicit even if a decoder contract is strengthened.
      if not Valid(Temp) then Result.Status:=Invalid_Memory;return;end if;
      Result:=(Status=>Described,At_Offset=>Cursor,Value=>Temp);
   end Describe;
end ACPI_GPIO_Controller_Resources;
