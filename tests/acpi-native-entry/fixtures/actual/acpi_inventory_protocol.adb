pragma Ada_2022;
package body ACPI_Inventory_Protocol with SPARK_Mode is
   Read_Bit : constant Unsigned_64 := 2 ** 0;
   Write_Bit : constant Unsigned_64 := 2 ** 1;
   Delegate_Bit : constant Unsigned_64 := 2 ** 2;
   Exact_Bit : constant Unsigned_64 := 2 ** 3;
   IO_Bit : constant Unsigned_64 := 2 ** 4;
   Width_First_Bit : constant := 8;
   Width_Bits : constant Unsigned_64 := 16#F00#;
   Known_Bits : constant Unsigned_64 :=
     Read_Bit or Write_Bit or Delegate_Bit or Exact_Bit or IO_Bit or Width_Bits;
   function Decode_Header (Item : Wire.Packet) return Header is
     (if Item.Label = Header_Reply and then Envelope_Valid (Item)
       and then Item.Data (0) = Outcome'Pos (Ready) and then Item.Data (1) /= 0
       and then Item.Data (3) = Version
      then (True, Item.Data (1), Item.Data (2)) else (Valid => False));
   function Decode_Part (Item : Wire.Packet) return Decoded_Part is
     (if Item.Label = Part_Reply and then Envelope_Valid (Item)
       and then Item.Data (0) = Outcome'Pos (Ready)
      then (True, [Item.Data (1), Item.Data (2), Item.Data (3)]) else (Valid => False));
   function Encode (Item : Description) return Parts is
      Flags : Unsigned_64 :=
        (if Item.Readable then Read_Bit else 0) or
        (if Item.Writable then Write_Bit else 0) or
        (if Item.Delegable then Delegate_Bit else 0) or
        (if Item.Exact_Write_Safe then Exact_Bit else 0) or
        (if Item.IO_Space then IO_Bit else 0);
   begin
      for Width in T.Access_Width loop
         if Item.Widths (Width) then
            Flags := Flags or Shift_Left (Unsigned_64'(1), Width_First_Bit + T.Access_Width'Pos (Width));
         end if;
      end loop;
      return [Identity_Part => [Item.ID, Item.Epoch, Flags],
              Region_Part => [Item.Base, Item.Length, Item.Offset],
              Register_Part => [T.Access_Width'Pos (Item.Width), Item.Write_Mask, 0]];
   end Encode;
   function Decode (Item : Parts) return Decoded_Description is
      Flags : constant Unsigned_64 := Item (Identity_Part) (2);
      Value : Description;
   begin
      if (Flags and not Known_Bits) /= 0 or else Item (Register_Part) (2) /= 0
        or else Item (Register_Part) (0) > T.Access_Width'Pos (T.Access_Width'Last)
      then return (Valid => False); end if;
      Value.ID := Item (Identity_Part) (0); Value.Epoch := Item (Identity_Part) (1);
      Value.Base := Item (Region_Part) (0); Value.Length := Item (Region_Part) (1);
      Value.Offset := Item (Region_Part) (2);
      Value.Width := T.Access_Width'Val (Item (Register_Part) (0));
      Value.Write_Mask := Item (Register_Part) (1);
      Value.Readable := (Flags and Read_Bit) /= 0;
      Value.Writable := (Flags and Write_Bit) /= 0;
      Value.Delegable := (Flags and Delegate_Bit) /= 0;
      Value.Exact_Write_Safe := (Flags and Exact_Bit) /= 0;
      Value.IO_Space := (Flags and IO_Bit) /= 0;
      for Width in T.Access_Width loop
         Value.Widths (Width) :=
           (Flags and Shift_Left (Unsigned_64'(1), Width_First_Bit + T.Access_Width'Pos (Width))) /= 0;
      end loop;
      return (if Valid (Value) then (True, Value) else (Valid => False));
   end Decode;
end ACPI_Inventory_Protocol;
