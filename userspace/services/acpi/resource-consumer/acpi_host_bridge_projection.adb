pragma Ada_2022;
package body ACPI_Host_Bridge_Projection with SPARK_Mode is
   use type H.R.Item_Count, H.R.Byte_Count;
   use type H.A.Decode_Status, H.E.Decode_Status, H.B.Decode_Status, H.M.Decode_Status;
   use type H.A.Resource_Kind, H.E.Extended_Kind;
   use type H.M.Memory_Encoding, H.B.IO_Decoding;
   use type W.Word;
   function Bit (Value : Boolean) return W.Word is (if Value then 1 else 0);
   function Resource (Kind : H.A.Resource_Kind) return W.Word is
     (case Kind is when H.A.Memory_Range => W.Memory_Kind,
      when H.A.IO_Range => W.IO_Kind, when H.A.Bus_Range => W.Bus_Kind,
      when H.A.PCC_Range => W.PCC_Kind, when H.A.Vendor_Range => W.Vendor_Kind);
   function Resource (Kind : H.E.Extended_Kind) return W.Word is
     (case Kind is when H.E.Memory_Range => W.Memory_Kind,
      when H.E.IO_Range => W.IO_Kind, when H.E.Bus_Range => W.Bus_Kind,
      when H.E.Vendor_Range => W.Vendor_Kind);
   procedure One (D : H.Descriptor; Number : Positive; Source_Bytes : H.R.Byte_Count;
      Data : out W.Row; OK : out Boolean) is
      Last : H.R.Byte_Count;
      Geometry : constant W.Word := W.Has_Minimum or W.Has_Maximum
        or W.Has_Granularity or W.Has_Translation or W.Has_Length;
   begin
      Data := W.Empty_Row; OK := False;
      if D.Extent = 0 or else D.Offset > Source_Bytes
        or else D.Extent > Source_Bytes - D.Offset then return; end if;
      Last := D.Offset + D.Extent;
      Data (W.Ordinal) := W.Word (Number);
      Data (W.Descriptor_Offset) := W.Word (D.Offset);
      Data (W.Descriptor_Extent) := W.Word (D.Extent);
      case D.Form is
         when H.Empty => return;
         when H.Address_Space =>
            if D.Address.Status /= H.A.Decoded or else D.Address.Next_Offset /= Last then return; end if;
            Data (W.Form) := (case D.Address.Width is when H.A.Word_Width => W.Word_Address,
              when H.A.DWord_Width => W.DWord_Address, when H.A.QWord_Width => W.QWord_Address);
            Data (W.Address_Bits) := (case D.Address.Width is when H.A.Word_Width => 16,
              when H.A.DWord_Width => 32, when H.A.QWord_Width => 64);
            Data (W.Kind) := Resource (D.Address.Information.Kind);
            Data (W.General_Flags) := W.Word (D.Address.General_Flags);
            Data (W.Specific_Flags) := W.Word (D.Address.Information.Raw);
            if D.Address.Information.Kind = H.A.Vendor_Range then
               Data (W.Vendor_Type) := W.Word (D.Address.Information.Vendor_Type);
            end if;
            Data (W.Claims) := Geometry or W.Has_General;
            Data (W.Minimum) := W.Word (D.Address.Minimum); Data (W.Maximum) := W.Word (D.Address.Maximum);
            Data (W.Granularity_Or_Alignment) := W.Word (D.Address.Granularity);
            Data (W.Translation) := W.Word (D.Address.Translation); Data (W.Length) := W.Word (D.Address.Length);
            if D.Address.Information.Kind = H.A.Memory_Range then
               Data (W.Claims) := Data (W.Claims) or W.Has_Cache or W.Has_Writable;
               Data (W.Cache) := W.Word (H.A.Memory_Cache_Claim'Pos (D.Address.Information.Cache));
               Data (W.Writable) := Bit (D.Address.Information.Declared_Writeable);
            end if;
            if D.Address.Source.Present then
               if D.Address.Source.Name_Offset <= D.Offset or else D.Address.Source.Name_Offset >= Last
                 or else D.Address.Source.Name_Length /= Last - D.Address.Source.Name_Offset - 1 then return; end if;
               Data (W.Claims) := Data (W.Claims) or W.Has_Source;
               Data (W.Source_Index) := W.Word (D.Address.Source.Index);
               Data (W.Source_Name_Offset) := W.Word (D.Address.Source.Name_Offset);
               Data (W.Source_Name_Length) := W.Word (D.Address.Source.Name_Length);
            end if;
         when H.Extended_Address =>
            if D.Extended.Status /= H.E.Decoded or else D.Extended.Next_Offset /= Last then return; end if;
            Data (W.Form) := W.Extended_Address; Data (W.Address_Bits) := 64;
            Data (W.Kind) := Resource (D.Extended.Information.Kind);
            Data (W.General_Flags) := W.Word (D.Extended.General_Flags);
            Data (W.Specific_Flags) := W.Word (D.Extended.Information.Raw);
            if D.Extended.Information.Kind = H.E.Vendor_Range then
               Data (W.Vendor_Type) := W.Word (D.Extended.Information.Vendor_Type);
            end if;
            Data (W.Claims) := Geometry or W.Has_General or W.Has_Attributes;
            Data (W.Minimum) := W.Word (D.Extended.Minimum); Data (W.Maximum) := W.Word (D.Extended.Maximum);
            Data (W.Granularity_Or_Alignment) := W.Word (D.Extended.Granularity);
            Data (W.Translation) := W.Word (D.Extended.Translation); Data (W.Length) := W.Word (D.Extended.Length);
            Data (W.Attributes) := W.Word (D.Extended.Attributes);
            if D.Extended.Information.Kind = H.E.Memory_Range then
               Data (W.Claims) := Data (W.Claims) or W.Has_Writable;
               Data (W.Writable) := Bit (D.Extended.Information.Declared_Writeable);
               -- This encoding ignores its cache bits; they remain in Specific_Flags.
            end if;
         when H.Legacy_Memory =>
            if D.Legacy.Status /= H.M.Decoded or else D.Legacy.Next_Offset /= Last then return; end if;
            Data (W.Form) := (if D.Legacy.Encoding = H.M.Memory_24 then W.Memory_24 else W.Memory_32);
            Data (W.Address_Bits) := (if D.Legacy.Encoding = H.M.Memory_24 then 24 else 32);
            Data (W.Kind) := W.Memory_Kind; Data (W.Specific_Flags) := W.Word (D.Legacy.Information);
            Data (W.Claims) := W.Has_Minimum or W.Has_Maximum or W.Has_Granularity
              or W.Has_Length or W.Has_Writable or W.Has_Encoded_Alignment;
            Data (W.Minimum) := W.Word (D.Legacy.Minimum_Base); Data (W.Maximum) := W.Word (D.Legacy.Maximum_Base);
            Data (W.Granularity_Or_Alignment) := W.Word (D.Legacy.Alignment);
            Data (W.Encoded_Alignment) := W.Word (D.Legacy.Encoded_Alignment);
            Data (W.Length) := W.Word (D.Legacy.Length); Data (W.Writable) := Bit (D.Legacy.Declared_Writeable);
         when H.Basic_Resource =>
            if D.Basic.Status /= H.B.Decoded or else D.Basic.Next_Offset /= Last then return; end if;
            case D.Basic.Value.Kind is
               when H.B.IRQ_Resource => return;
               when H.B.IO_Resource =>
                  Data (W.Form) := W.IO_Descriptor; Data (W.Kind) := W.IO_Kind; Data (W.Address_Bits) := 16;
                  Data (W.Claims) := W.Has_Minimum or W.Has_Maximum or W.Has_Granularity or W.Has_Length or W.Has_Decoding;
                  Data (W.Minimum) := W.Word (D.Basic.Value.IO.Minimum); Data (W.Maximum) := W.Word (D.Basic.Value.IO.Maximum);
                  Data (W.Granularity_Or_Alignment) := W.Word (D.Basic.Value.IO.Alignment);
                  Data (W.Length) := W.Word (D.Basic.Value.IO.Length);
                  Data (W.Decoding_Bits) := (if D.Basic.Value.IO.Decoding = H.B.Decode_16 then 16 else 10);
               when H.B.Fixed_IO_Resource =>
                  Data (W.Form) := W.Fixed_IO_Descriptor; Data (W.Kind) := W.IO_Kind; Data (W.Address_Bits) := 16;
                  Data (W.Claims) := W.Has_Minimum or W.Has_Length or W.Has_Decoding;
                  Data (W.Minimum) := W.Word (D.Basic.Value.Fixed_IO.Base);
                  Data (W.Length) := W.Word (D.Basic.Value.Fixed_IO.Length); Data (W.Decoding_Bits) := 10;
               when H.B.Fixed_Memory_32_Resource =>
                  Data (W.Form) := W.Fixed_Memory_32; Data (W.Kind) := W.Memory_Kind; Data (W.Address_Bits) := 32;
                  Data (W.Claims) := W.Has_Minimum or W.Has_Length or W.Has_Writable;
                  Data (W.Minimum) := W.Word (D.Basic.Value.Memory.Base); Data (W.Length) := W.Word (D.Basic.Value.Memory.Length);
                  Data (W.Writable) := Bit (D.Basic.Value.Memory.Declared_Writeable);
                  Data (W.Specific_Flags) := W.Word (D.Basic.Value.Memory.Information);
            end case;
      end case;
      OK := True;
   end One;
   procedure Project (Items : H.Descriptor_List; Count : H.R.Item_Count;
      Source_Bytes : H.R.Byte_Count; Data : out W.Rows; Status : out Outcome) is
      Accepted : Boolean;
   begin
      Data := [others => W.Empty_Row]; Status := Invalid_Descriptor;
      if Count > H.R.Item_Count (Items'Length) then return; end if;
      if Count > H.R.Item_Count (Data'Length) then Status := Insufficient_Capacity; return; end if;
      for Offset in 0 .. Natural (Count) - 1 loop
         One (Items (Items'First + Offset), Offset + 1, Source_Bytes, Data (Data'First + Offset), Accepted);
         if not Accepted then Data := [others => W.Empty_Row]; return; end if;
      end loop;
      Status := Complete;
   end Project;
end ACPI_Host_Bridge_Projection;
