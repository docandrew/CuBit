pragma Ada_2022;
package body AML_Container_Headers with SPARK_Mode is
   use type AML_Decode.Byte;
   use type AML_Decode.Status;
   use type AML_Names.Parse_Status;
   use type Interfaces.Unsigned_32;
   Scope_Opcode : constant AML_Decode.Byte := 16#10#;
   Device_Extension : constant AML_Decode.Byte := 16#82#;
   Processor_Extension : constant AML_Decode.Byte := 16#83#;
   Power_Extension : constant AML_Decode.Byte := 16#84#;
   Thermal_Extension : constant AML_Decode.Byte := 16#85#;
   Single_Opcode_Bytes : constant Positive := 1;
   Extended_Opcode_Bytes : constant Positive := 2;
   Processor_Tail_Bytes : constant Positive := 6;
   Power_Tail_Bytes : constant Positive := 3;
   Address_Bytes : constant Positive := 4;
   Bits_Per_Byte : constant Positive := 8;
   Byte_Radix : constant Positive := 2 ** Bits_Per_Byte;
   function Read_Header (Data : AML_Decode.Bytes) return Header_Result is
      Kind : Container_Kind;
      Opcode_Bytes : Positive;
   begin
      if Data'Length = 0 then return (Status => Truncated, Phase => Opcode_Phase); end if;
      if Data (Data'First) = Scope_Opcode then
         Kind := Scope_Container; Opcode_Bytes := Single_Opcode_Bytes;
      elsif Data (Data'First) = AML_Decode.Extended_Op then
         if Data'Length < Extended_Opcode_Bytes then
            return (Status => Truncated, Phase => Opcode_Phase);
         end if;
         Opcode_Bytes := Extended_Opcode_Bytes;
         case Data (Data'First + 1) is
            when Device_Extension => Kind := Device_Container;
            when Processor_Extension => Kind := Processor_Container;
            when Power_Extension => Kind := Power_Resource_Container;
            when Thermal_Extension => Kind := Thermal_Zone_Container;
            when others => return (Status => Unsupported);
         end case;
      else
         return (Status => Unsupported);
      end if;
      if Data'Length <= Opcode_Bytes then return (Status => Truncated, Phase => Package_Phase); end if;
      declare
         P : constant AML_Decode.Package_Result := AML_Decode.Read_Package
           (Data (Data'First + Opcode_Bytes .. Data'Last));
      begin
         if P.Kind = AML_Decode.Truncated then return (Status => Truncated, Phase => Package_Phase);
         elsif P.Kind /= AML_Decode.Accepted then return (Status => Bad_Package);
         end if;
         declare
            Extent : constant Positive := Opcode_Bytes + P.Extent;
            Name_Offset : constant Positive := Opcode_Bytes + P.Encoding_Bytes;
         begin
            if Name_Offset = Extent then return (Status => Truncated, Phase => Name_Phase); end if;
            declare
               Name : constant AML_Names.Name_Result := AML_Names.Read_Name
                 (Data (Data'First + Name_Offset .. Data'First + (Extent - 1)));
            begin
               if Name.Kind = AML_Names.Truncated then return (Status => Truncated, Phase => Name_Phase);
               elsif Name.Kind /= AML_Names.Accepted then return (Status => Bad_Name);
               end if;
               declare
                  Tail_Offset : constant Positive := Name_Offset + Name.Consumed;
                  Tail_Bytes : constant Natural :=
                    (case Kind is
                       when Processor_Container => Processor_Tail_Bytes,
                       when Power_Resource_Container => Power_Tail_Bytes,
                       when others => 0);
               begin
                  if Tail_Bytes > Extent - Tail_Offset then
                     return (Status => Truncated,
                       Phase => Attribute_Phase);
                  end if;
                  declare
                     Header_Kind : constant Container_Kind := Kind;
                     Metadata : Attributes (Header_Kind);
                     Body_Offset : constant Positive := Tail_Offset + Tail_Bytes;
                  begin
                     case Header_Kind is
                        when Processor_Container =>
                           declare
                              Address : Interfaces.Unsigned_32 := 0;
                           begin
                              for J in 0 .. Address_Bytes - 1 loop
                                 Address := Address or Interfaces.Shift_Left
                                   (Interfaces.Unsigned_32 (Data (Data'First + (Tail_Offset + 1 + J))),
                                    Bits_Per_Byte * J);
                              end loop;
                              Metadata :=
                                (Kind => Processor_Container,
                                 Processor_ID => Processor_Identifier (Data (Data'First + Tail_Offset)),
                                 Block_Address => Processor_Block_Address (Address),
                                 Block_Length => Processor_Block_Length
                                   (Data (Data'First + (Tail_Offset + Processor_Tail_Bytes - 1))));
                           end;
                        when Power_Resource_Container =>
                           Metadata :=
                             (Kind => Power_Resource_Container,
                              System_Level => Power_System_Level (Data (Data'First + Tail_Offset)),
                              Order => Resource_Order (Data (Data'First + (Tail_Offset + 1)))
                                + Resource_Order (Data (Data'First + (Tail_Offset + 2))) * Resource_Order (Byte_Radix));
                        when Scope_Container => Metadata := (Kind => Scope_Container);
                        when Device_Container => Metadata := (Kind => Device_Container);
                        when Thermal_Zone_Container => Metadata := (Kind => Thermal_Zone_Container);
                     end case;
                     return (Status => Accepted, Metadata => Metadata, Name => Name,
                       Extent => Extent, Body_Offset => Body_Offset,
                       Body_Length => Extent - Body_Offset);
                  end;
               end;
            end;
         end;
      end;
   end Read_Header;
end AML_Container_Headers;
