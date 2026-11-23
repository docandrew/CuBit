package body HID_Touchpad_Features with SPARK_Mode is
   use type Schema.Status, Schema.Report_Kind, Schema.Bit_Count, Schema.Numeric_Value;
   use type Schema.Word, Schema.Wide, Schema.Report_ID, Names.Status, Wire.D.Byte;
   Input_Mode : constant Schema.Word := 16#000D_0052#;
   Surface_Switch : constant Schema.Word := 16#000D_0057#;
   Button_Switch : constant Schema.Word := 16#000D_0058#;
   procedure Build
     (Layout : Schema.Result; Fields : Schema.Field_Table; Usages : Schema.Usage_Table;
      Command_Register, Data_Register : Wire.D.Register_Number; Result : out Plan)
   is
      Payload : Wire.D.Bytes (1 .. Wire.Maximum_Payload);
      Usage : Schema.Word;
      Resolved : Names.Status;
      Known, Wanted, Bad, Mode, Surface, Buttons : Boolean;
      Mode_Seen, Surface_Seen, Buttons_Seen : Boolean := False;
      Bits, Position, Byte_Position, Bit_Position, Value, Bytes : Natural;
   begin
      Result := (others => <>);
      if Layout.Outcome /= Schema.Decoded or else Fields'First /= 1
        or else Natural (Layout.Fields_Required) > Fields'Length then Result.Refused := True; return; end if;
      -- Each report is considered once; only known mutable fields may be written.
      for ID in Schema.Report_ID loop
         if Layout.Present (Schema.Feature, ID) then
            Payload := [others => 0]; Wanted := False; Bad := False;
            Mode := False; Surface := False; Buttons := False;
            Bits := Natural (Layout.Bits (Schema.Feature, ID));
            if Bits = 0 or else Bits > Wire.Maximum_Payload * 8 then Bad := True; end if;
            for Index in 1 .. Natural (Layout.Fields_Required) loop
               declare F : constant Schema.Field_Span := Fields (Index); begin
                  if F.Kind = Schema.Feature and then F.ID = ID and then (F.Flags and 1) = 0 then
                     if F.Size = 0 or else F.Size > 32 or else F.Elements > 512
                       or else Schema.Wide (F.Offset) + Schema.Wide (F.Size) * Schema.Wide (F.Elements) > Schema.Wide (Bits)
                     then Bad := True;
                     else
                        for Element in 1 .. Natural (F.Elements) loop
                           Names.Resolve (Layout,F,Usages,Schema.Bit_Count(Element - 1),Usage,Resolved);
                           Known := Resolved = Names.Resolved and then Usage in Input_Mode | Surface_Switch | Button_Switch;
                           if not Known then Bad := True;
                           else
                              if Usage = Input_Mode then Result.Mode_Advertised := True; Value := 3;
                              else Value := 1; end if;
                              if F.Logical_Minimum > Schema.Numeric_Value (Value) or else F.Logical_Maximum < Schema.Numeric_Value (Value)
                                or else Schema.Wide(Value) >= Schema.Wide(2) ** Natural(F.Size)
                                or else (Usage = Input_Mode and then (Mode_Seen or Mode))
                                or else (Usage = Surface_Switch and then (Surface_Seen or Surface))
                                or else (Usage = Button_Switch and then (Buttons_Seen or Buttons))
                              then Bad := True; Result.Refused := True;
                              else
                                 Wanted := True;
                                 Mode := Mode or Usage = Input_Mode; Surface := Surface or Usage = Surface_Switch;
                                 Buttons := Buttons or Usage = Button_Switch;
                                 Position := Natural(F.Offset) + (Element - 1) * Natural(F.Size);
                                 if not Bad then
                                    for Bit in 0 .. Natural(F.Size) - 1 loop
                                       Byte_Position := 1 + (Position + Bit) / 8; Bit_Position := (Position + Bit) mod 8;
                                       if Byte_Position not in Payload'Range then Bad := True; exit; end if;
                                       if Value mod 2 = 1 then Payload(Byte_Position) := Payload(Byte_Position) or Wire.D.Byte(2 ** Bit_Position); end if;
                                       Value := Value / 2;
                                    end loop;
                                 end if;
                              end if;
                           end if;
                        end loop;
                     end if;
                  end if;
               end;
            end loop;
            if Wanted and then not Bad and then Bits in 1 .. Wire.Maximum_Payload * 8
              and then Result.Count < Maximum_Commands then
               Bytes := (Bits + 7) / 8; Result.Count := Result.Count + 1;
               Result.Commands(Result.Count) := Wire.Encode(Command_Register,Data_Register,Wire.D.Byte(ID),Payload(1..Bytes),Mode,Surface,Buttons);
               Mode_Seen := Mode_Seen or Mode; Surface_Seen := Surface_Seen or Surface; Buttons_Seen := Buttons_Seen or Buttons;
               Result.Mode_Supported := Mode_Seen;
            elsif Wanted then Result.Refused := True; end if;
         end if;
      end loop;
   end Build;
end HID_Touchpad_Features;
