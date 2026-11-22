pragma Ada_2022;
package body ACPI_HID_I2C_Resources with SPARK_Mode is
   use type R.Frame_Status;
   use type R.Item_Encoding;
   use type R.Byte_Count;
   Serial_Name : constant R.Item_Name := 16#0E#;
   GPIO_Name : constant R.Item_Name := 16#0C#;
   Start_Dependent_Name : constant R.Item_Name := 6;
   End_Dependent_Name : constant R.Item_Name := 7;
   procedure Analyze (Data : Firmware_Tables.Bytes; Report : out Analysis_Report;
      Maximum_Items : R.Item_Count := R.Default_Item_Limit) is
      Offset : R.Byte_Offset := 0;
   begin
      Report := (others => <>);
      Report.Framing := R.Scan (Data, Maximum_Items);
      if Report.Framing.Status /= R.Complete then
         Report.Position := Report.Framing.Position; return;
      end if;
      for I in R.Item_Count range 1 .. Report.Framing.Items loop
         pragma Loop_Invariant (Offset <= R.Byte_Count (Data'Length));
         pragma Loop_Invariant (Report.I2C_Count <= I - 1);
         pragma Loop_Invariant (Report.Interrupt_Count <= I - 1);
         pragma Loop_Invariant (Report.Other_Items <= I - 1);
         pragma Loop_Invariant (if Report.I2C_Count > 0 then
           Report.I2C.Status = S.Decoded and then Report.I2C.Information.Kind = S.I2C_Bus);
         pragma Loop_Invariant (if Report.Interrupt_Count > 0 then
           Report.Interrupt.Status = G.Decoded
             and then Report.Interrupt.Information.Kind = G.Interrupt_Connection);
         declare
            Frame : constant R.Frame := R.Read_Frame (Data, Offset);
         begin
            Report.Position := Offset;
            if Frame.Status /= R.Decoded then return; end if;
            if Frame.Encoding = R.Small_Item and then
              Frame.Name in Start_Dependent_Name | End_Dependent_Name
            then Report.Outcome := Dependent_Alternatives; return;
            elsif Frame.Encoding = R.Large_Item and then Frame.Name = Serial_Name then
               declare
                  Connection : constant S.Decode_Result := S.Decode (Data, Offset);
               begin
                  if Connection.Status /= S.Decoded then
                     Report.Outcome := Invalid_Connection; return;
                  end if;
                  if Connection.Information.Kind = S.I2C_Bus then
                     Report.I2C_Count := Report.I2C_Count + 1;
                     if Report.I2C_Count = 1 then
                        Report.I2C := Connection; Report.I2C_Offset := Offset;
                     end if;
                  else Report.Other_Items := Report.Other_Items + 1;
                  end if;
               end;
            elsif Frame.Encoding = R.Large_Item and then Frame.Name = GPIO_Name then
               declare
                  Connection : constant G.Decode_Result := G.Decode (Data, Offset);
               begin
                  if Connection.Status /= G.Decoded then
                     Report.Outcome := Invalid_Connection; return;
                  end if;
                  if Connection.Information.Kind = G.Interrupt_Connection then
                     Report.Interrupt_Count := Report.Interrupt_Count + 1;
                     if Report.Interrupt_Count = 1 then
                        Report.Interrupt := Connection; Report.Interrupt_Offset := Offset;
                     end if;
                  else Report.Other_Items := Report.Other_Items + 1;
                  end if;
               end;
            else Report.Other_Items := Report.Other_Items + 1;
            end if;
            Offset := Frame.Next_Offset;
         end;
      end loop;
      Report.Position := Offset;
      if Report.I2C_Count = 0 then Report.Outcome := Missing_I2C;
      elsif Report.Interrupt_Count = 0 then Report.Outcome := Missing_Interrupt;
      elsif Report.I2C_Count > 1 or else Report.Interrupt_Count > 1 then
         Report.Outcome := Ambiguous_Connections;
      else Report.Outcome := Connections_Found;
      end if;
   end Analyze;
end ACPI_HID_I2C_Resources;
