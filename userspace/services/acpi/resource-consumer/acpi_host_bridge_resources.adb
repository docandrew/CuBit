pragma Ada_2022;
package body ACPI_Host_Bridge_Resources with SPARK_Mode is
   use type R.Template_Status, R.Frame_Status, R.Item_Encoding;
   use type R.Item_Name, R.Byte_Count, R.Item_Count;
   use type A.Decode_Status, E.Decode_Status, B.Decode_Status, M.Decode_Status;
   use type B.Resource_Kind;
   End_Name : constant R.Item_Name := 15;
   Word_Address_Name : constant R.Item_Name := 8;
   DWord_Address_Name : constant R.Item_Name := 7;
   QWord_Address_Name : constant R.Item_Name := 10;
   Extended_Address_Name : constant R.Item_Name := 11;
   Memory_24_Name : constant R.Item_Name := 1;
   Memory_32_Name : constant R.Item_Name := 5;
   Fixed_Memory_32_Name : constant R.Item_Name := 6;
   IO_Name : constant R.Item_Name := 8;
   Fixed_IO_Name : constant R.Item_Name := 9;
   procedure Collect (Data : Firmware_Tables.Bytes;
      Items : out Descriptor_List; Report : out Collection_Report) is
      Scan : constant R.Scan_Result := R.Scan (Data, R.Item_Count (Data'Length));
      Offset : R.Byte_Offset := 0;
      Count : R.Item_Count := 0;
      Failed : Boolean := False;
   begin
      Items := [others => (Form => Empty, others => <>)];
      Report := (others => <>);
      if Scan.Status /= R.Complete then
         Report.Failure_Offset := Scan.Position;
         return;
      end if;
      Report.Required := Scan.Items;
      if Scan.Items > R.Item_Count (Items'Length) then
         Report.Status := Insufficient_Capacity; return;
      end if;
      while Offset < R.Byte_Count (Data'Length) loop
         pragma Loop_Invariant (Report.Required = Scan.Items);
         pragma Loop_Invariant (Report.Status /= Complete);
         pragma Loop_Invariant (Count <= R.Item_Count (Items'Length));
         declare
            F : constant R.Frame := R.Read_Frame (Data, Offset);
            D : Descriptor := (Form => Empty, others => <>);
         begin
            if F.Status /= R.Decoded then
               Report.Status := Invalid_Template; Failed := True; exit;
            end if;
            exit when F.Encoding = R.Small_Item and then F.Name = End_Name;
            Report.Status := Unsupported_Descriptor;
            if F.Encoding = R.Large_Item then
               case F.Name is
                  when Word_Address_Name | DWord_Address_Name | QWord_Address_Name =>
                     declare V : constant A.Decode_Result := A.Decode (Data, Offset); begin
                        if V.Status = A.Decoded then
                           D := (Address_Space, Offset, F.Next_Offset - Offset, V);
                        else Report.Status := Invalid_Descriptor; end if;
                     end;
                  when Extended_Address_Name =>
                     declare V : constant E.Decode_Result := E.Decode (Data, Offset); begin
                        if V.Status = E.Decoded then
                           D := (Extended_Address, Offset, F.Next_Offset - Offset, V);
                        else Report.Status := Invalid_Descriptor; end if;
                     end;
                  when Memory_24_Name | Memory_32_Name =>
                     declare V : constant M.Decode_Result := M.Decode (Data, Offset); begin
                        if V.Status = M.Decoded then
                           D := (Legacy_Memory, Offset, F.Next_Offset - Offset, V);
                        else Report.Status := Invalid_Descriptor; end if;
                     end;
                  when Fixed_Memory_32_Name =>
                     declare V : constant B.Decode_Result := B.Decode (Data, Offset); begin
                        if V.Status = B.Decoded then
                           D := (Basic_Resource, Offset, F.Next_Offset - Offset, V);
                        else Report.Status := Invalid_Descriptor; end if;
                     end;
                  when others => null;
               end case;
            elsif F.Name = IO_Name or else F.Name = Fixed_IO_Name then
               declare V : constant B.Decode_Result := B.Decode (Data, Offset); begin
                  if V.Status = B.Decoded then
                     D := (Basic_Resource, Offset, F.Next_Offset - Offset, V);
                  else Report.Status := Invalid_Descriptor; end if;
               end;
            end if;
            if D.Form = Empty then Failed := True; exit; end if;
            -- Scan counted all non-EndTag frames; each contributes one slot.
            if Count >= R.Item_Count (Items'Length) then
               Report.Status := Insufficient_Capacity; Failed := True; exit;
            end if;
            Items (Items'First + Natural (Count)) := D;
            Count := Count + 1;
            Offset := F.Next_Offset;
         end;
      end loop;
      if Failed or else Count /= Scan.Items then
         Items := [others => (Form => Empty, others => <>)];
         Report.Failure_Offset := Offset;
         return;
      end if;
      Report.Status := Complete;
      Report.Count := Count;
   end Collect;
end ACPI_Host_Bridge_Resources;
