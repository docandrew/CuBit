pragma Ada_2022;
package body ACPI_Passive_Identity with SPARK_Mode is
   use type Reader.Outcome;
   use type Reader.Atom_Kind;
   use type Core.Values.Access_Status;
   use type Fields.Field_Name;
   use type Fields.Value_Kind;
   use type Wire.Word;
   use type Fields.U32;
   Canonical_ID_Max_Length : constant := 8;
   procedure Read
     (Service : in out Core.State; Device : Core.Node_Descriptor; Member : Member_Name;
      Ordinal : Fields.U32; Offset : Wire.Word; Pool_Offset : Fields.U32;
      Scratch_Data : out Firmware_Tables.Bytes; Scratch_Atoms : out Reader.Atoms;
      Text : out Firmware_Tables.Bytes; Field : out Fields.Field_Result; Result : out Report) is
      Item : Fields.Field_Record := (Name => Member, others => <>);
      Atom : Reader.Atom;
      Source : Reader.Member_Name;
      Count, Start : Natural := 0;
      ID : Wire.ID_Result;
      Limit : constant Wire.Node_Limit := Wire.Node_Limit (Core.Max_Namespace_Nodes);
      procedure Publish_Field is
         Extent : constant Wire.Word := Wire.Word (Pool_Offset) + Result.Text_Bytes;
      begin
         if Extent > Wire.Word (Fields.U32'Last)
           or else not Fields.Valid (Item, Limit, Fields.U32 (Extent)) then return; end if;
         Field := (True, Item); Result.Status := Field_Ready;
      end Publish_Field;
   begin
      Result := (others => <>); Field := (Accepted => False);
      Text := [others => 0]; Scratch_Data := [others => 0]; Scratch_Atoms := [others => (others => <>)];
      if (Member /= Fields.Compatible_ID and then Ordinal /= 0)
        or else (Member /= Fields.Unique_ID and then Offset /= 0)
      then Result.Status := Bad_Position; return; end if;
      Source := (case Member is
        when Fields.Hardware_ID => Reader.Hardware_ID,
        when Fields.Compatible_ID => Reader.Compatible_ID,
        when Fields.Unique_ID => Reader.Unique_ID,
        when Fields.Address => Reader.Address,
        when Fields.Device_Status => Reader.Device_Status);
      Reader.Read (Service, Device, Source, Scratch_Data, Scratch_Atoms, Result.Observation);
      declare O : Reader.Report renames Result.Observation; begin
         if O.Access_Result /= Core.Values.Available or else O.Cleanup_Result /= Core.Values.Available then return; end if;
         if O.Status = Reader.Capacity_Required then Result.Status := Scratch_Required; return; end if;
         if O.Status in Reader.Missing | Reader.Method_Valued | Reader.Unsupported then
            if Ordinal /= 0 or else Offset /= 0 then Result.Status := Bad_Position; return; end if;
            Item.Status := (case O.Status is when Reader.Missing => Wire.Absent,
              when Reader.Method_Valued => Wire.Method_Valued, when others => Wire.Unsupported);
            if Member = Fields.Device_Status and then O.Status = Reader.Missing then
               Item.Kind := Fields.Integer_Value; Item.Number := Wire.Default_STA; Item.Defaulted := True;
            end if;
            Publish_Field; return;
         end if;
         if O.Status /= Reader.Static_Value or else not O.Size_Known
           or else O.Copied_Atoms /= O.Required_Atoms or else O.Copied_Bytes /= O.Required_Bytes
           or else O.Copied_Atoms > Scratch_Atoms'Length or else O.Copied_Bytes > Scratch_Data'Length
         then return; end if;
         if Member = Fields.Compatible_ID then
            if Wire.Word (Ordinal) >= Wire.Word (O.Copied_Atoms) then
               Result.Status := (if Wire.Word (Ordinal) = Wire.Word (O.Copied_Atoms) then End_Of_List else Bad_Position);
               return;
            end if;
            Item.Ordinal := Ordinal;
            Result.More_CIDs := Wire.Word (Ordinal) + 1 < Wire.Word (O.Copied_Atoms);
         elsif O.Copied_Atoms /= 1 then return; end if;
         Atom := Scratch_Atoms (Scratch_Atoms'First + Natural (Ordinal));
      end;
      Item.Status := Wire.Present;
      if Atom.Kind = Reader.Integer_Atom then
         if Offset /= 0 then Result.Status := Bad_Position; return; end if;
         if Member in Fields.Hardware_ID | Fields.Compatible_ID then
            ID := Fields.From_EISA (Wire.Word (Atom.Number));
            if ID.Accepted then Item.Kind := Fields.Identifier; Item.ID := ID.Value;
            else Item := (Name => Member, Status => Wire.Invalid, others => <>); end if;
         else Item.Kind := Fields.Integer_Value; Item.Number := Wire.Word (Atom.Number); end if;
         Publish_Field; return;
      elsif Atom.Kind /= Reader.Text_Atom or else Member in Fields.Address | Fields.Device_Status then return; end if;
      if Atom.Offset > Result.Observation.Copied_Bytes
        or else Atom.Length > Result.Observation.Copied_Bytes - Atom.Offset then return; end if;
      if Member in Fields.Hardware_ID | Fields.Compatible_ID then
         if Atom.Length in 7 | 8 then
            declare Value : String (1 .. Canonical_ID_Max_Length) := [others => ' ']; begin
               for I in 0 .. Atom.Length - 1 loop
                  Value (1 + I) := Character'Val (Scratch_Data (Scratch_Data'First + (Atom.Offset + I)));
               end loop;
               ID := Wire.From_Text (Value (1 .. Atom.Length));
               if ID.Accepted then Item.Kind := Fields.Identifier; Item.ID := ID.Value; Publish_Field; return; end if;
            end;
         end if;
         if Member = Fields.Hardware_ID or else Atom.Length = 0 then
            Item := (Name => Member, Status => Wire.Invalid, others => <>); Publish_Field; return;
         end if;
         for I in 0 .. Atom.Length - 1 loop
            if Scratch_Data (Scratch_Data'First + (Atom.Offset + I)) = 0 then
               Item := (Name => Member, Status => Wire.Invalid, others => <>); Publish_Field; return;
            end if;
         end loop;
         Result.Required_Text_Bytes := Wire.Word (Atom.Length);
         if Atom.Length > Text'Length then Result.Status := Output_Required; return; end if;
         Count := Atom.Length;
      else
         if (Atom.Length = 0 and then Offset /= 0)
           or else (Atom.Length > 0 and then Offset >= Wire.Word (Atom.Length))
         then Result.Status := Bad_Position; return; end if;
         Start := Natural (Offset);
         if Atom.Length > 0 and then Text'Length = 0 then
            Result.Required_Text_Bytes := 1; Result.Status := Output_Required; return;
         end if;
         Count := Natural'Min (Atom.Length - Start, Text'Length);
      end if;
      if Wire.Word (Count) + Wire.Word (Pool_Offset) > Wire.Word (Fields.U32'Last) then return; end if;
      Item.Kind := Fields.Text_Chunk; Item.Logical_Offset := Offset; Item.Total_Length := Wire.Word (Atom.Length);
      Item.Text := (if Count = 0 then (0, 0) else (Pool_Offset, Fields.U32 (Count)));
      Result.Text_Bytes := Wire.Word (Count);
      -- Validate before copying, so a refused field never leaves text output.
      Publish_Field;
      if not Field.Accepted then return; end if;
      for I in 0 .. Count - 1 loop
         Text (Text'First + I) := Scratch_Data (Scratch_Data'First + (Atom.Offset + Start + I));
      end loop;
   end Read;
end ACPI_Passive_Identity;
