pragma Ada_2022;
with AML_Names;
package body ACPI_Passive_Path with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type N.Object_Kind;
   use type V.Access_Status;
   use type Fields.U32;
   use type Wire.Word;
   Segment_Bytes : constant Positive := AML_Names.Segment'Length;
   Encoded_Segment_Bytes : constant Positive := 1 + Segment_Bytes;
   procedure Read
     (Service : Core.State; Device : Core.Node_Descriptor;
      Segment_Offset, Pool_Offset : Fields.U32; Scratch : out Node_Array;
      Text : out Firmware_Tables.Bytes; Field : out Fields.Field_Result; Result : out Report) is
      Current, Root, Part : Core.Node_Descriptor;
      Depth, Count, Start : Natural := 0;
      Position : N.Node_ID;
      Item : Fields.Field_Record := (Name => Fields.Device_Path, Status => Wire.Present,
                                    Kind => Fields.Text_Chunk, others => <>);
      procedure Fail (Reason : Outcome) is
      begin
         Result.Status := Reason; Result.Text_Bytes := 0;
         Result.Copied_Segments := 0; Result.Next_Segment := 0; Result.More := False;
         Text := [others => 0]; Field := (Accepted => False);
      end Fail;
   begin
      Result := (others => <>); Field := (Accepted => False);
      Scratch := [others => N.Root]; Text := [others => 0];
      if not Core.Current_Node (Service, Device) or else V.Node_Kind (Device) /= N.Device_Object then
         Fail (Invalid_Device); return;
      end if;
      Core.Describe_Node (Service, N.Root, Root, Result.Access_Result);
      if Result.Access_Result /= V.Available or else not Core.Current_Node (Service, Root) then return; end if;
      Current := Device;
      loop
         Position := V.Position (Current);
         exit when Position = N.Root;
         if Depth = Core.Max_Namespace_Nodes then Fail (Observation_Failed); return; end if;
         Depth := Depth + 1; Result.Visited_Nodes := Depth;
         if Depth <= Scratch'Length then Scratch (Scratch'First + (Depth - 1)) := Position; end if;
         Core.Describe_Node (Service, V.Parent_Position (Current), Current, Result.Access_Result);
         if Result.Access_Result /= V.Available or else not Core.Current_Node (Service, Current) then return; end if;
      end loop;
      Result.Total_Segments := Depth; Result.Required_Nodes := Depth;
      if Wire.Word (Segment_Offset) >= Wire.Word (Depth) then Fail (Bad_Position); return; end if;
      if Scratch'Length < Depth then Fail (Scratch_Required); return; end if;
      if Text'Length < Encoded_Segment_Bytes then
         Result.Required_Text_Bytes := Wire.Word (Encoded_Segment_Bytes);
         Fail (Output_Required); return;
      end if;
      Start := Natural (Segment_Offset);
      Count := Natural'Min (Depth - Start, Text'Length / Encoded_Segment_Bytes);
      Result.Text_Bytes := Wire.Word (Count) * Wire.Word (Encoded_Segment_Bytes);
      if Result.Text_Bytes + Wire.Word (Pool_Offset) > Wire.Word (Fields.U32'Last) then
         Fail (Observation_Failed); return;
      end if;
      Item.Text := (Pool_Offset, Fields.U32 (Result.Text_Bytes));
      Item.Logical_Offset := Wire.Word (Start) * Wire.Word (Encoded_Segment_Bytes);
      Item.Total_Length := Wire.Word (Depth) * Wire.Word (Encoded_Segment_Bytes);
      if not Fields.Valid (Item, Wire.Node_Limit (Core.Max_Namespace_Nodes),
        Pool_Offset + Fields.U32 (Result.Text_Bytes)) then Fail (Observation_Failed); return; end if;
      for I in 0 .. Count - 1 loop
         Core.Describe_Node (Service, Scratch (Scratch'First + (Depth - Start - I - 1)), Part, Result.Access_Result);
         if Result.Access_Result /= V.Available or else not Core.Current_Node (Service, Part) then
            Fail (Observation_Failed); return;
         end if;
         declare
            Name : constant AML_Names.Segment := V.Node_Name (Part);
            At_Byte : constant Natural := I * Encoded_Segment_Bytes;
         begin
            Text (Text'First + At_Byte) := Character'Pos ((if Start + I = 0 then '\' else '.'));
            for J in 0 .. Segment_Bytes - 1 loop
               Text (Text'First + (At_Byte + 1 + J)) := Character'Pos (Name (Name'First + J));
            end loop;
         end;
      end loop;
      Result.Status := Field_Ready; Field := (True, Item);
      Result.Copied_Segments := Count; Result.Next_Segment := Fields.U32 (Start + Count);
      Result.More := Start + Count < Depth;
   end Read;
end ACPI_Passive_Path;
