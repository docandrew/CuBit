pragma Ada_2022;
with Interfaces;
package body ACPI_Describe_Observer with SPARK_Mode is
   package W renames Wire;
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type W.Decode_Status;
   use type W.Instance_ID;
   use type W.Generation;
   use type W.Operation;
   use type V.Access_Status;
   use type N.Object_Kind;
   use type Fields.U32;
   use type Fields.Value_Kind;
   use type Identity.Outcome;
   use type Paths.Outcome;
   Limit : constant W.Node_Limit := W.Node_Limit (Core.Max_Namespace_Nodes);
   Path_Record_Reserve : constant Positive := 2;
   UID_Record_Reserve : constant Positive := 3;
   CID_Record_Reserve : constant Positive := 1;
   procedure Handle
     (Service : in out Core.State; Stamp : ACPI_Protocol.Role_Tag;
      Scope : Search_Owner.Scope_State; Request_Data : Pages.Byte_Array;
      Path_Scratch : out Paths.Node_Array; Reader_Data : out Firmware_Tables.Bytes;
      Reader_Atoms : out Reader.Atoms; Text : out Firmware_Tables.Bytes;
      Payload, Response : out Pages.Byte_Array; Result : out Report) is
      Query : W.Decode_Result;
      Root, Device : Core.Node_Descriptor;
      Access_Result : V.Access_Status;
      Header : Replies.Header;
      Phase : W.Position_Kind := W.Description_Path;
      Offset : Fields.U32 := 0;
      Reserved : Positive := Path_Record_Reserve;
      Area, Available, Budget, Text_Capacity, Text_Count : W.Word := 0;
      Records : array (1 .. UID_Record_Reserve) of Fields.Field_Record := [others => (others => <>)];
      Count : Natural := 0;
      Failed : Boolean := False;
      Field : Fields.Field_Result;
      procedure Stop (Status : Outcome) is
      begin
         Result.Status := Status; Result.Written := 0; Response := [others => 0]; Failed := True;
      end Stop;
      procedure Emit is
         Accepted : Boolean;
      begin
         if Header.Payload_Bytes = 0 then
            Replies.Encode_Envelope (Header, [], Response, Limit, Result.Written, Accepted);
         else
            Replies.Encode_Envelope (Header, Payload (Payload'First .. Payload'First +
              (Natural (Header.Payload_Bytes) - 1)), Response, Limit, Result.Written, Accepted);
         end if;
         if Accepted then Result.Status := Reply_Ready; Result.Header := Header;
         else Stop (Backend_Failed); end if;
      end Emit;
      procedure Error_Reply (Status : W.Query_Status; Needed : W.Word := 0) is
      begin
         Payload := [others => 0]; Text := [others => 0];
         Header := (Status => Status, Context => (Query.Value.Key, (Present => False)),
                    Required_Bytes => Needed, others => <>);
         Result.Required_Bytes := Needed; Emit; Failed := True;
      end Error_Reply;
      procedure More (Part : W.Position_Kind; Next : Fields.U32) is
      begin Header.Context.Continuation := (True, (Query.Value.Key, Part, 0, Next, 0)); end More;
      procedure Pressure (Needed : W.Word) is
      begin
         if Available < Area + Needed then Error_Reply (W.Insufficient_Space, Replies.Header_Byte_Count + Area + Needed);
         else Result.Reason := Internal_Scratch_Limit; Error_Reply (W.Refused); end if;
      end Pressure;
      procedure Save_Text (Bytes : W.Word) is
      begin
         Text_Count := Bytes;
         if Bytes > Text_Capacity then Result.Reason := Backend_Refusal; Error_Reply (W.Refused); return; end if;
         for I in 0 .. Natural (Bytes) - 1 loop
            Payload (Payload'First + (Natural (Area) + I)) := Text (Text'First + I);
         end loop;
      end Save_Text;
      procedure Read_Identity (Member : Identity.Member_Name; Ordinal : Fields.U32 := 0;
                               Logical_Offset : W.Word := 0) is
      begin
         Identity.Read (Service, Device, Member, Ordinal, Logical_Offset, Fields.U32 (Area),
           Reader_Data, Reader_Atoms, Text (Text'First .. Text'First + (Natural (Text_Capacity) - 1)),
           Field, Result.Identity_Result);
         case Result.Identity_Result.Status is
            when Identity.Field_Ready => Count := Count + 1; Records (Count) := Field.Value;
            when Identity.End_Of_List => null;
            when Identity.Bad_Position => Error_Reply (W.Bad_Cursor);
            when Identity.Output_Required => Pressure (Result.Identity_Result.Required_Text_Bytes);
            when Identity.Scratch_Required => Result.Reason := Internal_Scratch_Limit; Error_Reply (W.Refused);
            when Identity.Observation_Failed => Result.Reason := Backend_Refusal; Error_Reply (W.Refused);
         end case;
      end Read_Identity;
   begin
      Result := (others => <>); Response := [others => 0]; Payload := [others => 0];
      Text := [others => 0]; Path_Scratch := [others => N.Root];
      Reader_Data := [others => 0]; Reader_Atoms := [others => (others => <>)];
      if not ACPI_Protocol.Is_Observer (Stamp) then Stop (Denied); return; end if;
      Query := Pages.Decode (Request_Data, Limit);
      if Query.Status /= W.Decoded then return; end if;
      if Response'Length < Replies.Header_Byte_Count then
         Result.Required_Bytes := Replies.Header_Byte_Count; Stop (Reply_Space_Required); return;
      end if;
      if Query.Value.Key.Kind /= W.Describe then Error_Reply (W.Refused); return; end if;
      if not Scope.Ready then Error_Reply (W.Namespace_Not_Loaded); return; end if;
      if Scope.Instance = 0 or else Scope.Revision = 0 then Error_Reply (W.Refused); return; end if;
      if Query.Value.Key.Instance /= Scope.Instance or else Query.Value.Key.Revision /= Scope.Revision then
         Error_Reply (W.Stale_Generation); return;
      end if;
      Core.Describe_Node (Service, N.Root, Root, Access_Result);
      if Access_Result /= V.Available or else not Core.Current_Node (Service, Root) then
         Error_Reply (W.Namespace_Not_Loaded); return;
      end if;
      if W.Word (Query.Value.Key.Node) > W.Word (Core.Namespace_Node_Count (Service)) then Error_Reply (W.Refused); return; end if;
      Core.Describe_Node (Service, N.Node_ID (Query.Value.Key.Node), Device, Access_Result);
      if Access_Result /= V.Available or else not Core.Current_Node (Service, Device)
        or else V.Node_Kind (Device) /= N.Device_Object then Error_Reply (W.Refused); return; end if;
      if Query.Value.Continuation.Present then
         Phase := Query.Value.Continuation.Value.Part; Offset := Query.Value.Continuation.Value.Offset;
      end if;
      Reserved := (case Phase is when W.Description_Path => Path_Record_Reserve,
        when W.Description_CID => CID_Record_Reserve, when others => UID_Record_Reserve);
      Area := W.Word (Reserved * Fields.Record_Byte_Count);
      Available := W.Word (Response'Length) - Replies.Header_Byte_Count;
      if Available < Area then Pressure (0); return; end if;
      Budget := W.Word'Min (Available, W.Word (Payload'Length));
      if Budget < Area then Result.Reason := Internal_Scratch_Limit; Error_Reply (W.Refused); return; end if;
      Text_Capacity := W.Word'Min (W.Word (Text'Length), Budget - Area);
      Header := (Status => W.OK, Context => (Query.Value.Key, (Present => False)), others => <>);
      Result.Backend_Invoked := True;
      case Phase is
         when W.Description_Path =>
            Paths.Read (Service, Device, Offset, Fields.U32 (Area), Path_Scratch,
              Text (Text'First .. Text'First + (Natural (Text_Capacity) - 1)), Field, Result.Path_Result);
            case Result.Path_Result.Status is
               when Paths.Field_Ready => Count := 1; Records (Count) := Field.Value; Save_Text (Result.Path_Result.Text_Bytes);
               when Paths.Bad_Position => Error_Reply (W.Bad_Cursor);
               when Paths.Output_Required => Pressure (Result.Path_Result.Required_Text_Bytes);
               when Paths.Scratch_Required => Result.Reason := Internal_Scratch_Limit; Error_Reply (W.Refused);
               when others => Result.Reason := Backend_Refusal; Error_Reply (W.Refused);
            end case;
            if Failed then return; end if;
            if Result.Path_Result.More then More (W.Description_Path, Result.Path_Result.Next_Segment);
            else Read_Identity (Fields.Hardware_ID); if Failed then return; end if; More (W.Description_CID, 0); end if;
         when W.Description_CID =>
            Read_Identity (Fields.Compatible_ID, Offset); if Failed then return; end if;
            if Result.Identity_Result.Status = Identity.End_Of_List then
               if Offset /= 0 then Error_Reply (W.Bad_Cursor); return; end if;
               More (W.Description_UID, 0);
            else
               Save_Text (Result.Identity_Result.Text_Bytes); if Failed then return; end if;
               if Result.Identity_Result.More_CIDs then More (W.Description_CID, Offset + 1);
               else More (W.Description_UID, 0); end if;
            end if;
         when W.Description_UID =>
            Read_Identity (Fields.Unique_ID, Logical_Offset => W.Word (Offset)); if Failed then return; end if;
            Save_Text (Result.Identity_Result.Text_Bytes); if Failed then return; end if;
            if Records (1).Kind = Fields.Text_Chunk and then Fields.Has_More
              (Records (1), Limit, Fields.U32 (Area + Text_Count)) then
               More (W.Description_UID, Fields.U32 (Records (1).Logical_Offset + Text_Count));
            else
               Read_Identity (Fields.Address); if Failed then return; end if;
               Read_Identity (Fields.Device_Status); if Failed then return; end if;
            end if;
         when others => Error_Reply (W.Bad_Cursor); return;
      end case;
      if Count = 0 then Header.Payload_Bytes := 0;
      else
         -- Compact text after the exact record directory. Source is above
         -- destination; ascending copy is safe even when the spans overlap.
         declare Prefix : constant Natural := Count * Fields.Record_Byte_Count; begin
            for I in 0 .. Natural (Text_Count) - 1 loop
               Payload (Payload'First + (Prefix + I)) := Payload (Payload'First + (Natural (Area) + I));
            end loop;
            Header.Payload_Bytes := Fields.U32 (W.Word (Prefix) + Text_Count);
            for Position in 1 .. Count loop
               if Records (Position).Text.Length > 0 then Records (Position).Text.Offset := Fields.U32 (Prefix); end if;
               if not Fields.Valid (Records (Position), Limit, Header.Payload_Bytes) then Stop (Backend_Failed); return; end if;
               declare Words : constant Fields.Record_Words := Fields.Encode (Records (Position), Limit, Header.Payload_Bytes); begin
                  for I in Words'Range loop
                     for J in 0 .. Pages.Bytes_Per_Word - 1 loop
                        Payload (Payload'First + ((Position - 1) * Fields.Record_Byte_Count + I * Pages.Bytes_Per_Word + J)) :=
                          Pages.Byte (Interfaces.Shift_Right (Words (I), Pages.Byte'Size * J) and 16#FF#);
                     end loop;
                  end loop;
               end;
            end loop;
         end;
      end if;
      Header.Record_Count := Fields.U32 (Count); Emit;
   end Handle;
end ACPI_Describe_Observer;
