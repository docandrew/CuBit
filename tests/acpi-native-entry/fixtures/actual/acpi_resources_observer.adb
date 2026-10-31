pragma Ada_2022;
with Interfaces;
package body ACPI_Resources_Observer with SPARK_Mode is
   package W renames Wire;
   package V renames Core.Values;
   package N renames Core.Namespace;
   package D renames Data_IO;
   use type W.Decode_Status;
   use type W.Operation;
   use type W.Instance_ID;
   use type W.Generation;
   use type V.Access_Status;
   use type N.Object_Kind;
   use type Reader.Outcome;
   use type D.Byte_Count;
   use type D.Item_Count;
   use type D.Template_Status;
   use type D.Frame_Status;
   use type D.Item_Encoding;
   use type Projection.Outcome;
   use type Records.U32;
   Limit : constant W.Node_Limit := W.Node_Limit (Core.Max_Namespace_Nodes);
   procedure Handle
     (Service : in out Core.State; Stamp : ACPI_Protocol.Role_Tag;
      Scope : Search_Owner.Scope_State; Request_Data : Pages.Byte_Array;
      Maximum_Items : D.Item_Count; Template : out Firmware_Tables.Bytes;
      Reader_Atoms : out Reader.Atoms; Source : out Firmware_Tables.Bytes;
      Payload, Response : out Pages.Byte_Array; Result : out Report) is
      Query : W.Decode_Result;
      Root, Device : Core.Node_Descriptor;
      Access_Result : V.Access_Status;
      Header : Replies.Header;
      Extent : Natural := 0;
      Position, Desired : D.Byte_Offset := 0;
      Subentry : Records.U32 := 0;
      First_Selected : Boolean := True;
      procedure Stop (Status : Outcome) is
      begin Result.Status := Status; Result.Written := 0; Response := [others => 0]; end Stop;
      procedure Emit is
         Accepted : Boolean;
      begin
         if Header.Payload_Bytes = 0 then Replies.Encode_Envelope (Header, [], Response, Limit, Result.Written, Accepted);
         else Replies.Encode_Envelope (Header, Payload (Payload'First .. Payload'First +
           (Natural (Header.Payload_Bytes) - 1)), Response, Limit, Result.Written, Accepted); end if;
         if Accepted then Result.Status := Reply_Ready; Result.Header := Header; else Stop (Backend_Failed); end if;
      end Emit;
      procedure Error_Reply (Status : W.Query_Status; Needed : W.Word := 0) is
      begin
         Payload := [others => 0]; Source := [others => 0];
         Header := (Status => Status, Context => (Query.Value.Key, (Present => False)),
                    Required_Bytes => Needed, others => <>);
         Result.Required_Bytes := Needed; Emit;
      end Error_Reply;
      procedure Refuse (Reason : Diagnostic; At_Offset : D.Byte_Offset := 0) is
      begin Result.Reason := Reason; Result.Failed_Offset := At_Offset; Error_Reply (W.Refused); end Refuse;
      procedure Process_Template is
         Data : Firmware_Tables.Bytes renames Template (Template'First .. Template'First + (Extent - 1));
         Frame, Next_Frame : D.Frame;
         Item : Records.Resource_Result;
         Check : Projection.Report;
         Needed : W.Word;
      begin
         Result.Scan := D.Scan (Data, Maximum_Items);
         if Result.Scan.Status /= D.Complete then
            Refuse ((if Result.Scan.Status = D.Item_Limit then Template_Quota else Invalid_Template), Result.Scan.Position); return;
         end if;
         -- One subentry validates the common descriptor shape; later GPIO pins
         -- and IRQ bits have the same checked shape and bounded integer domain.
         -- No complete-list expansion occurs during this bounded preflight.
         for I in 1 .. Result.Scan.Items loop
            Frame := D.Read_Frame (Data, Position);
            if Frame.Status /= D.Decoded then Refuse (Invalid_Template, Position); return; end if;
            Projection.Read (Data, Position, 0, 0, Source, Item, Check);
            Result.Checked_Descriptors := Result.Checked_Descriptors + 1;
            if Check.Status = Projection.Output_Required then
               Result.Required_Source_Bytes := Check.Required_Source_Bytes; Refuse (Internal_Scratch_Limit, Position); return;
            elsif Check.Status not in Projection.Record_Ready | Projection.Empty_Descriptor then
               Refuse (Invalid_Descriptor, Position); return;
            end if;
            Position := Frame.Next_Offset;
         end loop;
         Desired := 0; Position := 0;
         if Query.Value.Continuation.Present then
            if W.Word (Query.Value.Continuation.Value.Offset) >= W.Word (Extent) then Error_Reply (W.Bad_Cursor); return; end if;
            Desired := D.Byte_Offset (Query.Value.Continuation.Value.Offset);
            Subentry := Query.Value.Continuation.Value.Subentry;
         end if;
         -- Scan validated framing. Still reach the requested offset by walking
         -- frames; a valid-looking header inside a payload is never a cursor.
         for I in 1 .. Result.Scan.Items loop
            exit when Position >= Desired;
            Frame := D.Read_Frame (Data, Position);
            if Frame.Status /= D.Decoded then Refuse (Invalid_Template, Position); return; end if;
            Position := Frame.Next_Offset;
         end loop;
         if Position /= Desired then Error_Reply (W.Bad_Cursor); return; end if;
         for I in 0 .. Result.Scan.Items loop
            Projection.Read (Data, Position, Subentry, Records.U32 (Replies.Resource_Byte_Count), Source, Item, Result.Projected);
            case Result.Projected.Status is
               when Projection.End_Tag =>
                  if First_Selected and then Query.Value.Continuation.Present then Error_Reply (W.Bad_Cursor);
                  else Emit; end if;
                  return;
               when Projection.Empty_Descriptor =>
                  Position := Result.Projected.Next_Offset; Subentry := 0; First_Selected := False;
               when Projection.Record_Ready =>
                  Needed := Replies.Resource_Byte_Count + Result.Projected.Source_Bytes;
                  if W.Word (Response'Length) - Replies.Header_Byte_Count < Needed then
                     Error_Reply (W.Insufficient_Space, Replies.Header_Byte_Count + Needed); return;
                  elsif W.Word (Payload'Length) < Needed then Refuse (Internal_Scratch_Limit, Position); return; end if;
                  Header.Record_Count := 1; Header.Payload_Bytes := Records.U32 (Needed);
                  declare Words : constant Records.Record_Words := Records.Encode (Item.Value, Limit, Header.Payload_Bytes); begin
                     for J in Words'Range loop
                        for K in 0 .. Pages.Bytes_Per_Word - 1 loop
                           Payload (Payload'First + (J * Pages.Bytes_Per_Word + K)) := Pages.Byte
                             (Interfaces.Shift_Right (Words (J), Pages.Byte'Size * K) and 16#FF#);
                        end loop;
                     end loop;
                  end;
                  for J in 0 .. Natural (Result.Projected.Source_Bytes) - 1 loop
                     Payload (Payload'First + (Replies.Resource_Byte_Count + J)) := Source (Source'First + J);
                  end loop;
                  Next_Frame := D.Read_Frame (Data, Result.Projected.Next_Offset);
                  if Result.Projected.Next_Subentry > 0 or else Next_Frame.Status /= D.Decoded
                    or else Next_Frame.Encoding /= D.Small_Item or else Next_Frame.Name /= 15 then
                     Header.Context.Continuation := (True, (Query.Value.Key, W.Resource_Subentry, 0,
                       Records.U32 (Result.Projected.Next_Offset), Result.Projected.Next_Subentry));
                  end if;
                  Emit; return;
               when Projection.Bad_Subentry => Error_Reply (W.Bad_Cursor); return;
               when Projection.Output_Required =>
                  Result.Required_Source_Bytes := Result.Projected.Required_Source_Bytes; Refuse (Internal_Scratch_Limit, Position); return;
               when others => Refuse (Invalid_Descriptor, Position); return;
            end case;
         end loop;
         Refuse (Invalid_Template, Position);
      end Process_Template;
   begin
      Result := (others => <>); Response := [others => 0]; Payload := [others => 0]; Source := [others => 0];
      Template := [others => 0]; Reader_Atoms := [others => (others => <>)];
      if not ACPI_Protocol.Is_Observer (Stamp) then Stop (Denied); return; end if;
      Query := Pages.Decode (Request_Data, Limit); if Query.Status /= W.Decoded then return; end if;
      if Response'Length < Replies.Header_Byte_Count then
         Result.Required_Bytes := Replies.Header_Byte_Count; Stop (Reply_Space_Required); return;
      end if;
      if Query.Value.Key.Kind /= W.Resources then Error_Reply (W.Refused); return; end if;
      if not Scope.Ready then Error_Reply (W.Namespace_Not_Loaded); return; end if;
      if Scope.Instance = 0 or else Scope.Revision = 0 then Error_Reply (W.Refused); return; end if;
      if Query.Value.Key.Instance /= Scope.Instance or else Query.Value.Key.Revision /= Scope.Revision then
         Error_Reply (W.Stale_Generation); return;
      end if;
      Core.Describe_Node (Service, N.Root, Root, Access_Result);
      if Access_Result /= V.Available or else not Core.Current_Node (Service, Root) then Error_Reply (W.Namespace_Not_Loaded); return; end if;
      if W.Word (Query.Value.Key.Node) > W.Word (Core.Namespace_Node_Count (Service)) then Error_Reply (W.Refused); return; end if;
      Core.Describe_Node (Service, N.Node_ID (Query.Value.Key.Node), Device, Access_Result);
      if Access_Result /= V.Available or else not Core.Current_Node (Service, Device)
        or else V.Node_Kind (Device) /= N.Device_Object then Error_Reply (W.Refused); return; end if;
      Result.Backend_Invoked := True;
      Reader.Read (Service, Device, Reader.Current_Resources, Template, Reader_Atoms, Result.Observation);
      if Result.Observation.Access_Result /= V.Available or else Result.Observation.Cleanup_Result /= V.Available then Refuse (Backend_Refusal); return; end if;
      case Result.Observation.Status is
         when Reader.Missing => Error_Reply ((if Query.Value.Continuation.Present then W.Bad_Cursor else W.Not_Found)); return;
         when Reader.Capacity_Required => Refuse (Internal_Scratch_Limit); return;
         when Reader.Static_Value => null;
         when others => Refuse (Backend_Refusal); return;
      end case;
      if not Result.Observation.Size_Known or else Result.Observation.Copied_Atoms /= 0
        or else Result.Observation.Copied_Bytes /= Result.Observation.Required_Bytes
        or else Result.Observation.Copied_Bytes > Template'Length then Refuse (Backend_Refusal); return; end if;
      Extent := Result.Observation.Copied_Bytes;
      Header := (Status => W.OK, Context => (Query.Value.Key, (Present => False)), others => <>);
      Process_Template;
   end Handle;
end ACPI_Resources_Observer;
