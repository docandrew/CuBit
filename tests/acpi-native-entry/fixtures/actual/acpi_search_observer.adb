pragma Ada_2022;
with Interfaces;
package body ACPI_Search_Observer with SPARK_Mode is
   package W renames Wire;
   use type W.Instance_ID;
   use type W.Generation;
   use type W.Decode_Status;
   Limit : constant W.Node_Limit := W.Node_Limit (Core.Max_Namespace_Nodes);
   procedure Handle
     (Service : in out Core.State; Stamp : ACPI_Protocol.Role_Tag; Scope : Search.Scope_State;
      Request_Data : Pages.Byte_Array; Maximum_Node_Visits : Positive;
      Nodes : out Search.Node_Array; Reader_Data : out Firmware_Tables.Bytes;
      Reader_Atoms : out Reader.Atoms; Payload, Filter, Response : out Pages.Byte_Array;
      Result : out Report) is
      Query : W.Decode_Result;
      Single_Filter : W.Word := 0;
      Capacity : W.Word;
      Returned : Natural := 0;
      Header : Replies.Header;

      procedure Fail (Status : Outcome; Reason : Diagnostic) is
      begin
         Response := [others => 0]; Nodes := [others => Search.No_Reference];
         Result.Status := Status; Result.Reason := Reason;
         Result.Written := 0; Result.Returned_Nodes := 0;
      end Fail;

      procedure Emit is
         Accepted : Boolean;
         Count : constant W.Word := W.Word (Header.Filter_Bytes);
         Payload_Count : constant W.Word := W.Word (Header.Payload_Bytes);
         procedure Send (Payload_Part, Filter_Part : Pages.Byte_Array) is
         begin
            Replies.Encode_Envelope (Header, Payload_Part, Response, Limit, Result.Written, Accepted, Filter_Part);
         end Send;
      begin
         if Count > W.Word (Filter'Length) then
            Result.Required_Filter_Bytes := Count; Fail (Scratch_Required, Internal_Scratch_Limit); return;
         end if;
         if Payload_Count > W.Word (Payload'Length) then Fail (Scratch_Required, Internal_Scratch_Limit); return; end if;
         if Replies.Extent (Header) > W.Word (Response'Length) then
            Result.Required_Bytes := Replies.Extent (Header); Fail (Reply_Space_Required, Reply_Buffer_Limit); return;
         end if;
         if Count > W.Word (Natural'Last) then Fail (Backend_Failed, Backend_Refusal); return; end if;
         if Single_Filter > 0 then
            if Single_Filter > W.Word (Natural'Last)
              or else W.Word (W.Text_Offset (Query.Value.Key.ID)) > W.Word (Natural'Last)
            then Fail (Backend_Failed, Backend_Refusal); return; end if;
            declare Start : constant Natural := Natural (W.Text_Offset (Query.Value.Key.ID)); begin
               if Request_Data'Length = 0 or else Start > Request_Data'Last - Request_Data'First
                 or else Natural (Single_Filter) - 1 > Request_Data'Last - Request_Data'First - Start
               then Fail (Backend_Failed, Backend_Refusal); return; end if;
               for I in 0 .. Natural (Count) - 1 loop
                  Filter (Filter'First + I) := Request_Data
                    (Request_Data'First + (Start + I mod Natural (Single_Filter)));
               end loop;
            end;
         end if;
         if Replies.Header_Byte_Count + Payload_Count > W.Word (Replies.U32'Last) then
            Fail (Backend_Failed, Backend_Refusal); return;
         end if;
         Pages.Rebase_Text (Header.Context, Replies.U32 (Replies.Header_Byte_Count + Payload_Count), Limit, Accepted);
         if not Accepted or else not Replies.Valid (Header, Limit) then Fail (Backend_Failed, Backend_Refusal); return; end if;
         -- Extents are actual array-bounded lengths, checked before narrowing.
         if Payload_Count > W.Word (Natural'Last) then Fail (Backend_Failed, Backend_Refusal); return; end if;
         if Payload_Count = 0 then
            if Count = 0 then Send ([], []);
            else Send ([], Filter (Filter'First .. Filter'First + (Natural (Count) - 1))); end if;
         elsif Count = 0 then
            Send (Payload (Payload'First .. Payload'First + (Natural (Payload_Count) - 1)), []);
         else
            Send (Payload (Payload'First .. Payload'First + (Natural (Payload_Count) - 1)),
              Filter (Filter'First .. Filter'First + (Natural (Count) - 1)));
         end if;
         if not Accepted then Fail (Backend_Failed, Backend_Refusal); return; end if;
         Result.Status := Reply_Ready; Result.Header := Header;
         Result.Returned_Nodes := Natural (Header.Record_Count);
      end Emit;

      procedure Error_Reply (Status : W.Query_Status; Needed : W.Word := 0) is
      begin
         Nodes := [others => Search.No_Reference]; Payload := [others => 0]; Filter := [others => 0];
         Header := (Status => Status, Context => (Query.Value.Key, (Present => False)),
           Filter_Bytes => Replies.U32 (Single_Filter), Required_Bytes => Needed, others => <>);
         Result.Required_Bytes := Needed;
         Emit;
      end Error_Reply;
   begin
      Result := (others => <>); Response := [others => 0]; Payload := [others => 0]; Filter := [others => 0];
      Nodes := [others => Search.No_Reference]; Reader_Data := [others => 0]; Reader_Atoms := [others => (others => <>)];
      if not ACPI_Protocol.Is_Observer (Stamp) then Result.Status := Denied; return; end if;
      Query := Pages.Decode (Request_Data, Limit);
      if Query.Status /= W.Decoded then return; end if;
      if W.Is_External (Query.Value.Key.ID) then Single_Filter := W.Word (W.Text_Length (Query.Value.Key.ID)); end if;
      if W.Word (Response'Length) < Replies.Header_Byte_Count + Single_Filter then
         Result.Required_Bytes := Replies.Header_Byte_Count + Single_Filter;
         Fail (Reply_Space_Required, Reply_Buffer_Limit); return;
      end if;
      if W.Word (Filter'Length) < Single_Filter then
         Result.Required_Filter_Bytes := Single_Filter; Fail (Scratch_Required, Internal_Scratch_Limit); return;
      end if;
      if Query.Value.Key.Kind not in W.Find_By_ID | W.Find_By_Address then
         Result.Reason := Backend_Refusal; Error_Reply (W.Refused); return;
      end if;
      if Nodes'Length = 0 or else Payload'Length < Replies.Node_Byte_Count then
         Result.Reason := Internal_Scratch_Limit; Error_Reply (W.Refused); return;
      end if;
      Capacity := W.Word'Min (W.Word (Nodes'Length), W.Word (Payload'Length) / Replies.Node_Byte_Count);
      Capacity := W.Word'Min (Capacity, W.Word (Maximum_Node_Visits));
      -- Probe at least one slot so a header-only buffer can still receive a
      -- terminal empty result. A matching result instead gets explicit sizing.
      Capacity := W.Word'Min (Capacity, W.Word'Max (1,
        (W.Word (Response'Length) - Replies.Header_Byte_Count - Single_Filter) / Replies.Node_Byte_Count));
      if Capacity = 0 or else Capacity > W.Word (Natural'Last) then
         Result.Reason := Internal_Scratch_Limit; Error_Reply (W.Refused); return;
      end if;
      Result.Backend_Invoked := True;
      Search.Search (Service, Scope, Request_Data, Maximum_Node_Visits,
        Nodes (Nodes'First .. Nodes'First + (Natural (Capacity) - 1)), Reader_Data, Reader_Atoms, Result.Scan);
      case Result.Scan.Status is
         when Search.Namespace_Not_Ready => Error_Reply (W.Namespace_Not_Loaded); return;
         when Search.Stale_Reference => Error_Reply (W.Stale_Generation); return;
         when Search.Bad_Continuation => Error_Reply (W.Bad_Cursor); return;
         when Search.Invalid_Parent | Search.Invalid_Request | Search.Observation_Failed =>
            Result.Reason := Backend_Refusal; Error_Reply (W.Refused); return;
         when Search.Scratch_Required | Search.Output_Required =>
            Result.Reason := Internal_Scratch_Limit; Error_Reply (W.Refused); return;
         when Search.No_Static_Match => Error_Reply (W.Not_Found); return;
         when Search.Page_Ready => null;
      end case;
      Returned := Result.Scan.Copied_Nodes;
      Header := (Status => W.OK, Context => (Result.Scan.Key, Result.Scan.Next_Cursor), others => <>);
      declare
         Copies : constant W.Word := (if Header.Context.Continuation.Present then 2 else 1);
         Filter_Count : constant W.Word := Copies * Single_Filter;
         Available : W.Word;
      begin
         if Filter_Count > W.Word (Replies.U32'Last) then Fail (Scratch_Required, Internal_Scratch_Limit); return; end if;
         Header.Filter_Bytes := Replies.U32 (Filter_Count);
         if Filter_Count + Replies.Header_Byte_Count > W.Word (Response'Length) then
            Error_Reply (W.Insufficient_Space, Filter_Count + Replies.Header_Byte_Count
              + W.Word (Returned) * Replies.Node_Byte_Count); return;
         end if;
         Available := (W.Word (Response'Length) - Replies.Header_Byte_Count - Filter_Count) / Replies.Node_Byte_Count;
         if W.Word (Returned) > Available then
            if Available = 0 then
               Error_Reply (W.Insufficient_Space, Replies.Header_Byte_Count + Filter_Count + Replies.Node_Byte_Count); return;
            end if;
            -- Do not rescan. Resume at the first omitted MATCH, so every
            -- emitted page progresses and no observed match is skipped.
            Returned := Natural (Available);
            Header.Context.Continuation := (True,
              (Result.Scan.Key, W.Node_Scan, Nodes (Nodes'First + Returned).Index, 0, 0));
         end if;
      end;
      for I in 0 .. Returned - 1 loop
         declare Ref : constant W.Node_Ref := Nodes (Nodes'First + I); begin
            if Ref.Instance /= Header.Context.Key.Instance or else Ref.Revision /= Header.Context.Key.Revision
              or else Ref.Index not in 1 .. Limit then Fail (Backend_Failed, Backend_Refusal); return; end if;
            for J in 0 .. Replies.Node_Byte_Count - 1 loop
               Payload (Payload'First + (I * Replies.Node_Byte_Count + J)) := Pages.Byte
                 (Interfaces.Shift_Right (W.Word (Ref.Index), Pages.Byte'Size * J) and 16#FF#);
            end loop;
         end;
      end loop;
      Header.Record_Count := Replies.U32 (Returned);
      Header.Payload_Bytes := Replies.U32 (W.Word (Returned) * Replies.Node_Byte_Count);
      Emit;
   end Handle;
end ACPI_Search_Observer;
