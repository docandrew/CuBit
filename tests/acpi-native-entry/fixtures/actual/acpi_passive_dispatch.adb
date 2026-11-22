pragma Ada_2022;
package body ACPI_Passive_Dispatch with SPARK_Mode is
   use type Wire.Decode_Status;
   Limit : constant Wire.Node_Limit := Wire.Node_Limit (Core.Max_Namespace_Nodes);
   procedure Handle
     (Service : in out Core.State; Stamp : ACPI_Protocol.Role_Tag;
      Scope : Search.Scope_State; Request_Data : Pages.Byte_Array;
      Maximum_Node_Visits : Positive; Maximum_Resource_Items : Resources.Data_IO.Item_Count;
      Nodes : out Search.Node_Array; Parents : out Path_Owner.Node_Array;
      Reader_Data : out Firmware_Tables.Bytes; Reader_Atoms : out Reader.Atoms;
      Text_Source : out Firmware_Tables.Bytes; Filter, Payload, Response : out Pages.Byte_Array;
      Result : out Report) is
      Query : Wire.Decode_Result;
   begin
      Result := (others => <>); Nodes := [others => Search.No_Reference]; Parents := [others => Core.Namespace.Root];
      Reader_Data := [others => 0]; Reader_Atoms := [others => (others => <>)];
      Text_Source := [others => 0]; Filter := [others => 0]; Payload := [others => 0]; Response := [others => 0];
      if not ACPI_Protocol.Is_Observer (Stamp) then Result.Status := Denied; return; end if;
      Query := Pages.Decode (Request_Data, Limit); if Query.Status /= Wire.Decoded then return; end if;
      Result.Selected := Query.Value.Key.Kind; Result.Routed := True;
      case Result.Selected is
         when Wire.Find_By_ID | Wire.Find_By_Address =>
            Find.Handle (Service, Stamp, Scope, Request_Data, Maximum_Node_Visits,
              Nodes, Reader_Data, Reader_Atoms, Payload, Filter, Response, Result.Find_Result);
            Result.Status := (case Result.Find_Result.Status is
              when Find.Reply_Ready => Reply_Ready, when Find.Denied => Denied,
              when Find.Invalid_Request => Invalid_Request, when Find.Reply_Space_Required => Reply_Space_Required,
              when Find.Scratch_Required => Scratch_Required, when Find.Backend_Failed => Backend_Failed);
            Result.Written := Result.Find_Result.Written; Result.Required_Bytes := Result.Find_Result.Required_Bytes;
         when Wire.Describe =>
            Describe.Handle (Service, Stamp, Scope, Request_Data, Parents, Reader_Data, Reader_Atoms,
              Text_Source, Payload, Response, Result.Describe_Result);
            Result.Status := (case Result.Describe_Result.Status is
              when Describe.Reply_Ready => Reply_Ready, when Describe.Denied => Denied,
              when Describe.Invalid_Request => Invalid_Request, when Describe.Reply_Space_Required => Reply_Space_Required,
              when Describe.Backend_Failed => Backend_Failed);
            Result.Written := Result.Describe_Result.Written; Result.Required_Bytes := Result.Describe_Result.Required_Bytes;
         when Wire.Resources =>
            Resources.Handle (Service, Stamp, Scope, Request_Data, Maximum_Resource_Items,
              Reader_Data, Reader_Atoms, Text_Source, Payload, Response, Result.Resources_Result);
            Result.Status := (case Result.Resources_Result.Status is
              when Resources.Reply_Ready => Reply_Ready, when Resources.Denied => Denied,
              when Resources.Invalid_Request => Invalid_Request, when Resources.Reply_Space_Required => Reply_Space_Required,
              when Resources.Backend_Failed => Backend_Failed);
            Result.Written := Result.Resources_Result.Written; Result.Required_Bytes := Result.Resources_Result.Required_Bytes;
      end case;
   end Handle;
end ACPI_Passive_Dispatch;
