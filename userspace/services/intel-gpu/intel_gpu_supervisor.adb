with CuBit.Handles.IPC;
with Startup;

package body Intel_GPU_Supervisor is
   package IPC renames CuBit.Handles.IPC;
   use type IPC.Carried_Kind;

   function Endpoint return CuBit.Handles.Endpoint is (Startup.Device_Manager);

   function Submit (Msg : Message; Token : Unsigned_64) return Boolean is
     (IPC.Submit (Endpoint, Msg, Token));

   function Submit_Accepting (Msg : Message; Token : Unsigned_64) return Boolean is
     (IPC.Submit_Accepting (Endpoint, Msg, Token));

   function Reply_Tag (Receipt : Completion) return MessageTag is
     ((label    => Receipt.msg.tag.label,
       length   => Receipt.msg.tag.length,
       protocol => Receipt.msg.tag.protocol,
       ipc      => CuBit.Messages.No_IPC_Flags,  --  the envelope without the IPC layer's flags
       reserved => Receipt.msg.tag.reserved));

   function Granted_Region (Receipt : Completion) return CuBit.Handles.Device_Memory is
      Item : constant IPC.Carried := IPC.Carried_By (Receipt.msg);
   begin
      if Item.Kind /= IPC.Present then
         return CuBit.Handles.No_Device_Memory;
      end if;
      declare
         Region : constant IPC.Accepted_Device_Memory := IPC.Accept_Device_Memory (Item);
      begin
         if Region.OK then
            return Region.Value;
         end if;
      end;
      declare
         Other : constant IPC.Accepted_Endpoint := IPC.Accept_Endpoint (Item);
         Closed : IPC.Close_Result;
         pragma Unreferenced (Closed);
      begin
         if Other.OK then Closed := IPC.Close (Other.Value); end if;
      end;
      return CuBit.Handles.No_Device_Memory;
   end Granted_Region;

   function Granted_Endpoint (Receipt : Completion) return CuBit.Handles.Endpoint is
      Item : constant IPC.Carried := IPC.Carried_By (Receipt.msg);
   begin
      if Item.Kind /= IPC.Present then
         return CuBit.Handles.No_Endpoint;
      end if;
      declare
         Value : constant IPC.Accepted_Endpoint := IPC.Accept_Endpoint (Item);
      begin
         if Value.OK then
            return Value.Value;
         end if;
      end;
      declare
         Other : constant IPC.Accepted_Device_Memory := IPC.Accept_Device_Memory (Item);
         Closed : IPC.Close_Result;
         pragma Unreferenced (Closed);
      begin
         if Other.OK then Closed := IPC.Close (Other.Value); end if;
      end;
      return CuBit.Handles.No_Endpoint;
   end Granted_Endpoint;
end Intel_GPU_Supervisor;
