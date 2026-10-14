with CuBit.Handles.Fixture;
package body Intel_GPU_Supervisor is
   use CuBit.Messages;
   function Endpoint return CuBit.Handles.Endpoint is (CuBit.Handles.Fixture.Endpoint_Of (15));
   function Submit (Msg : Message; Token : Unsigned_64) return Boolean is
   begin
      raise Program_Error with "the GGTT mapping submits only accepting requests";
      return False;
   end Submit;
   function Submit_Accepting (Msg : Message; Token : Unsigned_64) return Boolean is
   begin
      pragma Assert (Msg.tag = (label => 16#0233#, length => 0,
                                protocol => 0, ipc => <>, reserved => 0));
      pragma Assert (Msg.words = [0, 0, 0, 0]);
      Submissions := Submissions + 1;
      Saved_Token := Token;
      return Mode /= 5;
   end Submit_Accepting;
   function Reply_Tag (Receipt : Completion) return MessageTag is
     ((Receipt.msg.tag with delta ipc => No_IPC_Flags));
   function Granted_Region (Receipt : Completion) return CuBit.Handles.Device_Memory is
     (CuBit.Handles.Fixture.Device_Memory_Of (Region_Slot));
   function Granted_Endpoint (Receipt : Completion) return CuBit.Handles.Endpoint is
     (CuBit.Handles.No_Endpoint);
end Intel_GPU_Supervisor;
