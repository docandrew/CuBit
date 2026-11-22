with CuBit.Handles.IPC;

package body Config_Object_Replies is
   procedure Save (Handle : out CuBit.Handles.Reply; Saved : out Boolean) is
      Result : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
   begin
      Saved := Result.Saved;
      Handle := (if Result.Saved then Result.Handle else CuBit.Handles.No_Reply);
   end Save;
end Config_Object_Replies;
