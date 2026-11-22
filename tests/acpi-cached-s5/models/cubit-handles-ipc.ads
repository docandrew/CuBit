pragma Ada_2022;
with CuBit.Messages;
package CuBit.Handles.IPC is
   type Saved_Reply is record Saved : Boolean; Handle : Reply; end record;
   function Save_Reply return Saved_Reply;
   function Answer (Token : Reply; Message : CuBit.Messages.Message) return Boolean;
end CuBit.Handles.IPC;
