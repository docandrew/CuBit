------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Busy_Poll;
with CuBit.Handles.IPC;

package body CuBit.Block_Driver_Runtime is

   procedure Next_Request
     (Sender : out Process_ID; Request : out Message; Found : out Boolean) is
   begin
      Poll_Service_Request (Sender, Request, Found);
   end Next_Request;

   procedure Wait_Request (Sender : out Process_ID; Request : out Message) is
   begin
      receive (Sender, Request);
   end Wait_Request;

   procedure Keep_Reply (Saved : out CuBit.Handles.Reply; Kept : out Boolean) is
      Result : constant CuBit.Handles.IPC.Saved_Reply :=
        CuBit.Handles.IPC.Save_Reply;
   begin
      Kept := Result.Saved;
      Saved := (if Result.Saved then Result.Handle else CuBit.Handles.No_Reply);
   end Keep_Reply;

   procedure Answer (Saved : CuBit.Handles.Reply; Response : Message) is
      Ignored : Unsigned_64;
   begin
      Ignored := replyCap (Saved, Response);
   end Answer;

   procedure Respond (To : Process_ID; Response : Message) is
      Ignored : Unsigned_64;
   begin
      Ignored := reply (To, Response);
   end Respond;

   function Now return Unsigned_64 is (CuBit.Busy_Poll.Now);

   function Within (Since, Micros : Unsigned_64) return Boolean is
     (CuBit.Busy_Poll.Within (Since, Micros));
end CuBit.Block_Driver_Runtime;
