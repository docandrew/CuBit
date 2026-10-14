with CuBit.Handles.IPC;

package body Intel_GPU_Replies is
   use CuBit.Handles;

   -- The saved reply of the set-aside request being dispatched, if any.
   Dispatched : Reply := No_Reply;

   function Held (Item : Owed) return Boolean is (Valid (Item.Handle));

   procedure Keep (Item : in out Owed; Kept : out Boolean) is
   begin
      Kept := False;
      if Valid (Item.Handle) then return; end if;
      if Valid (Dispatched) then
         Item.Handle := Dispatched;
         Dispatched := No_Reply;
         Kept := True;
         return;
      end if;
      declare
         Saved : constant IPC.Saved_Reply := IPC.Save_Reply;
      begin
         if Saved.Saved then
            Item.Handle := Saved.Handle;
            Kept := True;
         end if;
      end;
   end Keep;

   function Keep (Item : in out Owed) return Boolean is
      Kept : Boolean;
   begin
      Keep (Item, Kept);
      return Kept;
   end Keep;

   function Answer (Item : in out Owed; Msg : Message) return Unsigned_64 is
      Delivered : Unsigned_64 := 0;
   begin
      if Valid (Item.Handle) then
         Delivered := CuBit.Messages.replyCap (Item.Handle, Msg);
         Item.Handle := No_Reply;
      end if;
      return Delivered;
   end Answer;

   function Grantee (Item : Owed) return Reply is (Item.Handle);

   procedure Dispatch (Item : in out Owed) is
      Unused : Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Settle (Unused);
      Dispatched := Item.Handle;
      Item.Handle := No_Reply;
   end Dispatch;

   function Respond (From : Process_ID; Msg : Message) return Unsigned_64 is
      Delivered : Unsigned_64;
   begin
      if Valid (Dispatched) then
         Delivered := CuBit.Messages.replyCap (Dispatched, Msg);
         Dispatched := No_Reply;
         return Delivered;
      end if;
      return CuBit.Messages.reply (From, Msg);
   end Respond;

   procedure Settle (Msg : Message) is
      Delivered : Unsigned_64;
      pragma Unreferenced (Delivered);
   begin
      if Valid (Dispatched) then
         Delivered := CuBit.Messages.replyCap (Dispatched, Msg);
         Dispatched := No_Reply;
      end if;
   end Settle;
end Intel_GPU_Replies;
