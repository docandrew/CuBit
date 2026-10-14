with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Handles;

-- Replies the driver owes (CAP-002). A saved reply is a Reply handle the
-- kernel places in the driver's own table; there are no reply slots. The
-- reply owed to the request being dispatched is either the kernel's current
-- reply (a request just received) or a saved one (a request set aside and
-- dispatched later): Respond answers whichever it is, so a handler never
-- needs to know, and Keep moves either into an Owed to answer later.
-- Serialized by the single service loop.
package Intel_GPU_Replies is
   subtype Process_ID is CuBit.Messages.Process_ID;
   subtype Message is CuBit.Messages.Message;

   type Owed is limited private;
   function Held (Item : Owed) return Boolean;

   -- Move the reply owed to the request being dispatched into Item. Kept is
   -- False, and nothing changes, when Item already holds one or the kernel
   -- refuses (no request outstanding, handle table full).
   procedure Keep (Item : in out Owed; Kept : out Boolean);
   function Keep (Item : in out Owed) return Boolean;

   -- Answer the reply Item holds and empty it: 1 when delivered, 0 when not
   -- (also when Item is empty). The saved reply is used up either way.
   function Answer (Item : in out Owed; Msg : Message) return Unsigned_64;

   -- The saved reply, to grant memory to the process it answers
   -- (CuBit.Memory_Grants.Create_Via_Reply); No_Reply when empty.
   function Grantee (Item : Owed) return CuBit.Handles.Reply;

   -- Dispatch a set-aside request: the reply Item holds becomes the one
   -- owed to the request being dispatched, and Item is empty.
   procedure Dispatch (Item : in out Owed);

   -- Answer the request being dispatched: 1 when delivered.
   function Respond (From : Process_ID; Msg : Message) return Unsigned_64;

   -- After dispatching a set-aside request: if its handler neither answered
   -- nor kept the reply, answer it with Msg (a failure), never drop it.
   procedure Settle (Msg : Message);
private
   type Owed is limited record
      Handle : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
   end record;
end Intel_GPU_Replies;
