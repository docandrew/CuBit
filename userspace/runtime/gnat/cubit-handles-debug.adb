pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Handles.Kernel;
with CuBit.Messages;

package body CuBit.Handles.Debug is
   package K renames CuBit.Handles.Kernel;

   function Address_Word (A : System.Address) return Unsigned_64;

   --  One Call on Word; False when the reply was unreadable.
   procedure Raw
     (Word : Unsigned_64; Label, Length : Unsigned_64; W0, W1, W2 : Unsigned_64;
      Msg : out CuBit.Messages.Message; Readable : out Boolean);

   function Address_Word (A : System.Address) return Unsigned_64 is
     (Unsigned_64 (System.Storage_Elements.To_Integer (A)));

   function Valid (Item : Session) return Boolean is
     (Capability_ABI.Is_Handle_Word (Item.Word));

   procedure Raw
     (Word : Unsigned_64; Label, Length : Unsigned_64; W0, W1, W2 : Unsigned_64;
      Msg : out CuBit.Messages.Message; Readable : out Boolean)
   is
      Result : Unsigned_64;
   begin
      Msg := CuBit.Messages.NULL_MESSAGE;
      Msg.tag.label := Unsigned_32 (Label);
      Msg.tag.length := Unsigned_8 (Length);
      Msg.words (0) := W0;
      Msg.words (1) := W1;
      Msg.words (2) := W2;
      --  A kernel object answers at once; the deadline is the Call's own.
      Result := K.Call (K.Call_Via_Endpoint, Word, Address_Word (Msg'Address),
                        CuBit.Messages.Wait_Forever);
      Readable := Result /= 0 and then Result /= Unsigned_64'Last
        and then Unsigned_64 (Msg.tag.label) = Label
        and then Msg.tag.length = ABI.Reply_Words;
   end Raw;

   function Mint
     (Control : Process; Kind : ABI.Debug_Kind;
      Target : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process)
      return Mint_Result
   is
      Msg : CuBit.Messages.Message;
      Readable : Boolean;
   begin
      Raw (K.Word (Control), ABI.Mint_Debug_Label, ABI.Mint_Request_Words,
           ABI.Kind_Word (Kind), CuBit.Process_IDs.To_Word (Target), 0, Msg, Readable);
      if not Readable then
         return (Minted => False, Why_Word => 0);
      elsif Msg.words (ABI.Outcome_Word) /= ABI.Outcome_Word_Of (ABI.Done) then
         return (Minted => False, Why_Word => Msg.words (ABI.Outcome_Word));
      end if;
      return (Minted => True, Handle => (Word => Msg.words (1)),
              Session_Word => Msg.words (2));
   end Mint;

   function Revoke (Control : Process; Session_Word : Unsigned_64) return Unsigned_64 is
      Msg : CuBit.Messages.Message;
      Readable : Boolean;
   begin
      Raw (K.Word (Control), ABI.Revoke_Debug_Label, ABI.Revoke_Request_Words,
           Session_Word, 0, 0, Msg, Readable);
      return (if Readable then Msg.words (ABI.Outcome_Word) else 0);
   end Revoke;

   function Call
     (Item : Session; Op : ABI.Operation; W0, W1, W2 : Unsigned_64 := 0) return Reply
   is
      Msg : CuBit.Messages.Message;
      Readable : Boolean;
   begin
      Raw (Item.Word, ABI.Label_Of (Op), ABI.Request_Words (Op), W0, W1, W2, Msg, Readable);
      if not Readable then
         return (others => 0);
      end if;
      return (Outcome => Msg.words (ABI.Outcome_Word), W1 => Msg.words (1),
              W2 => Msg.words (2), W3 => Msg.words (3));
   end Call;

   function Threads (Item : Session; Buffer : System.Address; Bytes : Unsigned_64)
     return Reply is
     (Call (Item, ABI.Threads, Address_Word (Buffer), Bytes));

   function Registers
     (Item : Session; Selector : Unsigned_64; Buffer : System.Address; Bytes : Unsigned_64)
      return Reply is
     (Call (Item, ABI.Registers, Selector, Address_Word (Buffer), Bytes));

   function Map_View (Item : Session; Target_Address, View : Unsigned_64; Pages : Unsigned_64)
     return Reply is
     (Call (Item, ABI.Map_View, Target_Address, View, Pages));

   function Unmap_View (Item : Session; View, Pages : Unsigned_64) return Reply is
     (Call (Item, ABI.Unmap_View, View, Pages));
   function Copy (Item : Session; Kind : ABI.Debug_Kind; Kept : Rights) return Derivation is
     (Make (Capability_Kind (Kind), Item.Word, Kept, 0, 0, 0, 0));

   function Accept_Session
     (Item : CuBit.Handles.IPC.Carried; Kind : ABI.Debug_Kind) return Accepted_Session
   is
      use type CuBit.Handles.IPC.Carried_Kind;
      use type Capability_ABI.Kind;
      D : CuBit.Handles.IPC.Description;
   begin
      if Item.Kind = CuBit.Handles.IPC.Refused then
         return (OK => False, Why => Item.Why);
      elsif Item.Kind /= CuBit.Handles.IPC.Present then
         return (OK => False, Why => Capability_ABI.Invalid_Handle);
      end if;
      D := CuBit.Handles.IPC.Describe_Word (Item.Word);
      if not D.Current then
         return (OK => False, Why => D.Why);
      elsif D.Kind /= Capability_Kind (Kind) then
         return (OK => False, Why => Capability_ABI.Wrong_Kind);
      end if;
      return (OK => True, Value => (Word => Item.Word));
   end Accept_Session;

   function Close (Item : Session) return Boolean is
     (K.Call (K.Close_Handle, Item.Word) = 0);

   function Word (Item : Session) return Unsigned_64 is (Item.Word);
   function From_Word (Word : Unsigned_64) return Session is ((Word => Word));

end CuBit.Handles.Debug;
