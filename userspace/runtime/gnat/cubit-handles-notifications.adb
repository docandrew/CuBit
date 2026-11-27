pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Handles.Kernel;
with CuBit.Handles.Startup;
with CuBit.Messages;
with CuBit.Startup_Tables;

package body CuBit.Handles.Notifications is

   package K renames CuBit.Handles.Kernel;

   function Valid (Item : Waitable) return Boolean is (Capability_ABI.Is_Handle_Word (Item.Word));
   function Valid (Item : Waker) return Boolean is (Capability_ABI.Is_Handle_Word (Item.Word));

   function Address_Word (A : System.Address) return Unsigned_64 is
     (Unsigned_64 (System.Storage_Elements.To_Integer (A)));

   function Own_Process return Process is
     (CuBit.Handles.Startup.Process_Named (CuBit.Startup_Tables.Self_Process_Text));

   function To_Outcome (W : Unsigned_64) return ABI.Outcome is
     (case W is
        when 0 => ABI.Ok, when 2 => ABI.Not_Allowed, when 3 => ABI.Already,
        when 4 => ABI.Busy_Object, when 5 => ABI.Limit_Reached, when 6 => ABI.No_Memory,
        when 7 => ABI.Ended_Object, when others => ABI.Refused_Call);

   --  One Call on Word: the outcome and the reply's word 1.
   procedure Ask (Word : Unsigned_64; Label : Unsigned_64; W0, W1 : Unsigned_64;
                  Outcome : out ABI.Outcome; Result : out Unsigned_64);
   procedure Ask (Word : Unsigned_64; Label : Unsigned_64; W0, W1 : Unsigned_64;
                  Outcome : out ABI.Outcome; Result : out Unsigned_64) is
      Msg : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Status : Unsigned_64;
   begin
      Msg.tag.label := Unsigned_32 (Label);
      Msg.tag.length := 4;
      Msg.words := [W0, W1, 0, 0];
      Status := K.Call (K.Call_Via_Endpoint, Word, Address_Word (Msg'Address),
                        CuBit.Messages.Wait_Forever);
      if Status = 0 or else Status = Unsigned_64'Last or else Msg.tag.length /= 4 then
         Outcome := ABI.Refused_Call;
         Result := 0;
      else
         Outcome := To_Outcome (Msg.words (0));
         Result := Msg.words (1);
      end if;
   end Ask;

   function Create (Self : Process; Item : out Waitable) return ABI.Outcome is
      Outcome : ABI.Outcome;
      Word : Unsigned_64;
   begin
      Ask (K.Word (Self), ABI.Create_Label, 0, 0, Outcome, Word);
      Item := (if ABI."=" (Outcome, ABI.Ok) then (Word => Word) else No_Waitable);
      return Outcome;
   end Create;

   function Mint_Waker (Item : Waitable; Badge : Unsigned_64; Grant : Boolean;
                        Result : out Waker) return ABI.Outcome is
      Outcome : ABI.Outcome;
      Word : Unsigned_64;
   begin
      Ask (Item.Word, ABI.Mint_Wake_Label, Badge, (if Grant then 1 else 0), Outcome, Word);
      Result := (if ABI."=" (Outcome, ABI.Ok) then (Word => Word) else No_Waker);
      return Outcome;
   end Mint_Waker;

   function Wait (Item : Waitable; Deadline : Unsigned_64) return Unsigned_64 is
     (K.Call (K.Wait_One, ABI.Notification, Item.Word, 0, Deadline));

   function Wake (Item : Waker) return Unsigned_64 is
     (K.Call (K.Wake, ABI.Notification, Item.Word));

   function Simple (Word, Label, W0 : Unsigned_64) return ABI.Outcome;
   function Simple (Word, Label, W0 : Unsigned_64) return ABI.Outcome is
      Outcome : ABI.Outcome;
      Ignore : Unsigned_64;
   begin
      Ask (Word, Label, W0, 0, Outcome, Ignore);
      return Outcome;
   end Simple;

   function Bind_Leaf (Leaf : Waitable; Root : Waker) return ABI.Outcome is
     (Simple (Leaf.Word, ABI.Bind_Leaf_Label, Root.Word));
   function Unbind_Leaf (Leaf : Waitable) return ABI.Outcome is
     (Simple (Leaf.Word, ABI.Unbind_Leaf_Label, 0));
   function Bind_Thread (Root : Waitable) return ABI.Outcome is
     (Simple (Root.Word, ABI.Bind_Thread_Label, 0));
   function Unbind_Thread (Root : Waitable) return ABI.Outcome is
     (Simple (Root.Word, ABI.Unbind_Thread_Label, 0));

   function Close (Item : Waitable) return Boolean is (K.Call (K.Close_Handle, Item.Word) = 0);
   function Close (Item : Waker) return Boolean is (K.Call (K.Close_Handle, Item.Word) = 0);

   function Word_Of (Item : Waker) return Unsigned_64 is (Item.Word);
   function To_Waker (Word : Unsigned_64) return Waker is ((Word => Word));

   function Private_Wait (Where : System.Address; Expected, Deadline : Unsigned_64)
     return Unsigned_64 is
     (K.Call (K.Wait_One, ABI.Private_Word, Address_Word (Where), Expected, Deadline));

   function Private_Wake (Where : System.Address; Count : Unsigned_64) return Unsigned_64 is
     (K.Call (K.Wake, ABI.Private_Word, Address_Word (Where), Count));

end CuBit.Handles.Notifications;
