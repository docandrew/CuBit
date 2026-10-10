package body Native_GPU_Calls is
   use CuBit.Messages;
   Bounded : Boolean := False;
   Limit   : Timeout_Ms := Timeout_Ms'Last;
   Count   : Unsigned_32 := 0;
   Label   : Unsigned_32 := 0;

   procedure Bound_Calls (Milliseconds : Timeout_Ms) is
   begin
      Limit := Milliseconds;
      Bounded := True;
   end Bound_Calls;

   function Call (Slot : CapabilitySlot; Msg : in out Message) return MessageTag is
      Request  : constant Unsigned_32 := Msg.tag.label;
      Returned : MessageTag;
   begin
      Returned := capCall
        (Slot, Msg, (if Bounded then Deadline_After (Limit) else Wait_Forever));
      if Returned.label = REPLY_TIMEOUT then
         if Count < Unsigned_32'Last then Count := Count + 1; end if;
         Label := Request;
      end if;
      return Returned;
   end Call;

   function Bound_Ms return Unsigned_64 is (if Bounded then Limit else 0);
   function Timeouts return Unsigned_32 is (Count);
   function Last_Timed_Out_Label return Unsigned_32 is (Label);
end Native_GPU_Calls;
