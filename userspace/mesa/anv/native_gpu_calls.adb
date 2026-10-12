package body Native_GPU_Calls is
   use CuBit.Messages;
   Bounded : Boolean := False;
   Limit   : Timeout_Ms := Timeout_Ms'Last;
   Count   : Unsigned_32 := 0;
   Label   : Unsigned_32 := 0;
   --  Set by the first call or wait the bound ended: the service is taken to
   --  be stalled, and later calls fail at once instead of each waiting out
   --  the bound again (a stalled driver costs one bound in total).
   Stalled : Boolean := False;

   procedure Bound_Calls (Milliseconds : Timeout_Ms) is
   begin
      Limit := Milliseconds;
      Bounded := True;
   end Bound_Calls;

   function Call (Slot : CapabilitySlot; Msg : in out Message) return MessageTag is
      Request  : constant Unsigned_32 := Msg.tag.label;
      Returned : MessageTag;
   begin
      if Stalled then
         if Count < Unsigned_32'Last then Count := Count + 1; end if;
         Label := Request;
         return (label => REPLY_TIMEOUT, length => 0, flags => 0, reserved => 0);
      end if;
      Returned := capCall
        (Slot, Msg, (if Bounded then Deadline_After (Limit) else Wait_Forever));
      if Returned.label = REPLY_TIMEOUT then
         if Count < Unsigned_32'Last then Count := Count + 1; end if;
         Label := Request;
         Stalled := Bounded;
      end if;
      return Returned;
   end Call;

   function Bound_Ms return Unsigned_64 is (if Bounded then Limit else 0);
   procedure Note_Timeout (Label : Unsigned_32) is
   begin
      if Count < Unsigned_32'Last then Count := Count + 1; end if;
      Native_GPU_Calls.Label := Label;
      Stalled := Bounded;
   end Note_Timeout;
   function Call_Deadline return Unsigned_64 is
     (if Stalled then Deadline_After (0)
      elsif Bounded then Deadline_After (Limit) else Wait_Forever);
   function Service_Stalled return Boolean is (Stalled);
   function Timeouts return Unsigned_32 is (Count);
   function Last_Timed_Out_Label return Unsigned_32 is (Label);
end Native_GPU_Calls;
