pragma Ada_2022;
with CuBit.Process_IDs;
with GPIO_Control_Wire;
generic
   type Reply_Authority is private;
   with function Authorized (Caller : CuBit.Process_IDs.Process_ID;
      Badge : GPIO_Control_Wire.Word) return Boolean;
   with procedure Save (Reply : out Reply_Authority; Saved : out Boolean);
   with procedure Answer_Saved (Reply : Reply_Authority;
      Label : GPIO_Control_Wire.Operation; Value : GPIO_Control_Wire.Completion;
      Delivered : out Boolean);
   with procedure Answer_Current (Caller : CuBit.Process_IDs.Process_ID;
      Label : GPIO_Control_Wire.Operation; Value : GPIO_Control_Wire.Completion;
      Delivered : out Boolean);
   with procedure Offer (Caller : CuBit.Process_IDs.Process_ID;
      Badge : GPIO_Control_Wire.Word; Item : GPIO_Control_Wire.Request;
      Reply : Reply_Authority; Taken : out Boolean);
package GPIO_Request_Dispatch with SPARK_Mode is
   package W renames GPIO_Control_Wire;
   package P renames CuBit.Process_IDs;
   type Outcome is (Control_Retained, Rejected_Delivered, Rejected_Lost);
   type Report is record
      Result : Outcome;
      Status : W.Completion_Status;
      -- Status has meaning only for a rejection. Retained means Pending local
      -- work, never a remote Completed control response.
   end record;
   procedure Handle (Caller : P.Process_ID; Badge : W.Word;
      Label : W.Operation; Protocol : W.Word; Words : W.Message_Words;
      Frame_Valid : Boolean; Result : out Report);
   -- Only authentic service-request inbox entries may enter here. Caller/badge
   -- are kernel metadata, never words. Authorized must match an existing bound
   -- channel/opaque owner; badge geometry alone is insufficient authority.
   -- Save is required before Offer, including for asynchronous requests. If it
   -- fails (one-way requests included), no hardware work is admitted; the
   -- runtime retains current reply authority for one immediate Busy/rejection.
   -- True Taken transfers the saved reply to the channel; False leaves it here
   -- for a definite Busy reply. False Offer guarantees no admitted operation.
   -- Answer_Saved consumes its authority even if delivery fails. A current
   -- reply is attempted once without retaining any authority or destination
   -- here; failure does not establish delivery or kernel-side consumption.
   -- Single serialized owner; all callbacks bounded/nonraising/nonreentrant.
end GPIO_Request_Dispatch;
