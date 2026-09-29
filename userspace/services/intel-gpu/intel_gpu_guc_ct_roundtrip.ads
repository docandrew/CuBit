with Interfaces;
with Intel_GPU_GuC_CT_Receive;
generic
   with package Receiver is new Intel_GPU_GuC_CT_Receive (<>);
   with function Owner_Ready return Boolean;
   -- One serialized request with a one-DWORD, zero-DATA0 success response.
   -- Queue must reserve response capacity and must not reuse the fence.
   with procedure Queue (Fence : Interfaces.Unsigned_16; Success : out Boolean);
   with procedure Poll (Output : out Receiver.Message; Status : out Receiver.Result);
   -- Retain the complete owned event, without interpreting untrusted payloads.
   with procedure Retain_Event (Item : Receiver.Message; Success : out Boolean);
   with function Now_Us return Interfaces.Unsigned_64;
   with procedure Pause;
package Intel_GPU_GuC_CT_Roundtrip is
   type Attempt is limited private;
   type Result is (Rejected, Ownership_Lost, Invalid_Clock, Queue_Failed,
                   Receive_Failed, Invalid_Reply, Event_Overflow,
                   Retry_Requested, Firmware_Failed, Timed_Out, Complete);
   -- One attempt only, including timeout/failure. Caller retains all backing
   -- and quarantines the channel on failure; a late response is not reusable.
   -- Callbacks must be bounded, nonraising, nonreentrant. Clock uses Last as
   -- invalid sentinel. BUSY does not extend the fixed one-second deadline.
   procedure Execute
     (Object : in out Attempt; Fence : Interfaces.Unsigned_16;
      Poll_Limit : Positive; Reply : out Interfaces.Unsigned_32;
      Status : out Result);
private
   type Attempt is limited record
      Started : Boolean := False;
   end record;
end Intel_GPU_GuC_CT_Roundtrip;
