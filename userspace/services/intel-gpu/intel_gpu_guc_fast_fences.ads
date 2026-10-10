with Interfaces;
package Intel_GPU_GuC_Fast_Fences with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use Interfaces;
   -- Diagnostic wire IDs only. No ID proves command/GPU completion or
   -- authorizes backing reuse. Synchronous requests must use the low half.
   function Is_Fast (Fence : Unsigned_16) return Boolean is
     (Fence >= 16#8000#);
   -- FAST requests carry no success response (guc_messages_abi.h), so an ID
   -- is retired as soon as its publication outcome is known; the window
   -- 8000..FFFF is a recycled sequence, never a lifetime budget. Linux
   -- intel_guc_ct.c ct_get_next_fence likewise wraps one transport counter.
   subtype Fast_Fence is Unsigned_16 range 16#8000# .. 16#FFFF#;
   type Request_Count is mod 2 ** 64; -- diagnostic total, wraps harmlessly
   type Stream is limited private;
   type Outcome is (Not_Published, Published, Uncertain);
   function Failed (Object : Stream) return Boolean;
   function Pending (Object : Stream) return Boolean;
   -- Next diagnostic ID and total published FAST requests (log evidence).
   function Next_Fence (Object : Stream) return Fast_Fence;
   function Published_Count (Object : Stream) return Request_Count;
   -- Never exhausted: an unbroken stream with no publication in progress
   -- always issues an ID, regardless of how many requests preceded it.
   procedure Prepare
     (Object : in out Stream; Fence : out Unsigned_16; Accepted : out Boolean)
     with Post => (if Accepted then Is_Fast (Fence) and Pending (Object)
                   else Fence = 0) and then
                  (if not Failed (Object)'Old and not Pending (Object)'Old
                   then Accepted) and then
                  Published_Count (Object) = Published_Count (Object)'Old;
   -- Exactly one serialized result per Prepare. Not_Published requires the
   -- sender's guarantee that firmware could not see any part of the message.
   -- A known outcome other than Uncertain returns the stream to service.
   procedure Sent (Object : in out Stream; Result : Outcome)
     with Post =>
       (if Pending (Object)'Old and not Failed (Object)'Old and
           Result /= Uncertain
        then not Failed (Object) and not Pending (Object)) and then
       (if Failed (Object)'Old then Failed (Object));
   -- Caller has validated a firmware HXG failure. Attribution is deliberately
   -- transport-wide: a delayed failure after wrap cannot target a newer job.
   -- Low-half replies belong to the separate synchronous dispatcher.
   procedure Reject_Response (Object : in out Stream; Fence : Unsigned_16)
     with Post => (if Is_Fast (Fence) or Failed (Object)'Old then Failed (Object));
   procedure Fail (Object : in out Stream)
     with Post => Failed (Object);
private
   type Stream is limited record
      Next_ID : Fast_Fence := Fast_Fence'First;
      Published : Request_Count := 0;
      Sending : Boolean := False;
      Broken : Boolean := False;
   end record;
end Intel_GPU_GuC_Fast_Fences;
