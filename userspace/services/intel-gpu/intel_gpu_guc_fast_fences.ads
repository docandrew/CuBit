with Interfaces;
package Intel_GPU_GuC_Fast_Fences with SPARK_Mode is
   use Interfaces;
   -- Diagnostic wire IDs only. No ID proves command/GPU completion or
   -- authorizes backing reuse. Synchronous requests must use the low half.
   function Is_Fast (Fence : Unsigned_16) return Boolean is
     (Fence >= 16#8000#);
   type Stream is limited private;
   type Outcome is (Not_Published, Published, Uncertain);
   function Failed (Object : Stream) return Boolean;
   function Pending (Object : Stream) return Boolean;
   procedure Prepare
     (Object : in out Stream; Fence : out Unsigned_16; Accepted : out Boolean)
     with Post => (if Accepted then Is_Fast (Fence) and Pending (Object)
                   else Fence = 0);
   -- Exactly one serialized result per Prepare. Not_Published requires the
   -- sender's guarantee that firmware could not see any part of the message.
   procedure Sent (Object : in out Stream; Result : Outcome);
   -- Caller has validated a firmware HXG failure. Attribution is deliberately
   -- transport-wide: a delayed failure after wrap cannot target a newer job.
   -- Low-half replies belong to the separate synchronous dispatcher.
   procedure Reject_Response (Object : in out Stream; Fence : Unsigned_16)
     with Post => (if Is_Fast (Fence) or Failed (Object)'Old then Failed (Object));
   procedure Fail (Object : in out Stream)
     with Post => Failed (Object);
private
   type Stream is limited record
      Next_ID : Unsigned_16 range 16#8000# .. 16#FFFF# := 16#8000#;
      Sending : Boolean := False;
      Broken : Boolean := False;
   end record;
end Intel_GPU_GuC_Fast_Fences;
