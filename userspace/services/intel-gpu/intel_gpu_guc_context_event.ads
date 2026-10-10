with Interfaces; use Interfaces;
package Intel_GPU_GuC_Context_Event with SPARK_Mode is
   type Words is array (Natural range <>) of Unsigned_32;
   -- Context_Reset, Engine_Failure and GuC_Failure mean device loss until
   -- reset recovery exists (docs/gpu-async-submission.md H5, H9).
   -- Notification is informational (state capture, log flush, TLB done).
   type Kind is (Malformed, Other_Message, Request_Failure, Scheduling_Done,
                 Deregister_Done, Context_Reset, Engine_Failure, GuC_Failure,
                 Notification);
   subtype Device_Loss is Kind range Context_Reset .. GuC_Failure;
   type Event is record
      Tag : Kind := Malformed;
      Fence : Unsigned_16 := 0;
      ID, Runnable, Error_Code, Hint : Unsigned_32 := 0;
      -- Engine_Failure: engine class, instance and the GuC's reason.
      Engine_Class, Engine_Instance, Reason : Unsigned_32 := 0;
      -- GuC_Failure and Notification: the HXG action code.
      Action : Unsigned_32 := 0;
   end record;
   -- Input is an owned CT payload copy, WITHOUT CT header, whose length and
   -- transport framing have already been checked. Fence is from that header.
   -- Other_Message, Device_Loss and Notification events go to the channel
   -- dispatcher, which retains them for the service loop; never dropped.
   -- Decoding does not authenticate firmware or establish pending ownership.
   function Decode (Payload : Words; Fence : Unsigned_16) return Event
     with Post => (if Decode'Result.Tag in Scheduling_Done | Deregister_Done | Context_Reset then
       Decode'Result.ID < 65535 and Decode'Result.Runnable <= 1);
end Intel_GPU_GuC_Context_Event;
