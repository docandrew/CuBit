with Interfaces; use Interfaces;
package Intel_GPU_GuC_Context_Event with SPARK_Mode is
   type Words is array (Natural range <>) of Unsigned_32;
   type Kind is (Malformed, Other_Message, Request_Failure, Scheduling_Done,
                 Deregister_Done);
   type Event is record
      Tag : Kind := Malformed;
      Fence : Unsigned_16 := 0;
      ID, Runnable, Error_Code, Hint : Unsigned_32 := 0;
   end record;
   -- Input is an owned CT payload copy, WITHOUT CT header, whose length and
   -- transport framing have already been checked. Fence is from that header.
   -- Other_Message must go to the channel dispatcher, never silently dropped.
   -- Decoding does not authenticate firmware or establish pending ownership.
   function Decode (Payload : Words; Fence : Unsigned_16) return Event
     with Post => (if Decode'Result.Tag in Scheduling_Done | Deregister_Done then
       Decode'Result.ID < 65535 and Decode'Result.Runnable <= 1);
end Intel_GPU_GuC_Context_Event;
