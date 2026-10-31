pragma Ada_2022;
with CuBit.Control_Events;
with CuBit.Process_IDs;
package HID_Log_Lifetime with Pure, SPARK_Mode is
   function Related (Logstore : CuBit.Process_IDs.Process_ID;
      Item : CuBit.Control_Events.Event) return Boolean;
   -- Called only after HID_System_Events authenticates a kernel grant notice.
   -- Match the entire bound logstore lifetime. This conservative filter may
   -- close logging for another grant involving that same peer, but unrelated
   -- transfer grants do not disable logging. No grant retirement is inferred.
end HID_Log_Lifetime;
