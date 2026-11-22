with System;
with Interfaces;
-- Trusted, single-threaded integration fixture; not a client import ABI.
-- All lifecycle decisions are delegated to the production SPARK components.
package Native_Scene_Bridge with SPARK_Mode is
   subtype U32 is Interfaces.Unsigned_32;
   -- 0 success; 1 pending/deferred; 2 invalid call/clean rejection;
   -- 3 uncertain: retain everything, never reset/retry the context.
   procedure Open
     (Description, A, B, C, Submission, Source : System.Address;
      Allowed, Width, Height : U32; Result : out U32)
     with Export, Convention => C, External_Name => "cubit_native_scene_open";
   -- Starts recording, returns selected output slot 1..3. Caller may now
   -- record source/target preparation barriers on the borrowed command buffer.
   procedure Begin_Frame (Slot, Result : out U32)
     with Export, Convention => C, External_Name => "cubit_native_scene_begin";
   -- Captures full-size source plus green physical rectangle [4,12)x[4,12).
   procedure Record_Frame (Result : out U32)
     with Export, Convention => C, External_Name => "cubit_native_scene_record";
   -- Caller records final barriers/readback AFTER Record and BEFORE Submit.
   procedure Submit_Frame (Result : out U32)
     with Export, Convention => C, External_Name => "cubit_native_scene_submit";
   procedure Poll_Frame (Result : out U32)
     with Export, Convention => C, External_Name => "cubit_native_scene_poll";
   procedure Cancel_Frame (Result : out U32)
     with Export, Convention => C, External_Name => "cubit_native_scene_cancel";
   -- Only after Poll=0 and the readback consumer has returned. No scanout claim.
   procedure Release_Frame (Result : out U32)
     with Export, Convention => C, External_Name => "cubit_native_scene_release";
   procedure Close (Result : out U32)
     with Export, Convention => C, External_Name => "cubit_native_scene_close";
end Native_Scene_Bridge;
