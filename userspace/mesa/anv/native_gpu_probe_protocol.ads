with Interfaces; use Interfaces;

-- Read-only completed boot-probe export. This is not render admission.
package Native_GPU_Probe_Protocol with Pure, SPARK_Mode is
   Label : constant Unsigned_32 := 16#0A29#;
   Desktop_Binding_Label : constant Unsigned_32 := 16#0A2B#;
   Viewer_Tag : constant Unsigned_64 := 16#4750_5631#;
   Pixel_Bytes : constant Unsigned_64 := 16_384;
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   type Operation is (Read_Target, Retire_Target);
   type Status is (Success, Denied, Unavailable, Invalid_Request, Pending);
   for Status use (Success => 0, Denied => 1, Unavailable => 2,
                   Invalid_Request => 3, Pending => 4);

   function Valid_Request (Value : Words) return Boolean;
   function Valid_Reply (Action : Operation; Value : Words) return Boolean;
   function Request (Action : Operation; Reference : Unsigned_64 := 0)
     return Words with
     Post => Valid_Request (Request'Result) =
       (if Action = Read_Target then Reference = 0 else Reference /= 0);
   function Reply (Code : Status; Reference : Unsigned_64 := 0) return Words
     with Post =>
       (if Code = Success then
          Valid_Reply ((if Reference = 0 then Retire_Target else Read_Target), Reply'Result)
        else Valid_Reply (Retire_Target, Reply'Result) and then
          Valid_Reply (Read_Target, Reply'Result) = (Code /= Pending));
   function Valid_Desktop_Request (Value : Words) return Boolean;
   function Valid_Desktop_Reply (Value : Words) return Boolean;
   -- Reference validity is additionally checked by the grant codec. Zero
   -- success reference denotes retirement, never a successful read.

   -- Why the viewer ended: its exit code, which devmgr names in its
   -- "child gpu-viewer.app ... exited code N (<name>)" line (logstore).
   -- Before these, the viewer set no code and the start code exited with
   -- whatever main left in RAX (IdeaPad 46D4, 2026-10-24: "exited code 41").
   -- Codes stay clear of the runtime's startup refusal (78).
   type Viewer_Outcome is
     (Closed, Binding_Refused, Binding_Timed_Out, Binding_Malformed,
      Desktop_Unavailable, Target_Unavailable, Acquire_Failed,
      Surface_Failed, Forward_Failed, Return_Failed, Attach_Failed,
      Present_Failed);
   for Viewer_Outcome use
     (Closed => 0, Binding_Refused => 16, Binding_Timed_Out => 17,
      Binding_Malformed => 18, Desktop_Unavailable => 19,
      Target_Unavailable => 20, Acquire_Failed => 21, Surface_Failed => 22,
      Forward_Failed => 23, Return_Failed => 24, Attach_Failed => 25,
      Present_Failed => 26);

   function Outcome_Name (Outcome : Viewer_Outcome) return String is
     (case Outcome is
         when Closed              => "closed",
         when Binding_Refused     => "desktop binding refused by devmgr",
         when Binding_Timed_Out   => "no desktop within the binding deadline",
         when Binding_Malformed   => "desktop binding reply malformed",
         when Desktop_Unavailable => "desktop hello failed",
         when Target_Unavailable  => "intel-gpu has no completed probe target",
         when Acquire_Failed      => "probe pixel grant not acquired",
         when Surface_Failed      => "desktop surface not created",
         when Forward_Failed      => "probe grant not forwarded",
         when Return_Failed       => "probe grant not returned",
         when Attach_Failed       => "surface attach failed",
         when Present_Failed      => "present failed");

   -- The name of an exit code, or "unknown" for codes the viewer never sets.
   function Exit_Name (Code : Unsigned_64) return String;
end Native_GPU_Probe_Protocol;
