with Interfaces; use Interfaces;

-- Read-only completed boot-probe export. This is not render admission.
package Native_GPU_Probe_Protocol with Pure, SPARK_Mode is
   Label : constant Unsigned_32 := 16#0A29#;
   Desktop_Binding_Label : constant Unsigned_32 := 16#0A2B#;
   Viewer_Tag : constant Unsigned_64 := 16#4750_5631#;
   Driver_Recipient_Slot : constant := 56;
   Supervisor_Recipient_Slot : constant := 57;
   Viewer_Driver_Slot : constant := 4;
   Viewer_Supervisor_Slot : constant := 15;
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
end Native_GPU_Probe_Protocol;
