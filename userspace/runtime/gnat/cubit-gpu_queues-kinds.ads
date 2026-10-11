------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A GPU queue descriptor's Flags (CuBit.GPU_Queues): what the job does, a
--  hint for the driver's per-kind accounting of GPU execution time
--  (docs/gpu-async-submission.md). Any combination of the kind bits; every
--  other bit is reserved, zero, and refused. No kind is untagged.
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.GPU_Queues.Kinds with Pure, SPARK_Mode is
   Kind_Upload   : constant := 16#01#;   --  buffer to image
   Kind_Render   : constant := 16#02#;   --  a render pass
   Kind_Readback : constant := 16#04#;   --  image to buffer
   Kind_Internal : constant := 16#08#;   --  the driver library's own batch
   Kinds_Mask    : constant := 16#0F#;
   subtype Job_Kinds is Unsigned_8 range 0 .. Kinds_Mask;
end CuBit.GPU_Queues.Kinds;
