with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Pipe_Control;
with Intel_GPU_Submission_Backing;
package Intel_GPU_ADLN_Stencil_Sync with SPARK_Mode is
   -- TGL Vol2d101: A-step surface-state change needs post-sync write.
   -- Emit conservatively for initial null-stencil setup. Vol2a1126/1130
   -- defines Write Immediate as QWORD despite the workaround's DWORD wording.
   -- Dedicated private completion-page scratch bytes8..15, NOT final marker.
   Scratch_Offset : constant Unsigned_64 := 8;
   Scratch_VA : constant Unsigned_64 :=
     Intel_GPU_Submission_Backing.Completion_GPU_VA + Scratch_Offset;
   type B3 is mod 2 ** 3 with Size => 3;
   type B61 is mod 2 ** 61 with Size => 61;
   type Qword_Address is record
      Reserved_0 : B3 := 0;
      Address_Units : B61 := 0;
   end record with Size => 64, Bit_Order => System.Low_Order_First;
   for Qword_Address use record
      Reserved_0 at 0 range 0 .. 2;
      Address_Units at 0 range 3 .. 63;
   end record;
   function Encode (V : Qword_Address) return Unsigned_64 is
     (Unsigned_64 (V.Reserved_0) or Shift_Left (Unsigned_64 (V.Address_Units), 3));
   Address : constant Unsigned_64 := Encode
     (Qword_Address'(Address_Units => B61 (Scratch_VA / 8), others => <>));
   package PC renames Intel_GPU_ADLN_Pipe_Control;
   Packet : constant PC.Packet :=
     [PC.Encode (PC.Pipe_Header'(others => <>)),
      PC.Encode (PC.Pipe_Flags'(Post_Sync => 1, CS_Stall => 1,
        Render_Target_Flush => 1, Scoreboard_Stall => 1, others => <>)),
      Unsigned_32 (Address and 16#FFFFFFFF#), Unsigned_32 (Shift_Right (Address, 32)),
      0, 0];
   -- PPGTT, not GGTT; no Store_Index or LRI operation. Includes RT flush
   -- and scoreboard stall for prior binding-table association changes.
   pragma Compile_Time_Error
     (Scratch_VA mod 8 /= 0 or Scratch_Offset < 8 or
      Scratch_Offset + 8 > Intel_GPU_Submission_Backing.Sizes
        (Intel_GPU_Submission_Backing.Completion_Page),
      "stencil synchronization write escapes private scratch");
end Intel_GPU_ADLN_Stencil_Sync;
