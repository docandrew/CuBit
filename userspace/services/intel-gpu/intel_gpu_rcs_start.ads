with Interfaces;
generic
   -- Exclusive, reset ADL-N RCS; forcewake, MOCS and engine workarounds
   -- established. No submissions until this attempt completes.
   with function Owner_Ready return Boolean;
   -- Certifies a separately owned, zeroed, cache-flushed engine HWSP page
   -- published in GGTT, NOT the per-context HWSP or a caller-supplied address.
   with function Status_Page_Ready (GPU : Interfaces.Unsigned_64) return Boolean;
   with procedure Write32
     (Offset, Value : Interfaces.Unsigned_32; Success : out Boolean);
   with function Read32 (Offset : Interfaces.Unsigned_32)
     return Interfaces.Unsigned_32;
package Intel_GPU_RCS_Start is
   type Result is (Rejected, Ownership_Lost, Write_Failed, Readback_Failed, Ready);
   type Attempt is limited private;
   type Rejection_Reason is (None, Already_Attempted, Zero_Address,
                            Unaligned_Address, Outside_Runtime_Range);
   function Rejection (Object : Attempt) return Rejection_Reason;
   procedure Start
     (Object : in out Attempt; Status_GPU : Interfaces.Unsigned_64;
      Status : out Result);
private
   type Attempt is limited record
      Started : Boolean := False;
      Reason : Rejection_Reason := None;
   end record;
end Intel_GPU_RCS_Start;
