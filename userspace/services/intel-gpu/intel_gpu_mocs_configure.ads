with Interfaces;
generic
   -- Exclusive post-reset/quiesced ADL-N GT, retained forcewake and bounded
   -- UC control mappings. Call before GuC publication/startup, never live.
   with function Owner_Ready return Boolean;
   with function Read32 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   with procedure Write32 (Offset, Value : Interfaces.Unsigned_32; Success : out Boolean);
package Intel_GPU_MOCS_Configure is
   type Attempt is limited private;
   type Result is (Rejected, Ownership_Lost, Read_Failed, Write_Failed, Readback_Failed, Ready);
   -- Explicit strings survive the native runtime's Discard_Names setting.
   function Result_Name (Value : Result) return String is
     (case Value is
        when Rejected => "REJECTED",
        when Ownership_Lost => "OWNERSHIP-LOST",
        when Read_Failed => "READ-FAILED",
        when Write_Failed => "WRITE-FAILED",
        when Readback_Failed => "READBACK-FAILED",
        when Ready => "READY");
   procedure Configure (Object : in out Attempt; Status : out Result);
   function Last_Index (Object : Attempt) return Natural;
   function Last_Raw (Object : Attempt) return Interfaces.Unsigned_32;
private
   type Attempt is limited record
      Started : Boolean := False;
      Index : Natural range 0 .. 95 := 0;
      Raw : Interfaces.Unsigned_32 := 0;
   end record;
end Intel_GPU_MOCS_Configure;
