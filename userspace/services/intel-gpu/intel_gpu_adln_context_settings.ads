with Interfaces; use Interfaces;
package Intel_GPU_ADLN_Context_Settings with SPARK_Mode is
   type Segment_Words is array (Natural range 0 .. 13) of Unsigned_32;
   type Segment is record
      Valid : Boolean := False;
      Words : Segment_Words := [others => 0];
   end record;
   -- Six ADL-N render context settings: LRI + six address/value pairs + NOOP.
   -- This is a ring segment, not a standalone batch or engine-start sequence.
   -- Caller supplies an owned/forcewake-held MCR read of WM_CHICKEN2 (0x5584),
   -- and emits required barriers before and after this segment. The FF_MODE2
   -- value is a direct write because CPU readback is documented unreliable.
   -- These settings are separate from the indirect-context workaround batch.
   function Build (Read_Valid : Boolean; WM_Chicken2 : Unsigned_32) return Segment
     with Post => Build'Result.Valid =
       (Read_Valid and WM_Chicken2 /= Unsigned_32'Last) and then
       (if not Build'Result.Valid then
         (for all Word of Build'Result.Words => Word = 0));
end Intel_GPU_ADLN_Context_Settings;
