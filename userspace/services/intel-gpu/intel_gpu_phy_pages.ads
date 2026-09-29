with Interfaces; use Interfaces;
package Intel_GPU_PHY_Pages with SPARK_Mode is
   Request_Label : constant := 16#0234#;
   Virtual_Base : constant Unsigned_64 := 16#61500000#;
   subtype Page_Index is Natural range 0 .. 2;
   -- Three page-granularity grants, NOT per-register isolation. No caller-
   -- selected physical addresses or slots. Distinct from display-power pages.
   function Offset (Index : Page_Index) return Unsigned_64 is
     (case Index is when 0 => 16#64000#, when 1 => 16#162000#, when 2 => 16#6C000#);
   function Slot (Index : Page_Index) return Unsigned_64 is (29 + Unsigned_64 (Index));
   -- Local software allowlist for executor writes. Zero means reject. Lane
   -- read addresses and the process/voltage fuse are never writable here.
   function Write_Address (Register_Offset : Unsigned_32) return Unsigned_64
     with Global => null,
       Post => (Write_Address'Result = 0 or else
         (Write_Address'Result >= Virtual_Base and then
          Write_Address'Result < Virtual_Base + 3 * 4096 and then
          Write_Address'Result mod 4 = 0));
end Intel_GPU_PHY_Pages;
