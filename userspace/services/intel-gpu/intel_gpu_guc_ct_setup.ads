with Interfaces;
package Intel_GPU_GuC_CT_Setup with SPARK_Mode is
   use Interfaces;
   -- One retained GPU mapping: two descriptor pages,4KiB H2G,16KiB G2H.
   -- Zero/flush it before registration. Numeric validity is NOT ownership.
   Required_Bytes : constant Unsigned_64 := 28_672;
   type Words is array (Natural range 0 .. 3) of Unsigned_32;
   type Request is record
      Length : Natural range 0 .. 4 := 0;
      Data : Words := [others => 0];
   end record;
   type Requests is array (Positive range 1 .. 6) of Request;
   type Plan is record
      Valid : Boolean := False;
      Register_Buffers : Requests;
      Enable : Request;
   end record;
   function Prepare (GPU_Start, Backing_Bytes, Pin_Bias : Unsigned_64) return Plan;
   -- Each SELF_CFG needs success DATA0=1 (key recognized); enable needs0.
   -- These are complete single-word HXG responses, not just payload fields.
   function Registered_Response (Header : Unsigned_32) return Boolean is
     (Header = 16#F0000001#);
   function Enabled_Response (Header : Unsigned_32) return Boolean is
     (Header = 16#F0000000#);
end Intel_GPU_GuC_CT_Setup;
