with Interfaces; use Interfaces;
package Intel_GPU_ADS_Policies with SPARK_Mode is
   -- Linux v6.16 intel_guc_fwif.h guc_policies, packed little-endian.
   -- Pure encoding: not a live firmware update or ADS readiness certificate.
   type Policy_Bytes is array (Natural range 0 .. 95) of Unsigned_8;
   function Encode (Allow_Engine_Reset : Boolean) return Policy_Bytes
     with Post =>
       (for all I in Natural range 0 .. 63 => Encode'Result (I) = 0) and then
       Encode'Result (64) = 16#20# and then
       Encode'Result (65) = 16#A1# and then
       Encode'Result (66) = 16#07# and then
       Encode'Result (67) = 0 and then
       Encode'Result (68) = 1 and then
       (for all I in Natural range 69 .. 71 => Encode'Result (I) = 0) and then
       Encode'Result (72) = 15 and then
       (for all I in Natural range 73 .. 75 => Encode'Result (I) = 0) and then
       Encode'Result (76) = (if Allow_Engine_Reset then 0 else 1) and then
       (for all I in Natural range 77 .. 95 => Encode'Result (I) = 0);
   -- No default: callers must consciously select recovery policy. True is
   -- appropriate only after golden contexts and recovery machinery are ready;
   -- this serializer cannot establish that hardware/lifetime obligation.
end Intel_GPU_ADS_Policies;
