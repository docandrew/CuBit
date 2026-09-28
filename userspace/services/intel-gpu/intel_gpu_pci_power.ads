with Interfaces;
package Intel_GPU_PCI_Power with SPARK_Mode is
   use Interfaces;
   type Configuration is array (Natural range 0 .. 255) of Unsigned_8;
   type Power_Status is
     (Unavailable, Malformed, D0, D1, D2, D3_Hot);
   --  Decode a trusted discovery snapshot. No PCI writes or wakeups. Missing
   --  PM capability is unknown, never inferred to be D0. Type-0 headers only.
   function Decode (Data : Configuration) return Power_Status
     with Global => null;
end Intel_GPU_PCI_Power;
