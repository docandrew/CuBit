with Interfaces;
package Intel_GPU_PCI_Power with SPARK_Mode is
   use Interfaces;
   type Configuration is array (Natural range 0 .. 255) of Unsigned_8;
   type Power_Status is
     (Unavailable, Malformed, D0, D1, D2, D3_Hot);
   --  For the log ('Image of an enumeration is its position in this runtime).
   function Name (Status : Power_Status) return String is
     (case Status is
         when Unavailable => "unavailable",
         when Malformed   => "malformed",
         when D0          => "D0",
         when D1          => "D1",
         when D2          => "D2",
         when D3_Hot      => "D3hot");
   --  Decode a trusted discovery snapshot. No PCI writes or wakeups. Missing
   --  PM capability is unknown, never inferred to be D0. Type-0 headers only.
   function Decode (Data : Configuration) return Power_Status
     with Global => null;
end Intel_GPU_PCI_Power;
