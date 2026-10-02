with Interfaces; use Interfaces;
with Intel_GPU_Resources;
package Intel_GPU_Boot with SPARK_Mode is
   Configure_Label : constant := 16#4947#;
   Protocol_Version : constant Unsigned_64 := 4;
   -- Kernel-stamped startup broker authority, distinct from render-session
   -- and read-only probe tags. Only the trusted supervisor retains GRANT.
   Broker_Tag : constant Unsigned_64 := 16#4750_4252_4F4B_0001#;
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   -- w0 BAR0 low/high; w1 vendor/device/revision/class/header/reserved;
   -- w2 command register + bit16 trusted D0 evidence (required), upper bits
   -- zero; w3 bits15:0 version,31:16 PCI GGC,38:32 IRQ snapshot (Pack),
   -- remaining bits zero. IRQ snapshot is observation only, not a grant.
   -- No compatibility with the undeployed v1/v2/v3 protocols.
   -- Sender authentication and capability ownership belong to the adapter.
   function Decode (Data : Words) return Intel_GPU_Resources.Mapping_Plan
   with Global => null;
end Intel_GPU_Boot;
