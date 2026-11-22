with Interfaces;
generic
   -- Trusted local lifecycle/mapping state. Never supplied by application IPC.
   with function Owner_Ready return Boolean;
   with function Mapping_Bytes return Interfaces.Unsigned_64;
   -- Exact retained allocation/PTE binding, including scanout exclusion.
   -- No reentrancy or state mutation; the owner serializes the entire publish.
   with function Write_Allowed (Index, Value : Interfaces.Unsigned_64) return Boolean;
package Intel_GPU_Native_GGTT is
   procedure Read_PTE
     (Index : Interfaces.Unsigned_64; Value : out Interfaces.Unsigned_64;
      Success : out Boolean);
   procedure Write_PTE
     (Index, Value : Interfaces.Unsigned_64; Success : out Boolean);
   -- UC/NX mapping at GGTT_Mapping.Virtual_Base must be retained throughout.
   -- Only exact owned replacement; no bulk clearing, release, allocation or
   -- invalidation here. Replay prevention is enforced by the publisher/ledger.
   -- A failed write invocation never permits reclaiming the backing.
end Intel_GPU_Native_GGTT;
