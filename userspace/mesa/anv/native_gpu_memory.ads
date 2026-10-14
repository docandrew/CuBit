with Interfaces; use Interfaces;
with Native_GPU_Calls;

package Native_GPU_Memory is
   --  The endpoint handle must remain open for the call. Reference is the
   --  canonical CuBit.Grant_References wire identity, not a GPU address.
   --  Success (0) acquires one borrow; Output remains owned by the caller.
   function Acquire
     (Endpoint : Native_GPU_Calls.C_Endpoint; Reference, Offset, Bytes, Writable : Unsigned_64;
      Output : access Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_acquire_view";

   --  Return exactly one successfully acquired borrow. This is NOT unmap,
   --  grant revocation, GPU completion, or permission to reuse GPU backing.
   function Return_Borrow (Reference : Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_return_view";
end Native_GPU_Memory;
