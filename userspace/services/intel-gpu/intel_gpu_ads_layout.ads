with Interfaces; use Interfaces;
package Intel_GPU_ADS_Layout with SPARK_Mode is
   -- Pinned GuC ABI (Linux v6.16 intel_guc_fwif.h), packed little-endian.
   -- This lays out storage; it does not initialize firmware-consumable ADS.
   type Section is (Header, Policies, System_Info, Usage, Registers,
                    Golden_Contexts, Workarounds, Capture, Private_Data);
   type Extents is array (Section) of Unsigned_64;
   type Layout is record
      Valid : Boolean := False;
      Offset, Bytes : Extents := [others => 0];
      Total : Unsigned_64 := 0;
   end record;
   Limit : constant Unsigned_64 := 16#FEE0_0000#;
   function Sound (Value : Layout) return Boolean is
     (Value.Total > 0 and then Value.Total <= Limit and then
      Value.Total mod 4096 = 0 and then
      (for all S in Section =>
         Value.Offset (S) <= Value.Total and then
         Value.Bytes (S) <= Value.Total - Value.Offset (S)) and then
      (for all S in Section range Header .. Capture =>
         Value.Offset (S) + Value.Bytes (S) <= Value.Offset (Section'Succ (S))) and then
      (for all S in Section range Golden_Contexts .. Private_Data =>
         Value.Offset (S) mod 4096 = 0));
   -- Lengths supplied after engine/firmware admission; private size comes from
   -- CSS byte120, not file length. Zero-length optional sections are allowed.
   -- Backing is the actual retained allocation capacity, not a requested size.
   -- GPU base+Total, ownership and coherency need separate validation.
   function Plan (Register_Bytes, Golden_Bytes, Workaround_Bytes,
                  Capture_Bytes, Private_Bytes, Backing : Unsigned_64) return Layout
     with Post => (if Plan'Result.Valid then
                      Sound (Plan'Result) and then Plan'Result.Total <= Backing);
end Intel_GPU_ADS_Layout;
