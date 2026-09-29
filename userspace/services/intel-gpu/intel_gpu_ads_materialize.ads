with Interfaces; use Interfaces;
with Intel_GPU_ADS_Initialization;
package Intel_GPU_ADS_Materialize with SPARK_Mode is
   type Bytes is array (Natural range <>) of Unsigned_8;
   procedure Write
     (Image : Intel_GPU_ADS_Initialization.Prepared_Image;
      Buffer : in out Bytes; Success : out Boolean)
     with Post => (if not Success then Buffer = Buffer'Old);
   -- Private CPU backing only. Checks all extents before touching Buffer,
   -- zeros the entire supplied allocation (including padding), then writes
   -- the five initialized sections. Does not map, flush or publish memory.
   -- Caller must supply an authentic Prepare result and exclusive writable
   -- backing; checks here establish copy bounds, not authority or pointer trust.
end Intel_GPU_ADS_Materialize;
