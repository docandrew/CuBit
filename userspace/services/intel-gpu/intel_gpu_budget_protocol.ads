with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Backing;
package Intel_GPU_Budget_Protocol with SPARK_Mode is
   package B renames Intel_GPU_Buffer_Backing;
   use type B.Budget_Words;
   Label : constant Unsigned_32 := 16#0A2E#;
   OK : constant Unsigned_64 := 0;
   Bad_Request : constant Unsigned_64 := 1;
   Unavailable : constant Unsigned_64 := 2;
   Busy : constant Unsigned_64 := 4;
   -- Version-one request [1,0,0,0] on an authorized GPU endpoint.
   -- Response [status,total backing bytes,retained bytes,unused tickets].
   -- This observes the shared bootstrap pool, NOT GPU VA size, per-client
   -- quota, reclaimable memory or a reservation. Allocation can still fail.
   -- No physical/virtual address or render authority is disclosed.
   function Valid_Request
     (Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Data : B.Budget_Words) return Boolean is
     (Request_Label = Label and then Length = 4 and then Flags = 0 and then
      Reserved = 0 and then Data = B.Budget_Words'(1, 0, 0, 0));
   function Response (Value : B.Budget_Snapshot) return B.Budget_Words is
     (if Value.Known and then B.Budget_Valid
        (Value.Total_Bytes, Value.Retained_Bytes, Unsigned_64 (Value.Unused_Slots))
      then [OK, Value.Total_Bytes, Value.Retained_Bytes, Unsigned_64 (Value.Unused_Slots)]
      else [Unavailable, 0, 0, 0]);
end Intel_GPU_Budget_Protocol;
