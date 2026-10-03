with Interfaces; use Interfaces;
generic
   -- CPU-owned metadata only. These operations must not allocate GPU BOs or
   -- recursively consume a record from the registry being grown.
   with function Reserve (Bytes : Unsigned_64) return Unsigned_64;
   -- Success must establish writable backing for the entire requested span.
   with function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean;
   -- Success must initialize the entire new span before any records use it.
   -- A callback must report failure explicitly, not propagate an exception.
   with function Initialize (Address, Bytes : Unsigned_64) return Boolean;
package Intel_GPU_Metadata_Arena is
   Page_Bytes : constant Unsigned_64 := 4096;
   -- At most this much allocation/initialization work per Step. The caller
   -- yields to IPC between steps; this is not an unbounded grow-to-fit loop.
   Step_Bytes : constant Unsigned_64 := 65536;
   type State is (Empty, Reserved, Growing, Ready, Failed);
   type View is record
      Phase : State := Empty;
      Base, Limit, Committed, Published, Wanted : Unsigned_64 := 0;
   end record;
   type Arena is limited private;
   function Snapshot (Object : Arena) return View;
   -- Limit is an explicit metadata-byte quota, not GPU RAM/VRAM or GPU VA.
   -- One reservation per arena lifetime; no exception-driven allocation.
   procedure Open (Object : in out Arena; Limit : Unsigned_64; Accepted : out Boolean);
   -- Invalid/busy requests leave all state unchanged. Smaller requests never
   -- shrink committed storage or invalidate previously published records.
   procedure Request (Object : in out Arena; Bytes : Unsigned_64; Accepted : out Boolean);
   procedure Step (Object : in out Arena);
   -- Address is returned only for fully published bytes, never merely reserved
   -- or partially initialized storage. The registry owns record typing.
   function Address (Object : Arena; Offset, Bytes : Unsigned_64) return Unsigned_64;
   -- No implicit freeing: users must retire every borrowed reference before
   -- adding any explicit arena release operation. Failure retains reservation.
private
   type Arena is limited record
      Data : View;
   end record;
end Intel_GPU_Metadata_Arena;
