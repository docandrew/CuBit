with Interfaces; use Interfaces;
-- Metadata only. Serialize all accesses with one registry lock, including
-- different owners: physical backing is exclusive across this registry.
-- Owner is an authenticated address-space lifetime identity, NEVER a bare PID.
-- Physical frame ownership and mapping operations are separate obligations.
generic
   Capacity : Positive;
   First_Address, Limit_Address : Unsigned_64;
package Region_Registry with SPARK_Mode => On is
   subtype Slot is Positive range 1 .. Capacity;
   type Handle is record
      Index : Slot := Slot'First;
      Generation : Unsigned_64 := 0;
   end record;
   type Phase is (Absent, Reserved, Live, Retiring);
   type Registry is limited private;
   function Exclusive (Object : Registry) return Boolean with Ghost;
   type Description is record
      Status : Phase := Absent;
      Base, Bytes : Unsigned_64 := 0;
      Physical_Base : Unsigned_64 := 0;
   end record;
   function State (Object : Registry; Owner : Unsigned_64; Key : Handle) return Phase;
   function Describe (Object : Registry; Owner : Unsigned_64; Key : Handle)
     return Description
   with Post => (if Describe'Result.Status = Absent then
     Describe'Result.Base = 0 and Describe'Result.Bytes = 0 and
     Describe'Result.Physical_Base = 0);
   -- Query any nonempty, nonwrapping byte range, not only allocation-aligned
   -- ranges inside the aperture. Malformed queries fail closed (return True).
   function Overlaps (Object : Registry; Owner, Base, Bytes : Unsigned_64) return Boolean;
   procedure Reserve (Object : in out Registry; Owner, Base, Bytes : Unsigned_64;
                      Key : out Handle; Success : out Boolean)
     with Pre => Exclusive (Object), Post => Exclusive (Object);
   -- Initial backing form is one contiguous extent of exactly region Bytes.
   -- Caller must own freshly allocated RAM, never MMIO or an existing alias.
   -- Registration is one-shot while Reserved and does not itself allocate.
   procedure Bind_Backing (Object : in out Registry; Owner : Unsigned_64;
                          Key : Handle; Physical_Base : Unsigned_64;
                          Success : out Boolean)
     with Pre => Exclusive (Object), Post => Exclusive (Object);
   function Physical_Overlap (Object : Registry; Base, Bytes : Unsigned_64)
     return Boolean;
   -- Commit only after allocation/mapping succeeded; reserved ranges already
   -- block overlap. Partial allocation failure must retire, not drop metadata.
   procedure Commit (Object : in out Registry; Owner : Unsigned_64; Key : Handle;
                     Success : out Boolean)
     with Pre => Exclusive (Object), Post => Exclusive (Object);
   procedure Begin_Retirement (Object : in out Registry; Owner : Unsigned_64;
                               Key : Handle; Success : out Boolean)
     with Pre => Exclusive (Object), Post => Exclusive (Object);
   -- Call only after unmap + acknowledged shootdown + frame retirement.
   -- Metadata cannot establish that these physical operations occurred.
   procedure Finish_Retirement (Object : in out Registry; Owner : Unsigned_64;
                                Key : Handle; Success : out Boolean)
     with Pre => Exclusive (Object), Post => Exclusive (Object);
private
   type Entry_Record is record
      Owner, Base, Bytes : Unsigned_64 := 0;
      Physical_Base : Unsigned_64 := 0;
      Generation : Unsigned_64 := 1;
      Status : Phase := Absent;
      Exhausted : Boolean := False;
   end record;
   type Entries is array (Slot) of Entry_Record;
   type Registry is limited record
      Items : Entries;
   end record;
end Region_Registry;
