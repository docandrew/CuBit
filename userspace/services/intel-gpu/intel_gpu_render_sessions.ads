with Interfaces; use Interfaces;
package Intel_GPU_Render_Sessions with SPARK_Mode is
   -- One serialized registry for one GPU service endpoint incarnation.
   -- Only trusted admission code reserves/finalizes sessions. A request
   -- handler may Resolve using kernel-returned sender and authorityTag, never
   -- a tag/session supplied in message words. No kernel minting occurs here.
   Capacity : constant := 16;
   Tag_Base : constant Unsigned_64 := 16#4750_0000_0000_0000#;
   Tag_Last : constant Unsigned_64 := 16#4750_FFFF_FFFF_FFFF#;
   type Registry is limited private;
   subtype Slot_Index is Natural range 0 .. Capacity;
   --  Issued-record location only, NEVER authorization. Reserved and retired
   --  records remain locatable, including after quarantine, for exact cleanup
   --  bookkeeping. Zero means this registry never issued that identity.
   --  Consumers must still Resolve using the kernel sender/stamp for work.
   function Storage_Index (Object : Registry; Tag : Unsigned_64) return Slot_Index
     with Post => (if Storage_Index'Result /= 0 then
       Tag > Tag_Base and Tag <= Tag_Last);
   -- Reverse metadata lookup. Never fabricate an identity for an unissued
   -- slot; retirement/quarantine do not erase issued cleanup identities.
   function Issued_Tag (Object : Registry; Index : Slot_Index) return Unsigned_64
     with Post => (if Issued_Tag'Result /= 0 then
       Index /= 0 and Storage_Index (Object, Issued_Tag'Result) = Index);
   procedure Reserve
     (Object : in out Registry; Sender : Unsigned_64; Tag : out Unsigned_64)
     with Post => Tag = 0 or else
       (Sender /= 0 and Tag > Tag_Base and Tag <= Tag_Last and
        Storage_Index (Object, Tag) /= 0);
   -- Reserved tags do not resolve. After trusted grant completion, finalize
   -- exactly once; failed/uncertain grants retire the tag permanently.
   procedure Finalize
     (Object : in out Registry; Sender, Tag : Unsigned_64;
      Granted : Boolean; Accepted : out Boolean);
   function Resolve
     (Object : Registry; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64
     with Post => (Resolve'Result = 0 or else
       (Sender /= 0 and Resolve'Result = Stamped_Tag and
        Stamped_Tag > Tag_Base and Stamped_Tag <= Tag_Last and
        Storage_Index (Object, Stamped_Tag) /= 0));
   -- Closing revokes work authorization, not the retained cleanup identity.
   procedure Close (Object : in out Registry; Sender, Tag : Unsigned_64)
     with Post => Storage_Index (Object, Tag) = Storage_Index (Object, Tag)'Old
       and then Resolve (Object, Sender, Tag) = 0;
   -- Read-only cleanup observation only. Must NEVER substitute for Resolve
   -- in allocation, mapping, binding or submission authorization. Inputs are
   -- kernel envelope values, not application words. Quarantine fails closed.
   function Resolve_Retired
     (Object : Registry; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64
     with Post => (Resolve_Retired'Result = 0 or else
       (Sender /= 0 and Resolve_Retired'Result = Stamped_Tag and
        Stamped_Tag > Tag_Base and Stamped_Tag <= Tag_Last and
        Resolve (Object, Sender, Stamped_Tag) = 0));
   procedure Quarantine (Object : in out Registry);
   -- Sender alone never resolves an old session. PID reuse with a newly
   -- minted tag cannot recover earlier names. No tag/slot reuse or reset.
private
   type Phase is (Reserved, Active, Retired);
   type Entry_State is record
      Sender : Unsigned_64 := 0;
      Tag : Unsigned_64 := 0;
      State : Phase := Retired;
   end record;
   type Entries is array (Positive range 1 .. Capacity) of Entry_State;
   type Registry is limited record
      Used : Natural range 0 .. Capacity := 0;
      -- Independent of storage position. Exhaustion fails closed; neither
      -- close nor quarantine resets the issuer. Slot recycling is not yet
      -- enabled, and must never reset this high-water mark when added.
      Last_Issued : Unsigned_64 range Tag_Base .. Tag_Last := Tag_Base;
      Failed : Boolean := False;
      Items : Entries;
   end record;
end Intel_GPU_Render_Sessions;
