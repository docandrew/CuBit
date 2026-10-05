with Interfaces; use Interfaces;
with Intel_GPU_Metadata_Arena;
generic
   type Table_ID is (<>);
   with package Storage is new Intel_GPU_Metadata_Arena (<>);
   with function Owner_Ready return Boolean;
   with function Capacity (Table : Table_ID) return Natural;
   with procedure Extend
     (Table : Table_ID; Base, Bytes : Unsigned_64; Accepted : out Boolean);
   with procedure Admit (Count : Positive; Accepted : out Boolean);
package Intel_GPU_Metadata_Bundle is
   type Phase is (Idle, Checking, Opening, Requesting, Committing, Publishing,
                  Admitting, Failed);
   type Bundle is limited private;
   function State (Object : Bundle) return Phase;
   type Requirement is record
      Records : Natural := 0;
      Record_Quota : Positive := 1;
      Byte_Quota : Unsigned_64 := 0;
   end record;
   type Requirements is array (Table_ID) of Requirement;
   procedure Request
     (Object : in out Bundle; Count : Positive; Demand : Requirements;
      Accepted : out Boolean);
   -- Count is the final admission token/target, not each store's capacity.
   -- Each store may have a different demand and stable reservation byte quota.
   -- All quotas validate before any mutation or allocation. Caller retains the
   -- authenticated request/epoch; these values convey no allocation authority.
   procedure Request
     (Object : in out Bundle; Count, Record_Quota : Positive;
      Per_Table_Bytes : Unsigned_64; Accepted : out Boolean);
   -- Serialized owner must keep all tables quiescent until Idle/Failed. Each
   -- step touches at most one table; admission is a separate final barrier.
   -- Partial growth is retained on failure, but no larger namespace is exposed.
   procedure Step (Object : in out Bundle);
private
   type Arenas is array (Table_ID) of Storage.Arena;
   type Bundle is limited record
      Status : Phase := Idle;
      Target : Positive := 1;
      Current : Table_ID := Table_ID'First;
      Plan : Requirements := [others => (others => <>)];
      Items : Arenas;
   end record;
end Intel_GPU_Metadata_Bundle;
