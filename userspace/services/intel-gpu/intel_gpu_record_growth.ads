with Interfaces; use Interfaces;
with Intel_GPU_Metadata_Arena;
generic
   with package Storage is new Intel_GPU_Metadata_Arena (<>);
   with function Capacity return Positive;
   with procedure Publish
     (Base, Bytes : Unsigned_64; Accepted : out Boolean);
package Intel_GPU_Record_Growth is
   -- One controller per registry and retained CPU reservation. Callers must
   -- serialize registry mutation and keep it quiescent throughout growth.
   -- This grows metadata only, never BO backing or GPU address space.
   type Phase is (Empty, Idle, Opening, Requesting, Committing, Publishing,
                  Failed);
   type Failure is (None, Reservation_Failed, Byte_Quota_Exhausted,
                    Commit_Failed, Publication_Failed);
   type View is record
      State : Phase := Empty;
      Error : Failure := None;
      Target : Positive := 1;
      Published_Bytes : Unsigned_64 := 0;
   end record;
   type Controller is limited private;
   function Snapshot (Object : Controller) return View;
   procedure Configure
     (Object : in out Controller; Byte_Quota : Unsigned_64;
      Record_Quota : Positive; Accepted : out Boolean);
   -- Busy/invalid requests do not change the controller. An accepted request
   -- is retained until Idle or Failed; callers need not retry IPC allocations.
   procedure Request
     (Object : in out Controller; Records : Positive; Accepted : out Boolean);
   -- Exactly one phase, no grow-to-fit loop. Yield to IPC between calls.
   -- Publish must initialize typed records before exposing their capacity.
   procedure Step (Object : in out Controller);
private
   type Controller is limited record
      Data : View;
      Arena : Storage.Arena;
      Byte_Limit : Unsigned_64 := 0;
      Record_Limit : Positive := 1;
   end record;
end Intel_GPU_Record_Growth;
