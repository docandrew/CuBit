with Interfaces; use Interfaces;
with Intel_GPU_Metadata_Arena;
generic
   with package Storage is new Intel_GPU_Metadata_Arena (<>);
   with function Owner_Ready return Boolean;
package Intel_GPU_Update_Storage is
   type Pool is limited private;
   procedure Request
     (Object : in out Pool; Index : Positive; Byte_Quota : Unsigned_64;
      Accepted : out Boolean);
   procedure Step (Object : in out Pool);
   function Pending (Object : Pool) return Boolean;
   function Ready (Object : Pool) return Boolean;
   function Table_Metadata_Bytes return Unsigned_64;
   function Mirror_Metadata_Bytes return Unsigned_64;
   -- CPU-only retained image storage, never GPU tables/BO backing. Commit and
   -- clear at most64KiB per step; final typed construction initializes one
   -- bootstrap-sized image before installing its reference, then attaches
   -- committed mirror storage in <=64KiB steps. Failure is
   -- sticky; no release or replay of uncertain mappings.
private
   type Phase is (Idle, Growing, Installing, Attaching, Mirroring, Failed);
   type Pool is limited record
      State : Phase := Idle;
      Arena : Storage.Arena;
      Index : Positive := 1;
      Offset, Limit : Unsigned_64 := 0;
      Ledger_Offset : Unsigned_64 := 0;
      Mirror_Offset, Mirror_Published : Unsigned_64 := 0;
   end record;
end Intel_GPU_Update_Storage;
