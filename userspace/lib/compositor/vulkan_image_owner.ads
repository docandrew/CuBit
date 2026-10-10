with Compositor_Storage;
with Interfaces;
with Vulkan_Submission;
with System;
-- One aggregate ledger per authorized device, shared by all these owners.
-- Bounded scene inventory: three targets, every source slot and two staging buffers
-- (upload and readback). This is not the driver's general BO allocator.
-- Do not copy/reset an owner or ledger, or publish handles before Live.
-- Rearm is the only supported owner reuse; the device ledger is never reset.
package Vulkan_Image_Owner with SPARK_Mode is
   Target_Images : constant := 3;
   Staging_Buffers : constant := 2;
   package Accounting is new Compositor_Storage
     (Slot_Count => Target_Images + Vulkan_Submission.Source_Capacity + Staging_Buffers);
   use type Accounting.State, Accounting.Ticket;
   subtype U32 is Interfaces.Unsigned_32;
   type Phase is (Fresh, Prepared, Live, Closed, Quarantined);
   type State is private;
   function Status (S : State) return Phase;
   function Required_Bytes (S : State) return Natural;
   function Lease (S : State) return Accounting.Ticket;
   function Can_Release (S : State; Budget : Accounting.State) return Boolean;
   -- Matching device ledger required. Rearm only after confirmed native closure
   -- and refund. It never releases resources or resets allocation identities.
   -- The next Prepare requires fresh, exclusively owned native request metadata;
   -- this transition does not reset a foreign request or grant import authority.
   procedure Rearm (S : in out State; Budget : Accounting.State;
                    Accepted : out Boolean)
     with Pre => Accounting.Valid (Budget),
       Post => Accepted = (Status (S'Old) = Closed and then
                          not Accounting.Current (Budget, Lease (S'Old))) and
         (if Accepted then Status (S) = Fresh and Required_Bytes (S) = 0 and
              Lease (S) = Accounting.No_Ticket
          else S = S'Old);
   procedure Prepare (S : in out State; Request : System.Address)
     with Pre => Status (S) = Fresh,
       Post => Status (S) /= Fresh and Status (S) /= Live;
   -- Allowed_Types is a trusted mask from the matching physical device's memory
   -- properties, filtered for the caller's requirements. Select first compatible
   -- type, bounded to 32; charge the real driver requirement before allocation.
   procedure Allocate (S : in out State; Budget : in out Accounting.State;
                       Allowed_Types : U32)
     with Pre => Status (S) = Prepared and Accounting.Valid (Budget),
       Post => Accounting.Valid (Budget) and
         Accounting.Limit (Budget) = Accounting.Limit (Budget'Old) and Status (S) /= Prepared and
         (if Status (S) = Live then Can_Release (S, Budget));
   procedure Release (S : in out State; Budget : in out Accounting.State;
                      All_Readers_Retired : Boolean)
     with Pre => Accounting.Valid (Budget) and Can_Release (S, Budget),
       Post => Accounting.Valid (Budget) and
         Accounting.Limit (Budget) = Accounting.Limit (Budget'Old) and
         (if not All_Readers_Retired then S = S'Old and Budget = Budget'Old
          else Status (S) = Closed or Status (S) = Quarantined);
private
   type State is record
      Mode : Phase := Fresh;
      Request : System.Address := System.Null_Address;
      Bytes : Natural := 0;
      Types : U32 := 0;
      Ticket : Accounting.Ticket := Accounting.No_Ticket;
   end record;
   function Status (S : State) return Phase is (S.Mode);
   function Required_Bytes (S : State) return Natural is (S.Bytes);
   function Lease (S : State) return Accounting.Ticket is (S.Ticket);
   use type Accounting.Phase;
   function Can_Release (S : State; Budget : Accounting.State) return Boolean is
     (S.Mode = Live and then Accounting.Current (Budget, S.Ticket) and then
      Accounting.Status (Budget, S.Ticket) = Accounting.Live);
end Vulkan_Image_Owner;
