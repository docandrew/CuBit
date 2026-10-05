with Interfaces; use Interfaces;
with System;
with Intel_GPU_Image_Lease;
package Intel_GPU_Image_Consumers is
   -- Serialized internal lifetime accounting, NOT completion authentication.
   -- Keep the ledger at a stable address until all caller-owned tokens return.
   type Domain is (GPU, CPU, Display);
   type Ledger is limited private;
   type Obligation is limited private;
   procedure Open
     (Object : in out Ledger; Key : Intel_GPU_Image_Lease.Identity;
      Accepted : out Boolean);
   -- Must succeed BEFORE handing access to any consumer. Tokens cannot be
   -- copied. No allocator, fixed slot pool, or unbounded scan is involved.
   procedure Reserve
     (Object : in out Ledger; Key : Intel_GPU_Image_Lease.Identity;
      Kind : Domain; Token : in out Obligation; Accepted : out Boolean);
   procedure Stop
     (Object : in out Ledger; Key : Intel_GPU_Image_Lease.Identity;
      Accepted : out Boolean);
   -- Confirmed means exact authenticated hardware/kernel/consumer evidence.
   -- Queue acceptance, timeout, new-front latch and unknown replies are NOT
   -- completion. A never-exposed reservation may return only after proving
   -- no handoff occurred. This routine itself does not establish that proof.
   procedure Complete
     (Object : in out Ledger; Key : Intel_GPU_Image_Lease.Identity;
      Token : in out Obligation; Confirmed : Boolean; Accepted : out Boolean);
   function Drained
     (Object : Ledger; Key : Intel_GPU_Image_Lease.Identity) return Boolean;
   procedure Quarantine (Object : in out Ledger);
private
   type Phase is (Fresh, Admitting, Closing, Failed);
   type Counts is array (Domain) of Unsigned_64;
   type Ledger is limited record
      State : Phase := Fresh;
      Key : Intel_GPU_Image_Lease.Identity;
      Last_Issued : Unsigned_64 := 0;
      Pending : Counts := [others => 0];
   end record;
   type Obligation is limited record
      Active : Boolean := False;
      Origin : System.Address := System.Null_Address;
      Serial : Unsigned_64 := 0;
      Kind : Domain := GPU;
   end record;
end Intel_GPU_Image_Consumers;
