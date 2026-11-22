pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Log_Protocol;

--  Every log publisher in the shared driver budget (CuBit.Log_Protocol's
--  budget 15) has its own publication issuance, so its tag is unique: no
--  two processes publish under one tag, and logstore never has to tell
--  them apart by process.
--
--  The fixed publishers (one process each, named here) take 1 .. the count
--  of Fixed_Publisher; every other publisher devmgr starts (generic PCI and
--  ACPI drivers, each instance of a multi-instance driver: xhci, hda, nvme)
--  takes the next issuance from devmgr's Allocator, above every fixed one,
--  and never one handed out before.
package Driver_Log_Issuances with Pure, SPARK_Mode is
   subtype Issuance is CuBit.Log_Protocol.Publication_Issuance;

   Budget : constant CuBit.Log_Protocol.Budget_Id := 15;

   type Fixed_Publisher is
     (Intel_GPU, Netmgr, Wifi, Filesystem, Config, Netstack, Virtio_GPU, Virtio_Net, PS2, Mixer,
      --  The launchers' own warnings and console text (Deferred_Warnings).
      Device_Manager, Process_Manager);

   function Fixed (P : Fixed_Publisher) return Issuance is
     (Unsigned_64 (Fixed_Publisher'Pos (P)) + 1);

   First_Allocated : constant Issuance := Unsigned_64 (Fixed_Publisher'Pos (Fixed_Publisher'Last)) + 2;

   --  Distinct publishers, distinct issuances, all below First_Allocated.
   function Fixed_Unique return Boolean is
     (for all A in Fixed_Publisher =>
        Fixed (A) < First_Allocated
        and then (for all B in Fixed_Publisher => (if A /= B then Fixed (A) /= Fixed (B))));

   type Allocator is record
      Next : Unsigned_64 := First_Allocated;
   end record;

   --  The next issuance, never one handed out before; Granted False once
   --  the issuances are used up (the publisher then gets no logstore).
   procedure Allocate (A : in out Allocator; Value : out Unsigned_64; Granted : out Boolean)
   with Pre  => A.Next >= First_Allocated,
        Post => A.Next >= First_Allocated
                and then (if Granted then Value = A.Next'Old and then Value in Issuance
                                          and then Value >= First_Allocated and then A.Next > Value
                          else A.Next = A.Next'Old);
end Driver_Log_Issuances;
