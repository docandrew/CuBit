------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The platform-register pool from devmgr (ACPI-POOL-001,
--  Platform_Pool_ABI): one request for one capability.
--
--  @description
--  The pool is chipset and firmware MMIO devmgr discovered on its own (the
--  host bridge's register blocks, firmware-reserved spans), excluding every
--  BAR or window a bound driver holds (kernel claims). A program approved
--  with (acpi-firmware-data approve-declared) holds devmgr's endpoint
--  badged with CuBit.Firmware_Data_Exchange.Authority_Tag (startup name
--  Data_Label (Binding)); over it, and only over it, Pool_Request asks for
--  the pool. Synchronous calls go down: the requester calls devmgr with a
--  finite deadline of its own; devmgr never calls back.
--
--  Request (a call accepting a capability), Request_Words words:
--    Request_Word   the requester's request id (never zero)
--  Reply, label Pool_Request, Reply_Words words:
--    Reply_Request_Word  the request id
--    Disposition_Word    Disposition'Enum_Rep
--    Members_Word        the pool's intervals when it was answered
--  Granted carries one Device_Memory capability tagged
--  Platform_Pool_ABI.Platform_Pool_Tag, read-write without grant. The
--  requester inspects it (tag, rights) and maps through it with MAP_DEVICE
--  (Platform_Pool_ABI); it never trusts these words for authority. Asked
--  again it gets another copy of the same pool.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Platform_Pool_Exchange with Pure, SPARK_Mode is

   --  Beside devmgr's firmware-data window requests
   --  (CuBit.Firmware_Data_Exchange.Data_Window_Request, 16#024D#).
   Pool_Request : constant Unsigned_32 := 16#024E#;

   Request_Words : constant := 1;
   Request_Word  : constant := 0;

   Reply_Words        : constant := 3;
   Reply_Request_Word : constant := 0;
   Disposition_Word   : constant := 1;
   Members_Word       : constant := 2;

   type Disposition is
     (Granted,        --  the pool capability, read-write, no grant
      Not_Approved,   --  not through the firmware-data badge
      No_Pool,        --  devmgr holds no pool (the kernel gave none)
      Refused,        --  the requester's table refused the copy
      Malformed);     --  request id zero
   for Disposition use
     (Granted      => 1,
      Not_Approved => 2,
      No_Pool      => 3,
      Refused      => 4,
      Malformed    => 5);

   function Name (D : Disposition) return String is
     (case D is
        when Granted      => "GRANTED",
        when Not_Approved => "NOT_APPROVED",
        when No_Pool      => "NO_POOL",
        when Refused      => "REFUSED",
        when Malformed    => "MALFORMED");

   function Valid_Disposition (Word : Unsigned_64) return Boolean is
     (for some D in Disposition => Disposition'Enum_Rep (D) = Word);

end CuBit.Platform_Pool_Exchange;
