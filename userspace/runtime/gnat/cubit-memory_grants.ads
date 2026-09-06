------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Generation-checked shared-memory grant references.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

with CuBit.Messages;

package CuBit.Memory_Grants is
   PAGE_SIZE : constant Unsigned_64 := 4096;
   MAXIMUM_GLOBAL_SLOT : constant Unsigned_64 := 4095;
   MAXIMUM_GENERATION : constant Unsigned_64 := Unsigned_64 (Unsigned_32'Last);

   subtype Global_Grant_Slot is Unsigned_64 range 0 .. MAXIMUM_GLOBAL_SLOT;
   subtype Grant_Generation is Unsigned_64 range 1 .. MAXIMUM_GENERATION;

   type Grant_Reference is record
      slot       : Global_Grant_Slot := 0;
      generation : Grant_Generation := 1;
   end record;

   type Required_Access is (Read_Access, Write_Access);
   for Required_Access use (Read_Access => 0, Write_Access => 1);

   procedure Create_For_Process
     (grantee   : CuBit.Messages.ProcessID;
      localAddr : System.Address;
      numPages  : Natural;
      readWrite : Boolean;
      reference : out Grant_Reference;
      success   : out Boolean);

   procedure Create_Via_Capability
     (slot      : CuBit.Messages.CapabilitySlot;
      localAddr : System.Address;
      numPages  : Natural;
      readWrite : Boolean;
      reference : out Grant_Reference;
      success   : out Boolean);

   procedure Acquire
     (reference     : Grant_Reference;
      expectedOwner : CuBit.Messages.ProcessID;
      byteOffset    : Unsigned_64;
      byteLength    : Unsigned_64;
      requiredAccess : Required_Access;
      mappedAddress : out System.Address;
      success       : out Boolean);

   --  Acquire from the service named by an endpoint capability.  The kernel
   --  derives and generation-checks the expected grant owner from that
   --  authority, avoiding a caller-supplied PID at service boundaries.
   procedure Acquire_Via_Capability
     (slot           : CuBit.Messages.CapabilitySlot;
      reference      : Grant_Reference;
      byteOffset     : Unsigned_64;
      byteLength     : Unsigned_64;
      requiredAccess : Required_Access;
      mappedAddress  : out System.Address;
      success        : out Boolean);

   procedure Return_Acquisition
     (reference : Grant_Reference;
      success   : out Boolean);

   procedure Revoke
     (reference : Grant_Reference;
      success   : out Boolean);
end CuBit.Memory_Grants;
