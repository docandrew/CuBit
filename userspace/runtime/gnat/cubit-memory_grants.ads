------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Generation-checked shared-memory grant references.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

with CuBit.Messages;
with CuBit.Grant_References;

package CuBit.Memory_Grants is
   PAGE_SIZE : constant Unsigned_64 := 4096;
   MAXIMUM_GLOBAL_SLOT : constant Unsigned_64 :=
     CuBit.Grant_References.Maximum_Slot;
   MAXIMUM_GENERATION : constant Unsigned_64 :=
     CuBit.Grant_References.Maximum_Generation;

   subtype Global_Grant_Slot is CuBit.Grant_References.Global_Slot;
   subtype Grant_Generation is CuBit.Grant_References.Generation;
   subtype Grant_Reference is CuBit.Grant_References.Reference;

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
