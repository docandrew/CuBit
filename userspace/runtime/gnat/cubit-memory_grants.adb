------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;

package body CuBit.Memory_Grants is
   function To_Address is new Ada.Unchecked_Conversion
     (Unsigned_64, System.Address);

   procedure Finish_Creation
     (rawSlot   : Unsigned_64;
      reference : out Grant_Reference;
      success   : out Boolean);

   procedure Finish_Creation
     (rawSlot   : Unsigned_64;
      reference : out Grant_Reference;
      success   : out Boolean)
   is
      rawGeneration : Unsigned_64;
   begin
      reference := (slot => 0, generation => 1);
      success := False;
      if rawSlot > MAXIMUM_GLOBAL_SLOT then
         CuBit.Messages.debugPrint
           ("memory-grants: creation returned invalid global slot" & ASCII.LF);
         return;
      end if;

      rawGeneration := CuBit.Messages.syscall
        (CuBit.Messages.SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
         rawSlot);
      if rawGeneration = Unsigned_64'Last or else
         rawGeneration = 0 or else rawGeneration > MAXIMUM_GENERATION
      then
         CuBit.Messages.debugPrint
           ("memory-grants: owner generation lookup failed" & ASCII.LF);
         CuBit.Messages.revokeGrant (rawSlot);
         return;
      end if;

      reference :=
        (slot       => Global_Grant_Slot (rawSlot),
         generation => Grant_Generation (rawGeneration));
      success := True;
   end Finish_Creation;

   procedure Create_For_Process
     (grantee   : CuBit.Messages.ProcessID;
      localAddr : System.Address;
      numPages  : Natural;
      readWrite : Boolean;
      reference : out Grant_Reference;
      success   : out Boolean)
   is
      rawSlot : Unsigned_64;
      created : Boolean;
   begin
      CuBit.Messages.createGrant
        (grantee, localAddr, numPages, readWrite, rawSlot, created);
      if not created then
         CuBit.Messages.debugPrint
           ("memory-grants: create-for-process syscall failed" & ASCII.LF);
         reference := (slot => 0, generation => 1);
         success := False;
         return;
      end if;
      Finish_Creation (rawSlot, reference, success);
   end Create_For_Process;

   procedure Create_Via_Capability
     (slot      : CuBit.Messages.CapabilitySlot;
      localAddr : System.Address;
      numPages  : Natural;
      readWrite : Boolean;
      reference : out Grant_Reference;
      success   : out Boolean)
   is
      rawSlot : Unsigned_64;
      created : Boolean;
   begin
      CuBit.Messages.createGrantViaCap
        (slot, localAddr, numPages, readWrite, rawSlot, created);
      if not created then
         CuBit.Messages.debugPrint
           ("memory-grants: create-via-capability syscall failed" & ASCII.LF);
         reference := (slot => 0, generation => 1);
         success := False;
         return;
      end if;
      Finish_Creation (rawSlot, reference, success);
   end Create_Via_Capability;

   procedure Create_Forwardable_Via_Capability
     (slot : CuBit.Messages.CapabilitySlot; localAddr : System.Address;
      numPages : Natural; readWrite : Boolean;
      reference : out Grant_Reference; success : out Boolean)
   is
      function To_Number is new Ada.Unchecked_Conversion
        (System.Address, Unsigned_64);
      Raw : Unsigned_64;
   begin
      reference := (slot => 0, generation => 1);
      success := False;
      if numPages not in 1 .. 4096 then
         return;
      end if;
      Raw := CuBit.Messages.syscall
        (CuBit.Messages.SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
         slot, To_Number (localAddr), Unsigned_64 (numPages),
         (if readWrite then 3 else 2));
      if Raw = Unsigned_64'Last then
         return;
      end if;
      Finish_Creation (Raw, reference, success);
   end Create_Forwardable_Via_Capability;

   procedure Derive_Via_Capability
     (recipient : CuBit.Messages.CapabilitySlot; parent : Grant_Reference;
      pageOffset, numPages : Natural; readWrite : Boolean;
      reference : out Grant_Reference; success : out Boolean)
   is
      Raw : Unsigned_64;
   begin
      reference := (slot => 0, generation => 1);
      success := False;
      if pageOffset > 4095 or else numPages not in 1 .. 4096 then
         return;
      end if;
      Raw := CuBit.Messages.syscall
        (CuBit.Messages.SYSCALL_DERIVE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
         recipient, parent.slot, parent.generation, Unsigned_64 (pageOffset),
         Unsigned_64 (numPages), (if readWrite then 1 else 0));
      if CuBit.Grant_References.Valid_Wire (Raw) then
         reference := CuBit.Grant_References.Decode (Raw);
         success := True;
      end if;
   end Derive_Via_Capability;

   procedure Acquire
     (reference     : Grant_Reference;
      expectedOwner : CuBit.Messages.ProcessID;
      byteOffset    : Unsigned_64;
      byteLength    : Unsigned_64;
      requiredAccess : Required_Access;
      mappedAddress : out System.Address;
      success       : out Boolean)
   is
      rawAddress : constant Unsigned_64 := CuBit.Messages.syscall
        (CuBit.Messages.SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT,
         reference.slot,
         reference.generation,
         expectedOwner,
         byteOffset,
         byteLength,
         Required_Access'Enum_Rep (requiredAccess));
   begin
      if rawAddress = Unsigned_64'Last then
         mappedAddress := System.Null_Address;
         success := False;
      else
         mappedAddress := To_Address (rawAddress);
         success := True;
      end if;
   end Acquire;

   procedure Acquire_Via_Capability
     (slot           : CuBit.Messages.CapabilitySlot;
      reference      : Grant_Reference;
      byteOffset     : Unsigned_64;
      byteLength     : Unsigned_64;
      requiredAccess : Required_Access;
      mappedAddress  : out System.Address;
      success        : out Boolean)
   is
      rawAddress : constant Unsigned_64 := CuBit.Messages.syscall
        (CuBit.Messages.SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
         slot,
         reference.slot,
         reference.generation,
         byteOffset,
         byteLength,
         Required_Access'Enum_Rep (requiredAccess));
   begin
      if rawAddress = Unsigned_64'Last then
         mappedAddress := System.Null_Address;
         success := False;
      else
         mappedAddress := To_Address (rawAddress);
         success := True;
      end if;
   end Acquire_Via_Capability;

   procedure Return_Acquisition
     (reference : Grant_Reference;
      success   : out Boolean)
   is
      result : constant Unsigned_64 := CuBit.Messages.syscall
        (CuBit.Messages.SYSCALL_RETURN_SHARED_MEMORY_GRANT_ACQUISITION,
         reference.slot,
         reference.generation);
   begin
      success := result = 1;
   end Return_Acquisition;

   procedure Revoke
     (reference : Grant_Reference;
      success   : out Boolean)
   is
      result : constant Unsigned_64 := CuBit.Messages.syscall
        (CuBit.Messages.SYSCALL_REVOKE_SHARED_MEMORY_GRANT_REFERENCE,
         reference.slot,
         reference.generation);
   begin
      success := result = 1;
   end Revoke;

   function Retirement_Confirmed (reference : Grant_Reference) return Boolean
   is
      observed : constant Unsigned_64 := CuBit.Messages.syscall
        (CuBit.Messages.SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
         reference.slot);
   begin
      return CuBit.Grant_References.Retirement_Confirmed (reference, observed);
   end Retirement_Confirmed;
end CuBit.Memory_Grants;
