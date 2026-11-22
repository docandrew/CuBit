------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Shared-memory grant calls that name their peer by handle (CAP-002); the
--  typed wrapper is CuBit.Memory_Grants. Results are the kernel's words.
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.Handles.Grants is

   --  CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: to the process Peer names
   --  (an endpoint with Read, or a saved reply: the caller it answers).
   function Create (Peer : Endpoint; Address, Pages, Flags : Unsigned_64)
     return Unsigned_64;
   function Create (Peer : Reply; Address, Pages, Flags : Unsigned_64)
     return Unsigned_64;

   --  ACQUIRE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: the owner named by Owner.
   function Acquire
     (Owner : Endpoint; Grant_Slot, Generation, Offset, Length, Access_Mode : Unsigned_64)
      return Unsigned_64;

   --  DERIVE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: for the process Recipient
   --  names.
   function Derive
     (Recipient : Endpoint; Grant_Slot, Generation, Page_Offset, Pages, Write : Unsigned_64)
      return Unsigned_64;

end CuBit.Handles.Grants;
