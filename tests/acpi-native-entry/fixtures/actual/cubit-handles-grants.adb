------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Handles.Grants - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.Kernel;

package body CuBit.Handles.Grants is
   package K renames CuBit.Handles.Kernel;

   function Create (Peer : Endpoint; Address, Pages, Flags : Unsigned_64)
     return Unsigned_64 is
     (K.Call (K.Create_Grant_Via_Capability, K.Word (Peer), Address, Pages, Flags));
   function Create (Peer : Reply; Address, Pages, Flags : Unsigned_64)
     return Unsigned_64 is
     (K.Call (K.Create_Grant_Via_Capability, K.Word (Peer), Address, Pages, Flags));

   function Acquire
     (Owner : Endpoint; Grant_Slot, Generation, Offset, Length, Access_Mode : Unsigned_64)
      return Unsigned_64 is
     (K.Call (K.Acquire_Grant_Via_Capability, K.Word (Owner), Grant_Slot, Generation,
              Offset, Length, Access_Mode));

   function Derive
     (Recipient : Endpoint; Grant_Slot, Generation, Page_Offset, Pages, Write : Unsigned_64)
      return Unsigned_64 is
     (K.Call (K.Derive_Grant_Via_Capability, K.Word (Recipient), Grant_Slot, Generation,
              Page_Offset, Pages, Write));
end CuBit.Handles.Grants;
