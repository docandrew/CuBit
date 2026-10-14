------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Capability handles across the C boundary (CAP-002).
--
--  @description
--  C code holds a handle as an opaque one-word struct of its kind
--  (cubit.h: cubit_endpoint, ...), never an integer slot. Only Ada glue
--  that passes a handle to C, or takes one back, uses these conversions;
--  the word is never built from an integer on either side.
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.Handles.C_ABI with Pure, SPARK_Mode is

   --  struct cubit_endpoint { uint64_t opaque; }
   type C_Endpoint is record
      Opaque : Unsigned_64 := ABI.No_Handle;
   end record
   with Convention => C_Pass_By_Copy;

   No_C_Endpoint : constant C_Endpoint := (Opaque => ABI.No_Handle);

   function To_C (Item : Endpoint) return C_Endpoint;
   function To_Endpoint (Item : C_Endpoint) return Endpoint;

private
   function To_C (Item : Endpoint) return C_Endpoint is
     ((Opaque => Item.Word));
   function To_Endpoint (Item : C_Endpoint) return Endpoint is
     ((Word => Item.Opaque));
end CuBit.Handles.C_ABI;
