with Interfaces;
generic
   with function PW1_Held return Boolean;
   with function Display_Pages_Ready return Boolean;
   with function PHY_Pages_Ready return Boolean;
   with function Now_Us return Interfaces.Unsigned_64;
package Intel_GPU_Native_DC is
   -- Caller owns the serialized boot transition and inherited firmware handoff.
   -- Only retained static mappings are supported. Never release/replay after
   -- a partial failure. Held means the full transition, not only clear DC bits.
   function Execute (Owner : Boolean) return String;
   function Held return Boolean;
   function PHY_Diagnostic return String;
end Intel_GPU_Native_DC;
