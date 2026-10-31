------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Capability_ABI - implementation
------------------------------------------------------------------------------
pragma Ada_2022;

package body Capability_ABI with SPARK_Mode is

   function To_Refusal (Word : Unsigned_64) return Refusal is
   begin
      for Why in Refusal loop
         if Refusal_Word (Why) = Word then
            return Why;
         end if;
         pragma Loop_Invariant
           (for all Earlier in Refusal'First .. Why => Refusal_Word (Earlier) /= Word);
      end loop;
      --  Unreachable: the codes are contiguous from Refusal'First.
      pragma Assert (False);
      return Refusal'First;
   end To_Refusal;

end Capability_ABI;
