-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- Capability_Derivation - implementation (decisions only; no table access).
-------------------------------------------------------------------------------
pragma Ada_2022;

package body Capability_Derivation with SPARK_Mode is

    function Derive (Source : Capability; Item : Request) return Outcome is
    begin
        if Derivable (Source, Item) then
            return (Accepted => True, Copy => Copy_Of (Source, Item));
        elsif not Transferable (Source.capType) then
            return (Accepted => False, Why => ABI.Not_Transferable);
        elsif not Source.rights (RIGHT_GRANT) then
            return (Accepted => False, Why => ABI.Insufficient_Rights);
        elsif not Copyable (Source) then
            -- A device-vector capability, or a malformed interrupt range.
            return (Accepted => False, Why => ABI.Not_Transferable);
        elsif Item.Set_Tag and then Source.capType not in CAP_ENDPOINT | CAP_NOTIFICATION then
            return (Accepted => False, Why => ABI.Invalid_Argument);
        elsif Item.Narrow and then not Range_Kind (Source.capType)
          and then Source.capType /= CAP_SCHEDULING
        then
            return (Accepted => False, Why => ABI.Invalid_Argument);
        else
            -- More rights, a wider object, or a new tag on a tagged endpoint.
            return (Accepted => False, Why => ABI.Would_Amplify);
        end if;
    end Derive;

end Capability_Derivation;
