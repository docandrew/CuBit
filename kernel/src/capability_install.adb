-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- Capability_Install - implementation (decisions only; no table access).
-------------------------------------------------------------------------------
pragma Ada_2022;

package body Capability_Install with SPARK_Mode is

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

    function Cspace_Admits
      (Cspace            : Capability;
       Target            : Unsigned_64;
       Target_Generation : Generation;
       Target_Started    : Boolean) return Admission is
    begin
        if Cspace.capType /= CAP_CSPACE then
            return (Admitted => False, Why => ABI.Wrong_Kind);
        elsif not Cspace.rights (RIGHT_GRANT) then
            return (Admitted => False, Why => ABI.Insufficient_Rights);
        elsif Cspace.object.ref /= Target or else Cspace.gen /= Target_Generation then
            return (Admitted => False, Why => ABI.Target_Gone);
        elsif Target_Started then
            return (Admitted => False, Why => ABI.Sealed);
        else
            return (Admitted => True);
        end if;
    end Cspace_Admits;

    function Reply_Admits (Reply : Capability) return Admission is
    begin
        if Reply.capType /= CAP_REPLY then
            return (Admitted => False, Why => ABI.Wrong_Kind);
        elsif not Reply.rights (RIGHT_GRANT) then
            return (Admitted => False, Why => ABI.Not_Accepted);
        else
            return (Admitted => True);
        end if;
    end Reply_Admits;

end Capability_Install;
