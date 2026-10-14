-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Copying a capability into a table: admission, derivation, then
-- kernel-chosen placement (CAP-002).
--
-- @description
-- Place is the whole of INSTALL_CAPABILITY and REPLY_WITH_CAPABILITY once
-- the kernel has looked up the CSPACE (or reply authority) and the source
-- under the right locks: it refuses unless Admitted, refuses unless the
-- source may be copied as asked (Capability_Install.Derive), and otherwise
-- inserts the copy where the table chooses.
--
-- Proved: a refusal changes nothing; an installation changes exactly one
-- slot, which was not live, to the derived copy (Capability_Tables'
-- Opened_Into), so it never overwrites or touches an existing entry, and
-- the copy never has more rights or a wider object than its source.
-------------------------------------------------------------------------------
pragma Ada_2022;

with Capability_Tables;

generic
    with package Tables is new Capability_Tables
      (Element => Capabilities.Capability,
       Empty_Element => Capabilities.NULL_CAPABILITY,
       others => <>);
package Capability_Install.Placement with SPARK_Mode is
    use type Tables.Handle_Check;
    use type Tables.Slot_Base;
    use type ABI.Refusal;

    type Place_Status is (Placed, Refused);

    type Place_Result (Status : Place_Status := Refused) is record
        case Status is
            when Placed  => Handle : Tables.Handle;
            when Refused => Why    : ABI.Refusal := ABI.Not_Permitted;
        end case;
    end record;

    -- The refusal for an insertion the table did not make.
    function Refusal_Of (Status : Tables.Insert_Status) return ABI.Refusal is
        (case Status is
            when Tables.Opened        => ABI.Not_Permitted,
            when Tables.Limit_Reached => ABI.Limit_Reached,
            when Tables.Table_Full    => ABI.Table_Full,
            when Tables.Out_Of_Memory => ABI.Out_Of_Memory);

    -- Place's effect, given the table's model and live count before it.
    function Placement_Done
      (Target    : Tables.Table;
       Admitted  : Admission;
       Source    : Capabilities.Capability;
       Item      : Request;
       Result    : Place_Result;
       Old_Model : Tables.Model_Array;
       Old_Live  : Tables.Slot_Count) return Boolean
    is
      ((if Result.Status = Placed then
           Admitted.Admitted
           and then Derivable (Source, Item)
           and then Tables.Names_Slot (Result.Handle)
           and then Tables.Opened_Into
                      (Old_Model, Tables.Model (Target),
                       Tables.Slot_Of (Result.Handle), Copy_Of (Source, Item))
           and then Is_Derivation (Copy_Of (Source, Item), Source)
           and then Tables.Check (Target, Result.Handle) = Tables.Current
        else
           Tables.Unchanged (Old_Model, Tables.Model (Target))
           and then Tables.Live_Count (Target) = Old_Live)
       and then
       (if not Admitted.Admitted then
           Result.Status = Refused and then Result.Why = Admitted.Why))
    with Ghost;

    procedure Place
      (Target   : in out Tables.Table;
       Admitted : Admission;
       Source   : Capabilities.Capability;
       Item     : Request;
       Result   : out Place_Result)
    with
        Pre  => Tables.Valid (Target) and then not Result'Constrained,
        Post => Placement_Done
                  (Target, Admitted, Source, Item, Result,
                   Tables.Model (Target)'Old, Tables.Live_Count (Target)'Old)
                and then Tables.Valid (Target);

end Capability_Install.Placement;
