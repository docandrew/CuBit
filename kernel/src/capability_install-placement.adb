-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- Capability_Install.Placement - implementation
-------------------------------------------------------------------------------
pragma Ada_2022;

package body Capability_Install.Placement with SPARK_Mode is

    procedure Place
      (Target   : in out Tables.Table;
       Admitted : Admission;
       Source   : Capabilities.Capability;
       Item     : Request;
       Result   : out Place_Result)
    is
        use type Tables.Insert_Status;
        Outcome  : Capability_Install.Outcome;
        Inserted : Tables.Insert_Result;
    begin
        if not Admitted.Admitted then
            Result := (Status => Refused, Why => Admitted.Why);
            return;
        end if;
        Outcome := Derive (Source, Item);
        if not Outcome.Accepted then
            Result := (Status => Refused, Why => Outcome.Why);
            return;
        end if;
        Tables.Insert (Target, Outcome.Copy, Inserted);
        if Inserted.Status = Tables.Opened then
            Result := (Status => Placed, Handle => Inserted.Opened_Handle);
        else
            Result := (Status => Refused, Why => Refusal_Of (Inserted.Status));
        end if;
    end Place;

end Capability_Install.Placement;
