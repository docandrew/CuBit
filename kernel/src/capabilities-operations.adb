-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2024 Jon Andrew
--
-- Capability operations - implementation
-------------------------------------------------------------------------------
package body Capabilities.Operations with
    SPARK_Mode => On
is

    procedure takeReplyCapFrom (source : in out Capability;
                                cap    :    out Capability;
                                taken  :    out Boolean)
    is
    begin
        if source.capType /= CAP_REPLY then
            cap := NULL_CAPABILITY;
            taken := False;
            return;
        end if;
        cap := source;
        source := NULL_CAPABILITY;
        taken := True;
    end takeReplyCapFrom;

    procedure proveThreadReplySingleUse
      (source      : in out Capability;
       firstTaken  :    out Boolean;
       secondTaken :    out Boolean)
    is
        first, second : Capability;
    begin
        takeReplyCapFrom (source, first, firstTaken);
        takeReplyCapFrom (source, second, secondTaken);
    end proveThreadReplySingleUse;

    procedure resolveCurrentEndpoint
      (cap               : in     Capability;
       rights            : in     CapabilityRights;
       currentGeneration : in     Generation;
       destPID           :    out Unsigned_64;
       authorityTag      :    out Authority_Tag;
       status            :    out OperationStatus)
    is
    begin
        destPID := 0;
        authorityTag := NO_AUTHORITY_TAG;
        if cap.capType = CAP_NULL then
            status := OP_NULL_CAPABILITY;
        elsif cap.capType /= CAP_ENDPOINT then
            status := OP_INVALID_SLOT;
        elsif not isSubsetOf (rights, cap.rights) then
            status := OP_INSUFFICIENT_RIGHTS;
        elsif cap.gen /= currentGeneration then
            status := OP_STALE_GENERATION;
        else
            destPID := cap.object.ref;
            authorityTag := cap.authorityTag;
            status := OP_OK;
        end if;
    end resolveCurrentEndpoint;

end Capabilities.Operations;
