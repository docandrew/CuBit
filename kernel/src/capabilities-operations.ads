-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2024 Jon Andrew
--
-- @summary
-- Capability operations on single capabilities
--
-- Endpoint resolution and the per-thread reply authority. Tables of
-- capabilities are Process_Capabilities (CAP-002): a system call finds the
-- capability its handle names there, then resolves it here.
-------------------------------------------------------------------------------
package Capabilities.Operations with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- Operation status codes returned by resolution procedures.
    ---------------------------------------------------------------------------
    type OperationStatus is (
        OP_OK,
        OP_INVALID_SLOT,
        OP_STALE_GENERATION,
        OP_INSUFFICIENT_RIGHTS,
        OP_NULL_CAPABILITY
    );

    ---------------------------------------------------------------------------
    -- Per-thread reply authority (docs/threads.md). The reply capability for
    -- the request a thread is serving lives in that thread's record, so two
    -- threads of one process serving requests at once cannot overwrite each
    -- other's reply authority. MOVE_REPLY_CAPABILITY places it in the
    -- process's table as an ordinary kernel-placed handle.
    ---------------------------------------------------------------------------

    -- Consume a thread's current reply capability, if present.
    procedure takeReplyCapFrom (source : in out Capability;
                                cap    :    out Capability;
                                taken  :    out Boolean) with
        Post =>
          (if source'Old.capType = CAP_REPLY then
               taken and then cap = source'Old
               and then source = NULL_CAPABILITY
           else
               not taken and then cap = NULL_CAPABILITY
               and then source = source'Old);

    -- Ghost: a thread's reply authority is single-use.
    procedure proveThreadReplySingleUse
      (source      : in out Capability;
       firstTaken  :    out Boolean;
       secondTaken :    out Boolean) with
        Ghost,
        Post => not (firstTaken and secondTaken);

    -- Replace a thread's reply authority: the slot takes incoming (the
    -- authority a receive just minted, or none). The authority it held, if
    -- any, is never dropped: it comes back as displaced, and the kernel ends
    -- that request (IPC-000 rule 3, BUG-004). Every way out of the slot is
    -- therefore an answer (takeReplyCapFrom), a move to the process's table
    -- (MOVE_REPLY_CAPABILITY, which takes the same way), or displaced.
    procedure replaceReplyCap (slot      : in out Capability;
                               incoming  : in     Capability;
                               displaced :    out Capability) with
        Pre  => incoming.capType in CAP_NULL | CAP_REPLY,
        Post =>
          slot = incoming
          and then
          (if slot'Old.capType = CAP_REPLY then displaced = slot'Old
           else displaced = NULL_CAPABILITY);

    -- Ghost: a server that receives twice without answering. The first
    -- request's authority comes back from the second receive, to be ended;
    -- only the second can still be answered, and only once.
    procedure proveSecondReceiveDisplacesFirst
      (slot          : in out Capability;
       first, second : in     Capability;
       displaced     :    out Capability;
       answered      :    out Capability) with
        Ghost,
        Pre  => first.capType = CAP_REPLY and then second.capType = CAP_REPLY,
        Post => displaced = first and then answered = second
                and then slot = NULL_CAPABILITY;

    ---------------------------------------------------------------------------
    -- isCurrentEndpointResolution
    -- Proof-only model of endpoint resolution: kind, rights and the target
    -- object's current generation. A failed resolution discloses neither an
    -- object reference nor an authority tag.
    ---------------------------------------------------------------------------
    function isCurrentEndpointResolution
      (cap               : Capability;
       rights            : CapabilityRights;
       currentGeneration : Generation;
       destPID           : Unsigned_64;
       authorityTag      : Authority_Tag;
       status            : OperationStatus) return Boolean is
        (if cap.capType = CAP_NULL then
             status = OP_NULL_CAPABILITY
             and then destPID = 0
             and then authorityTag = NO_AUTHORITY_TAG
         elsif cap.capType /= CAP_ENDPOINT then
             status = OP_INVALID_SLOT
             and then destPID = 0
             and then authorityTag = NO_AUTHORITY_TAG
         elsif not isSubsetOf (rights, cap.rights) then
             status = OP_INSUFFICIENT_RIGHTS
             and then destPID = 0
             and then authorityTag = NO_AUTHORITY_TAG
         elsif cap.gen /= currentGeneration then
             status = OP_STALE_GENERATION
             and then destPID = 0
             and then authorityTag = NO_AUTHORITY_TAG
         else
             status = OP_OK
             and then destPID = cap.object.ref
             and then authorityTag = cap.authorityTag)
        with Ghost;

    ---------------------------------------------------------------------------
    -- resolveCurrentEndpoint
    -- Resolve an endpoint capability and reject one from an obsolete object
    -- generation. The caller obtains currentGeneration only after validating
    -- that the capability's object reference is safe to use as an index.
    ---------------------------------------------------------------------------
    procedure resolveCurrentEndpoint
      (cap               : in     Capability;
       rights            : in     CapabilityRights;
       currentGeneration : in     Generation;
       destPID           :    out Unsigned_64;
       authorityTag      :    out Authority_Tag;
       status            :    out OperationStatus) with
        Post => isCurrentEndpointResolution
          (cap, rights, currentGeneration, destPID, authorityTag, status);

end Capabilities.Operations;
