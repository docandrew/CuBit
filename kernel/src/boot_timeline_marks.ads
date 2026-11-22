-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Marking a boot milestone (docs/boot-timeline.md): a Call on a
-- notification handle (ABI-002; the BOOT_TIMELINE system call is gone).
--
-- @description
-- Only the service that owns a milestone's event may mark it: the caller's
-- handle must be a CAP_NOTIFICATION with Write that covers the owner's
-- driver role, the same authority that registers that role. The request
-- and reply are Boot_Timeline_Layout's (Mark_Label, Mark_Outcome).
-------------------------------------------------------------------------------
pragma Ada_2022;
with Capabilities;
with Process;

package Boot_Timeline_Marks is

    -- The request's label is Boot_Timeline_Layout.Mark_Label.
    function Is_Mark (Request : Process.Message) return Boolean;

    -- Mark the requested milestone for Caller if Authority (the handle's
    -- capability, a CAP_NOTIFICATION) may; Reply is the whole answer.
    procedure Call (Caller    : Process.ProcessID;
                    Authority : Capabilities.Capability;
                    Request   : Process.Message;
                    Reply     : out Process.Message)
    with Pre => Is_Mark (Request);
end Boot_Timeline_Marks;
