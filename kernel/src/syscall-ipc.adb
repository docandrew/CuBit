-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Syscall IPC handler implementations: replies, events, waits and memory
-- grants.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with Capabilities;
with Capabilities.Operations;
with Capability_ABI;
with Capability_Derivation;
with Process_Capabilities;
with IPC_Labels;
with Memory_Grants;
with PerCpuData;
with Channel_Wake_ABI;
with Process;
with Process_Identities;
with Process.IPC;
with Process.Futex;
with Time;
with Wait_ABI;
with Wait_Policy;
with Process.User_Memory;
with Spinlocks;
with Platform_Pool;
with Service_Registry;
with TextIO; use TextIO;
with Util;

use type Process.MessageTag;
use type Process.ProcessMode;
use type Capabilities.Operations.OperationStatus;

package body Syscall.IPC is
    use type Platform_Pool.Claim_Outcome;
    use type Process_Capabilities.Tables.Slot_Base;
    -- WAIT's Ready_Word answer is the activity wait's (IPC-010 option B).
    pragma Compile_Time_Error
      (Process.IPC.Activity_Word /= Wait_ABI.Ready_Word,
       "Activity_Word must be Wait_ABI.Ready_Word");
    -- Shared-memory grant flags (arg3 of the create-grant calls): bit 0
    -- read-write, then these.
    Grant_Flag_Forwardable : constant Unsigned_64 := 2;
    -- Post grant lifecycle events for it (docs/data-plane.md).
    Grant_Flag_Notify      : constant Unsigned_64 := 4;

    function toErr is
        new Ada.Unchecked_Conversion (Long_Integer, Unsigned_64);
    reterr : constant Unsigned_64 := toErr (-1);

    function tagToU64 is new Ada.Unchecked_Conversion
        (Process.MessageTag, Unsigned_64);
    function u64ToTag is new Ada.Unchecked_Conversion
        (Unsigned_64, Process.MessageTag);
    function u64ToRights is new Ada.Unchecked_Conversion
        (Unsigned_8, Capabilities.CapabilityRights);

    ---------------------------------------------------------------------------
    -- Authority by handle (CAP-002): every call names the capability it
    -- relies on; nothing searches the caller's table for one of a type.
    -- The *Held forms expect the caller's mailbox lock to be held.
    ---------------------------------------------------------------------------
    -- The capability a handle names in the caller's table (a snapshot), or
    -- the null capability.
    function ownCap (callerPID : Process.ProcessID; handle : Unsigned_64)
                     return Capabilities.Capability
    is
        found : Process_Capabilities.Found;
    begin
        Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
        found := Process_Capabilities.Find_Any (Process.proctab(callerPID).caps, handle);
        Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
        return (if found.OK then found.Cap else Capabilities.NULL_CAPABILITY);
    end ownCap;

    ---------------------------------------------------------------------------
    -- handleReply
    ---------------------------------------------------------------------------
    procedure handleReply (arg0, arg1, arg2, arg3,
                           arg4, arg5 : Unsigned_64;
                           retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is



        replyMsg : constant Process.Message := (
            tag      => u64ToTag (arg1),
            authorityTag => 0,
            words    => (arg2, arg3, arg4, arg5));
    begin
        -- A stale identity names no one: NO_PROCESS, which reply refuses.
        retval := Process.IPC.reply (
            replyTo => Process.processOfIdentity (Process_Identities.From_Word (arg0)),
            msg     => replyMsg);
    end handleReply;

    ---------------------------------------------------------------------------
    -- handleReplyCap
    ---------------------------------------------------------------------------
    procedure handleReplyCap (arg0, arg1, arg2, arg3,
                              arg4, arg5 : Unsigned_64;
                              retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        replyMsg : constant Process.Message := (
            tag      => u64ToTag (arg1),
            authorityTag => 0,
            words    => (arg2, arg3, arg4, arg5));
    begin
        -- arg0: a saved reply's handle (MOVE_REPLY_CAPABILITY). The current
        -- reply is answered with REPLY or REPLY_WAIT, by the caller's identity.
        retval := Process.IPC.replyCap (handle => arg0, msg => replyMsg);
    end handleReplyCap;

    ---------------------------------------------------------------------------
    -- handleReplyWithCap (REPLY_WITH_CAPABILITY, CAP-002): arg0 = the
    -- requester's identity, arg1 = address of the reply message, arg2 =
    -- address of a Capability_ABI.Derivation. See Process.IPC.
    ---------------------------------------------------------------------------
    -- A capability derivation the caller is about to place in targetPID:
    -- device memory there is a claim on the platform pool (ACPI-POOL-001).
    -- False: no room for the claim; the caller refuses (CAPACITY).
    function claimDelegation (callerPID, targetPID : Process.ProcessID;
                              derivation : Capability_ABI.Derivation) return Boolean is
        use type Process.ProcessID;
    begin
        if targetPID = Process.NO_PROCESS or else targetPID = callerPID
          or else not Capability_Derivation.Well_Formed (derivation)
        then
            return True;
        end if;
        declare
            copy : constant Capability_Derivation.Outcome := Capability_Derivation.Derive
              (ownCap (callerPID, derivation (Capability_ABI.Source)),
               Capability_Derivation.Decode (derivation));
        begin
            return not copy.Accepted
              or else Platform_Pool.Delegating (targetPID, copy.Copy)
                      /= Platform_Pool.Refused_Capacity;
        end;
    end claimDelegation;

    procedure handleReplyWithCap (callerPID : Process.ProcessID;
                                  arg0, arg1, arg2 : Unsigned_64;
                                  retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        replyMsg   : Process.Message;
        derivation : aliased Capability_ABI.Derivation := [others => 0];
        ok         : Boolean;
    begin
        retval := Capability_ABI.Refusal_Word (Capability_ABI.Invalid_Argument);
        Process.User_Memory.Read_Message (callerPID, arg1, replyMsg, ok);
        if not ok then
            return;
        end if;
        Process.User_Memory.Copy
          (callerPID, arg2, derivation'Address, Capability_ABI.Derivation_Bytes, ok);
        if not ok then
            return;
        end if;
        -- Device memory given in the reply is a claim on the platform pool,
        -- recorded before the copy exists (Platform_Pool).
        if not claimDelegation
          (callerPID, Process.processOfIdentity (Process_Identities.From_Word (arg0)),
           derivation)
        then
            retval := Capability_ABI.Refusal_Word (Capability_ABI.Table_Full);
            return;
        end if;
        -- The kernel stamps the reply's authority tag (the new handle).
        replyMsg.authorityTag := 0;
        retval := Process.IPC.replyWithCapability
          (replyTo    => Process.processOfIdentity (Process_Identities.From_Word (arg0)),
           msg        => replyMsg,
           derivation => derivation);
    end handleReplyWithCap;

    procedure handleReplySavedWithCap (callerPID : Process.ProcessID;
                                       arg0, arg1, arg2 : Unsigned_64;
                                       retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        replyMsg   : Process.Message;
        derivation : aliased Capability_ABI.Derivation := [others => 0];
        ok         : Boolean;
    begin
        retval := Capability_ABI.Refusal_Word (Capability_ABI.Invalid_Argument);
        Process.User_Memory.Read_Message (callerPID, arg1, replyMsg, ok);
        if not ok then
            return;
        end if;
        Process.User_Memory.Copy
          (callerPID, arg2, derivation'Address, Capability_ABI.Derivation_Bytes, ok);
        if not ok then
            return;
        end if;
        declare
            targetPID : Process.ProcessID;
            targetTID : Process.ThreadID;
        begin
            Process.IPC.replyTargetOf (ownCap (callerPID, arg0), targetPID, targetTID);
            if not claimDelegation (callerPID, targetPID, derivation) then
                retval := Capability_ABI.Refusal_Word (Capability_ABI.Table_Full);
                return;
            end if;
        end;
        replyMsg.authorityTag := 0;
        retval := Process.IPC.replySavedWithCapability (arg0, replyMsg, derivation);
    end handleReplySavedWithCap;

    ---------------------------------------------------------------------------
    -- handleWaitForActivity
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- handleReceiveEventNB
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- handleSendEvent
    ---------------------------------------------------------------------------
    procedure handleSendEvent (callerPID : Process.ProcessID;
                               arg0, arg1, arg2, arg3,
                               arg4, arg5, arg6 : Unsigned_64;
                               retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;




        destPID : Process.ProcessID;
        generation : Capabilities.Generation;
        hasCap  : Boolean := False;
        accepted : Boolean;
        authorityTag : Capabilities.Authority_Tag := Capabilities.NO_AUTHORITY_TAG;
        resolvedPID : Unsigned_64;
        resolvedAuthorityTag : Capabilities.Authority_Tag;
        resolveStatus : Capabilities.Operations.OperationStatus;
        publishRights : constant Capabilities.CapabilityRights :=
          (Capabilities.RIGHT_WRITE => True, others => False);
        eventMsg : Process.Message;
    begin
        -- Channel_Wake_ABI's label is the kernel's wake-word notice (IPC-010):
        -- nobody sends it.
        if u64ToTag (arg1).label = Channel_Wake_ABI.Wake_Label then
            retval := reterr;
            return;
        end if;
        Process.resolveIdentity (Process_Identities.From_Word (arg0), destPID, generation);
        if destPID = Process.NO_PROCESS then
            retval := reterr;
            return;
        end if;
        eventMsg := (
            tag      => u64ToTag (arg1),
            authorityTag => 0,
            words    => (arg2, arg3, arg4, arg5));

        -- The kernel's own events (child exits, faults, grant lifecycle,
        -- control messages) and flags cannot be forged: receivers act on them.
        if IPC_Labels.Is_Kernel_Event (eventMsg.tag.label)
          or else Capability_ABI.Claims_Kernel_Flags (eventMsg.tag.ipc)
        then
            retval := reterr;
            return;
        end if;

        -- Kernel-mode threads are exempt
        if Process.threadOf (callerPID).mode = Process.KERNEL then
            hasCap := True;
        end if;

        --  A read-only notification capability is narrowly scoped event
        --  publication authority.  Its object is a service/driver class, and
        --  it reaches only the process currently registered for that class.
        --  The complementary RIGHT_WRITE authorizes registration itself.
        -- arg6 names the authority: a notification capability (RIGHT_READ)
        -- covering a role destPID is registered for, or an endpoint
        -- (RIGHT_WRITE) to destPID in its current incarnation.
        if not hasCap then
            declare
                found : Process_Capabilities.Found;
            begin
                Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
                found := Process_Capabilities.Find_Any (Process.proctab(callerPID).caps, arg6);
                Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
                if not found.OK then
                    null;
                elsif found.Cap.capType = Capabilities.CAP_NOTIFICATION then
                    if found.Cap.rights (Capabilities.RIGHT_READ)
                      and then found.Cap.object.param > 0
                      and then found.Cap.object.ref <= Service_Registry.DriverID'Last
                    then
                        for role in found.Cap.object.ref ..
                          (if found.Cap.object.param - 1 >=
                                Service_Registry.DriverID'Last - found.Cap.object.ref
                           then Service_Registry.DriverID'Last
                           else found.Cap.object.ref + (found.Cap.object.param - 1))
                        loop
                            if Service_Registry.Registered (role) = destPID then
                                hasCap := True;
                                authorityTag := found.Cap.authorityTag;
                                exit;
                            end if;
                        end loop;
                    end if;
                elsif found.Cap.object.ref = Unsigned_64 (destPID) then
                    Capabilities.Operations.resolveCurrentEndpoint
                      (cap => found.Cap,
                       rights => publishRights,
                       currentGeneration => Process.generationOf (destPID),
                       destPID => resolvedPID,
                       authorityTag => resolvedAuthorityTag,
                       status => resolveStatus);
                    if resolveStatus = Capabilities.Operations.OP_OK and then
                       resolvedPID = Unsigned_64 (destPID)
                    then
                        hasCap := True;
                        authorityTag := resolvedAuthorityTag;
                    end if;
                end if;
            end;
        end if;

        if not hasCap then
            Process.IPC.notifySupervisor (
                callerPID,
                IPC_Labels.EVENT_CAP_FAULT,
                Unsigned_64 (SyscallNumber'Enum_Rep (
                    SYSCALL_SEND_EVENT)),
                arg0, 0);
            retval := reterr;
        else
            --  The receiver learns which granted authority published the
            --  event. A userspace payload cannot forge this authority tag because the
            --  kernel overwrites it after resolving the caller's capability.
            eventMsg.authorityTag := authorityTag;
            Process.IPC.trySendEvent (
                dest => destPID,
                msg  => eventMsg,
                publisher => callerPID,
                accepted => accepted,
                expectedGeneration => generation);
            retval := (if accepted then 1 else 0);
        end if;
    end handleSendEvent;

    ---------------------------------------------------------------------------
    -- handlePollServiceRequest
    --
    -- Non-blocking service-request receive. This syscall is intentionally
    -- narrow: it accepts client work for a server but will not consume input
    -- events, IRQ-style notifications, or lifecycle events queued in the same
    -- mailbox ring.
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- handleGrant
    ---------------------------------------------------------------------------
    procedure handleGrant (callerPID : Process.ProcessID;
                           arg0, arg1, arg2, arg3 : Unsigned_64;
                           retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;


        granteePID : Process.ProcessID;
        generation : Capabilities.Generation;
        replyPID   : Process.ProcessID;
        replyTID   : Process.ThreadID;
        hasCap : Boolean := False;
        gid : Natural;
        perm : Process.GrantPermission;
        ok : Boolean;
    begin
        Process.resolveIdentity (Process_Identities.From_Word (arg0), granteePID, generation);
        if granteePID = Process.NO_PROCESS or else
           not Memory_Grants.Valid_Creation_Request (arg2, arg3) then
            retval := reterr;
            return;
        end if;

        -- Kernel-mode threads exempt
        if Process.threadOf (callerPID).mode = Process.KERNEL then
            hasCap := True;
        else
            -- An endpoint or a saved reply to the grantee is named by
            -- handle: CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY. Here only
            -- the calling thread's current reply (answering the grantee) or
            -- the grantee's opt-in below authorize.
            -- The calling thread's current reply authority.
            if not hasCap then
                Process.IPC.replyTargetOf
                  (Process.threadtab (PerCPUData.getCurrentThread).replyCap,
                   replyPID, replyTID);
                hasCap := replyPID = granteePID;
            end if;
        end if;

        -- Reverse: grantee has endpoint to caller
        -- with RIGHT_GRANT (explicit opt-in to
        -- receiving grants from that service).
        -- This looks in the grantee's table, never the caller's: the
        -- grantee consented by holding it (follow-up: name the opt-in
        -- explicitly once channels replace this path).
        if not hasCap then
            Spinlocks.enterCriticalSection (Process.mailtab(granteePID).lock);
            for slot in 0 .. Process_Capabilities.Tables.High_Water
                               (Process.proctab(granteePID).caps) - 1
            loop
                if Process_Capabilities.Tables.Is_Live_At (Process.proctab(granteePID).caps, slot) then
                    declare
                        cap : constant Capabilities.Capability :=
                          Process_Capabilities.Tables.Item_At (Process.proctab(granteePID).caps, slot);
                    begin
                        if cap.capType = Capabilities.CAP_ENDPOINT and then
                           cap.object.ref = Unsigned_64 (callerPID) and then
                           cap.gen = Process.generationOf (callerPID) and then
                           cap.rights (Capabilities.RIGHT_GRANT)
                        then
                            hasCap := True;
                            exit;
                        end if;
                    end;
                end if;
            end loop;
            Spinlocks.exitCriticalSection (Process.mailtab(granteePID).lock);
        end if;

        if not hasCap then
            Process.IPC.notifySupervisor (
                callerPID,
                IPC_Labels.EVENT_CAP_FAULT,
                Unsigned_64
                  (SyscallNumber'Enum_Rep
                     (SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID)),
                arg0, arg1);
            retval := reterr;
        else
            if (arg3 and 1) /= 0 then
                perm := Process.GRANT_READWRITE;
            else
                perm := Process.GRANT_READ;
            end if;

            Process.IPC.createGrant (
                grantee   => granteePID,
                localAddr => Util.numToAddr(arg1),
                numPages  => Natural(arg2),
                perm      => perm,
                id        => gid,
                success   => ok,
                expectedGeneration => generation,
                forwardable => (arg3 and Grant_Flag_Forwardable) /= 0,
                notify => (arg3 and Grant_Flag_Notify) /= 0);

            if ok then
                retval := Unsigned_64(gid);
            else
                retval := reterr;
            end if;
        end if;
    end handleGrant;

    ---------------------------------------------------------------------------
    -- handleRevoke
    ---------------------------------------------------------------------------
    procedure handleRevoke (callerPID : Process.ProcessID;
                            arg0      : Unsigned_64;
                            retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        --  arg0 is the grant's global slot; revokeGrant checks the caller
        --  owns it (KERN-003 step 2: the slot no longer encodes the owner).
        success : Boolean;
    begin
        pragma Unreferenced (callerPID);
        if arg0 = 0 or else arg0 > Unsigned_64 (Memory_Grants.Global_Slot'Last) then
            retval := 0;
            return;
        end if;

        -- Admission and revocation share grantLock. Do not inspect grant
        -- storage outside IPC, especially as records become lazily allocated.
        Process.IPC.revokeGrant (id => Memory_Grants.Global_Slot (arg0), success => success);
        retval := (if success then 1 else 0);
    end handleRevoke;

    procedure handleGetOwnedGrantGeneration
      (arg0   : Unsigned_64;
       retval : out Unsigned_64)
    is
        generation : Memory_Grants.Grant_Generation;
    begin
        -- Slot 0 names no grant: refused, never reported as retired.
        if arg0 < Unsigned_64 (Memory_Grants.Grant_Slot'First) or else
           arg0 > Unsigned_64 (Memory_Grants.Grant_Slot'Last)
        then
            retval := reterr;
            return;
        end if;

        Process.IPC.getOwnedGrantGeneration
          (slot       => Memory_Grants.Grant_Slot (arg0),
           generation => generation);
        retval := Unsigned_64 (generation);
    end handleGetOwnedGrantGeneration;

    procedure handleAcquireGrant
      (arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
       retval : out Unsigned_64)
    is
        mappedAddress : System.Address;
        success : Boolean;
        -- The owner the grantee expects, by identity: a stale one names no
        -- one, so the acquire fails.
        expectedOwner : constant Process.ProcessID := Process.processOfIdentity (Process_Identities.From_Word (arg2));
    begin
        if arg0 > Unsigned_64 (Memory_Grants.Global_Slot'Last) or else
           not Memory_Grants.Is_Valid_Generation_Field (arg1) or else
           expectedOwner = Process.NO_PROCESS or else
           arg5 > 1
        then
            retval := reterr;
            return;
        end if;

        Process.IPC.acquireGrant
          (reference =>
             (slot       => Memory_Grants.Global_Slot (arg0),
              generation => Memory_Grants.To_Live_Generation (arg1)),
           expectedOwner => expectedOwner,
           byteOffset    => arg3,
           byteLength    => arg4,
           requiredWrite => arg5 = 1,
           mappedAddress => mappedAddress,
           success       => success);
        retval := (if success then Util.addrToNum (mappedAddress) else reterr);
    end handleAcquireGrant;

    procedure handleAcquireGrantViaCap
      (callerPID : Process.ProcessID;
       arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
       retval : out Unsigned_64)
    is
        use type Capabilities.CapabilityType;
        cap : Capabilities.Capability;
        ownerPID : Process.ProcessID;
        mappedAddress : System.Address;
        success : Boolean;
    begin
        if arg1 > Unsigned_64 (Memory_Grants.Global_Slot'Last) or else
           not Memory_Grants.Is_Valid_Generation_Field (arg2) or else
           arg5 > 1
        then
            retval := reterr;
            return;
        end if;

        cap := ownCap (callerPID, arg0);
        if cap.capType /= Capabilities.CAP_ENDPOINT or else
           not cap.rights (Capabilities.RIGHT_READ) or else
           cap.object.ref = 0 or else
           cap.object.ref > Unsigned_64 (Process.ProcessID'Last)
        then
            retval := reterr;
            return;
        end if;

        ownerPID := Process.ProcessID (cap.object.ref);
        if cap.gen /= Process.generationOf (ownerPID) then
            retval := reterr;
            return;
        end if;

        Process.IPC.acquireGrant
          (reference =>
             (slot       => Memory_Grants.Global_Slot (arg1),
              generation => Memory_Grants.To_Live_Generation (arg2)),
           expectedOwner => ownerPID,
           byteOffset    => arg3,
           byteLength    => arg4,
           requiredWrite => arg5 = 1,
           mappedAddress => mappedAddress,
           success       => success);
        retval := (if success then Util.addrToNum (mappedAddress) else reterr);
    end handleAcquireGrantViaCap;

    procedure handleReturnGrant
      (arg0, arg1 : Unsigned_64;
       retval : out Unsigned_64)
    is
        success : Boolean;
    begin
        if arg0 > Unsigned_64 (Memory_Grants.Global_Slot'Last) or else
           not Memory_Grants.Is_Valid_Generation_Field (arg1)
        then
            retval := 0;
            return;
        end if;

        Process.IPC.returnGrant
          (reference =>
             (slot       => Memory_Grants.Global_Slot (arg0),
              generation => Memory_Grants.To_Live_Generation (arg1)),
           success => success);
        retval := (if success then 1 else 0);
    end handleReturnGrant;

    procedure handleRevokeGrantReference
      (arg0, arg1 : Unsigned_64;
       retval : out Unsigned_64)
    is
        success : Boolean;
    begin
        if arg0 > Unsigned_64 (Memory_Grants.Global_Slot'Last) or else
           not Memory_Grants.Is_Valid_Generation_Field (arg1)
        then
            retval := 0;
            return;
        end if;

        Process.IPC.revokeGrantReference
          (reference =>
             (slot       => Memory_Grants.Global_Slot (arg0),
              generation => Memory_Grants.To_Live_Generation (arg1)),
           success => success);
        retval := (if success then 1 else 0);
    end handleRevokeGrantReference;

    ---------------------------------------------------------------------------
    -- handleGrantViaCap
    -- Create a shared memory grant using a capability slot to identify the
    -- grantee, instead of a raw PID.
    -- arg0 = cap slot, arg1 = local addr, arg2 = pages,
    -- arg3 = bit0 RW, bit1 explicit owner forwarding opt-in
    ---------------------------------------------------------------------------
    procedure handleGrantViaCap (callerPID : Process.ProcessID;
                                  arg0, arg1, arg2, arg3 : Unsigned_64;
                                  retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;

        cap : Capabilities.Capability;
        granteePID : Process.ProcessID;
        gid : Natural;
        perm : Process.GrantPermission;
        ok : Boolean;
    begin
        if not Memory_Grants.Valid_Creation_Request (arg2, arg3) then
            println
              ("CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: invalid size or flags");
            retval := reterr;
            return;
        end if;

        cap := ownCap (callerPID, arg0);

        -- A saved reply names the process it answers: a server may share
        -- memory with a client whose request it set aside.
        if cap.capType = Capabilities.CAP_REPLY then
            declare
                replyPID : Process.ProcessID;
                replyTID : Process.ThreadID;
            begin
                Process.IPC.replyTargetOf (cap, replyPID, replyTID);
                if replyPID = Process.NO_PROCESS then
                    retval := reterr;
                    return;
                end if;
                cap := (capType => Capabilities.CAP_ENDPOINT,
                        rights  => Capabilities.READ_WRITE,
                        authorityTag => Capabilities.NO_AUTHORITY_TAG,
                        object  => (ref => Unsigned_64 (replyPID), param => 0),
                        gen     => Process.generationOf (replyPID));
            end;
        end if;

        if cap.capType /= Capabilities.CAP_ENDPOINT then
            println
              ("CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: slot is not " &
               "CAP_ENDPOINT");
            retval := reterr;
            return;
        end if;

        if not cap.rights(Capabilities.RIGHT_READ) then
            println
              ("CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: no RIGHT_READ " &
               "on endpoint");
            retval := reterr;
            return;
        end if;

        if cap.object.ref > Unsigned_64 (Process.ProcessID'Last) or
           cap.object.ref = 0
        then
            println
              ("CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: invalid PID " &
               "in capability");
            retval := reterr;
            return;
        end if;

        granteePID := Process.ProcessID (cap.object.ref);

        -- Validate generation (stale cap check)
        if cap.gen /= Process.generationOf (granteePID) then
            println
              ("CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: stale " &
               "capability");
            retval := reterr;
            return;
        end if;

        if (arg3 and 1) /= 0 then
            perm := Process.GRANT_READWRITE;
        else
            perm := Process.GRANT_READ;
        end if;

        Process.IPC.createGrant (
            grantee   => granteePID,
            localAddr => Util.numToAddr(arg1),
            numPages  => Natural(arg2),
            perm      => perm,
            id        => gid,
            success   => ok,
                expectedGeneration => cap.gen,
                forwardable => (arg3 and Grant_Flag_Forwardable) /= 0,
                notify => (arg3 and Grant_Flag_Notify) /= 0);

        if ok then
            retval := Unsigned_64(gid);
        else
            println ("CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: create failed");
            retval := reterr;
        end if;
    end handleGrantViaCap;

    -- Recipient endpoint, parent slot/generation, page offset/count, RW flag.
    -- Return the child slot+generation atomically in canonical reference form.
    procedure handleDeriveGrantViaCap
      (callerPID : Process.ProcessID;
       arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
       retval : out Unsigned_64)
    is
        use type Capabilities.CapabilityType;
        cap : Capabilities.Capability;
        derived : Memory_Grants.Reference;
        success : Boolean;
    begin
        retval := reterr;
        if arg1 > Unsigned_64 (Memory_Grants.Global_Slot'Last) or else
           not Memory_Grants.Is_Valid_Generation_Field (arg2) or else
           arg3 > Unsigned_64 (Memory_Grants.Page_Offset'Last) or else
           arg4 not in 1 .. Unsigned_64 (Memory_Grants.Page_Count'Last) or else
           arg5 > 1
        then
            return;
        end if;
        cap := ownCap (callerPID, arg0);
        if cap.capType /= Capabilities.CAP_ENDPOINT or else
           not cap.rights (Capabilities.RIGHT_READ) or else
           cap.object.ref = 0 or else
           cap.object.ref > Unsigned_64 (Process.ProcessID'Last)
        then
            return;
        end if;
        Process.IPC.deriveGrant
          ((Memory_Grants.Global_Slot (arg1), Memory_Grants.To_Live_Generation (arg2)),
           Process.ProcessID (cap.object.ref), cap.gen,
           Memory_Grants.Page_Offset (arg3), Memory_Grants.Page_Count (arg4),
           (if arg5 = 1 then Process.GRANT_READWRITE else Process.GRANT_READ),
           derived, success);
        if success then
            retval := Shift_Left (Unsigned_64 (derived.generation), 32) or
              Unsigned_64 (derived.slot);
        end if;
    end handleDeriveGrantViaCap;


    ---------------------------------------------------------------------------
    -- WAIT (ABI-002 slice 2, Wait_ABI): one entry point for every wait.
    -- Wait_Policy decides the form; each form is the primitive the entry
    -- point it replaces used, so the queues, reply authority and wakeups
    -- behave as before.
    ---------------------------------------------------------------------------
    procedure handleWait (sources, deadline, a, b, c, wakeWord : Unsigned_64;
                          retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        package WA renames Wait_ABI;
        use type Wait_Policy.Form;
        caller : constant Process.ProcessID := PerCPUData.getCurrentPID;
        form   : constant Wait_Policy.Form := Wait_Policy.Decide (sources, deadline, b, c);
        from   : Process.ProcessID;
        msg    : Process.Message;
        found  : Boolean;
        ok     : Boolean;

        function identityWord (pid : Process.ProcessID) return Unsigned_64 is
          (if pid = Process.NO_PROCESS then 0
           else Process_Identities.To_Word (Process.identityOf (pid)));

        -- A taken message to a: its sender's identity word (0: the kernel).
        procedure deliver is
        begin
            Process.User_Memory.Write_Message (caller, a, msg, ok);
            retval := (if ok then identityWord (from) else WA.Nothing);
        end deliver;
    begin
        retval := WA.Unsupported_Form;
        case form is
            when Wait_Policy.Unsupported =>
                return;

            when Wait_Policy.Messages_Forever | Wait_Policy.Messages_Until
               | Wait_Policy.Messages_Now | Wait_Policy.Requests_Now =>
                -- a is checked, never dereferenced: a message is taken only
                -- when it can be delivered.
                if not Process.User_Memory.Message_Writable (caller, a) then
                    return;
                end if;
                case form is
                    when Wait_Policy.Messages_Forever =>
                        Process.IPC.receive (from, msg);
                        found := True;
                    when Wait_Policy.Messages_Until =>
                        Process.IPC.receiveUntil (deadline, from, msg, found);
                    when Wait_Policy.Messages_Now =>
                        Process.IPC.receiveAnyIpcNB (from, msg, found);
                    when others =>
                        Process.IPC.receiveServiceRequestNB (from, msg, found);
                end case;
                if found then
                    deliver;
                else
                    retval := WA.Nothing;
                end if;

            when Wait_Policy.Events_Now =>
                if not Process.User_Memory.Message_Writable (caller, a) or else
                   (b /= 0 and then not Process.User_Memory.Writable_Range
                                          (caller, b, Unsigned_64'Size / 8))
                then
                    return;
                end if;
                Process.IPC.receiveEventNB (msg, found, from);
                retval := WA.No_Event;
                if found then
                    Process.User_Memory.Write_Message (caller, a, msg, ok);
                    if ok and then b /= 0 then
                        declare
                            -- The publisher's identity, 0 for the kernel's
                            -- own notices: the sender a receiver
                            -- authenticates.
                            identity : aliased Unsigned_64 := identityWord (from);
                        begin
                            Process.User_Memory.Copy_To_User
                              (caller, b, identity'Address, Unsigned_64'Size / 8, ok);
                        end;
                    end if;
                    retval := (if ok then WA.Taken else WA.No_Event);
                end if;

            when Wait_Policy.Completions_Wait | Wait_Policy.Completions_Now =>
                declare
                    taken : Natural;
                    maxEntries : constant Natural :=
                      (if b > Unsigned_64 (Process.COMPLETION_QUEUE_SIZE)
                       then Process.COMPLETION_QUEUE_SIZE else Natural (b));
                    minEntries : constant Natural :=
                      (if c > Unsigned_64 (maxEntries) then maxEntries else Natural (c));
                begin
                    -- a is written through Process.User_Memory; the whole
                    -- range is checked before anything is taken.
                    Process.IPC.waitCompletion
                      (destination => a, maxEntries => maxEntries,
                       minWait => minEntries, numReturned => taken, ok => ok);
                    if ok then
                        retval := Unsigned_64 (taken);
                    end if;
                end;

            when Wait_Policy.Ready =>
                -- a, when nonzero, receives a pending kernel notice (a
                -- channel wake, grant lifecycle, child exit) in this call.
                if a /= 0 and then not Process.User_Memory.Message_Writable (caller, a) then
                    retval := WA.Ready_Closed;
                    return;
                end if;
                Process.IPC.waitForActivityUntil (deadline, a /= 0, msg, retval);
                if retval = Process.IPC.Activity_Event then
                    Process.User_Memory.Write_Message (caller, a, msg, ok);
                    if not ok then
                        retval := WA.Ready_Closed;
                    end if;
                end if;

            when Wait_Policy.Ready_Word =>
                -- As Ready, and b this process's wake word, c its last value
                -- (IPC-010 option B). A word that is not the caller's own
                -- aligned memory answers Ready_Word at once (a fault reads
                -- as moved), so a loop never sleeps on it.
                if a /= 0 and then not Process.User_Memory.Message_Writable (caller, a) then
                    retval := WA.Ready_Closed;
                    return;
                end if;
                if b = 0 or else b mod 4 /= 0 then
                    retval := WA.Unsupported_Form;
                    return;
                end if;
                Process.IPC.waitForActivityUntil
                  (deadline, a /= 0, msg, retval, wakeWord => b, expected => c);
                if retval = Process.IPC.Activity_Event then
                    Process.User_Memory.Write_Message (caller, a, msg, ok);
                    if not ok then
                        retval := WA.Ready_Closed;
                    end if;
                end if;

            when Wait_Policy.Futex =>
                -- a the word (an address, or a shared word), b the expected
                -- value; the deadline as given; wakeWord the hand-off's
                -- wake (IPC-010).
                retval := Process.Futex.waitForm
                  (shared     => WA.Waits_Shared (sources),
                   word       => a,
                   expected   => b,
                   deadlineMs => deadline,
                   wakeFirst  => WA.Wakes_First (sources),
                   wakeShared => WA.Wakes_Shared (sources),
                   wakeWord   => wakeWord);

            when Wait_Policy.Offered =>
                retval := Process.IPC.acceptOffered
                  (Process.processOfIdentity (Process_Identities.From_Word (a)));

            when Wait_Policy.Timer =>
                -- a: an absolute time on user space's monotonic clock
                -- (Time.Read_User_Monotonic, CuBit.Monotonic). A time
                -- already passed returns at once.
                declare
                    nowUs : Unsigned_64;
                    available : Boolean;
                begin
                    Time.Read_User_Monotonic (nowUs, available);
                    if available and then a > nowUs then
                        Process.sleep (Time.Duration (a - nowUs));
                    end if;
                    retval := 0;
                end;
        end case;
    end handleWait;

end Syscall.IPC;
