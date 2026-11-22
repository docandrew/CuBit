-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- SPAWN's admission rules (CAP-012, docs/authority-model.md section 7.3):
-- which CAP_PROCESS may spawn, whom it may name as the child's parent, and
-- what the spawner's handles for the child carry.
--
-- 1. Spawn authority is the wildcard: a CAP_PROCESS with Execute that names
--    no single process (object.ref = 0). A child's Control (object.ref = that
--    child) never spawns, whatever its rights; Execute on it still resumes
--    that child and gives it launch arguments (RESUME, LAUNCH-ARGS).
-- 2. No amplification: the spawner's Control over the child carries exactly
--    the authority's rights (at most Read, Write, Execute and Grant); the
--    child's untagged endpoint (at most Read, Write and Grant) and its CSPACE
--    are given only when the authority has Grant, since both exist to pass
--    capabilities to the child.
-- 3. The parent is the caller, or a process the authority holds Grant over
--    (that process or every process). The parent receives the child's exit
--    and may send it control messages (SEND_CONTROL), so naming one is
--    giving it authority.
--
-- Pure decisions over capability snapshots; syscall-ipc.adb's handleSpawn
-- applies them. Proved with tests/spawn-admission.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;

package Spawn_Admission with SPARK_Mode is

    -- CAP_PROCESS's object.ref naming every process.
    Every_Process : constant Unsigned_64 := 0;

    -- The rights the spawner's handles may carry at most.
    Control_Ceiling : constant CapabilityRights :=
        (RIGHT_READ | RIGHT_WRITE | RIGHT_EXECUTE | RIGHT_GRANT => True,
         others => False);
    Endpoint_Ceiling : constant CapabilityRights :=
        (RIGHT_READ | RIGHT_WRITE | RIGHT_GRANT => True, others => False);
    Cspace_Rights : constant CapabilityRights :=
        (RIGHT_GRANT => True, others => False);

    ---------------------------------------------------------------------------
    -- Rule 1
    ---------------------------------------------------------------------------
    function Is_Spawn_Authority (Authority : Capability) return Boolean is
        (Authority.capType = CAP_PROCESS
         and then Authority.rights (RIGHT_EXECUTE)
         and then Authority.object.ref = Every_Process);

    ---------------------------------------------------------------------------
    -- Rule 3
    ---------------------------------------------------------------------------
    -- Authority holds Grant over the live process (Ref, Gen): the wildcard,
    -- or a handle naming exactly that process's current life.
    function Holds_Grant_Over
        (Authority : Capability; Ref : Unsigned_64; Gen : Generation)
        return Boolean is
        (Authority.capType = CAP_PROCESS
         and then Authority.rights (RIGHT_GRANT)
         and then (Authority.object.ref = Every_Process
                   or else (Authority.object.ref = Ref
                            and then Authority.gen = Gen)));

    function Parent_Admitted
        (Authority : Capability; Parent_Is_Caller : Boolean;
         Parent_Ref : Unsigned_64; Parent_Gen : Generation) return Boolean is
        (Parent_Is_Caller or else Holds_Grant_Over (Authority, Parent_Ref, Parent_Gen));

    ---------------------------------------------------------------------------
    -- Rule 2
    ---------------------------------------------------------------------------
    function Control_Rights (Authority : Capability) return CapabilityRights is
        (Authority.rights and Control_Ceiling)
    with Post => isSubsetOf (Control_Rights'Result, Authority.rights)
                 and then isSubsetOf (Control_Rights'Result, Control_Ceiling);

    -- The child's endpoint and CSPACE are given.
    function Gives_Mailbox (Authority : Capability) return Boolean is
        (Authority.rights (RIGHT_GRANT));

    function Endpoint_Rights (Authority : Capability) return CapabilityRights is
        (Authority.rights and Endpoint_Ceiling)
    with Post => isSubsetOf (Endpoint_Rights'Result, Authority.rights)
                 and then isSubsetOf (Endpoint_Rights'Result, Endpoint_Ceiling);

    -- Whatever SPAWN hands back is within the authority: every handle's
    -- rights are a subset of the authority's.
    function Within_Authority (Authority : Capability) return Boolean is
        (isSubsetOf (Control_Rights (Authority), Authority.rights)
         and then (if Gives_Mailbox (Authority) then
                      isSubsetOf (Endpoint_Rights (Authority), Authority.rights)
                      and then isSubsetOf (Cspace_Rights, Authority.rights)));

    -- Stated for the prover: every authority's handles are within it.
    procedure Lemma_Within (Authority : Capability)
    with Ghost, Global => null, Post => Within_Authority (Authority);

end Spawn_Admission;
