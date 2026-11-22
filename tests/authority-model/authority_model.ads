-------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The authority delegation model (CAP-012, docs/authority-model.md): a
--  derivation graph of authority, the operations that may move it, and the
--  invariants every operation keeps, as SPARK contracts.
--
--  @description
--  This is a model, not kernel or service code. A node is one copy of one
--  authority: who holds it, its kind, its scope (a set of objects), its
--  rights, the node it was derived from and how. Nodes are appended, never
--  reused, so a node's parent always comes before it; a node stays in the
--  graph after its holder lets it go (Held False) until it is revoked, so
--  revocation still reaches the copies made through it.
--
--  The configuration is the static part a booted system has: the root
--  issuer of each kind (the kernel's boot placement), the programs'
--  declared requests (manifests), the startup plan's approvals, the IPC
--  layering, and which brokers may carry authority for which programs.
--
--  The operations (each a procedure whose precondition is the rule):
--    Issue_Root   the kernel's boot placement, before Finish_Boot only;
--    Spawn        a new program, authorized by spawn authority over every
--                 process (the wildcard); the spawner gets the child's
--                 control, scoped to that child, with the authority's
--                 rights, and installs into it only if the authority has
--                 Grant; the child's parent (its launcher, which takes its
--                 exit and may control it) is the spawner or a program the
--                 authority holds Grant over (CuBit's SPAWN rules,
--                 kernel/src/spawn_admission.ads);
--    Derive       an attenuated copy in the holder's own table;
--    Install      an attenuated copy into a child before it first runs,
--                 by the child's spawner (the CSPACE rule); into a child
--                 another program launched, only within a node that
--                 launcher holds, rights included (procmgr's
--                 Launch_Attenuation);
--    Introduce    an endpoint copy to a running child, by its spawner
--                 (parent-only brokering, IPC-001);
--    Transfer     a move to another program (the holder's copy is let go);
--    Carry        a copy to a broker for one beneficiary; the broker holds
--                 it but may not exercise or derive it;
--    Deliver      the broker's carried copy to its beneficiary (a move);
--    Close        the holder lets its copy go;
--    Revoke       a holder of a node or of one of its ancestors revokes it
--                 and every copy derived from it;
--    Exercise     use of authority, recorded in the exercise log;
--    Call         a synchronous call, only down the layering.
--
--  Invariants (Invariant, the conjunction; each a named function):
--    no amplification     every derived node is the same kind as its
--                         parent, its scope inside the parent's and its
--                         rights a subset of the parent's (Attenuated);
--    provenance           every node traces through valid parents to a root
--                         the kernel placed with the root issuer (Rooted,
--                         Closed; Lemma_Traces_To_Root), and every node that
--                         can be exercised is held by a program that
--                         declared that kind and that the plan approved
--                         (Exercisable_Only_If_Approved), and so is every
--                         exercise ever made (Exercises_Approved);
--    brokers inert        a carried node is for a declared, approved
--                         beneficiary other than the broker, and is never
--                         exercisable (Carried_For_Beneficiary,
--                         Exercisable);
--    revocation reaches   Revoke's postcondition: after it, a node is valid
--    every copy           exactly when it was valid and did not descend
--                         from the revoked node;
--    calls go down        every recorded synchronous call goes from a
--                         higher layer to a lower one (Calls_Go_Down), so
--                         the call graph has no cycle (Lemma_No_Call_Cycle);
--    spawns admitted      every spawned program came from the wildcard
--                         spawn authority, may be installed into only if
--                         that authority had Grant, and has as parent its
--                         spawner or a program the authority held Grant
--                         over (Spawns_Admitted);
--    within envelope      every node installed into a launched program lies
--                         within a node of its session's envelope (held by
--                         the session's supervisor, its spawner) or within a
--                         node its requester introduces from its own
--                         holdings: same kind, scope and rights inside
--                         (Within_Envelope; docs/authority-model.md 7.5);
--    launch gains nothing a launch (Spawn for a requester, Install or
--                         Introduce into its child) leaves every node the
--                         requester holds unchanged and gives it none
--                         (Launch_Gains_Nothing, the operations' Post); the
--                         requester's one gain is a launch handle, which is
--                         not a node: it names the child and carries none
--                         of the child's authority;
--    handles kept         a launch handle (CAP-012 7.5 item 5) names a child
--                         launched into a session, is held by its requester
--                         or narrowed from such a handle (never wider: Debug
--                         only from a handle with Debug), and revoking one
--                         reaches every handle narrowed from it and every
--                         debug session minted through them (Handles_Kept);
--    debug within envelope a debug session (Debugging, DBG-001) is minted
--                         only by the target's supervisor from the target's
--                         own control, scoped to the target, never with
--                         Grant or Spawn, never on a never-debuggable
--                         program, and only for the holder of a live handle
--                         with Debug on that target or of the session's debug
--                         authority (Debug_Within_Envelope).
--
--  Launch-into-S (docs/authority-model.md 7.5): the child's parent is its
--  supervisor (the spawner, procmgr), not the requester; the requester gets
--  a handle (Spawn's Post).
--
--  Proved by gnatprove (tests/authority-model/run.sh --prove), level 1 or
--  2: every operation keeps Invariant, and the lemmas. Checked exhaustively
--  on small configurations by exhaustive.adb, which also shows that each
--  rule CuBit's kernel or services lack today breaks an invariant.
-------------------------------------------------------------------------------
pragma Ada_2022;

package Authority_Model with SPARK_Mode is

   ---------------------------------------------------------------------------
   --  Programs
   ---------------------------------------------------------------------------
   Program_Bound : constant := 4;
   type Process_Id is range 0 .. Program_Bound;
   --  The kernel: holds nothing in the model; it only places roots.
   Kernel : constant Process_Id := 0;
   subtype Program is Process_Id range 1 .. Program_Bound;

   ---------------------------------------------------------------------------
   --  Kinds of authority (docs/authority-model.md, "Authority kinds")
   ---------------------------------------------------------------------------
   type Kind is
     (Process_Control,   --  spawn, resume, stop, launch arguments
      Hardware,          --  device and firmware memory, ports, lines, vectors, DMA, pool
      Service_Endpoint,  --  a service's endpoint, badged per client
      Data_Grant,        --  a shared-memory region on the data plane
      Observation,       --  logs, metrics, process lists, traces
      Administration,    --  plans, trust lists, settings
      Debugging);        --  a debug session on one process (DBG-001)

   ---------------------------------------------------------------------------
   --  Scopes: sets of objects (a device range, a process, a path prefix)
   ---------------------------------------------------------------------------
   type Object_Set is mod 2 ** 8;
   Everything : constant Object_Set := Object_Set'Last;
   No_Objects : constant Object_Set := 0;

   function Inside (Inner, Outer : Object_Set) return Boolean is
     ((Inner and not Outer) = 0);

   --  The object naming one program (its bit), for process control.
   function Object_Of (P : Program) return Object_Set is
     (case P is
        when 1 => 2#0000_0010#,
        when 2 => 2#0000_0100#,
        when 3 => 2#0000_1000#,
        when 4 => 2#0001_0000#);

   ---------------------------------------------------------------------------
   --  Rights
   ---------------------------------------------------------------------------
   type Right is (Invoke, Grant, Spawn);
   type Rights is array (Right) of Boolean;
   No_Rights : constant Rights := (others => False);

   function Narrower (Inner, Outer : Rights) return Boolean is
     (for all R in Right => (if Inner (R) then Outer (R)));

   ---------------------------------------------------------------------------
   --  The derivation graph
   ---------------------------------------------------------------------------
   Node_Bound : constant := 32;
   type Node_Count is range 0 .. Node_Bound;
   subtype Node is Node_Count range 1 .. Node_Bound;
   No_Node : constant Node_Count := 0;

   type Origin is
     (Root, Derived, Installed, Introduced, Transferred, Carried, Delivered,
      Spawned_Control,
      --  A debug session minted from a child's control by its supervisor.
      Debug_Minted);

   ---------------------------------------------------------------------------
   --  Launch handles (CAP-012 7.5 item 5): not authority nodes. A handle
   --  names one child; its holder may ask the child's supervisor to
   --  observe, stop or (with Debug) debug it.
   ---------------------------------------------------------------------------
   Handle_Bound : constant := 4;
   type Handle_Count is range 0 .. Handle_Bound;
   subtype Handle_Index is Handle_Count range 1 .. Handle_Bound;
   No_Handle : constant Handle_Count := 0;

   type Launch_Handle is record
      Valid  : Boolean    := False;
      Holder : Process_Id := Kernel;
      Child  : Process_Id := Kernel;
      --  Carries the Debug right.
      Debug  : Boolean    := False;
      --  Narrowed from (No_Handle: issued by the launch).
      Parent : Handle_Count := No_Handle;
   end record;
   type Handle_Table is array (Handle_Index) of Launch_Handle;

   type Authority is record
      --  A node of the graph, not revoked.
      Valid       : Boolean    := False;
      --  Its holder still holds it (False after Close, Transfer, Deliver).
      Held        : Boolean    := False;
      Holder      : Process_Id := Kernel;
      Of_Kind     : Kind       := Process_Control;
      Scope       : Object_Set := No_Objects;
      Has         : Rights     := No_Rights;
      Parent      : Node_Count := No_Node;
      How         : Origin     := Root;
      --  Carried: the program it is carried for.
      Beneficiary : Process_Id := Kernel;
      --  Installed into a launched program: the envelope node or the
      --  requester's node it lies within (Within_Envelope).
      Within      : Node_Count := No_Node;
      --  A node of a session supervisor's envelope (Launch-into-S, 7.5),
      --  held by the supervisor (procmgr): what its launches may grant.
      Envelope    : Boolean    := False;
      --  Debug_Minted: the program it debugs, and the launch handle it was
      --  minted through (No_Handle: the session's debug authority).
      Target      : Process_Id := Kernel;
      Via         : Handle_Count := No_Handle;
   end record;

   type Graph is array (Node) of Authority;

   ---------------------------------------------------------------------------
   --  Static configuration
   ---------------------------------------------------------------------------
   type Layer is range 0 .. Program_Bound;
   type Kind_Flags is array (Program, Kind) of Boolean;
   type Issuers is array (Kind) of Program;
   type Layers is array (Program) of Layer;
   type Relation is array (Program, Program) of Boolean;
   type Program_Flags is array (Program) of Boolean;

   type Configuration is record
      Root_Issuer : Issuers;
      --  The program's manifest declares a request of the kind.
      Declared    : Kind_Flags;
      --  The trusted startup plan approves that request.
      Approved    : Kind_Flags;
      --  The IPC layering: synchronous calls go to a lower level only.
      Level       : Layers;
      --  Brokers (B, T): the plan lets B carry authority for T.
      Brokers     : Relation;
      --  Never debuggable (secrets.svc, the trusted path, procmgr, devmgr).
      Never_Debug : Program_Flags;
      --  Holds the session's debug authority (the user's Debugger).
      Session_Debugger : Program_Flags;
   end record;

   --  A program may exercise a kind: declared and approved.
   function May_Exercise (C : Configuration; P : Program; K : Kind) return Boolean is
     (C.Declared (P, K) and then C.Approved (P, K));

   --  The boot policy declares and approves each root issuer's own kinds.
   function Well_Formed (C : Configuration) return Boolean is
     (for all K in Kind => May_Exercise (C, C.Root_Issuer (K), K));

   ---------------------------------------------------------------------------
   --  State
   ---------------------------------------------------------------------------
   type Program_Record is record
      Exists  : Boolean    := False;
      Started : Boolean    := False;
      Spawner : Process_Id := Kernel;
      --  The spawn authority it was spawned through (No_Node: placed at
      --  boot), whether its spawner may install into it (its CSPACE), its
      --  parent, and the program it was launched for (Kernel: the plan).
      Spawned_By  : Node_Count := No_Node;
      Installable : Boolean    := False;
      Parent      : Process_Id := Kernel;
      Launcher    : Process_Id := Kernel;
   end record;
   type Program_Table is array (Program) of Program_Record;
   type Exercise_Log is array (Program, Kind) of Boolean;

   type State is record
      Nodes     : Graph;
      Used      : Node_Count   := No_Node;
      Handles   : Handle_Table;
      Handles_Used : Handle_Count := No_Handle;
      Programs  : Program_Table;
      Exercised : Exercise_Log := (others => (others => False));
      Calls     : Relation     := (others => (others => False));
      Booted    : Boolean      := False;
   end record;

   ---------------------------------------------------------------------------
   --  Invariants
   ---------------------------------------------------------------------------

   --  Nodes past Used are unused; held nodes are valid; held and valid
   --  nodes have a program as holder; parents come first.
   function Well_Kept (S : State) return Boolean is
     ((for all I in Node =>
         (if I > S.Used then not S.Nodes (I).Valid and then not S.Nodes (I).Held))
      and then (for all I in Node => (if S.Nodes (I).Held then S.Nodes (I).Valid))
      and then (for all I in Node =>
                  (if S.Nodes (I).Valid then S.Nodes (I).Holder in Program))
      and then (for all I in Node => S.Nodes (I).Parent < I));

   --  A valid node's parent is valid.
   function Closed (S : State) return Boolean is
     (for all I in Node =>
        (if S.Nodes (I).Valid and then S.Nodes (I).Parent /= No_Node
         then S.Nodes (S.Nodes (I).Parent).Valid));

   --  A node's kind follows its parent's: the same kind, or a debug session
   --  minted from process control.
   function Kind_Follows (Child_Kind, Parent_Kind : Kind; How : Origin) return Boolean is
     (Child_Kind = Parent_Kind
      or else (Child_Kind = Debugging and then How = Debug_Minted
               and then Parent_Kind = Process_Control));

   --  No amplification: a derived node is no wider than its parent.
   function Attenuated (S : State) return Boolean is
     (for all I in Node =>
        (if S.Nodes (I).Valid and then S.Nodes (I).Parent /= No_Node then
           Kind_Follows (S.Nodes (I).Of_Kind, S.Nodes (S.Nodes (I).Parent).Of_Kind,
                         S.Nodes (I).How)
           and then Inside (S.Nodes (I).Scope, S.Nodes (S.Nodes (I).Parent).Scope)
           and then Narrower (S.Nodes (I).Has, S.Nodes (S.Nodes (I).Parent).Has)));

   --  A node without a parent is a root the kernel placed with the issuer.
   function Rooted (C : Configuration; S : State) return Boolean is
     (for all I in Node =>
        (if S.Nodes (I).Valid and then S.Nodes (I).Parent = No_Node then
           S.Nodes (I).How = Root
           and then S.Nodes (I).Holder = C.Root_Issuer (S.Nodes (I).Of_Kind)));

   --  Only root placements have no parent.
   function Roots_Only_At_Top (S : State) return Boolean is
     (for all I in Node =>
        (if S.Nodes (I).Valid and then S.Nodes (I).How = Root
         then S.Nodes (I).Parent = No_Node));

   --  The holder can exercise the node.
   function Exercisable (S : State; I : Node) return Boolean is
     (S.Nodes (I).Valid and then S.Nodes (I).Held
      and then S.Nodes (I).Has (Invoke) and then S.Nodes (I).How /= Carried);

   --  Provenance at the holder: only a declared, approved program can
   --  exercise a node.
   function Exercisable_Only_If_Approved (C : Configuration; S : State) return Boolean is
     (for all I in Node =>
        (if Exercisable (S, I) then
           S.Nodes (I).Holder in Program
           and then May_Exercise (C, S.Nodes (I).Holder, S.Nodes (I).Of_Kind)));

   --  Brokers are inert: a carried node is carried for someone else, who
   --  declared it and was approved, by a broker the plan allows.
   function Carried_For_Beneficiary (C : Configuration; S : State) return Boolean is
     (for all I in Node =>
        (if S.Nodes (I).Valid and then S.Nodes (I).How = Carried then
           S.Nodes (I).Holder in Program
           and then S.Nodes (I).Beneficiary in Program
           and then S.Nodes (I).Beneficiary /= S.Nodes (I).Holder
           and then C.Brokers (S.Nodes (I).Holder, S.Nodes (I).Beneficiary)
           and then May_Exercise (C, S.Nodes (I).Beneficiary, S.Nodes (I).Of_Kind)));

   --  Every exercise made was by a declared, approved program.
   function Exercises_Approved (C : Configuration; S : State) return Boolean is
     (for all P in Program =>
        (for all K in Kind => (if S.Exercised (P, K) then May_Exercise (C, P, K))));

   --  Synchronous calls go down the layering.
   function Calls_Go_Down (C : Configuration; S : State) return Boolean is
     (for all A in Program =>
        (for all B in Program => (if S.Calls (A, B) then C.Level (A) > C.Level (B))));

   --  Every valid node's holder exists, and every launcher and parent.
   function Holders_Exist (S : State) return Boolean is
     ((for all I in Node =>
         (if S.Nodes (I).Valid and then S.Nodes (I).Holder in Program
          then S.Programs (S.Nodes (I).Holder).Exists))
      and then (for all P in Program =>
                  (if S.Programs (P).Exists and then S.Programs (P).Launcher in Program
                   then S.Programs (S.Programs (P).Launcher).Exists))
      and then (for all P in Program => S.Programs (P).Launcher /= P));

   --  The SPAWN rules, as they hold for every spawned program: the spawn
   --  authority is process control with Spawn over every process (rule 1);
   --  the spawner installs into the child only if it had Grant (rule 2; the
   --  control's rights are the authority's: Attenuated); the parent is the
   --  spawner, or the authority had Grant over it (rule 3).
   function Spawns_Admitted (S : State) return Boolean is
     (for all P in Program =>
        (if S.Programs (P).Exists and then S.Programs (P).Spawned_By /= No_Node then
           S.Programs (P).Spawned_By <= S.Used
           and then S.Nodes (S.Programs (P).Spawned_By).Of_Kind = Process_Control
           and then S.Nodes (S.Programs (P).Spawned_By).Has (Spawn)
           and then S.Nodes (S.Programs (P).Spawned_By).Scope = Everything
           and then S.Nodes (S.Programs (P).Spawned_By).Holder = S.Programs (P).Spawner
           and then (if S.Programs (P).Installable
                     then S.Nodes (S.Programs (P).Spawned_By).Has (Grant))
           and then S.Programs (P).Parent in Program
           and then S.Programs (P).Launcher /= P
           and then (S.Programs (P).Parent = S.Programs (P).Spawner
                     or else (S.Nodes (S.Programs (P).Spawned_By).Has (Grant)
                              and then Inside (Object_Of (S.Programs (P).Parent),
                                               S.Nodes (S.Programs (P).Spawned_By).Scope)))));

   --  What a launched program was installed lies within a node of its
   --  session's envelope (held by its spawner, the supervisor) or one its
   --  requester introduces from its own holdings: same kind, scope inside,
   --  rights a subset.
   function Envelope_Or_Requester (S : State; W : Node; P : Program) return Boolean is
     ((S.Nodes (W).Envelope and then S.Nodes (W).Holder = S.Programs (P).Spawner)
      or else S.Nodes (W).Holder = S.Programs (P).Launcher);

   function Within_Envelope_Of (S : State; I : Node) return Boolean is
     (S.Nodes (I).Within /= No_Node and then S.Nodes (I).Within < I
      and then S.Nodes (I).Holder in Program
      and then Envelope_Or_Requester (S, S.Nodes (I).Within, S.Nodes (I).Holder)
      and then S.Nodes (S.Nodes (I).Within).Of_Kind = S.Nodes (I).Of_Kind
      and then Inside (S.Nodes (I).Scope, S.Nodes (S.Nodes (I).Within).Scope)
      and then Narrower (S.Nodes (I).Has, S.Nodes (S.Nodes (I).Within).Has));

   function Within_Envelope (S : State) return Boolean is
     (for all I in Node =>
        (if S.Nodes (I).Valid and then S.Nodes (I).How in Installed | Introduced
           and then S.Nodes (I).Holder in Program
           and then S.Programs (S.Nodes (I).Holder).Launcher in Program
         then Within_Envelope_Of (S, I)));

   --  Launch_Gains_Nothing between two states: every node Requester held
   --  is unchanged, and no new node is Requester's.
   function Gains_Nothing (Old, New_State : State; Requester : Process_Id) return Boolean is
     ((for all I in Node =>
         (if I <= Old.Used and then Old.Nodes (I).Holder = Requester
          then New_State.Nodes (I) = Old.Nodes (I)))
      and then (for all I in Node =>
                  (if I > Old.Used and then I <= New_State.Used
                   then New_State.Nodes (I).Holder /= Requester)));

   --  Launch handles: past Handles_Used unused; each valid one names a child
   --  launched for a requester and is that requester's, or narrowed from a
   --  valid handle on the same child (Debug only from Debug).
   function Handles_Kept (S : State) return Boolean is
     ((for all H in Handle_Index => (if H > S.Handles_Used then not S.Handles (H).Valid))
      and then
      (for all H in Handle_Index =>
         (if S.Handles (H).Valid then
            S.Handles (H).Holder in Program
            and then S.Programs (S.Handles (H).Holder).Exists
            and then S.Handles (H).Child in Program
            and then S.Programs (S.Handles (H).Child).Exists
            and then S.Programs (S.Handles (H).Child).Launcher in Program
            and then S.Handles (H).Parent < H
            and then (if S.Handles (H).Parent = No_Handle then
                        S.Handles (H).Holder = S.Programs (S.Handles (H).Child).Launcher
                      else
                        S.Handles (S.Handles (H).Parent).Valid
                        and then S.Handles (S.Handles (H).Parent).Child = S.Handles (H).Child
                        and then (if S.Handles (H).Debug
                                  then S.Handles (S.Handles (H).Parent).Debug)))));

   --  A debug session lies within the envelope (docs/debugging.md 2.7):
   --  minted from the target's own control, held by its supervisor (the
   --  spawner), scoped to the target, no Grant or Spawn, never on a
   --  never-debuggable program, and for the holder of a live handle with
   --  Debug on that target or of the session's debug authority.
   function Debug_Minted_Ok (C : Configuration; S : State; I : Node) return Boolean is
     (S.Nodes (I).How = Debug_Minted
      and then S.Nodes (I).Parent /= No_Node and then S.Nodes (I).Parent < I
      and then S.Nodes (S.Nodes (I).Parent).How = Spawned_Control
      and then S.Nodes (I).Target in Program
      and then S.Programs (S.Nodes (I).Target).Exists
      and then S.Nodes (I).Scope = Object_Of (S.Nodes (I).Target)
      and then S.Nodes (S.Nodes (I).Parent).Holder = S.Programs (S.Nodes (I).Target).Spawner
      and then not C.Never_Debug (S.Nodes (I).Target)
      and then not S.Nodes (I).Has (Grant) and then not S.Nodes (I).Has (Spawn)
      and then (if S.Nodes (I).Via /= No_Handle then
                  S.Handles (S.Nodes (I).Via).Valid
                  and then S.Handles (S.Nodes (I).Via).Debug
                  and then S.Handles (S.Nodes (I).Via).Child = S.Nodes (I).Target
                  and then S.Handles (S.Nodes (I).Via).Holder = S.Nodes (I).Holder
                else S.Nodes (I).Holder in Program
                     and then C.Session_Debugger (S.Nodes (I).Holder)));

   function Debug_Within_Envelope (C : Configuration; S : State) return Boolean is
     (for all I in Node =>
        (if S.Nodes (I).Valid and then (S.Nodes (I).Of_Kind = Debugging
                                        or else S.Nodes (I).How = Debug_Minted)
         then S.Nodes (I).Of_Kind = Debugging and then Debug_Minted_Ok (C, S, I)));

   function Invariant (C : Configuration; S : State) return Boolean is
     (Well_Kept (S) and then Closed (S) and then Attenuated (S)
      and then Rooted (C, S) and then Roots_Only_At_Top (S)
      and then Exercisable_Only_If_Approved (C, S)
      and then Carried_For_Beneficiary (C, S)
      and then Exercises_Approved (C, S)
      and then Calls_Go_Down (C, S)
      and then Holders_Exist (S)
      and then Spawns_Admitted (S)
      and then Within_Envelope (S)
      and then Handles_Kept (S)
      and then Debug_Within_Envelope (C, S));

   ---------------------------------------------------------------------------
   --  Derivation chains
   ---------------------------------------------------------------------------

   --  I is A or was derived, directly or through others, from A.
   function Descends (G : Graph; I, A : Node) return Boolean is
     (I = A
      or else (G (I).Parent /= No_Node and then G (I).Parent < I
               and then Descends (G, G (I).Parent, A)))
   with Subprogram_Variant => (Decreases => I);

   --  I's chain of parents is valid and ends at a root the kernel placed.
   function Traces_To_Root (C : Configuration; G : Graph; I : Node) return Boolean is
     (G (I).Valid
      and then (if G (I).Parent = No_Node then
                  G (I).How = Root and then G (I).Holder = C.Root_Issuer (G (I).Of_Kind)
                else G (I).Parent < I
                     and then Kind_Follows (G (I).Of_Kind, G (G (I).Parent).Of_Kind, G (I).How)
                     and then Traces_To_Root (C, G, G (I).Parent)))
   with Subprogram_Variant => (Decreases => I);

   --  A descendant is never earlier than its ancestor.
   procedure Lemma_Descends_Later (G : Graph; I, A : Node)
   with Ghost, Global => null,
        Pre  => Descends (G, I, A),
        Post => I >= A,
        Subprogram_Variant => (Decreases => I);

   --  So nothing earlier than A descends from it.
   procedure Lemma_None_Before (G : Graph; A : Node)
   with Ghost, Global => null,
        Post => (for all J in Node => (if J < A then not Descends (G, J, A)));

   --  Provenance: every valid node traces back to a root.
   procedure Lemma_Traces_To_Root (C : Configuration; S : State; I : Node)
   with Ghost, Global => null,
        Pre  => Invariant (C, S) and then S.Nodes (I).Valid,
        Post => Traces_To_Root (C, S.Nodes, I),
        Subprogram_Variant => (Decreases => I);

   --  B is reached from A by Steps synchronous calls.
   subtype Steps is Natural range 1 .. Program_Bound + 1;
   function Reaches (S : State; A, B : Program; N : Steps) return Boolean is
     (if N = 1 then S.Calls (A, B)
      else (for some M in Program => S.Calls (A, M) and then Reaches (S, M, B, N - 1)))
   with Subprogram_Variant => (Decreases => N);

   procedure Lemma_Calls_Descend (C : Configuration; S : State; A, B : Program; N : Steps)
   with Ghost, Global => null,
        Pre  => Calls_Go_Down (C, S) and then Reaches (S, A, B, N),
        Post => C.Level (A) > C.Level (B),
        Subprogram_Variant => (Decreases => N);

   --  The synchronous call graph has no cycle.
   procedure Lemma_No_Call_Cycle (C : Configuration; S : State; A : Program; N : Steps)
   with Ghost, Global => null,
        Pre  => Calls_Go_Down (C, S),
        Post => not Reaches (S, A, A, N);

   ---------------------------------------------------------------------------
   --  Narrowing a copy
   ---------------------------------------------------------------------------
   type Narrowing is (Same, Without_Grant, Fewer_Objects);

   --  Fewer_Objects keeps the lowest object of the scope.
   function Lowest (Scope : Object_Set) return Object_Set is
     (Scope and (not Scope + 1));

   function Scope_After (V : Narrowing; Scope : Object_Set) return Object_Set is
     (if V = Fewer_Objects then Lowest (Scope) else Scope);

   function Rights_After (V : Narrowing; Has : Rights) return Rights is
     (if V = Without_Grant then (Has with delta Grant => False) else Has);

   procedure Lemma_Narrowing (V : Narrowing; Scope : Object_Set; Has : Rights)
   with Ghost, Global => null,
        Post => Inside (Scope_After (V, Scope), Scope)
                and then Narrower (Rights_After (V, Has), Has);

   ---------------------------------------------------------------------------
   --  Admission rules: when each operation is allowed
   ---------------------------------------------------------------------------
   function Room (S : State) return Boolean is (S.Used < Node_Bound);

   function Live (S : State; I : Node) return Boolean is
     (I <= S.Used and then S.Nodes (I).Valid and then S.Nodes (I).Held);

   function May_Issue_Root (C : Configuration; S : State; K : Kind) return Boolean is
     (not S.Booted and then Room (S) and then K /= Debugging
      and then S.Programs (C.Root_Issuer (K)).Exists);

   --  Spawn authority is exercisable process control with the Spawn right
   --  over every process: a child's own control (scoped to that child)
   --  never spawns (the RIGHT_EXECUTE finding, rule 1). A child launched
   --  for another program (Launcher, which becomes its parent) needs Grant
   --  over that program (rule 3); Kernel: the plan's, the spawner its parent.
   --  Launch-into-S: the child's supervisor S is the spawner (procmgr), its
   --  parent; the requester (Launcher) gets a launch handle instead.
   function Parent_For (S : State; By : Node) return Process_Id is
     (S.Nodes (By).Holder);

   function Handle_Room (S : State) return Boolean is (S.Handles_Used < Handle_Bound);

   function May_Spawn (S : State; By : Node; Child : Program; Launcher : Process_Id := Kernel)
     return Boolean is
     (Room (S) and then Live (S, By) and then Exercisable (S, By)
      and then S.Nodes (By).Of_Kind = Process_Control
      and then S.Nodes (By).Has (Spawn)
      and then S.Nodes (By).Scope = Everything
      and then not S.Programs (Child).Exists
      and then (if Launcher in Program then
                  S.Programs (Launcher).Exists and then Launcher /= Child
                  and then Handle_Room (S)
                  and then (Launcher = S.Nodes (By).Holder
                            or else (S.Nodes (By).Has (Grant)
                                     and then Inside (Object_Of (Launcher), S.Nodes (By).Scope)))));

   --  A copy may be made from a held, uncarried node with Grant.
   function May_Copy (S : State; From : Node) return Boolean is
     (Room (S) and then Live (S, From) and then S.Nodes (From).How /= Carried
      and then S.Nodes (From).Has (Grant));

   function May_Derive (S : State; From : Node) return Boolean is (May_Copy (S, From));

   --  Into a launched child, a copy lies within a live envelope node of its
   --  session (held by its supervisor) or a live node its requester
   --  introduces: by kind, scope and rights, not by kind alone.
   function Covered_By_Launcher
     (S : State; From : Node; Target : Program; V : Narrowing; Within : Node_Count)
      return Boolean is
     (if S.Programs (Target).Launcher in Program then
        Within /= No_Node and then Within <= S.Used and then Live (S, Within)
        and then Envelope_Or_Requester (S, Within, Target)
        and then S.Nodes (Within).Of_Kind = S.Nodes (From).Of_Kind
        and then Inside (Scope_After (V, S.Nodes (From).Scope), S.Nodes (Within).Scope)
        and then Narrower (Rights_After (V, S.Nodes (From).Has), S.Nodes (Within).Has)
      else Within = No_Node);

   function May_Install
     (C : Configuration; S : State; From : Node; Target : Program;
      V : Narrowing := Same; Within : Node_Count := No_Node) return Boolean is
     (May_Copy (S, From)
      and then S.Programs (Target).Exists and then not S.Programs (Target).Started
      and then S.Programs (Target).Spawner = S.Nodes (From).Holder
      and then S.Programs (Target).Installable
      and then May_Exercise (C, Target, S.Nodes (From).Of_Kind)
      and then Covered_By_Launcher (S, From, Target, V, Within));

   function May_Introduce
     (C : Configuration; S : State; From : Node; Target : Program;
      V : Narrowing := Same; Within : Node_Count := No_Node) return Boolean is
     (May_Copy (S, From) and then S.Nodes (From).Of_Kind = Service_Endpoint
      and then S.Programs (Target).Exists and then S.Programs (Target).Started
      and then S.Programs (Target).Spawner = S.Nodes (From).Holder
      and then May_Exercise (C, Target, S.Nodes (From).Of_Kind)
      and then Covered_By_Launcher (S, From, Target, V, Within));

   function May_Transfer (C : Configuration; S : State; From : Node; Target : Program)
     return Boolean is
     (May_Copy (S, From) and then S.Programs (Target).Exists
      and then Target /= S.Nodes (From).Holder
      and then May_Exercise (C, Target, S.Nodes (From).Of_Kind));

   function May_Carry
     (C : Configuration; S : State; From : Node; Broker, Beneficiary : Program)
      return Boolean is
     (May_Copy (S, From) and then S.Programs (Broker).Exists
      and then Broker /= Beneficiary and then C.Brokers (Broker, Beneficiary)
      and then May_Exercise (C, Beneficiary, S.Nodes (From).Of_Kind));

   function May_Deliver (S : State; Item : Node) return Boolean is
     (Room (S) and then Live (S, Item) and then S.Nodes (Item).How = Carried
      and then S.Nodes (Item).Beneficiary in Program
      and then S.Programs (S.Nodes (Item).Beneficiary).Exists);

   --  The revoker holds the node or one of its ancestors.
   function May_Revoke (S : State; Via, Target : Node) return Boolean is
     (Live (S, Via) and then Target <= S.Used and then S.Nodes (Target).Valid
      and then Descends (S.Nodes, Target, Via));

   function May_Call (C : Configuration; S : State; Caller, Callee : Program) return Boolean is
     (S.Programs (Caller).Exists and then S.Programs (Callee).Exists
      and then C.Level (Caller) > C.Level (Callee));

   ---------------------------------------------------------------------------
   --  Operations
   ---------------------------------------------------------------------------

   --  An empty system: the root issuers of the configuration exist.
   procedure Boot (C : Configuration; S : out State)
   with Pre  => Well_Formed (C),
        Post => Invariant (C, S) and then not S.Booted and then S.Used = No_Node
                and then (for all K in Kind => S.Programs (C.Root_Issuer (K)).Exists);

   procedure Issue_Root (C : Configuration; S : in out State; K : Kind; Scope : Object_Set; Has : Rights)
   with Pre  => Well_Formed (C) and then Invariant (C, S) and then May_Issue_Root (C, S, K),
        Post => Invariant (C, S) and then S.Used = S.Used'Old + 1;

   procedure Finish_Boot (C : Configuration; S : in out State)
   with Pre  => Invariant (C, S),
        Post => Invariant (C, S) and then S.Booted;

   procedure Spawn
     (C : Configuration; S : in out State; By : Node; Child : Program;
      Launcher : Process_Id := Kernel)
   with Pre  => Invariant (C, S) and then May_Spawn (S, By, Child, Launcher),
        Post => Invariant (C, S) and then S.Programs (Child).Exists
                and then not S.Programs (Child).Started
                and then S.Nodes (S.Used).Scope = Object_Of (Child)
                and then S.Nodes (S.Used).Has = S.Nodes (By).Has
                and then S.Programs (Child).Installable = S.Nodes (By).Has (Grant)
                and then S.Programs (Child).Parent = Parent_For (S'Old, By)
                and then (if Launcher in Program and then Launcher /= S'Old.Nodes (By).Holder
                          then Gains_Nothing (S'Old, S, Launcher))
                --  The requester's one gain: a handle naming the child.
                and then (if Launcher in Program then
                            S.Handles_Used = S'Old.Handles_Used + 1
                            and then S.Handles (S.Handles_Used) =
                                       (Valid => True, Holder => Launcher, Child => Child,
                                        Debug => True, Parent => No_Handle)
                          else S.Handles = S'Old.Handles
                               and then S.Handles_Used = S'Old.Handles_Used);

   procedure Start (C : Configuration; S : in out State; P : Program)
   with Pre  => Invariant (C, S) and then S.Programs (P).Exists,
        Post => Invariant (C, S) and then S.Programs (P).Started;

   --  Envelope: the copy is a node of a session supervisor's envelope.
   procedure Derive
     (C : Configuration; S : in out State; From : Node; V : Narrowing;
      Envelope : Boolean := False)
   with Pre  => Invariant (C, S) and then May_Derive (S, From),
        Post => Invariant (C, S);

   procedure Install
     (C : Configuration; S : in out State; From : Node; Target : Program; V : Narrowing;
      Within : Node_Count := No_Node)
   with Pre  => Invariant (C, S) and then May_Install (C, S, From, Target, V, Within),
        Post => Invariant (C, S)
                and then (if S'Old.Programs (Target).Launcher in Program
                            and then S'Old.Programs (Target).Launcher /= S'Old.Nodes (From).Holder
                          then Gains_Nothing (S'Old, S, S'Old.Programs (Target).Launcher));

   procedure Introduce
     (C : Configuration; S : in out State; From : Node; Target : Program; V : Narrowing;
      Within : Node_Count := No_Node)
   with Pre  => Invariant (C, S) and then May_Introduce (C, S, From, Target, V, Within),
        Post => Invariant (C, S)
                and then (if S'Old.Programs (Target).Launcher in Program
                            and then S'Old.Programs (Target).Launcher /= S'Old.Nodes (From).Holder
                          then Gains_Nothing (S'Old, S, S'Old.Programs (Target).Launcher));

   procedure Transfer (C : Configuration; S : in out State; From : Node; Target : Program)
   with Pre  => Invariant (C, S) and then May_Transfer (C, S, From, Target),
        Post => Invariant (C, S) and then not S.Nodes (From).Held;

   procedure Carry
     (C : Configuration; S : in out State; From : Node; Broker, Beneficiary : Program)
   with Pre  => Invariant (C, S) and then May_Carry (C, S, From, Broker, Beneficiary),
        Post => Invariant (C, S) and then not Exercisable (S, S.Used);

   procedure Deliver (C : Configuration; S : in out State; Item : Node)
   with Pre  => Invariant (C, S) and then May_Deliver (S, Item),
        Post => Invariant (C, S) and then not S.Nodes (Item).Held;

   procedure Close (C : Configuration; S : in out State; Item : Node)
   with Pre  => Invariant (C, S) and then Live (S, Item),
        Post => Invariant (C, S) and then not S.Nodes (Item).Held;

   --  Revocation reaches every copy derived from Target, and nothing else.
   procedure Revoke (C : Configuration; S : in out State; Via, Target : Node)
   with Pre  => Invariant (C, S) and then May_Revoke (S, Via, Target),
        Post => Invariant (C, S)
                and then S.Used = S.Used'Old
                and then (for all I in Node =>
                            S.Nodes (I).Valid =
                              (S'Old.Nodes (I).Valid
                               and then not Descends (S'Old.Nodes, I, Target)))
                and then (for all I in Node =>
                            (if Descends (S'Old.Nodes, I, Target)
                             then not S.Nodes (I).Valid and then not S.Nodes (I).Held));

   procedure Exercise (C : Configuration; S : in out State; Item : Node)
   with Pre  => Invariant (C, S) and then Item <= S.Used and then Exercisable (S, Item),
        Post => Invariant (C, S);

   procedure Call (C : Configuration; S : in out State; Caller, Callee : Program)
   with Pre  => Invariant (C, S) and then May_Call (C, S, Caller, Callee),
        Post => Invariant (C, S);

   ---------------------------------------------------------------------------
   --  Launch handles and debug sessions
   ---------------------------------------------------------------------------
   function Handle_Live (S : State; H : Handle_Count) return Boolean is
     (H /= No_Handle and then H <= S.Handles_Used and then S.Handles (H).Valid);

   --  H is A or was narrowed, directly or through others, from A.
   function Handle_Descends (T : Handle_Table; H, A : Handle_Index) return Boolean is
     (H = A
      or else (T (H).Parent /= No_Handle and then T (H).Parent < H
               and then Handle_Descends (T, T (H).Parent, A)))
   with Subprogram_Variant => (Decreases => H);

   --  A narrower handle on the same child, for To (passed on).
   function May_Narrow (S : State; From : Handle_Count; Debug : Boolean; To : Program)
     return Boolean is
     (Handle_Live (S, From) and then Handle_Room (S)
      and then S.Programs (To).Exists
      and then (if Debug then S.Handles (From).Debug));

   procedure Narrow_Handle
     (C : Configuration; S : in out State; From : Handle_Index; Debug : Boolean; To : Program)
   with Pre  => Invariant (C, S) and then May_Narrow (S, From, Debug, To),
        Post => Invariant (C, S) and then S.Nodes = S'Old.Nodes and then S.Used = S'Old.Used
                and then S.Handles_Used = S'Old.Handles_Used + 1;

   --  Revoking a handle reaches every handle narrowed from it and every
   --  debug session minted through any of them, and nothing else.
   procedure Revoke_Handle (C : Configuration; S : in out State; H : Handle_Index)
   with Pre  => Invariant (C, S) and then Handle_Live (S, H),
        Post => Invariant (C, S)
                and then S.Handles_Used = S'Old.Handles_Used and then S.Used = S'Old.Used
                and then (for all G in Handle_Index =>
                            S.Handles (G).Valid =
                              (S'Old.Handles (G).Valid
                               and then not Handle_Descends (S'Old.Handles, G, H)))
                and then (for all I in Node =>
                            S.Nodes (I).Valid =
                              (S'Old.Nodes (I).Valid
                               and then not (S'Old.Nodes (I).How = Debug_Minted
                                             and then S'Old.Nodes (I).Via /= No_Handle
                                             and then Handle_Descends
                                                        (S'Old.Handles, S'Old.Nodes (I).Via, H))));

   --  The supervisor mints a debug session on Target from Target's own
   --  control (Control), for Holder: through Via, a live handle with Debug
   --  on Target that Holder holds, or (Via = No_Handle) the session's debug
   --  authority.
   function May_Mint_Debug
     (C : Configuration; S : State; Control : Node; Target, Holder : Program; Via : Handle_Count)
      return Boolean is
     (Room (S) and then Live (S, Control)
      and then S.Nodes (Control).How = Spawned_Control
      and then S.Nodes (Control).Of_Kind = Process_Control
      and then S.Nodes (Control).Has (Invoke)
      and then S.Programs (Target).Exists
      and then S.Nodes (Control).Scope = Object_Of (Target)
      and then S.Nodes (Control).Holder = S.Programs (Target).Spawner
      and then not C.Never_Debug (Target)
      and then S.Programs (Holder).Exists
      and then May_Exercise (C, Holder, Debugging)
      and then (if Via /= No_Handle then
                  Handle_Live (S, Via) and then S.Handles (Via).Debug
                  and then S.Handles (Via).Child = Target
                  and then S.Handles (Via).Holder = Holder
                else C.Session_Debugger (Holder)));

   procedure Mint_Debug
     (C : Configuration; S : in out State; Control : Node; Target, Holder : Program;
      Via : Handle_Count)
   with Pre  => Invariant (C, S) and then May_Mint_Debug (C, S, Control, Target, Holder, Via),
        Post => Invariant (C, S) and then S.Used = S'Old.Used + 1
                and then S.Nodes (S.Used).Of_Kind = Debugging
                and then S.Nodes (S.Used).Holder = Holder
                and then S.Nodes (S.Used).Scope = Object_Of (Target)
                and then S.Nodes (S.Used).Parent = Control
                and then not S.Nodes (S.Used).Has (Grant)
                and then S.Handles = S'Old.Handles;

   ---------------------------------------------------------------------------
   --  Classifying a real path (the inventory in model.ccl)
   ---------------------------------------------------------------------------
   type Fact is
     (Enforced,   --  the kernel or a proved unit guarantees it
      Observed,   --  holds by reading the code or by a test, not enforced
      Fails,      --  does not hold
      Unknown);   --  not established
   type Invariant_Name is
     (Provenance, No_Amplification, Broker_Inert, Revocation, Call_Order);
   type Facts is array (Invariant_Name) of Fact;
   type Verdict is (Fits, Violates, Unclear);

   function Classify (F : Facts) return Verdict is
     (if (for some I in Invariant_Name => F (I) = Fails) then Violates
      elsif (for some I in Invariant_Name => F (I) = Unknown) then Unclear
      else Fits)
   with Post =>
     (Classify'Result = Fits) =
       (for all I in Invariant_Name => F (I) in Enforced | Observed)
     and then (Classify'Result = Violates) =
       (for some I in Invariant_Name => F (I) = Fails);

end Authority_Model;
