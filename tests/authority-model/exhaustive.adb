--  Exhaustive check of the authority model (CAP-012) on a small
--  configuration: every sequence of operations up to a depth bound, from
--  the booted state, with every invariant checked after every step, by
--  the model's own predicates and by independent re-computations here.
--
--  It then runs each rule CuBit lacks today as a mutant and requires a
--  counterexample for each (mutation discrimination), and replays the
--  two spawn placements of CAP-012 as scenarios.
--
--  The search is bounded (Depth_Bound, State_Bound): it reports "bound
--  reached" rather than growing. run.sh runs it under a memory cap.
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Text_IO; use Ada.Text_IO;
with Authority_Model; use Authority_Model;

procedure Exhaustive is

   --  The configuration: devmgr is the kernel's root issuer of every kind,
   --  procmgr a broker for the driver and the app, the app declared
   --  hardware without the plan's approval.
   Devmgr  : constant Program := 1;
   Procmgr : constant Program := 2;
   Driver  : constant Program := 3;
   App     : constant Program := 4;

   Config : Configuration :=
     (Root_Issuer => (others => Devmgr),
      Declared    => (others => (others => False)),
      Approved    => (others => (others => False)),
      Level       => (Devmgr => 2, Procmgr => 3, Driver => 1, App => 4),
      Brokers     => (others => (others => False)),
      --  devmgr and procmgr are never debuggable; the app holds the
      --  session's debug authority.
      Never_Debug => (Devmgr | Procmgr => True, others => False),
      Session_Debugger => (App => True, others => False));

   --  The kinds the search places roots for.
   Checked_Kinds : constant array (1 .. 3) of Kind :=
     (Process_Control, Hardware, Service_Endpoint);

   Depth_Bound : Natural := 6;
   --  At most this many distinct states (argument 2; run.sh --deep raises it).
   State_Bound : Natural := 3_000_000;
   Node_Limit  : constant Node_Count := 8;

   ---------------------------------------------------------------------------
   --  Operations
   ---------------------------------------------------------------------------
   type Op_Kind is
     (Op_Spawn, Op_Start, Op_Derive, Op_Install, Op_Introduce, Op_Transfer,
      Op_Carry, Op_Deliver, Op_Close, Op_Revoke, Op_Exercise, Op_Call,
      --  Mutants: rules CuBit lacked (the first three SPAWN rules, launch
      --  attenuation by kind alone) or still lacks today.
      Op_Spawn_Any_Control, Op_Spawn_Amplifying, Op_Spawn_Any_Parent,
      Op_Install_By_Kind, Op_Launch_Leaking,
      Op_Revoke_One, Op_Deliver_Keeping, Op_Call_Up,
      Op_Mint, Op_Install_Unapproved,
      --  Launch handles and debug sessions (CAP-012 7.5 item 5, DBG-001).
      Op_Narrow_Handle, Op_Revoke_Handle, Op_Mint_Debug,
      --  Their mutants: a mint on a never-debuggable program, through a
      --  handle without Debug, or carrying Grant; a revocation that leaves
      --  narrowed handles and their sessions; a narrowing that adds Debug.
      Op_Mint_Debug_Never, Op_Mint_Debug_Unrighted, Op_Mint_Debug_Granting,
      Op_Revoke_Handle_Shallow, Op_Narrow_Amplifying);

   type Operation is record
      What : Op_Kind := Op_Start;
      N, M : Node_Count := No_Node;
      P, Q : Process_Id := Kernel;
      V    : Narrowing := Same;
      K    : Kind := Process_Control;
      E    : Boolean := False;
      --  A launch handle, and its Debug right (narrowing).
      H    : Handle_Count := No_Handle;
      D    : Boolean := False;
   end record;

   function Image (O : Operation) return String is
     (O.What'Image & " n" & O.N'Image & " m" & O.M'Image & " p" & O.P'Image &
      " q" & O.Q'Image & " " & O.V'Image & " " & O.K'Image &
      (if O.E then " envelope" else "") &
      (if O.H /= No_Handle then " handle" & O.H'Image else "") &
      (if O.D then " debug" else ""));

   type Rule is
     (Model, Spawn_With_Any_Control, Spawn_Handles_Amplify, Parent_Without_Grant,
      Launch_By_Kind, Requester_Gains, Revoke_Without_Tree, Broker_Keeps_Copy,
      Calls_Either_Way, Mint_After_Boot, Install_Without_Approval,
      Debug_Never_Debuggable, Debug_Without_Right, Debug_With_Grant,
      Handle_Revoke_Shallow, Handle_Narrow_Amplifies);

   ---------------------------------------------------------------------------
   --  Independent checks (not the model's predicates)
   ---------------------------------------------------------------------------

   function Ancestor (S : State; I, A : Node) return Boolean is
      J : Node_Count := I;
   begin
      while J /= No_Node loop
         if J = A then
            return True;
         end if;
         exit when S.Nodes (J).Parent >= J;
         J := S.Nodes (J).Parent;
      end loop;
      return False;
   end Ancestor;

   --  Every valid node walks through valid parents of its kind, never
   --  wider, to a root the issuer holds.
   function Chains_Ok (S : State) return Boolean is
   begin
      for I in 1 .. S.Used loop
         if S.Nodes (I).Valid then
            declare
               J : Node := I;
            begin
               loop
                  declare
                     P : constant Node_Count := S.Nodes (J).Parent;
                  begin
                     if P = No_Node then
                        if S.Nodes (J).How /= Root
                          or else S.Nodes (J).Holder /= Config.Root_Issuer (S.Nodes (J).Of_Kind)
                        then
                           return False;
                        end if;
                        exit;
                     end if;
                     if P >= J or else not S.Nodes (P).Valid
                       or else not (S.Nodes (P).Of_Kind = S.Nodes (J).Of_Kind
                                    or else (S.Nodes (J).Of_Kind = Debugging
                                             and then S.Nodes (J).How = Debug_Minted
                                             and then S.Nodes (P).Of_Kind = Process_Control))
                       or else (S.Nodes (J).Scope and not S.Nodes (P).Scope) /= 0
                       or else (for some R in Right => S.Nodes (J).Has (R) and not S.Nodes (P).Has (R))
                     then
                        return False;
                     end if;
                     J := P;
                  end;
               end loop;
            end;
         end if;
      end loop;
      return True;
   end Chains_Ok;

   --  Nothing a program can exercise, or has exercised, lacks a declared
   --  request and the plan's approval; no broker can exercise what it carries.
   function Exercise_Ok (S : State) return Boolean is
   begin
      for I in 1 .. S.Used loop
         declare
            X : Authority renames S.Nodes (I);
         begin
            if X.Valid and then X.Held and then X.Has (Invoke) and then X.How /= Carried
              and then not (X.Holder in Program
                            and then Config.Declared (X.Holder, X.Of_Kind)
                            and then Config.Approved (X.Holder, X.Of_Kind))
            then
               return False;
            end if;
         end;
      end loop;
      for P in Program loop
         for K in Kind loop
            if S.Exercised (P, K)
              and then not (Config.Declared (P, K) and Config.Approved (P, K))
            then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Exercise_Ok;

   --  Every spawned program came through the wildcard with Spawn; it is
   --  installable only if that authority had Grant; its parent is its
   --  spawner or the authority had Grant over every process (the SPAWN
   --  rules, recomputed).
   function Spawns_Ok (S : State) return Boolean is
   begin
      for P in Program loop
         declare
            R : Program_Record renames S.Programs (P);
         begin
            if R.Exists and then R.Spawned_By /= No_Node then
               if R.Spawned_By > S.Used then
                  return False;
               end if;
               declare
                  A : Authority renames S.Nodes (R.Spawned_By);
               begin
                  if A.Of_Kind /= Process_Control or else not A.Has (Spawn)
                    or else A.Scope /= Everything
                    or else (R.Installable and then not A.Has (Grant))
                    or else (R.Parent /= R.Spawner and then not A.Has (Grant))
                  then
                     return False;
                  end if;
               end;
            end if;
         end;
      end loop;
      return True;
   end Spawns_Ok;

   --  Every node installed or introduced into a launched program is within
   --  a node its launcher holds (kind, scope, rights).
   function Launches_Ok (S : State) return Boolean is
   begin
      for I in 1 .. S.Used loop
         declare
            X : Authority renames S.Nodes (I);
         begin
            if X.Valid and then X.How in Installed | Introduced and then X.Holder in Program
              and then S.Programs (X.Holder).Launcher in Program
            then
               if X.Within = No_Node or else X.Within >= I then
                  return False;
               end if;
               declare
                  W : Authority renames S.Nodes (X.Within);
               begin
                  if not ((W.Envelope and then W.Holder = S.Programs (X.Holder).Spawner)
                          or else W.Holder = S.Programs (X.Holder).Launcher)
                    or else W.Of_Kind /= X.Of_Kind
                    or else (X.Scope and not W.Scope) /= 0
                    or else (for some R in Right => X.Has (R) and not W.Has (R))
                  then
                     return False;
                  end if;
               end;
            end if;
         end;
      end loop;
      return True;
   end Launches_Ok;

   --  Every valid launch handle names a child launched for a requester and
   --  is its requester's, or is narrowed from a valid handle on the same
   --  child, never adding Debug (recomputed).
   function Handles_Ok (S : State) return Boolean is
   begin
      for H in 1 .. S.Handles_Used loop
         declare
            X : Launch_Handle renames S.Handles (H);
         begin
            if X.Valid then
               if X.Child not in Program or else X.Holder not in Program
                 or else not S.Programs (X.Child).Exists
                 or else S.Programs (X.Child).Launcher not in Program
                 or else X.Parent >= H
               then
                  return False;
               elsif X.Parent = No_Handle then
                  if X.Holder /= S.Programs (X.Child).Launcher then
                     return False;
                  end if;
               elsif not S.Handles (X.Parent).Valid
                 or else S.Handles (X.Parent).Child /= X.Child
                 or else (X.Debug and then not S.Handles (X.Parent).Debug)
               then
                  return False;
               end if;
            end if;
         end;
      end loop;
      return True;
   end Handles_Ok;

   --  Every valid debug session: minted from its target's own control by
   --  the target's spawner, scoped to the target, no Grant or Spawn, the
   --  target debuggable, through a live handle with Debug on it held by the
   --  session's holder or by the session's debug authority (recomputed).
   function Debug_Ok (S : State) return Boolean is
   begin
      for I in 1 .. S.Used loop
         declare
            X : Authority renames S.Nodes (I);
         begin
            if X.Valid and then (X.Of_Kind = Debugging or else X.How = Debug_Minted) then
               if X.Of_Kind /= Debugging or else X.How /= Debug_Minted
                 or else X.Parent = No_Node or else X.Parent >= I
                 or else X.Target not in Program
               then
                  return False;
               end if;
               declare
                  P : Authority renames S.Nodes (X.Parent);
               begin
                  if P.How /= Spawned_Control or else P.Of_Kind /= Process_Control
                    or else P.Holder /= S.Programs (X.Target).Spawner
                    or else X.Scope /= Object_Of (X.Target)
                    or else Config.Never_Debug (X.Target)
                    or else X.Has (Grant) or else X.Has (Spawn)
                  then
                     return False;
                  end if;
               end;
               if X.Via /= No_Handle then
                  if not S.Handles (X.Via).Valid or else not S.Handles (X.Via).Debug
                    or else S.Handles (X.Via).Child /= X.Target
                    or else S.Handles (X.Via).Holder /= X.Holder
                  then
                     return False;
                  end if;
               elsif X.Holder not in Program or else not Config.Session_Debugger (X.Holder) then
                  return False;
               end if;
            end if;
         end;
      end loop;
      return True;
   end Debug_Ok;

   --  The synchronous call graph has no cycle (transitive closure).
   function Acyclic (S : State) return Boolean is
      R : Relation := S.Calls;
   begin
      for M in Program loop
         for A in Program loop
            for B in Program loop
               if R (A, M) and R (M, B) then
                  R (A, B) := True;
               end if;
            end loop;
         end loop;
      end loop;
      return (for all A in Program => not R (A, A));
   end Acyclic;

   ---------------------------------------------------------------------------
   --  Applying an operation: False when it is not admitted
   ---------------------------------------------------------------------------
   Last_Revoked : Node_Count := No_Node;
   --  The requester of the launch the last operation was part of, and the
   --  state before it (Launch_Gains_Nothing, checked independently).
   Last_Requester : Process_Id := Kernel;
   Before : State;

   function Apply (S : in out State; O : Operation) return Boolean is
      C : Configuration renames Config;
   begin
      Last_Revoked := No_Node;
      Last_Requester := Kernel;
      Before := S;
      if S.Used >= Node_Limit
        and then O.What in Op_Spawn | Op_Derive | Op_Install | Op_Introduce | Op_Transfer |
                           Op_Carry | Op_Deliver | Op_Spawn_Any_Control |
                           Op_Spawn_Amplifying | Op_Spawn_Any_Parent | Op_Install_By_Kind |
                           Op_Launch_Leaking |
                           Op_Deliver_Keeping | Op_Mint | Op_Install_Unapproved |
                           Op_Mint_Debug | Op_Mint_Debug_Never | Op_Mint_Debug_Unrighted |
                           Op_Mint_Debug_Granting
      then
         return False;
      end if;
      case O.What is
         when Op_Spawn =>
            if not May_Spawn (S, O.N, O.P, O.Q) then return False; end if;
            if O.Q in Program and then O.Q /= S.Nodes (O.N).Holder then
               Last_Requester := O.Q;
            end if;
            Spawn (C, S, O.N, O.P, O.Q);
         when Op_Start =>
            if not S.Programs (O.P).Exists or else S.Programs (O.P).Started then
               return False;
            end if;
            Start (C, S, O.P);
         when Op_Derive =>
            if not May_Derive (S, O.N) or else (O.V = Same and then not O.E) then
               return False;
            end if;
            Derive (C, S, O.N, O.V, O.E);
         when Op_Install =>
            if not May_Install (C, S, O.N, O.P, O.V, O.M) then return False; end if;
            if S.Programs (O.P).Launcher in Program
              and then S.Programs (O.P).Launcher /= S.Nodes (O.N).Holder
            then
               Last_Requester := S.Programs (O.P).Launcher;
            end if;
            Install (C, S, O.N, O.P, O.V, O.M);
         when Op_Introduce =>
            if not May_Introduce (C, S, O.N, O.P, O.V, O.M) then return False; end if;
            if S.Programs (O.P).Launcher in Program
              and then S.Programs (O.P).Launcher /= S.Nodes (O.N).Holder
            then
               Last_Requester := S.Programs (O.P).Launcher;
            end if;
            Introduce (C, S, O.N, O.P, O.V, O.M);

         --  A launch that gives its requester something too: a copy of
         --  the new child's control (the requester would hold the app's
         --  authority, not a handle).
         when Op_Launch_Leaking =>
            if not (O.Q in Program and then May_Spawn (S, O.N, O.P, O.Q)
                    and then O.Q /= S.Nodes (O.N).Holder and then S.Used + 1 < Node_Limit)
            then
               return False;
            end if;
            Last_Requester := O.Q;
            Spawn (C, S, O.N, O.P, O.Q);
            S.Used := S.Used + 1;
            S.Nodes (S.Used) := (S.Nodes (S.Used - 1) with delta Holder => O.Q,
                                 Parent => S.Used - 1, How => Transferred);
         when Op_Transfer =>
            if not May_Transfer (C, S, O.N, O.P) then return False; end if;
            Transfer (C, S, O.N, O.P);
         when Op_Carry =>
            if not May_Carry (C, S, O.N, O.P, O.Q) then return False; end if;
            Carry (C, S, O.N, O.P, O.Q);
         when Op_Deliver =>
            if not May_Deliver (S, O.N) then return False; end if;
            Deliver (C, S, O.N);
         when Op_Close =>
            if not Live (S, O.N) then return False; end if;
            Close (C, S, O.N);
         when Op_Revoke =>
            if not May_Revoke (S, O.M, O.N) then return False; end if;
            Revoke (C, S, O.M, O.N);
            Last_Revoked := O.N;
         when Op_Exercise =>
            if O.N > S.Used or else not Exercisable (S, O.N)
              or else S.Exercised (S.Nodes (O.N).Holder, S.Nodes (O.N).Of_Kind)
            then
               return False;
            end if;
            Exercise (C, S, O.N);
         when Op_Call =>
            if not May_Call (C, S, O.P, O.Q) or else S.Calls (O.P, O.Q) then
               return False;
            end if;
            Call (C, S, O.P, O.Q);

         --  SPAWN before CAP-012 (k3w syscall-ipc.adb:210-216): any process
         --  control with the spawn right, even a child's own control.
         when Op_Spawn_Any_Control =>
            if May_Spawn (S, O.N, O.P)
              or else not (Room (S) and then Live (S, O.N) and then Exercisable (S, O.N)
                           and then S.Nodes (O.N).Of_Kind = Process_Control
                           and then S.Nodes (O.N).Has (Spawn)
                           and then not S.Programs (O.P).Exists)
            then
               return False;
            end if;
            S.Used := S.Used + 1;
            S.Nodes (S.Used) :=
              (Valid => True, Held => True, Holder => S.Nodes (O.N).Holder,
               Of_Kind => Process_Control, Scope => Object_Of (O.P),
               Has => S.Nodes (O.N).Has, Parent => O.N, How => Spawned_Control,
               Beneficiary => Kernel, Within => No_Node, Envelope => False,
               Target => Kernel, Via => No_Handle);
            S.Programs (O.P) :=
              (Exists => True, Started => False, Spawner => S.Nodes (O.N).Holder,
               Spawned_By => O.N, Installable => S.Nodes (O.N).Has (Grant),
               Parent => S.Nodes (O.N).Holder, Launcher => Kernel);

         --  SPAWN before CAP-012 (k3w :300-332): the spawner always got the
         --  child's CSPACE, whatever the authority's rights.
         when Op_Spawn_Amplifying =>
            if not May_Spawn (S, O.N, O.P) or else S.Nodes (O.N).Has (Grant) then
               return False;
            end if;
            Spawn (C, S, O.N, O.P);
            S.Programs (O.P).Installable := True;

         --  SPAWN before CAP-012 (k3w :261-266): any live process could be
         --  named the parent.
         when Op_Spawn_Any_Parent =>
            if O.Q not in Program or else May_Spawn (S, O.N, O.P, O.Q)
              or else not May_Spawn (S, O.N, O.P)
              or else not S.Programs (O.Q).Exists or else O.Q = O.P
            then
               return False;
            end if;
            Spawn (C, S, O.N, O.P);
            S.Programs (O.P).Parent := O.Q;
            S.Programs (O.P).Launcher := O.Q;

         --  OP_LAUNCH before CAP-012 (procmgr main.adb:1411-1423, 3518): the
         --  launcher holding the kind at all was enough, whatever its
         --  rights or scope.
         when Op_Install_By_Kind =>
            if not (May_Copy (S, O.N)
                    and then S.Programs (O.P).Exists and then not S.Programs (O.P).Started
                    and then S.Programs (O.P).Spawner = S.Nodes (O.N).Holder
                    and then S.Programs (O.P).Installable
                    and then May_Exercise (C, O.P, S.Nodes (O.N).Of_Kind)
                    and then S.Programs (O.P).Launcher in Program
                    and then O.M /= No_Node and then O.M <= S.Used and then Live (S, O.M)
                    and then Envelope_Or_Requester (S, O.M, O.P)
                    and then S.Nodes (O.M).Of_Kind = S.Nodes (O.N).Of_Kind)
              or else May_Install (C, S, O.N, O.P, O.V, O.M)
            then
               return False;
            end if;
            S.Used := S.Used + 1;
            S.Nodes (S.Used) :=
              (Valid => True, Held => True, Holder => O.P, Of_Kind => S.Nodes (O.N).Of_Kind,
               Scope => Scope_After (O.V, S.Nodes (O.N).Scope),
               Has => Rights_After (O.V, S.Nodes (O.N).Has), Parent => O.N,
               How => Installed, Beneficiary => Kernel, Within => O.M, Envelope => False,
               Target => Kernel, Via => No_Handle);

         --  Kernel capabilities today: no derivation record, so a
         --  "revocation" reaches only the copy named.
         when Op_Revoke_One =>
            if not May_Revoke (S, O.M, O.N) then return False; end if;
            S.Nodes (O.N).Valid := False;
            S.Nodes (O.N).Held := False;
            Last_Revoked := O.N;

         --  A broker that keeps a usable copy of what it delivered.
         when Op_Deliver_Keeping =>
            if not May_Deliver (S, O.N) then return False; end if;
            Deliver (C, S, O.N);
            S.Nodes (O.N).Held := True;
            S.Nodes (O.N).How := Delivered;

         --  A synchronous call up the layering.
         when Op_Call_Up =>
            if not S.Programs (O.P).Exists or else not S.Programs (O.Q).Exists
              or else O.P = O.Q or else C.Level (O.P) > C.Level (O.Q)
              or else S.Calls (O.P, O.Q)
            then
               return False;
            end if;
            S.Calls (O.P, O.Q) := True;

         --  POLICY_MINT (retired with CAP-002): authority from nothing.
         when Op_Mint =>
            if not S.Booted or else not S.Programs (O.P).Exists then return False; end if;
            S.Used := S.Used + 1;
            S.Nodes (S.Used) :=
              (Valid => True, Held => True, Holder => O.P, Of_Kind => O.K,
               Scope => Everything, Has => (others => True), Parent => No_Node,
               How => Root, Beneficiary => Kernel, Within => No_Node, Envelope => False,
               Target => Kernel, Via => No_Handle);

         --  A grant the plan never approved (an exact-name or bootstrap
         --  approval standing in for the plan).
         when Op_Install_Unapproved =>
            if not (May_Copy (S, O.N)
                    and then S.Programs (O.P).Exists and then not S.Programs (O.P).Started
                    and then S.Programs (O.P).Spawner = S.Nodes (O.N).Holder
                    and then S.Programs (O.P).Installable
                    and then S.Programs (O.P).Launcher not in Program)
              or else May_Install (C, S, O.N, O.P)
            then
               return False;
            end if;
            S.Used := S.Used + 1;
            S.Nodes (S.Used) :=
              (Valid => True, Held => True, Holder => O.P, Of_Kind => S.Nodes (O.N).Of_Kind,
               Scope => S.Nodes (O.N).Scope, Has => S.Nodes (O.N).Has, Parent => O.N,
               How => Installed, Beneficiary => Kernel, Within => No_Node, Envelope => False,
               Target => Kernel, Via => No_Handle);

         --  Launch handles and debug sessions.
         when Op_Narrow_Handle =>
            if O.P not in Program or else not May_Narrow (S, O.H, O.D, O.P) then
               return False;
            end if;
            Narrow_Handle (C, S, O.H, O.D, O.P);
         when Op_Revoke_Handle =>
            if not Handle_Live (S, O.H) then return False; end if;
            Revoke_Handle (C, S, O.H);
         when Op_Mint_Debug =>
            if O.N = No_Node or else O.P not in Program or else O.Q not in Program
              or else not May_Mint_Debug (C, S, O.N, O.P, O.Q, O.H)
            then
               return False;
            end if;
            Mint_Debug (C, S, O.N, O.P, O.Q, O.H);

         --  A mint whose one missing rule is the one named, with the rest
         --  of May_Mint_Debug kept.
         when Op_Mint_Debug_Never | Op_Mint_Debug_Unrighted | Op_Mint_Debug_Granting =>
            declare
               Relaxed : Configuration := C;
               Base : constant Boolean :=
                 O.N /= No_Node and then O.N <= S.Used and then O.P in Program
                 and then O.Q in Program;
            begin
               if not Base then return False; end if;
               if O.What = Op_Mint_Debug_Never then
                  if not C.Never_Debug (O.P) then return False; end if;
                  Relaxed.Never_Debug (O.P) := False;
                  if not May_Mint_Debug (Relaxed, S, O.N, O.P, O.Q, O.H) then return False; end if;
               elsif O.What = Op_Mint_Debug_Unrighted then
                  if not Handle_Live (S, O.H) or else S.Handles (O.H).Debug then return False; end if;
                  S.Handles (O.H).Debug := True;
                  if not May_Mint_Debug (C, S, O.N, O.P, O.Q, O.H) then return False; end if;
                  S.Handles (O.H).Debug := False;
               else
                  if not May_Mint_Debug (C, S, O.N, O.P, O.Q, O.H)
                    or else not S.Nodes (O.N).Has (Grant)
                  then
                     return False;
                  end if;
               end if;
               S.Used := S.Used + 1;
               S.Nodes (S.Used) :=
                 (Valid => True, Held => True, Holder => O.Q, Of_Kind => Debugging,
                  Scope => Object_Of (O.P),
                  Has => (if O.What = Op_Mint_Debug_Granting then S.Nodes (O.N).Has
                          else (Invoke => True, others => False)),
                  Parent => O.N, How => Debug_Minted, Beneficiary => Kernel,
                  Within => No_Node, Envelope => False, Target => O.P, Via => O.H);
            end;
         when Op_Revoke_Handle_Shallow =>
            if not Handle_Live (S, O.H) then return False; end if;
            S.Handles (O.H).Valid := False;
         when Op_Narrow_Amplifying =>
            if O.P not in Program or else not Handle_Live (S, O.H)
              or else S.Handles (O.H).Debug or else not Handle_Room (S)
              or else not S.Programs (O.P).Exists
            then
               return False;
            end if;
            S.Handles_Used := S.Handles_Used + 1;
            S.Handles (S.Handles_Used) :=
              (Valid => True, Holder => O.P, Child => S.Handles (O.H).Child, Debug => True,
               Parent => O.H);
      end case;
      return True;
   end Apply;

   --  Every invariant, by the model and independently; Why names the first
   --  that fails.
   function Check (S : State; Why : out Natural) return Boolean is
   begin
      Why := 0;
      if not Invariant (Config, S) then Why := 1;
      elsif not Chains_Ok (S) then Why := 2;
      elsif not Exercise_Ok (S) then Why := 3;
      elsif not Acyclic (S) then Why := 4;
      elsif not Spawns_Ok (S) then Why := 6;
      elsif not Launches_Ok (S) then Why := 7;
      elsif not Handles_Ok (S) then Why := 9;
      elsif not Debug_Ok (S) then Why := 10;
      elsif Last_Requester in Program and then not Gains_Nothing (Before, S, Last_Requester)
      then Why := 8;
      elsif Last_Revoked /= No_Node
        and then (for some I in 1 .. S.Used =>
                    S.Nodes (I).Valid and then Ancestor (S, I, Last_Revoked))
      then Why := 5;
      end if;
      return Why = 0;
   end Check;

   function Why_Text (Why : Natural) return String is
     (case Why is
        when 1 => "the model's Invariant",
        when 2 => "independent chain check (provenance / no amplification)",
        when 3 => "independent exercise check (provenance / brokers inert)",
        when 4 => "independent cycle check (calls go down)",
        when 5 => "independent revocation check (a derived copy survived)",
        when 6 => "independent SPAWN rules check (rules 1 to 3)",
        when 7 => "independent launch check (within the envelope or the requester's own)",
        when 8 => "Launch_Gains_Nothing (the requester gained or lost a node)",
        when 9 => "independent handle check (a handle wider than its source, or orphaned)",
        when 10 => "independent debug check (a session outside its envelope)",
        when others => "none");

   ---------------------------------------------------------------------------
   --  The search
   ---------------------------------------------------------------------------
   function Key (S : State) return String is
      Out_Text : String (1 .. 2 + Natural (Node_Limit) * 12 + 4 * 7 + 4 * 6 + 16 + 1 + 4 * 5) :=
        (others => ' ');
      Pos : Positive := 1;
      procedure Put (Ch : Character) is
      begin
         Out_Text (Pos) := Ch;
         Pos := Pos + 1;
      end Put;
      function B (X : Boolean) return Character is (if X then '1' else '0');
      function D (X : Natural) return Character is (Character'Val (48 + X));
   begin
      Put (D (Natural (S.Used)));
      Put (B (S.Booted));
      for I in 1 .. S.Used loop
         declare
            X : Authority renames S.Nodes (I);
         begin
            Put (B (X.Valid)); Put (B (X.Held)); Put (D (Natural (X.Holder)));
            Put (D (Kind'Pos (X.Of_Kind)));
            Put (Character'Val (Natural (X.Scope)));
            Put (D ((if X.Has (Invoke) then 1 else 0) + (if X.Has (Grant) then 2 else 0) +
                    (if X.Has (Spawn) then 4 else 0)));
            Put (D (Natural (X.Parent)));
            Put (D (Origin'Pos (X.How) * 8 + Natural (X.Beneficiary)));
            Put (D (Natural (X.Within)));
            Put (B (X.Envelope));
            Put (D (Natural (X.Target)));
            Put (D (Natural (X.Via)));
         end;
      end loop;
      Put (D (Natural (S.Handles_Used)));
      for H in 1 .. S.Handles_Used loop
         Put (B (S.Handles (H).Valid)); Put (D (Natural (S.Handles (H).Holder)));
         Put (D (Natural (S.Handles (H).Child))); Put (B (S.Handles (H).Debug));
         Put (D (Natural (S.Handles (H).Parent)));
      end loop;
      for P in Program loop
         Put (B (S.Programs (P).Exists)); Put (B (S.Programs (P).Started));
         Put (D (Natural (S.Programs (P).Spawner)));
         Put (D (Natural (S.Programs (P).Spawned_By)));
         Put (B (S.Programs (P).Installable));
         Put (D (Natural (S.Programs (P).Parent)));
         Put (D (Natural (S.Programs (P).Launcher)));
         for K of Checked_Kinds loop
            Put (B (S.Exercised (P, K)));
         end loop;
         for Q in Program loop
            Put (B (S.Calls (P, Q)));
         end loop;
      end loop;
      return Out_Text (1 .. Pos - 1);
   end Key;

   package Seen_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String, Element_Type => Natural, Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   Seen : Seen_Maps.Map;

   type Trace is array (1 .. 16) of Operation;
   Path : Trace;
   Found : Boolean;
   Found_Path : Trace;
   Found_Length : Natural;
   Found_Why : Natural;
   Bound_Reached : Boolean;
   Steps_Taken : Natural;

   procedure Candidates (S : State; R : Rule; Visit : not null access procedure (O : Operation)) is
   begin
      for H in 1 .. S.Handles_Used loop
         Visit ((What => Op_Revoke_Handle, H => H, others => <>));
         if R = Handle_Revoke_Shallow then
            Visit ((What => Op_Revoke_Handle_Shallow, H => H, others => <>));
         end if;
         for P in Program loop
            Visit ((What => Op_Narrow_Handle, H => H, P => P, D => False, others => <>));
            Visit ((What => Op_Narrow_Handle, H => H, P => P, D => True, others => <>));
            if R = Handle_Narrow_Amplifies then
               Visit ((What => Op_Narrow_Amplifying, H => H, P => P, others => <>));
            end if;
         end loop;
      end loop;
      for N in 1 .. S.Used loop
         if S.Nodes (N).How = Spawned_Control then
            for P in Program loop
               for Q in Program loop
                  for H in 0 .. S.Handles_Used loop
                     Visit ((What => Op_Mint_Debug, N => N, P => P, Q => Q, H => H, others => <>));
                     if R = Debug_Never_Debuggable then
                        Visit ((What => Op_Mint_Debug_Never, N => N, P => P, Q => Q, H => H,
                                others => <>));
                     elsif R = Debug_Without_Right then
                        Visit ((What => Op_Mint_Debug_Unrighted, N => N, P => P, Q => Q, H => H,
                                others => <>));
                     elsif R = Debug_With_Grant then
                        Visit ((What => Op_Mint_Debug_Granting, N => N, P => P, Q => Q, H => H,
                                others => <>));
                     end if;
                  end loop;
               end loop;
            end loop;
         end if;
      end loop;
      for P in Program loop
         Visit ((What => Op_Start, P => P, others => <>));
         for Q in Program loop
            Visit ((What => Op_Call, P => P, Q => Q, others => <>));
            if R = Calls_Either_Way then
               Visit ((What => Op_Call_Up, P => P, Q => Q, others => <>));
            end if;
         end loop;
         if R = Mint_After_Boot then
            for K of Checked_Kinds loop
               Visit ((What => Op_Mint, P => P, K => K, others => <>));
            end loop;
         end if;
      end loop;
      for N in 1 .. S.Used loop
         for P in Program loop
            for Q in Process_Id loop
               Visit ((What => Op_Spawn, N => N, P => P, Q => Q, others => <>));
               if R = Parent_Without_Grant then
                  Visit ((What => Op_Spawn_Any_Parent, N => N, P => P, Q => Q, others => <>));
               end if;
            end loop;
            if R = Spawn_With_Any_Control then
               Visit ((What => Op_Spawn_Any_Control, N => N, P => P, others => <>));
            end if;
            if R = Spawn_Handles_Amplify then
               Visit ((What => Op_Spawn_Amplifying, N => N, P => P, others => <>));
            end if;
            if R = Requester_Gains then
               for Q in Program loop
                  Visit ((What => Op_Launch_Leaking, N => N, P => P, Q => Q, others => <>));
               end loop;
            end if;
         end loop;
      end loop;
      for N in 1 .. S.Used loop
         Visit ((What => Op_Close, N => N, others => <>));
         Visit ((What => Op_Exercise, N => N, others => <>));
         Visit ((What => Op_Deliver, N => N, others => <>));
         if R = Broker_Keeps_Copy then
            Visit ((What => Op_Deliver_Keeping, N => N, others => <>));
         end if;
         Visit ((What => Op_Derive, N => N, V => Without_Grant, others => <>));
         Visit ((What => Op_Derive, N => N, V => Fewer_Objects, others => <>));
         --  A session supervisor's envelope node.
         Visit ((What => Op_Derive, N => N, V => Same, E => True, others => <>));
         for M in 1 .. S.Used loop
            Visit ((What => Op_Revoke, N => N, M => M, others => <>));
            if R = Revoke_Without_Tree then
               Visit ((What => Op_Revoke_One, N => N, M => M, others => <>));
            end if;
         end loop;
         for P in Program loop
            Visit ((What => Op_Transfer, N => N, P => P, others => <>));
            if R = Install_Without_Approval then
               Visit ((What => Op_Install_Unapproved, N => N, P => P, others => <>));
            end if;
            for V in Narrowing loop
               --  Within: none (a plan's child), or a launcher's node.
               for M in 0 .. S.Used loop
                  Visit ((What => Op_Install, N => N, P => P, V => V, M => M, others => <>));
                  Visit ((What => Op_Introduce, N => N, P => P, V => V, M => M, others => <>));
                  if R = Launch_By_Kind then
                     Visit ((What => Op_Install_By_Kind, N => N, P => P, V => V, M => M,
                             others => <>));
                  end if;
               end loop;
            end loop;
            for Q in Program loop
               Visit ((What => Op_Carry, N => N, P => P, Q => Q, others => <>));
            end loop;
         end loop;
      end loop;
   end Candidates;

   procedure Search (S : State; Depth : Natural; R : Rule) is
      procedure Try (O : Operation) is
         Next : State := S;
         Why : Natural;
      begin
         if Found or else Bound_Reached then return; end if;
         if not Apply (Next, O) then return; end if;
         Steps_Taken := Steps_Taken + 1;
         Path (Depth + 1) := O;
         if not Check (Next, Why) then
            Found := True;
            Found_Path := Path;
            Found_Length := Depth + 1;
            Found_Why := Why;
            return;
         end if;
         if Depth + 1 < Depth_Bound then
            declare
               K : constant String := Key (Next);
               C : constant Seen_Maps.Cursor := Seen.Find (K);
            begin
               if Seen_Maps.Has_Element (C) and then Seen_Maps.Element (C) <= Depth + 1 then
                  return;
               end if;
               if Natural (Seen.Length) >= State_Bound then
                  Bound_Reached := True;
                  return;
               end if;
               Seen.Include (K, Depth + 1);
            end;
            Search (Next, Depth + 1, R);
         end if;
      end Try;
   begin
      Candidates (S, R, Try'Access);
   end Search;

   --  The booted state: devmgr exists and holds the roots.
   function Booted_State return State is
      S : State;
      Has_All : constant Rights := (others => True);
      Has_Use_Grant : constant Rights := (Invoke | Grant => True, Spawn => False);
   begin
      Boot (Config, S);
      Issue_Root (Config, S, Process_Control, Everything, Has_All);
      Issue_Root (Config, S, Hardware, 2#11#, Has_Use_Grant);
      Issue_Root (Config, S, Service_Endpoint, 2#1#, Has_Use_Grant);
      Finish_Boot (Config, S);
      return S;
   end Booted_State;

   Failures : Natural := 0;

   procedure Run (R : Rule; Bound : Natural) is
      Expect_Violation : constant Boolean := R /= Model;
   begin
      --  Mutants deepen one step at a time, so the counterexample shown is
      --  a shortest one; the model searches the whole bound at once.
      Steps_Taken := 0;
      Found := False;
      for D in (if Expect_Violation then 1 else Bound) .. Bound loop
         Depth_Bound := D;
         Seen.Clear;
         Bound_Reached := False;
         Search (Booted_State, 0, R);
         exit when Found;
      end loop;
      Put (R'Image & ": " & Steps_Taken'Image & " steps," & Seen.Length'Image &
           " states, depth" & Depth_Bound'Image);
      if Bound_Reached then
         Put (", STATE BOUND REACHED");
      end if;
      New_Line;
      if Found then
         Put_Line ("  counterexample (" & Why_Text (Found_Why) & "):");
         for I in 1 .. Found_Length loop
            Put_Line ("    " & Image (Found_Path (I)));
         end loop;
      end if;
      if Found /= Expect_Violation then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & R'Image &
                   (if Expect_Violation then " found no violation (mutant survived)"
                    else " violated an invariant"));
      end if;
   end Run;

   ---------------------------------------------------------------------------
   --  Scenarios: the two spawn placements
   ---------------------------------------------------------------------------
   procedure Scenario (Name : String; Ops : Trace; Count : Natural) is
      S : State := Booted_State;
      Why : Natural;
   begin
      for I in 1 .. Count loop
         if not Apply (S, Ops (I)) then
            Failures := Failures + 1;
            Put_Line ("FAIL: " & Name & ": step" & I'Image & " refused: " & Image (Ops (I)));
            return;
         elsif not Check (S, Why) then
            Failures := Failures + 1;
            Put_Line ("FAIL: " & Name & ": step" & I'Image & " broke " & Why_Text (Why));
            return;
         end if;
      end loop;
      Put_Line (Name & ": " & Count'Image & " steps admitted, invariants kept");
   end Scenario;

   --  Node numbers: 1 process root, 2 hardware root, 3 endpoint root.
   Placement_A : constant Trace :=
     --  devmgr spawns procmgr and moves spawn authority to it, keeping no
     --  spawn right; procmgr spawns the driver; devmgr's hardware reaches
     --  the driver through procmgr as a carried copy.
     ((What => Op_Spawn, N => 1, P => Procmgr, others => <>),            --  node 4
      (What => Op_Transfer, N => 1, P => Procmgr, others => <>),         --  node 5
      (What => Op_Carry, N => 2, P => Procmgr, Q => Driver, others => <>), --  node 6
      (What => Op_Start, P => Procmgr, others => <>),
      (What => Op_Spawn, N => 5, P => Driver, others => <>),             --  node 7
      (What => Op_Deliver, N => 6, others => <>),                        --  node 8
      (What => Op_Start, P => Driver, others => <>),
      (What => Op_Call, P => Procmgr, Q => Devmgr, others => <>),
      (What => Op_Exercise, N => 8, others => <>),
      (What => Op_Revoke, N => 2, M => 2, others => <>),
      others => <>);
   Placement_B : constant Trace :=
     --  devmgr keeps driver spawn and installs hardware itself; procmgr
     --  gets a spawn copy for programs.
     ((What => Op_Spawn, N => 1, P => Procmgr, others => <>),            --  node 4
      (What => Op_Install, N => 1, P => Procmgr, V => Same, others => <>), --  node 5
      (What => Op_Start, P => Procmgr, others => <>),
      (What => Op_Spawn, N => 1, P => Driver, others => <>),             --  node 6
      (What => Op_Install, N => 2, P => Driver, V => Fewer_Objects, others => <>), --  node 7
      (What => Op_Start, P => Driver, others => <>),
      (What => Op_Spawn, N => 5, P => App, others => <>),                --  node 8
      (What => Op_Call, P => Procmgr, Q => Devmgr, others => <>),
      (What => Op_Exercise, N => 7, others => <>),
      (What => Op_Revoke, N => 2, M => 2, others => <>),
      others => <>);

begin
   if Ada.Command_Line.Argument_Count >= 1 then
      Depth_Bound := Natural'Value (Ada.Command_Line.Argument (1));
   end if;
   if Ada.Command_Line.Argument_Count >= 2 then
      State_Bound := Natural'Value (Ada.Command_Line.Argument (2));
   end if;
   --  devmgr: every kind (root issuer). procmgr: process control,
   --  endpoints, observation, administration; it brokers for the driver
   --  and the app. The driver: hardware and endpoints. The app: endpoints;
   --  it declares hardware but the plan does not approve it.
   for K in Kind loop
      Config.Declared (Devmgr, K) := True;
      Config.Approved (Devmgr, K) := True;
   end loop;
   for K in Kind loop
      if K in Process_Control | Service_Endpoint | Observation | Administration then
         Config.Declared (Procmgr, K) := True;
         Config.Approved (Procmgr, K) := True;
      end if;
   end loop;
   Config.Declared (Driver, Hardware) := True;
   Config.Approved (Driver, Hardware) := True;
   Config.Declared (Driver, Service_Endpoint) := True;
   Config.Approved (Driver, Service_Endpoint) := True;
   Config.Declared (App, Service_Endpoint) := True;
   Config.Approved (App, Service_Endpoint) := True;
   Config.Declared (App, Hardware) := True;
   --  Debugging: the app (the session's Debugger) and the driver (as a
   --  launch requester with a handle).
   Config.Declared (App, Debugging) := True;
   Config.Approved (App, Debugging) := True;
   Config.Declared (Driver, Debugging) := True;
   Config.Approved (Driver, Debugging) := True;
   Config.Brokers (Procmgr, Driver) := True;
   Config.Brokers (Procmgr, App) := True;
   pragma Assert (Well_Formed (Config));

   Scenario ("placement (a), procmgr the only spawner", Placement_A, 10);
   Scenario ("placement (b), devmgr spawns drivers", Placement_B, 10);
   declare
      Bound : constant Natural := Depth_Bound;
   begin
      for R in Rule loop
         Run (R, Bound);
      end loop;
   end;
   if Failures = 0 then
      Put_Line ("authority model: exhaustive check passed");
   else
      Put_Line ("authority model:" & Failures'Image & " failures");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Exhaustive;
