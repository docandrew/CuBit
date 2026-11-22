pragma Ada_2022;

package body Authority_Model with SPARK_Mode is

   ---------------------------------------------------------------------------
   --  Lemmas
   ---------------------------------------------------------------------------

   procedure Lemma_Descends_Later (G : Graph; I, A : Node) is
   begin
      if I /= A then
         Lemma_Descends_Later (G, G (I).Parent, A);
      end if;
   end Lemma_Descends_Later;

   procedure Lemma_None_Before (G : Graph; A : Node) is
   begin
      for J in Node loop
         if J < A and then Descends (G, J, A) then
            Lemma_Descends_Later (G, J, A);
         end if;
         pragma Loop_Invariant
           (for all X in Node range Node'First .. J =>
              (if X < A then not Descends (G, X, A)));
      end loop;
   end Lemma_None_Before;

   procedure Lemma_Traces_To_Root (C : Configuration; S : State; I : Node) is
   begin
      if S.Nodes (I).Parent /= No_Node then
         pragma Assert (S.Nodes (I).Parent < I);
         pragma Assert (S.Nodes (S.Nodes (I).Parent).Valid);
         Lemma_Traces_To_Root (C, S, S.Nodes (I).Parent);
      end if;
   end Lemma_Traces_To_Root;

   procedure Lemma_Calls_Descend (C : Configuration; S : State; A, B : Program; N : Steps) is
   begin
      if N > 1 then
         for M in Program loop
            if S.Calls (A, M) and then Reaches (S, M, B, N - 1) then
               Lemma_Calls_Descend (C, S, M, B, N - 1);
               pragma Assert (C.Level (A) > C.Level (B));
               return;
            end if;
            pragma Loop_Invariant
              (for all X in Program range Program'First .. M =>
                 not (S.Calls (A, X) and then Reaches (S, X, B, N - 1)));
         end loop;
      end if;
   end Lemma_Calls_Descend;

   procedure Lemma_No_Call_Cycle (C : Configuration; S : State; A : Program; N : Steps) is
   begin
      if Reaches (S, A, A, N) then
         Lemma_Calls_Descend (C, S, A, A, N);
      end if;
   end Lemma_No_Call_Cycle;

   procedure Lemma_Narrowing (V : Narrowing; Scope : Object_Set; Has : Rights) is null;

   ---------------------------------------------------------------------------
   --  Appending a node
   ---------------------------------------------------------------------------

   --  The node a copy of From makes for Holder: narrowed, attenuated.
   function Copy_Of
     (S : State; From : Node; Holder : Program; How : Origin; V : Narrowing;
      Beneficiary : Process_Id := Kernel; Within : Node_Count := No_Node) return Authority
   is ((Valid       => True,
        Held        => True,
        Holder      => Holder,
        Of_Kind     => S.Nodes (From).Of_Kind,
        Scope       => Scope_After (V, S.Nodes (From).Scope),
        Has         => Rights_After (V, S.Nodes (From).Has),
        Parent      => From,
        How         => How,
        Beneficiary => Beneficiary,
        Within      => Within,
        Envelope    => False,
        Target      => Kernel,
        Via         => No_Handle));

   --  Append Item after the last used node.
   procedure Append (S : in out State; Item : Authority)
   with Pre  => Room (S),
        Post => S.Used = S.Used'Old + 1
                and then S.Nodes (S.Used) = Item
                and then (for all I in Node =>
                            (if I /= S.Used then S.Nodes (I) = S.Nodes'Old (I)))
                and then S.Programs = S.Programs'Old
                and then S.Exercised = S.Exercised'Old
                and then S.Calls = S.Calls'Old
                and then S.Booted = S.Booted'Old
                and then S.Handles = S.Handles'Old
                and then S.Handles_Used = S.Handles_Used'Old
   is
   begin
      S.Used := S.Used + 1;
      S.Nodes (S.Used) := Item;
   end Append;

   --  Let one node go: its holder no longer holds it.
   procedure Let_Go (S : in out State; Item : Node)
   with Post => S.Nodes (Item) = (S.Nodes'Old (Item) with delta Held => False)
                and then (for all I in Node =>
                            (if I /= Item then S.Nodes (I) = S.Nodes'Old (I)))
                and then S.Used = S.Used'Old
                and then S.Programs = S.Programs'Old
                and then S.Exercised = S.Exercised'Old
                and then S.Calls = S.Calls'Old
                and then S.Booted = S.Booted'Old
                and then S.Handles = S.Handles'Old
                and then S.Handles_Used = S.Handles_Used'Old
   is
   begin
      S.Nodes (Item).Held := False;
   end Let_Go;

   ---------------------------------------------------------------------------
   --  Operations
   ---------------------------------------------------------------------------

   procedure Boot (C : Configuration; S : out State) is
   begin
      S := (Nodes     => (others => <>),
            Used      => No_Node,
            Handles   => (others => <>),
            Handles_Used => No_Handle,
            Programs  => (others => <>),
            Exercised => (others => (others => False)),
            Calls     => (others => (others => False)),
            Booted    => False);
      for K in Kind loop
         S.Programs (C.Root_Issuer (K)) :=
           (Exists => True, Started => True, Spawner => Kernel, others => <>);
         pragma Loop_Invariant
           (for all J in Kind range Kind'First .. K =>
              S.Programs (C.Root_Issuer (J)).Exists);
         pragma Loop_Invariant (S.Used = No_Node and then not S.Booted);
         pragma Loop_Invariant
           (S.Handles_Used = No_Handle and then (for all H in Handle_Index => not S.Handles (H).Valid));
         pragma Loop_Invariant
           (for all P in Program =>
              S.Programs (P).Launcher = Kernel and then S.Programs (P).Spawned_By = No_Node);
         pragma Loop_Invariant
           (for all I in Node => not S.Nodes (I).Valid and then not S.Nodes (I).Held
                                 and then S.Nodes (I).Parent = No_Node);
         pragma Loop_Invariant
           (for all A in Program => (for all B in Program => not S.Calls (A, B)));
         pragma Loop_Invariant
           (for all P in Program => (for all J in Kind => not S.Exercised (P, J)));
      end loop;
   end Boot;

   procedure Issue_Root (C : Configuration; S : in out State; K : Kind; Scope : Object_Set; Has : Rights) is
   begin
      Append (S, (Valid       => True,
                  Held        => True,
                  Holder      => C.Root_Issuer (K),
                  Of_Kind     => K,
                  Scope       => Scope,
                  Has         => Has,
                  Parent      => No_Node,
                  How         => Root,
                  Beneficiary => Kernel,
                  Within      => No_Node,
                  Envelope    => False,
                  Target      => Kernel,
                  Via         => No_Handle));
   end Issue_Root;

   procedure Finish_Boot (C : Configuration; S : in out State) is
   begin
      S.Booted := True;
   end Finish_Boot;

   procedure Spawn
     (C : Configuration; S : in out State; By : Node; Child : Program;
      Launcher : Process_Id := Kernel)
   is
      Spawner : constant Program := S.Nodes (By).Holder;
      Parent  : constant Process_Id := Parent_For (S, By);
      Old     : constant State := S with Ghost;
      Control : constant Authority :=
        (Valid       => True,
         Held        => True,
         Holder      => Spawner,
         Of_Kind     => Process_Control,
         Scope       => Object_Of (Child),
         Has         => S.Nodes (By).Has,
         Parent      => By,
         How         => Spawned_Control,
         Beneficiary => Kernel,
         Within      => No_Node,
         Envelope    => False,
         Target      => Kernel,
         Via         => No_Handle);
   begin
      pragma Assert (May_Exercise (C, Spawner, Process_Control));
      pragma Assert (Inside (Object_Of (Child), Everything));
      --  No node is held by the child yet: it did not exist.
      pragma Assert (for all I in Node =>
                       (if S.Nodes (I).Valid then S.Nodes (I).Holder /= Child));
      Append (S, Control);
      S.Programs (Child) :=
        (Exists => True, Started => False, Spawner => Spawner, Spawned_By => By,
         Installable => S.Nodes (By).Has (Grant), Parent => Parent, Launcher => Launcher);
      S.Exercised (Spawner, Process_Control) := True;
      --  Launch-into-S: the requester's launch handle on the child.
      --  No handle named the child yet: it did not exist.
      pragma Assert (for all H in Handle_Index =>
                       (if Old.Handles (H).Valid then Old.Handles (H).Child /= Child));
      pragma Assert (for all P in Program =>
                       (if P /= Child then S.Programs (P) = Old.Programs (P)));
      pragma Assert (for all I in Node =>
                       (if I /= S.Used and then S.Nodes (I).Valid
                        then S.Nodes (I) = Old.Nodes (I) and then S.Nodes (I).Holder /= Child));
      pragma Assert (Within_Envelope (S));
      if Launcher in Program then
         S.Handles_Used := S.Handles_Used + 1;
         S.Handles (S.Handles_Used) :=
           (Valid => True, Holder => Launcher, Child => Child, Debug => True, Parent => No_Handle);
         pragma Assert (S.Programs (Child).Launcher = Launcher);
         pragma Assert (for all H in Handle_Index =>
                          (if H /= S.Handles_Used then S.Handles (H) = Old.Handles (H)));
         pragma Assert
           (for all H in Handle_Index =>
              (if H /= S.Handles_Used and then S.Handles (H).Valid then
                 S.Programs (S.Handles (H).Child) = Old.Programs (Old.Handles (H).Child)));
      end if;
      pragma Assert (Handles_Kept (S));
      --  The sessions minted before: nothing they name changed.
      pragma Assert
        (for all I in Node =>
           (if I /= S.Used and then S.Nodes (I).Valid
              and then (S.Nodes (I).Of_Kind = Debugging or else S.Nodes (I).How = Debug_Minted)
            then Debug_Minted_Ok (C, Old, I)
                 and then S.Nodes (I) = Old.Nodes (I)
                 and then Old.Nodes (I).Target /= Child
                 and then S.Programs (Old.Nodes (I).Target) = Old.Programs (Old.Nodes (I).Target)
                 and then S.Nodes (Old.Nodes (I).Parent) = Old.Nodes (Old.Nodes (I).Parent)
                 and then (if Old.Nodes (I).Via /= No_Handle
                           then Old.Nodes (I).Via <= Old.Handles_Used
                                and then S.Handles (Old.Nodes (I).Via) = Old.Handles (Old.Nodes (I).Via))));
      pragma Assert (Debug_Within_Envelope (C, S));
      pragma Assert (Launcher /= Child);
      pragma Assert (for all P in Program => S.Programs (P).Launcher /= P);
      pragma Assert (for all P in Program =>
                       (if S.Programs (P).Exists and then S.Programs (P).Launcher in Program
                        then S.Programs (S.Programs (P).Launcher).Exists));
      pragma Assert (S.Used = Old.Used + 1 and then S.Nodes (S.Used).Holder = Spawner);
      pragma Assert (for all I in Node => (if I /= S.Used then S.Nodes (I) = Old.Nodes (I)));
      pragma Assert (if Launcher in Program and then Launcher /= Spawner
                     then Gains_Nothing (Old, S, Launcher));
   end Spawn;

   procedure Start (C : Configuration; S : in out State; P : Program) is
   begin
      S.Programs (P).Started := True;
   end Start;

   procedure Derive
     (C : Configuration; S : in out State; From : Node; V : Narrowing;
      Envelope : Boolean := False)
   is
      Item : constant Authority :=
        (Copy_Of (S, From, S.Nodes (From).Holder, Derived, V) with delta Envelope => Envelope);
   begin
      Lemma_Narrowing (V, S.Nodes (From).Scope, S.Nodes (From).Has);
      Append (S, Item);
   end Derive;

   procedure Install
     (C : Configuration; S : in out State; From : Node; Target : Program; V : Narrowing;
      Within : Node_Count := No_Node)
   is
      Item : constant Authority := Copy_Of (S, From, Target, Installed, V, Within => Within);
      Old  : constant State := S with Ghost;
   begin
      Lemma_Narrowing (V, S.Nodes (From).Scope, S.Nodes (From).Has);
      Append (S, Item);
      pragma Assert (S.Nodes (S.Used).Holder = Target and then S.Used = Old.Used + 1);
      pragma Assert (Old.Programs (Target).Launcher /= Target);
      pragma Assert (if Old.Programs (Target).Launcher in Program
                     then Gains_Nothing (Old, S, Old.Programs (Target).Launcher));
   end Install;

   procedure Introduce
     (C : Configuration; S : in out State; From : Node; Target : Program; V : Narrowing;
      Within : Node_Count := No_Node)
   is
      Item : constant Authority := Copy_Of (S, From, Target, Introduced, V, Within => Within);
      Old  : constant State := S with Ghost;
   begin
      Lemma_Narrowing (V, S.Nodes (From).Scope, S.Nodes (From).Has);
      Append (S, Item);
      pragma Assert (S.Nodes (S.Used).Holder = Target and then S.Used = Old.Used + 1);
      pragma Assert (Old.Programs (Target).Launcher /= Target);
      pragma Assert (if Old.Programs (Target).Launcher in Program
                     then Gains_Nothing (Old, S, Old.Programs (Target).Launcher));
   end Introduce;

   procedure Transfer (C : Configuration; S : in out State; From : Node; Target : Program) is
      Item : constant Authority := Copy_Of (S, From, Target, Transferred, Same);
   begin
      Lemma_Narrowing (Same, S.Nodes (From).Scope, S.Nodes (From).Has);
      Append (S, Item);
      Let_Go (S, From);
   end Transfer;

   procedure Carry
     (C : Configuration; S : in out State; From : Node; Broker, Beneficiary : Program)
   is
      Item : constant Authority := Copy_Of (S, From, Broker, Carried, Same, Beneficiary);
   begin
      Lemma_Narrowing (Same, S.Nodes (From).Scope, S.Nodes (From).Has);
      Append (S, Item);
   end Carry;

   procedure Deliver (C : Configuration; S : in out State; Item : Node) is
      Target : constant Program := S.Nodes (Item).Beneficiary;
      Copy   : constant Authority :=
        Copy_Of (S, Item, Target, Delivered, Without_Grant);
   begin
      pragma Assert (S.Nodes (Item).How = Carried);
      pragma Assert (May_Exercise (C, Target, S.Nodes (Item).Of_Kind));
      Lemma_Narrowing (Without_Grant, S.Nodes (Item).Scope, S.Nodes (Item).Has);
      Append (S, Copy);
      Let_Go (S, Item);
   end Deliver;

   procedure Close (C : Configuration; S : in out State; Item : Node) is
   begin
      Let_Go (S, Item);
   end Close;

   procedure Revoke (C : Configuration; S : in out State; Via, Target : Node) is
      Old : constant State := S with Ghost;
   begin
      Lemma_None_Before (Old.Nodes, Target);
      for I in Target .. S.Used loop
         if I = Target
           or else (S.Nodes (I).Parent /= No_Node
                    and then not S.Nodes (S.Nodes (I).Parent).Valid)
         then
            S.Nodes (I).Valid := False;
            S.Nodes (I).Held := False;
         end if;
         pragma Loop_Invariant (S.Used = Old.Used);
         pragma Loop_Invariant (S.Programs = Old.Programs);
         pragma Loop_Invariant (S.Exercised = Old.Exercised);
         pragma Loop_Invariant (S.Calls = Old.Calls);
         pragma Loop_Invariant (S.Booted = Old.Booted);
         pragma Loop_Invariant (S.Handles = Old.Handles and then S.Handles_Used = Old.Handles_Used);
         pragma Loop_Invariant
           (for all J in Node =>
              S.Nodes (J) = (Old.Nodes (J) with delta
                               Valid => S.Nodes (J).Valid, Held => S.Nodes (J).Held));
         pragma Loop_Invariant
           (for all J in Node =>
              (if J < Target or else J > I then S.Nodes (J) = Old.Nodes (J)));
         pragma Loop_Invariant
           (for all J in Node =>
              (if J in Target .. I then
                 S.Nodes (J).Valid =
                   (Old.Nodes (J).Valid and then not Descends (Old.Nodes, J, Target))
                 and then S.Nodes (J).Held =
                   (Old.Nodes (J).Held and then not Descends (Old.Nodes, J, Target))));
      end loop;
      --  Nodes before Target never descend from it; nodes past Used are unused.
      pragma Assert
        (for all J in Node =>
           (if J < Target then not Descends (Old.Nodes, J, Target)));
      pragma Assert
        (for all J in Node =>
           S.Nodes (J).Valid =
             (Old.Nodes (J).Valid and then not Descends (Old.Nodes, J, Target)));
   end Revoke;

   procedure Exercise (C : Configuration; S : in out State; Item : Node) is
   begin
      S.Exercised (S.Nodes (Item).Holder, S.Nodes (Item).Of_Kind) := True;
   end Exercise;

   procedure Call (C : Configuration; S : in out State; Caller, Callee : Program) is
   begin
      S.Calls (Caller, Callee) := True;
   end Call;

   ---------------------------------------------------------------------------
   --  Launch handles and debug sessions
   ---------------------------------------------------------------------------

   --  A handle descends only from earlier handles.
   procedure Lemma_Handle_Descends_Later (T : Handle_Table; H, A : Handle_Index)
   with Ghost, Global => null,
        Pre  => Handle_Descends (T, H, A),
        Post => H >= A,
        Subprogram_Variant => (Decreases => H);

   procedure Lemma_Handle_Descends_Later (T : Handle_Table; H, A : Handle_Index) is
   begin
      if H /= A then
         Lemma_Handle_Descends_Later (T, T (H).Parent, A);
      end if;
   end Lemma_Handle_Descends_Later;

   --  A debug session has no copies: nothing valid is derived from one.
   procedure Lemma_Debug_Childless (C : Configuration; S : State; I : Node)
   with Ghost, Global => null,
        Pre  => Invariant (C, S) and then S.Nodes (I).How = Debug_Minted,
        Post => (for all J in Node =>
                   (if S.Nodes (J).Valid then S.Nodes (J).Parent /= I));

   procedure Lemma_Debug_Childless (C : Configuration; S : State; I : Node) is
   begin
      for J in Node loop
         if S.Nodes (J).Valid and then S.Nodes (J).Parent = I then
            pragma Assert (S.Nodes (I).Valid);
            pragma Assert (Kind_Follows (S.Nodes (J).Of_Kind, S.Nodes (I).Of_Kind, S.Nodes (J).How));
            pragma Assert (S.Nodes (I).Of_Kind = Debugging);
            pragma Assert (S.Nodes (J).Of_Kind = Debugging);
            pragma Assert (Debug_Minted_Ok (C, S, J));
            pragma Assert (False);
         end if;
         pragma Loop_Invariant
           (for all K in Node range Node'First .. J =>
              (if S.Nodes (K).Valid then S.Nodes (K).Parent /= I));
      end loop;
   end Lemma_Debug_Childless;

   procedure Narrow_Handle
     (C : Configuration; S : in out State; From : Handle_Index; Debug : Boolean; To : Program)
   is
      Child : constant Process_Id := S.Handles (From).Child;
   begin
      S.Handles_Used := S.Handles_Used + 1;
      S.Handles (S.Handles_Used) :=
        (Valid => True, Holder => To, Child => Child, Debug => Debug, Parent => From);
   end Narrow_Handle;

   procedure Revoke_Handle (C : Configuration; S : in out State; H : Handle_Index) is
      Old : constant State := S with Ghost;
      --  The table as it was, for the sessions' revocation below.
      Old_Handles : constant Handle_Table := S.Handles;
   begin
      for G in H .. S.Handles_Used loop
         if G = H
           or else (S.Handles (G).Parent /= No_Handle
                    and then not S.Handles (S.Handles (G).Parent).Valid)
         then
            S.Handles (G).Valid := False;
         end if;
         pragma Loop_Invariant (S.Nodes = Old.Nodes and then S.Used = Old.Used);
         pragma Loop_Invariant (S.Programs = Old.Programs);
         pragma Loop_Invariant (S.Exercised = Old.Exercised);
         pragma Loop_Invariant (S.Calls = Old.Calls);
         pragma Loop_Invariant (S.Booted = Old.Booted);
         pragma Loop_Invariant (S.Handles_Used = Old.Handles_Used);
         pragma Loop_Invariant
           (for all K in Handle_Index =>
              S.Handles (K) = (Old.Handles (K) with delta Valid => S.Handles (K).Valid));
         pragma Loop_Invariant
           (for all K in Handle_Index =>
              (if K < H or else K > G then S.Handles (K) = Old.Handles (K)));
         pragma Loop_Invariant
           (for all K in Handle_Index =>
              (if K in H .. G then
                 S.Handles (K).Valid =
                   (Old.Handles (K).Valid and then not Handle_Descends (Old.Handles, K, H))));
      end loop;
      pragma Assert
        (for all K in Handle_Index =>
           (if K < H then not Handle_Descends (Old.Handles, K, H)));
      for K in Handle_Index loop
         if K < H and then Handle_Descends (Old_Handles, K, H) then
            Lemma_Handle_Descends_Later (Old_Handles, K, H);
         end if;
         pragma Loop_Invariant
           (for all J in Handle_Index range Handle_Index'First .. K =>
              (if J < H then not Handle_Descends (Old.Handles, J, H)));
      end loop;
      pragma Assert
        (for all K in Handle_Index =>
           S.Handles (K).Valid =
             (Old.Handles (K).Valid and then not Handle_Descends (Old.Handles, K, H)));
      --  The debug sessions minted through any of them.
      for I in Node loop
         if S.Nodes (I).How = Debug_Minted and then S.Nodes (I).Via /= No_Handle
           and then Handle_Descends (Old_Handles, S.Nodes (I).Via, H)
         then
            Lemma_Debug_Childless (C, Old, I);
            S.Nodes (I).Valid := False;
            S.Nodes (I).Held := False;
         end if;
         pragma Loop_Invariant (Old_Handles = Old.Handles);
         pragma Loop_Invariant (S.Used = Old.Used and then S.Programs = Old.Programs);
         pragma Loop_Invariant (S.Exercised = Old.Exercised and then S.Calls = Old.Calls);
         pragma Loop_Invariant (S.Booted = Old.Booted and then S.Handles_Used = Old.Handles_Used);
         pragma Loop_Invariant
           (for all K in Handle_Index =>
              S.Handles (K).Valid =
                (Old.Handles (K).Valid and then not Handle_Descends (Old.Handles, K, H)));
         pragma Loop_Invariant
           (for all K in Handle_Index =>
              S.Handles (K) = (Old.Handles (K) with delta Valid => S.Handles (K).Valid));
         pragma Loop_Invariant
           (for all J in Node =>
              S.Nodes (J) = (Old.Nodes (J) with delta
                               Valid => S.Nodes (J).Valid, Held => S.Nodes (J).Held));
         pragma Loop_Invariant
           (for all J in Node =>
              (if J > I then S.Nodes (J) = Old.Nodes (J)));
         pragma Loop_Invariant
           (for all J in Node range Node'First .. I =>
              S.Nodes (J).Valid =
                (Old.Nodes (J).Valid
                 and then not (Old.Nodes (J).How = Debug_Minted
                               and then Old.Nodes (J).Via /= No_Handle
                               and then Handle_Descends (Old.Handles, Old.Nodes (J).Via, H)))
              and then S.Nodes (J).Held =
                (Old.Nodes (J).Held
                 and then not (Old.Nodes (J).How = Debug_Minted
                               and then Old.Nodes (J).Via /= No_Handle
                               and then Handle_Descends (Old.Handles, Old.Nodes (J).Via, H))));
      end loop;
   end Revoke_Handle;

   procedure Mint_Debug
     (C : Configuration; S : in out State; Control : Node; Target, Holder : Program;
      Via : Handle_Count)
   is
      Old : constant State := S with Ghost;
   begin
      --  The sessions minted before: their parents come before them.
      pragma Assert (for all I in Node =>
                       (if Old.Nodes (I).Valid and then (Old.Nodes (I).Of_Kind = Debugging
                                                         or else Old.Nodes (I).How = Debug_Minted)
                        then Debug_Minted_Ok (C, Old, I)
                             and then Old.Nodes (I).Parent < I and then I <= Old.Used));
      Append (S, (Valid       => True,
                  Held        => True,
                  Holder      => Holder,
                  Of_Kind     => Debugging,
                  Scope       => Object_Of (Target),
                  Has         => (Invoke => True, others => False),
                  Parent      => Control,
                  How         => Debug_Minted,
                  Beneficiary => Kernel,
                  Within      => No_Node,
                  Envelope    => False,
                  Target      => Target,
                  Via         => Via));
      pragma Assert (Debug_Minted_Ok (C, S, S.Used));
      pragma Assert (for all I in Node =>
                       (if I /= S.Used then S.Nodes (I) = Old.Nodes (I)));
      pragma Assert (for all I in Node =>
                       (if I /= S.Used and then S.Nodes (I).Valid
                          and then (S.Nodes (I).Of_Kind = Debugging
                                    or else S.Nodes (I).How = Debug_Minted)
                        then S.Nodes (S.Nodes (I).Parent) = Old.Nodes (Old.Nodes (I).Parent)
                             and then Debug_Minted_Ok (C, S, I)));
   end Mint_Debug;

end Authority_Model;
