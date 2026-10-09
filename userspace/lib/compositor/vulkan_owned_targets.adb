with Vulkan_Owned_Target_FFI;
package body Vulkan_Owned_Targets with SPARK_Mode is
   use type System.Address, I.U32, P.Ticket, P.ID;
   procedure Record_Readback (S : State; Submission : in out V.State;
      Pool : P.State; Staging : System.Address; Accepted : out Boolean) is
      Result : I.U32;
   begin
      V.Admit_Draw (Submission, Accepted);
      if not Accepted then return; end if;
      if not Ready (S) or else not Same_Submission (S, Submission) or else
         P.Faulted (Pool) or else P.Readback (Pool) = P.None or else
         P.Epoch (Pool) /= Output_Epoch (S) or else Staging = System.Null_Address
      then Accepted := False;
      else
         Vulkan_Owned_Target_FFI.Record_Readback (S.Description,
           V.Owner_Context (Submission), Staging, I.U32 (P.Readback (Pool).Buffer), Result);
         Accepted := Result = 0;
      end if;
      if not Accepted then V.Reject_Frame (Submission); end if;
   end Record_Readback;
   procedure Record_Readback_Regions (S : State; Submission : in out V.State;
      Pool : P.State; Staging : System.Address; Repair : Compositor_Target_Damage.D.State; Accepted : out Boolean) is
      Result : I.U32;
   begin
      V.Admit_Draw (Submission, Accepted);
      if not Accepted then return; end if;
      if not Ready (S) or else not Same_Submission (S, Submission) or else
         P.Faulted (Pool) or else P.Readback (Pool) = P.None or else
         P.Epoch (Pool) /= Output_Epoch (S) or else Staging = System.Null_Address
      then Accepted := False;
      else
         Vulkan_Owned_Target_FFI.Record_Readback_Regions (S.Description,
           V.Owner_Context (Submission), Staging, I.U32 (P.Readback (Pool).Buffer), Repair, Result);
         Accepted := Result = 0;
      end if;
      if not Accepted then V.Reject_Frame (Submission); end if;
   end Record_Readback_Regions;
   procedure Initialize
     (S : in out State; Context : in out C.State;
      Requests : Vulkan_Frame.Targets; Description : System.Address;
      Epoch : P.Live_ID; Submission : V.State;
      Budget : in out A.State; Allowed_Types : I.U32)
   is
   begin
      if C.Current (Context) /= C.Live or else
        C.Context (Context) /= V.Owner_Context (Submission)
      then S.Mode := Closed; return; end if;
      C.Register_Child (Context, S.Parent_Ticket);
      if S.Parent_Ticket = C.No_Child then S.Mode := Closed; return; end if;
      pragma Assert (Parent_Held (S, Context));
      Allocate (S, Requests, Budget, Allowed_Types);
      pragma Assert (Parent_Held (S, Context));
      if Current (S) = Backed then
         Attach (S, Description, Epoch, Submission, Budget);
         pragma Assert (Parent_Held (S, Context));
      end if;
      if Current (S) = Closed then
         C.Retire_Child (Context, S.Parent_Ticket, True);
         S.Parent_Ticket := C.No_Child;
      end if;
   end Initialize;
   procedure Close
     (S : in out State; Context : in out C.State;
      Submission : V.State; Pool : P.State;
      Budget : in out A.State; Released : out Boolean)
   is
   begin
      Released := False;
      if not Parent_Held (S, Context) then return; end if;
      -- Closed also supports callers that already completed low-level Close;
      -- it cannot turn uncertain or still-live backing into retired storage.
      if Current (S) = Closed then Released := True;
      else Close (S, Submission, Pool, Budget, Released);
      end if;
      if Released then
         C.Retire_Child (Context, S.Parent_Ticket, True);
         S.Parent_Ticket := C.No_Child;
      end if;
   end Close;
   procedure Prepare_Frame (S : State; Submission : V.State; Pool : P.State;
      Damage : Compositor_Target_Damage.State; Accepted : out Boolean) is
      package D renames Compositor_Target_Damage;
      use type V.Phase, P.Slot, P.ID;
      Result : I.U32;
      Slot : constant P.Slot := D.Active (Damage);
   begin
      Accepted := False;
      if not Ready (S) or else not Same_Submission (S, Submission) or else
         V.Current (Submission) /= V.Recording or else V.Pass_Started (Submission) or else
         not P.Valid (Pool) or else not P.Rendering (Pool) or else
         Output_Epoch (S) /= P.Epoch (Pool) or else Slot = 0 or else
         Slot /= P.Writer (Pool).Buffer or else D.Faulted (Damage) or else
         D.Bounds (Damage).Left /= 0 or else D.Bounds (Damage).Top /= 0
      then return; end if;
      if not D.Initialized (Damage, Slot) and then
         not D.D.Covers (D.Painting (Damage), D.Bounds (Damage)) then return; end if;
      Vulkan_Owned_Target_FFI.Prepare_Frame (S.Description, S.Context,
        I.U32 (Slot), I.U32 (D.Bounds (Damage).Right), I.U32 (D.Bounds (Damage).Bottom),
        not D.Initialized (Damage, Slot), Result);
      Accepted := Result = 0;
   end Prepare_Frame;
   procedure Free_Backing (S : in out State; Budget : in out A.State)
     with Pre => A.Valid (Budget),
       Post => A.Valid (Budget) and A.Limit (Budget) = A.Limit (Budget'Old) and Same_Parent (S, S'Old) and Current (S) in Closed | Quarantined
   is
   begin
      S.Mode := Closed;
      for N in P.Live_Slot loop
         pragma Loop_Invariant (A.Valid (Budget));
         pragma Loop_Invariant (A.Limit (Budget) = A.Limit (Budget'Loop_Entry));
         pragma Loop_Invariant (S.Mode in Closed | Quarantined);
         if I.Status (S.Backing (N)) = I.Live and then I.Can_Release (S.Backing (N), Budget) then
            I.Release (S.Backing (N), Budget, True);
         end if;
         if I.Status (S.Backing (N)) not in I.Fresh | I.Closed then S.Mode := Quarantined; end if;
      end loop;
   end Free_Backing;
   procedure Allocate (S : in out State; Requests : Vulkan_Frame.Targets;
                       Budget : in out A.State; Allowed_Types : I.U32) is
   begin
      for N in P.Live_Slot loop
         if Requests (N) = System.Null_Address then S.Mode := Closed; return; end if;
         for M in P.Live_Slot loop
            if M /= N and Requests (M) = Requests (N) then S.Mode := Closed; return; end if;
         end loop;
      end loop;
      S.Requests := Requests;
      for N in P.Live_Slot loop
         pragma Loop_Invariant (A.Valid (Budget));
         pragma Loop_Invariant (A.Limit (Budget) = A.Limit (Budget'Loop_Entry));
         pragma Loop_Invariant (T.Current (S.Views) = T.Fresh);
         pragma Loop_Invariant (for all M in N .. P.Live_Slot'Last => I.Status (S.Backing (M)) = I.Fresh);
         pragma Loop_Invariant (for all M in P.Live_Slot'First .. N - 1 => I.Status (S.Backing (M)) = I.Live);
         I.Prepare (S.Backing (N), Requests (N));
         if I.Status (S.Backing (N)) = I.Prepared then I.Allocate (S.Backing (N), Budget, Allowed_Types); end if;
         if I.Status (S.Backing (N)) /= I.Live then Free_Backing (S, Budget); return; end if;
      end loop;
      S.Mode := Backed;
   end Allocate;
   procedure Attach (S : in out State; Description : System.Address;
                     Epoch : P.Live_ID; Submission : V.State; Budget : in out A.State) is
      Result : I.U32;
      Accepted : Boolean;
   begin
      S.Context := V.Owner_Context (Submission);
      S.Description := Description;
      if S.Context = System.Null_Address then Free_Backing (S, Budget); return; end if;
      Vulkan_Owned_Target_FFI.Bind (Description, S.Requests (1), S.Requests (2), S.Requests (3), S.Context, Result);
      if Result /= 0 then S.Mode := Quarantined; return; end if;
      T.Initialize (S.Views, Description, Epoch, Accepted);
      if Accepted then S.Mode := Live;
      elsif T.Current (S.Views) = T.Closed then Free_Backing (S, Budget);
      else S.Mode := Quarantined;
      end if;
   end Attach;
   procedure Close (S : in out State; Submission : V.State; Pool : P.State;
                    Budget : in out A.State; Released : out Boolean) is
      Views_Released : Boolean;
   begin
      Released := False;
      if S.Mode = Backed then
         -- Unpublished images cannot have submission/display readers. Callers
         -- must not submit these private handles before Attach succeeds.
         Free_Backing (S, Budget); Released := S.Mode = Closed; return;
      end if;
      if not Can_Close (S, Submission, Pool) then return; end if;
      T.Close (S.Views, Submission, Pool, Views_Released);
      if Views_Released then Free_Backing (S, Budget); Released := S.Mode = Closed;
      else S.Mode := Quarantined;
      end if;
   end Close;
end Vulkan_Owned_Targets;
