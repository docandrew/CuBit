with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with CuBit.GPU_Queues;
with CuBit.GPU_Queue_Clients;
with GPU_Queue_Model;
with Intel_GPU_Queue_Service;
with Native_GPU_Job_Rules;
with Native_GPU_Timeline;
-- GPU-001 step 3, Linux-hosted: Mesa's GPU timeline logic (Native_GPU_Timeline)
-- and job rules, driven as the Mesa glue drives them, through the production
-- queue client against the step 2 queue service and its modelled GPU
-- (tests/intel-gpu/gpu_queue_model). Points are added after a submit, and
-- timelines resolve against completed values taken as Native_GPU_Queue.Observe
-- takes them: the status line, or a Completed record reaped since. Plus a
-- randomized comparison of the timeline with a plain reference model. Not
-- hardware evidence, and not the C sync glue (test-gpu-timeline.py has that).
procedure GPU_Timeline_Tests is
   package Q renames CuBit.GPU_Queues;
   package Clients renames CuBit.GPU_Queue_Clients;
   package M renames GPU_Queue_Model;
   package TL renames Native_GPU_Timeline;
   package Rules renames Native_GPU_Job_Rules;
   use type Q.Timeline_Value, Q.Context_State, Q.Opcode, TL.Wait_Set, TL.Sync_Value, TL.Add_Result, TL.Wait_Kind,
            TL.Point, TL.Wait;

   Hang_Budget : constant := 1_000_000;   -- 1 s, as the driver
   package Service is new Intel_GPU_Queue_Service
     (M.Session_Id, M.Select_Context, M.Owner_Ready, M.Batch_Ready, M.Scheduling_Resident, M.Publish_Ready,
      M.Write_Segment, M.Segment_Bytes, M.Kick, M.Read_Timeline, M.Read_Job_Time, M.Now_Us, M.Quarantine,
      M.Call_Finished, M.Answer_Wake, M.Client_Region, M.Server_Region, Hang_Budget);

   T : access Service.Table;
   Client : constant access Clients.Client := new Clients.Client;
   Reaped : TL.Completed_Values := [others => 0];
   Failed_Records : Natural := 0;
   Failures, Checks, Turns : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   Completed_Code : constant Unsigned_32 := Q.Completion_Status'Enum_Rep (Q.Completed);

   -- One driver turn, then what Native_GPU_Queue.Submit's Reap_All does.
   procedure Turn is
      Item : Clients.Q.Completion;
      Got : Boolean;
   begin
      Service.Turn (T.all);
      M.Advance;
      Turns := Turns + 1;
      loop
         Clients.Reap (Client.all, Item, Got);
         exit when not Got;
         if Item.Answer.Status /= Completed_Code then
            Failed_Records := Failed_Records + 1;
         elsif Q.Timeline_Value (Item.Answer.Value) > Reaped (Q.Context_Index (Item.Answer.Context)) then
            Reaped (Q.Context_Index (Item.Answer.Context)) := Q.Timeline_Value (Item.Answer.Value);
         end if;
      end loop;
   end Turn;

   -- Native_GPU_Queue.Observe's completed values.
   function Completed return TL.Completed_Values is
      Result : TL.Completed_Values := Reaped;
      Line : Q.Status_Line;
      OK : Boolean;
   begin
      for C in Q.Context_Index loop
         Clients.Read_Status (Client.all, C, Line, OK);
         if OK and then Q.Timeline_Value (Line.Completed) > Result (C) then
            Result (C) := Q.Timeline_Value (Line.Completed);
         end if;
      end loop;
      return Result;
   end Completed;

   function Healthy return Boolean is
      Line : Q.Status_Line;
      OK : Boolean;
      All_Healthy : Boolean := True;
   begin
      for C in Q.Context_Index loop
         Clients.Read_Status (Client.all, C, Line, OK);
         All_Healthy := All_Healthy and then (not OK or else Rules.Healthy (Line.State));
      end loop;
      return All_Healthy;
   end Healthy;

   procedure Fresh (Contexts : Positive := 1) is
      Opened : Boolean;
   begin
      M.Reset;
      T := new Service.Table;
      Reaped := [others => 0];
      Failed_Records := 0;
      for C in 0 .. Contexts - 1 loop
         Service.Open_Context (T.all, 1, Q.Context_Index (C), 1, M.Setup_Bytes, Opened);
         Check (Opened, "open context");
      end loop;
      Service.Open_Queue (T.all, 1, Opened);
      Check (Opened, "open queue");
      Clients.Attach (Client.all, M.Client_Region (1), M.Server_Region (1));
   end Fresh;

   -- A job the way queue_submit_locked builds it: waits through the wait set.
   procedure Submit
     (Context : Q.Context_Index; Waits : TL.Wait_Set; Signal : out TL.GPU_Value; OK : out Boolean;
      Operation : Q.Opcode := Q.Execute)
   is
      First, Second : TL.Wait;
      Fits, Kick : Boolean;
      Tag : Clients.Token;
   begin
      TL.Select_Waits (Waits, First, Second, Fits);
      Signal := 0;
      OK := False;
      if not Fits then
         return;
      end if;
      Clients.Submit
        (Client.all,
         (Operation => Operation, Context => Context,
          Handle => (if Operation = Q.Execute then 7 else 0),
          GPU => (if Operation = Q.Execute then 16#10_0000# else 0), Offset => 0,
          Bytes => (if Operation = Q.Execute then 64 else 0),
          First => (Context => First.Context, Target => First.Target),
          Second => (Context => Second.Context, Target => Second.Target),
          Deadline => Q.No_Deadline, Kinds => 0),
         Tag, Signal, OK, Kick);
   end Submit;

   No_Waits : TL.Wait_Set;
   type Positive_Array is array (Positive range <>) of Positive;
   type GPU_Values is array (Positive range <>) of TL.GPU_Value;

   ------------------------------------------------------------------------
   -- 1. Desktop's shape at depth: jobs in flight, a semaphore point each;
   --    the timeline is never ahead of the GPU, ends exact, stays bounded.
   ------------------------------------------------------------------------
   procedure Many_In_Flight (Total : Positive; Depth : Positive) is
      S : TL.Timeline;
      Signal : TL.GPU_Value;
      OK : Boolean;
      Result : TL.Add_Result;
      Next : TL.Sync_Value := 1;
      Start : constant Natural := Turns;
      Submitted : Natural := 0;
      Ahead, Unbounded : Natural := 0;
      Peak : Natural := 0;
      Limit : constant Natural := Turns + Total * (M.Latency + 2) + 10_000;
      -- Each point's GPU value, and how many points the model GPU has
      -- really reached (they are in order on one context).
      GPU_Of : constant access GPU_Values := new GPU_Values (1 .. Total);
      Truth : Natural := 0;
   begin
      Fresh;
      TL.Initialize (S, 0);
      Service.Reset_Peak (T.all);
      while S.Value < TL.Sync_Value (Total) and then Turns < Limit loop
         while Submitted < Total and then
           Natural (Unsigned_64 (Next - 1) - Unsigned_64 (S.Value)) < Depth and then
           Clients.Can_Submit (Client.all)
         loop
            Submit (0, No_Waits, Signal, OK);
            exit when not OK;
            TL.Resolve (S, Completed);
            TL.Add_Point (S, Next, 0, Signal, Result);
            Check (Result = TL.Added, "point added");
            GPU_Of (Natural (Next)) := Signal;
            Next := Next + 1;
            Submitted := Submitted + 1;
            if S.Count > TL.Max_Points then
               Unbounded := Unbounded + 1;
            end if;
         end loop;
         Turn;
         TL.Resolve (S, Completed);
         while Truth < Natural (Next - 1) and then
           GPU_Of (Truth + 1) <= Q.Timeline_Value (M.Completed (1, 0))
         loop
            Truth := Truth + 1;
         end loop;
         if S.Value > TL.Sync_Value (Truth) then
            Ahead := Ahead + 1;
         end if;
         Peak := Natural'Max (Peak, Service.Stats (T.all).In_Flight_Peak);
      end loop;
      Check (S.Value = TL.Sync_Value (Total) and then S.Count = 0,
             "depth" & Depth'Image & ": every point resolved");
      Check (Ahead = 0, "depth" & Depth'Image & ": never ahead of the GPU");
      Check (Unbounded = 0, "depth" & Depth'Image & ": at most a queue's worth of points");
      Check (Failed_Records = 0 and then Healthy, "depth" & Depth'Image & ": healthy");
      Check (Peak <= Natural'Min (Depth, Q.Max_In_Flight_Per_Context) and then
             (Depth = 1 or else Peak > 1),
             "depth" & Depth'Image & ": jobs in flight" & Peak'Image);
      Put_Line ("  depth" & Depth'Image & ":" & Total'Image & " jobs, peak in flight" & Peak'Image &
                "," & Natural'Image (Turns - Start) & " turns");
   end Many_In_Flight;

   ------------------------------------------------------------------------
   -- 2. Waits: a chain of jobs each waiting on the previous one's point on
   --    the same context carries no descriptor wait (ring order covers it;
   --    on hardware a held head costs a driver turn and a kick per job,
   --    which this model does not time), runs pipelined and completes. A
   --    wait on another context holds the job until that context gets there.
   ------------------------------------------------------------------------
   function Chain (Total : Positive) return Natural is
      Previous : TL.GPU_Value := 0;
      Signal : TL.GPU_Value;
      OK : Boolean;
      Waits : TL.Wait_Set;
      Start : constant Natural := Turns;
      Done : Natural := 0;
      With_Waits : Natural := 0;
   begin
      Fresh;
      for N in 1 .. Total loop
         TL.Clear (Waits);
         if Previous /= 0 then
            TL.Merge (Waits, 0, (Value => 1, GPU => Previous, Context => 0));
         end if;
         if Waits /= No_Waits then
            With_Waits := With_Waits + 1;
         end if;
         loop
            Submit (0, Waits, Signal, OK);
            exit when OK;
            Turn;
         end loop;
         Previous := Signal;
      end loop;
      while Completed (0) < Previous and then Done < 100_000 loop
         Turn;
         Done := Done + 1;
      end loop;
      Check (Completed (0) = Previous and then Failed_Records = 0, "chain completes");
      Check (With_Waits = 0, "same-context waits are dropped");
      Check (Turns - Start <= Total * M.Latency + Q.Max_In_Flight_Per_Context,
             "the chain runs pipelined");
      return Turns - Start;
   end Chain;

   procedure Waits_Across_Contexts is
      S : TL.Timeline;
      A, B : TL.GPU_Value;
      OK : Boolean;
      Result : TL.Add_Result;
      Kind : TL.Wait_Kind;
      P : TL.Point;
      Waits : TL.Wait_Set;
      B_Done_Before_A : Boolean := False;
   begin
      Fresh (Contexts => 2);
      TL.Initialize (S, 0);
      M.Latency := 6;
      Submit (0, No_Waits, A, OK);
      Check (OK, "job on context 0");
      TL.Add_Point (S, 1, 0, A, Result);
      TL.Find (S, 1, Kind, P);
      Check (Kind = TL.On_GPU and then P.Context = 0 and then P.GPU = A, "wait found on context 0");
      TL.Clear (Waits);
      TL.Merge (Waits, 1, P);
      Submit (1, Waits, B, OK);
      Check (OK, "job on context 1 waiting for context 0");
      for Look in 1 .. 200 loop
         Turn;
         if Q.Timeline_Value (M.Completed (1, 1)) >= B and then
           Q.Timeline_Value (M.Completed (1, 0)) < A
         then
            B_Done_Before_A := True;
         end if;
      end loop;
      Check (not B_Done_Before_A, "a cross-context wait holds the job");
      Check (Completed (1) = B and then Completed (0) = A, "both complete");
      -- Two other contexts fit, each at its highest value.
      TL.Clear (Waits);
      TL.Merge (Waits, 0, (Value => 1, GPU => 5, Context => 2));
      TL.Merge (Waits, 0, (Value => 2, GPU => 9, Context => 1));
      TL.Merge (Waits, 0, (Value => 3, GPU => 7, Context => 1));
      TL.Merge (Waits, 0, (Value => 4, GPU => 3, Context => 2));
      declare
         First, Second : TL.Wait;
         Fits : Boolean;
      begin
         TL.Select_Waits (Waits, First, Second, Fits);
         Check (Fits and then First = (Context => 1, Target => 9) and then
                Second = (Context => 2, Target => 5), "two waits, highest per context");
      end;
      -- Three contexts do not fit one descriptor.
      TL.Clear (Waits);
      for C in Q.Context_Index range 1 .. 3 loop
         TL.Merge (Waits, 0, (Value => 1, GPU => 1, Context => C));
      end loop;
      declare
         First, Second : TL.Wait;
         Fits : Boolean;
      begin
         TL.Select_Waits (Waits, First, Second, Fits);
         Check (not Fits, "three contexts overflow a descriptor");
      end;
   end Waits_Across_Contexts;

   ------------------------------------------------------------------------
   -- 3. A hung GPU: the context is lost, no point resolves, records fail.
   ------------------------------------------------------------------------
   procedure Hang is
      S : TL.Timeline;
      Signal : TL.GPU_Value;
      OK : Boolean;
      Result : TL.Add_Result;
   begin
      Fresh;
      TL.Initialize (S, 0);
      M.Hung := True;
      Submit (0, No_Waits, Signal, OK);
      TL.Add_Point (S, 1, 0, Signal, Result);
      for Look in 1 .. 1_200 loop   -- past the 1 s hang budget
         Turn;
         TL.Resolve (S, Completed);
      end loop;
      Check (S.Value = 0 and then S.Count = 1, "a hung job never resolves");
      Check (not Healthy and then Failed_Records = 1, "the session reports loss");
   end Hang;

   ------------------------------------------------------------------------
   -- 4. The timeline against a reference model, random operations.
   ------------------------------------------------------------------------
   procedure Random_Reference (Steps : Positive) is
      subtype Small is Natural range 0 .. 15;
      package Random is new Ada.Numerics.Discrete_Random (Small);
      G : Random.Generator;
      S, Other : TL.Timeline;
      -- Reference: the reached value and the pending points as a list.
      type Ref_Point is record
         Value : TL.Sync_Value;
         GPU : TL.GPU_Value;
         Context : Q.Context_Index;
      end record;
      type Ref_List is array (1 .. TL.Max_Points) of Ref_Point;
      Ref_Value : TL.Sync_Value := 0;
      Ref : Ref_List;
      Ref_Count : Natural := 0;
      Done : TL.Completed_Values := [others => 0];
      Next_Value : TL.Sync_Value := 1;
      Mismatches : Natural := 0;
      procedure Compare is
      begin
         if S.Value /= Ref_Value or else S.Count /= Ref_Count then
            Mismatches := Mismatches + 1;
            return;
         end if;
         for I in 1 .. Ref_Count loop
            if S.Points (I).Value /= Ref (I).Value or else S.Points (I).GPU /= Ref (I).GPU or else
              S.Points (I).Context /= Ref (I).Context
            then
               Mismatches := Mismatches + 1;
               return;
            end if;
         end loop;
      end Compare;
   begin
      Random.Reset (G, 2026);
      TL.Initialize (S, 0);
      for Step in 1 .. Steps loop
         case Random.Random (G) is
            when 0 .. 6 =>
               -- Add: usually increasing, sometimes not.
               declare
                  C : constant Q.Context_Index := Q.Context_Index (Random.Random (G) mod 4);
                  Pick : constant Small := Random.Random (G);
                  V : constant TL.Sync_Value :=
                    (if Pick = 0 and then Next_Value > 2 then Next_Value - 2
                     elsif Pick = 1 and then Next_Value > 1 then Next_Value - 1   -- the last value again
                     else Next_Value);
                  GPU : constant TL.GPU_Value := Done (C) + TL.GPU_Value (Random.Random (G) + 1);
                  Result : TL.Add_Result;
                  Last : constant TL.Sync_Value :=
                    (if Ref_Count = 0 then Ref_Value else Ref (Ref_Count).Value);
               begin
                  TL.Add_Point (S, V, C, GPU, Result);
                  if V > Last and then Ref_Count < TL.Max_Points then
                     Ref_Count := Ref_Count + 1;
                     Ref (Ref_Count) := (V, GPU, C);
                     Next_Value := V + 1;
                     Check (Result = TL.Added, "reference: added");
                  else
                     Check (Result /= TL.Added, "reference: refused");
                  end if;
               end;
            when 7 .. 10 =>
               -- The GPU moves on, then resolve.
               declare
                  C : constant Q.Context_Index := Q.Context_Index (Random.Random (G) mod 4);
                  Highest : Natural := 0;
               begin
                  Done (C) := Done (C) + TL.GPU_Value (Random.Random (G));
                  TL.Resolve (S, Done);
                  for I in 1 .. Ref_Count loop
                     if Ref (I).GPU <= Done (Ref (I).Context) then
                        Highest := I;
                     end if;
                  end loop;
                  if Highest > 0 then
                     Ref_Value := Ref (Highest).Value;
                     Ref (1 .. Ref_Count - Highest) := Ref (Highest + 1 .. Ref_Count);
                     Ref_Count := Ref_Count - Highest;
                  end if;
               end;
            when 11 =>
               -- A CPU signal, sometimes backwards.
               declare
                  R : constant TL.Sync_Value := TL.Sync_Value (Random.Random (G));
                  V : constant TL.Sync_Value :=
                    (if R < 3 and then Ref_Value >= 3 then Ref_Value - (3 - R) else Ref_Value + R);
                  OK : Boolean;
                  Keep : Natural := 0;
               begin
                  TL.Signal (S, V, OK);
                  Check (OK = (V >= Ref_Value), "reference: signal accepted iff not backwards");
                  if V >= Ref_Value then
                     Ref_Value := V;
                     for I in 1 .. Ref_Count loop
                        if Ref (I).Value > V then
                           Keep := Keep + 1;
                           Ref (Keep) := Ref (I);
                        end if;
                     end loop;
                     Ref_Count := Keep;
                     Next_Value := TL.Sync_Value'Max (Next_Value, V + 1);
                  end if;
               end;
            when 12 .. 13 =>
               -- Find for a value around what is there.
               declare
                  W : constant TL.Sync_Value := Ref_Value + TL.Sync_Value (Random.Random (G));
                  Kind : TL.Wait_Kind;
                  P : TL.Point;
                  Last : constant TL.Sync_Value :=
                    (if Ref_Count = 0 then Ref_Value else Ref (Ref_Count).Value);
                  Expected : TL.Point := (others => <>);
               begin
                  TL.Find (S, W, Kind, P);
                  for I in reverse 1 .. Ref_Count loop
                     if Ref (I).Value >= W then
                        Expected := (Ref (I).Value, Ref (I).GPU, Ref (I).Context);
                     end if;
                  end loop;
                  Check ((if W <= Ref_Value then Kind = TL.Reached
                          elsif W > Last then Kind = TL.Unsubmitted
                          else Kind = TL.On_GPU and then P = Expected),
                         "reference: find");
               end;
            when others =>
               -- Move to another timeline and back (a binary payload).
               declare
                  Next_Point : constant TL.Sync_Value :=
                    (if Ref_Count = 0 then Ref_Value else Ref (1).Value);
                  Target_Next, Source_Next, Back_Next, Back_Source : TL.Sync_Value;
                  OK : Boolean;
               begin
                  TL.Initialize (Other, 0);
                  TL.Move (Other, S, Next_Point, Target_Next, Source_Next, OK);
                  Check (OK = (Next_Point <= (if Ref_Count = 0 then Ref_Value else Ref (Ref_Count).Value)),
                         "reference: move needs a signalled or pending event");
                  if OK then
                     Check (S.Count = 0 and then Source_Next > S.Value, "reference: source unsignalled");
                     TL.Move (S, Other, Target_Next, Back_Next, Back_Source, OK);
                     Check (OK and then Back_Next = Next_Point, "reference: moved back");
                  end if;
               end;
         end case;
         Compare;
      end loop;
      Check (Mismatches = 0, "reference: timeline and model agree after every step");
   end Random_Reference;

   ------------------------------------------------------------------------
   -- 5. The job rules at their edges.
   ------------------------------------------------------------------------
   procedure Job_Rules is
      Max : constant := Rules.Max_Batch_Bytes;
   begin
      Check (Rules.Valid_Batch (7, 16#20000#, 0, 4096), "valid batch");
      Check (Rules.Valid_Batch (7, 16#20000#, Max - 4096, 4096), "batch at the end of its BO");
      Check (not Rules.Valid_Batch (0, 16#20000#, 0, 4096), "no handle");
      Check (not Rules.Valid_Batch (7, 0, 0, 4096), "no address");
      Check (not Rules.Valid_Batch (7, 16#20004#, 0, 4096), "misaligned address");
      Check (not Rules.Valid_Batch (7, 2 ** 48, 0, 4096), "address beyond 48 bits");
      Check (not Rules.Valid_Batch (7, 2 ** 48 - 8, 0, 16), "batch wrapping the address space");
      Check (not Rules.Valid_Batch (7, 16#20000#, 4, 4096), "misaligned offset");
      Check (not Rules.Valid_Batch (7, 16#20000#, 0, 6), "partial dword");
      Check (not Rules.Valid_Batch (7, 16#20000#, 0, Max + 4), "batch too large");
      Check (not Rules.Valid_Batch (7, 16#20000#, Max - 4088, 4096), "batch past its BO's limit");
      Check (Rules.Call_Deadline (100, Rules.No_Timeout_Ns) = Rules.Forever, "no timeout");
      Check (Rules.Call_Deadline (100, 0) = 100, "zero");
      Check (Rules.Call_Deadline (100, 1) = 101, "rounded up");
      Check (Rules.Call_Deadline (100, 1_000_000) = 101, "one millisecond");
      Check (Rules.Call_Deadline (100, 1_000_001) = 102, "just over");
      Check (Rules.Call_Deadline (100, Rules.No_Timeout_Ns - 1) < Rules.Forever, "finite stays finite");
      Check (Rules.Call_Deadline (Rules.Forever - 2, 5_000_000) = Rules.Forever - 1, "saturates below Forever");
      Check (Rules.Healthy (Q.Context_State'Enum_Rep (Q.Active)) and then
             Rules.Healthy (Q.Context_State'Enum_Rep (Q.Unused)), "healthy states");
      for S in Q.Faulted .. Q.Retired loop
         Check (not Rules.Healthy (Q.Context_State'Enum_Rep (S)), "unhealthy " & S'Image);
      end loop;
      Check (not Rules.Healthy (99), "unknown state");
   end Job_Rules;

   Chained : Natural;
begin
   TL.Clear (No_Waits);
   for Depth of Positive_Array'(1, 2, 8, 32, 64) loop
      Many_In_Flight (10_000, Depth);
   end loop;
   Many_In_Flight (40_000, 32);
   Chained := Chain (1_000);
   Waits_Across_Contexts;
   Hang;
   Random_Reference (200_000);
   Job_Rules;
   if Failures = 0 then
      Put_Line ("GPU timeline tests PASS:" & Checks'Image & " checks, depths 1-64 and 40000 jobs at 32, "
                & "1000 chained jobs in" & Chained'Image & " turns with no descriptor wait"
                & ", cross-context wait, hang, 200000 random steps against a reference model");
   else
      Put_Line ("GPU timeline tests FAILED:" & Failures'Image & " of" & Checks'Image);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end GPU_Timeline_Tests;
