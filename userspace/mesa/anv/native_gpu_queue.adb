pragma Ada_2022;
with System;
with CuBit.GPU_Queue_Clients;
with CuBit.GPU_Sessions;
with CuBit.Messages;
with CuBit.Monotonic;
with Native_GPU_Job_Rules;

package body Native_GPU_Queue is

   package Sessions renames CuBit.GPU_Sessions;
   package Clients renames CuBit.GPU_Queue_Clients;
   package Rules renames Native_GPU_Job_Rules;
   use type GQ.Timeline_Value, Sessions.Wait_Result;

   pragma Compile_Time_Error
     (Rules.Forever /= CuBit.Messages.Wait_Forever, "Rules.Forever must be Wait_Forever");

   type Flag is new Boolean with Atomic;
   type Value_Array is array (GQ.Context_Index) of GQ.Timeline_Value with Atomic_Components;

   type Slot_State is limited record
      Session : Sessions.Session;
      --  A completion record or status line said the session failed (sticky).
      Failed  : Flag := False;
      --  The highest value a Completed record carried, per context: the
      --  status line may still show less for a turn.
      Reaped  : Value_Array := [others => GQ.No_Wait];
      --  The last value submitted per context.
      Last    : Value_Array := [others => GQ.No_Wait];
      --  1 while a thread sleeps in this session's one OP_GPU_WAKE.
      Waiter  : Unsigned_8 := 0 with Volatile;
   end record;

   --  No binder elaborates Mesa's Ada units (C imports them by name), so
   --  this table must need no elaboration code: it starts zeroed, which is
   --  every slot closed, and Open sets each field it uses before use.
   type Slot_Table is array (CuBit.Messages.CapabilitySlot) of Slot_State;
   pragma Suppress_Initialization (Slot_Table);
   Slots : Slot_Table;

   function Exchange (Where : System.Address; Value : Unsigned_8; Order : Integer)
     return Unsigned_8
     with Import, Convention => Intrinsic, External_Name => "__atomic_exchange_1";
   procedure Store (Where : System.Address; Value : Unsigned_8; Order : Integer)
     with Import, Convention => Intrinsic, External_Name => "__atomic_store_1";
   Sequentially_Consistent : constant := 5;   --  __ATOMIC_SEQ_CST

   Completed_Status : constant Unsigned_32 := GQ.Completion_Status'Enum_Rep (GQ.Completed);
   Execute_Code : constant Unsigned_32 := GQ.Opcode'Enum_Rep (GQ.Execute);
   Signal_Code  : constant Unsigned_32 := GQ.Opcode'Enum_Rep (GQ.Signal);

   function Valid_Slot (Slot : Unsigned_64) return Boolean is
     (Slot <= Unsigned_64 (CuBit.Messages.CapabilitySlot'Last));

   function Valid_Context (C : Unsigned_32) return Boolean is
     (C <= Unsigned_32 (GQ.Context_Index'Last));

   --  Take every completion record waiting (at most a ring of them).
   procedure Reap_All (State : in out Slot_State);
   procedure Reap_All (State : in out Slot_State) is
      Item : Clients.Q.Completion;
      Got : Boolean;
   begin
      for Look in 1 .. GQ.Slots loop
         Sessions.Reap (State.Session, Item, Got);
         exit when not Got;
         if Item.Answer.Status /= Completed_Status or else
           not Valid_Context (Item.Answer.Context)
         then
            State.Failed := True;
         elsif GQ.Timeline_Value (Item.Answer.Value) >
           State.Reaped (GQ.Context_Index (Item.Answer.Context))
         then
            State.Reaped (GQ.Context_Index (Item.Answer.Context)) :=
              GQ.Timeline_Value (Item.Answer.Value);
         end if;
      end loop;
   end Reap_All;

   function Open (Slot : Unsigned_64) return Queue_Status is
      Opened : Boolean;
   begin
      if not Valid_Slot (Slot) then
         return Invalid;
      end if;
      declare
         State : Slot_State renames Slots (CuBit.Messages.CapabilitySlot (Slot));
      begin
         if Sessions.Is_Open (State.Session) then
            return Invalid;
         end if;
         Sessions.Open (State.Session, CuBit.Messages.CapabilitySlot (Slot), Opened);
         if not Opened then
            return Closed;
         end if;
         State.Failed := False;
         Store (State.Waiter'Address, 0, Sequentially_Consistent);
         for C in GQ.Context_Index loop
            State.Reaped (C) := GQ.No_Wait;
            --  Next_Signal is the status line's Last_Accepted + 1.
            State.Last (C) := (if Sessions.Next_Signal (State.Session, C) = GQ.No_Wait then GQ.No_Wait
                               else Sessions.Next_Signal (State.Session, C) - 1);
         end loop;
         return OK;
      end;
   end Open;

   procedure Close (Slot : Unsigned_64) is
   begin
      if Valid_Slot (Slot) then
         Sessions.Close (Slots (CuBit.Messages.CapabilitySlot (Slot)).Session);
      end if;
   end Close;

   --  The job as the client submits it, or Valid False.
   procedure Decode (Item : Job; Decoded : out Clients.Job; Valid : out Boolean);
   procedure Decode (Item : Job; Decoded : out Clients.Job; Valid : out Boolean) is
      function Wait_Of (W : Wait_Words) return Clients.Wait is
        ((Context => (if W.Target = 0 then 0 else GQ.Context_Index (W.Context)),
          Target  => GQ.Timeline_Value (W.Target)));
      Waits_Valid : constant Boolean :=
        (Item.First.Target = 0 or else Valid_Context (Item.First.Context)) and then
        (Item.Second.Target = 0 or else Valid_Context (Item.Second.Context)) and then
        Item.First.Reserved = 0 and then Item.Second.Reserved = 0;
   begin
      Decoded := (others => <>);
      Valid := Item.Reserved = 0 and then Valid_Context (Item.Context) and then Waits_Valid and then
        ((Item.Operation = Execute_Code and then
            Rules.Valid_Batch (Item.Handle, Item.GPU, Item.Offset, Item.Bytes)) or else
         (Item.Operation = Signal_Code and then Item.Handle = 0 and then Item.GPU = 0 and then
            Item.Offset = 0 and then Item.Bytes = 0));
      if not Valid then
         return;
      end if;
      Decoded :=
        (Operation => (if Item.Operation = Execute_Code then GQ.Execute else GQ.Signal),
         Context   => GQ.Context_Index (Item.Context),
         Handle    => Item.Handle,
         GPU       => Item.GPU,
         Offset    => Item.Offset,
         Bytes     => Item.Bytes,
         First     => Wait_Of (Item.First),
         Second    => Wait_Of (Item.Second),
         Deadline  => GQ.Deadline_Us (Item.Deadline_Us));
   end Decode;

   function Submit
     (Slot : Unsigned_64; Item : access constant Job; Signal : access Unsigned_64)
      return Queue_Status
   is
      Decoded : Clients.Job;
      Valid, Submitted : Boolean := False;
      Tag : Clients.Token;
      Value : GQ.Timeline_Value := GQ.No_Wait;
      Ignore : Unsigned_64;
   begin
      if Signal /= null then
         Signal.all := 0;
      end if;
      if Item = null or else Signal = null or else not Valid_Slot (Slot) then
         return Invalid;
      end if;
      declare
         State : Slot_State renames Slots (CuBit.Messages.CapabilitySlot (Slot));
      begin
         if not Sessions.Is_Open (State.Session) then
            return Closed;
         end if;
         Decode (Item.all, Decoded, Valid);
         if not Valid then
            return Invalid;
         end if;
         --  A full queue is backpressure: look again, without a call, until
         --  room appears or the budget is spent.
         for Look in 0 .. Room_Budget_Ms / Room_Pause_Ms loop
            Reap_All (State);
            exit when State.Failed;
            Sessions.Submit (State.Session, Decoded, Tag, Value, Submitted);
            exit when Submitted;
            Ignore := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_SLEEP, Room_Pause_Ms);
         end loop;
         if Boolean (State.Failed) or else not Submitted then
            State.Failed := True;
            return Failed;
         end if;
         State.Last (Decoded.Context) := Value;
         Signal.all := Unsigned_64 (Value);
         return OK;
      end;
   end Submit;

   function Observe
     (Slot : Unsigned_64; Completed, Submitted : access Native_GPU_Timeline.Completed_Values)
      return Queue_Status
   is
      Line : GQ.Status_Line;
      Read : Boolean;
      Healthy : Boolean := True;
   begin
      if Completed = null or else Submitted = null or else not Valid_Slot (Slot) then
         return Invalid;
      end if;
      Completed.all := [others => GQ.No_Wait];
      Submitted.all := [others => GQ.No_Wait];
      declare
         State : Slot_State renames Slots (CuBit.Messages.CapabilitySlot (Slot));
      begin
         if not Sessions.Is_Open (State.Session) then
            return Closed;
         end if;
         for C in GQ.Context_Index loop
            Sessions.Read_Status (State.Session, C, Line, Read);
            --  A line the driver kept rewriting is read again next time.
            if Read and then not Rules.Healthy (Line.State) then
               Healthy := False;
            end if;
            Completed (C) := State.Reaped (C);
            if Read and then GQ.Timeline_Value (Line.Completed) > Completed (C) then
               Completed (C) := GQ.Timeline_Value (Line.Completed);
            end if;
            Submitted (C) := State.Last (C);
         end loop;
         return (if Healthy and then not Boolean (State.Failed) then OK else Failed);
      end;
   end Observe;

   function Wait
     (Slot : Unsigned_64; Context : Unsigned_32; Target, Remaining_Ns : Unsigned_64)
      return Queue_Status
   is
      Result : Sessions.Wait_Result;
      Deadline : Unsigned_64;
   begin
      if not Valid_Slot (Slot) or else not Valid_Context (Context) then
         return Invalid;
      end if;
      declare
         State : Slot_State renames Slots (CuBit.Messages.CapabilitySlot (Slot));
      begin
         if not Sessions.Is_Open (State.Session) then
            return Closed;
         elsif State.Failed then
            return Failed;
         end if;
         Deadline := Rules.Call_Deadline (CuBit.Monotonic.Milliseconds, Remaining_Ns);
         if Exchange (State.Waiter'Address, 1, Sequentially_Consistent) = 0 then
            Sessions.Wait_Reached
              (State.Session, GQ.Context_Index (Context), GQ.Timeline_Value (Target), Deadline, Result);
            Store (State.Waiter'Address, 0, Sequentially_Consistent);
         else
            Sessions.Poll_Reached
              (State.Session, GQ.Context_Index (Context), GQ.Timeline_Value (Target), Deadline, Result);
         end if;
         return (case Result is
                   when Sessions.Reached_Target => OK,
                   when Sessions.Context_Failed => Failed,
                   when Sessions.Deadline_Reached => Timed_Out,
                   when Sessions.Queue_Ended => Closed);
      end;
   end Wait;

end Native_GPU_Queue;
