-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- Boot_Timeline_Records body: see the specification.
-------------------------------------------------------------------------------
pragma Ada_2022;

package body Boot_Timeline_Records with
   SPARK_Mode => On
is
   --  Field by field: no whole-State temporary on a 4 KiB kernel stack.
   procedure Initialize (S : in out State) is
      Empty_Process : constant Process_Entry := (others => <>);
      Empty_Wait    : constant Wait_Entry := (others => <>);
   begin
      S.Wire.Head := (others => <>);
      S.Wire.Stages := [others => Not_Recorded];
      S.Wire.Milestones := [others => (others => <>)];
      for N in Process_Number loop
         S.Wire.Processes (N) := Empty_Process;
      end loop;
      for N in Wait_Number loop
         S.Wire.Waits (N) := Empty_Wait;
      end loop;
      S.Map := [others => 0];
   end Initialize;

   procedure Expire (S : in out State; Now : Ticks) is
   begin
      if Recording (S) and then S.Wire.Head.Window_End /= Not_Recorded
        and then Now >= S.Wire.Head.Window_End
      then
         S.Wire.Head.Recording := Closed;
         S.Wire.Head.Closed_At := Now;
      end if;
   end Expire;

   procedure Mark_Stage (S : in out State; Stage : Kernel_Stage; Now : Ticks) is
   begin
      Expire (S, Now);
      if Recording (S) and then S.Wire.Stages (Slot_Of (Stage)) = Not_Recorded then
         S.Wire.Stages (Slot_Of (Stage)) := Now;
      end if;
   end Mark_Stage;

   procedure Set_Frequency (S : in out State; Frequency : Unsigned_64; Now : Ticks) is
      Window : Ticks;
   begin
      if S.Wire.Head.Frequency /= 0 or else Frequency = 0 or else not Recording (S) then
         return;
      end if;
      --  Frequency * Recording_Window_Seconds, saturating.
      if Frequency > Ticks'Last / Recording_Window_Seconds then
         Window := Ticks'Last;
      else
         Window := Frequency * Recording_Window_Seconds;
      end if;
      S.Wire.Head.Frequency := Frequency;
      S.Wire.Head.Window_End := Saturating_Add (Now, Window);
      if S.Wire.Head.Window_End = Not_Recorded then
         S.Wire.Head.Window_End := 1;
      end if;
      S.Wire.Head.Minimum_Wait := Frequency / Wait_Fraction;
   end Set_Frequency;

   procedure Note_Spawn
     (S            : in out State;
      Slot         : Process_Slot;
      Identity     : Identity_Word;
      Spawner      : Identity_Word;
      Spawner_Slot : Process_Slot;
      Name         : Name_Text;
      Started      : Ticks;
      Loaded       : Ticks)
   is
      Next : Process_Number;
      Parent : Process_Count;
   begin
      Expire (S, Loaded);
      if not Recording (S) then
         return;
      end if;
      if Spawner /= No_Identity then
         Parent := S.Map (Spawner_Slot);
         if Parent /= 0 and then S.Wire.Processes (Parent).Identity = Spawner then
            S.Wire.Processes (Parent).Ready := Not_Recorded;
         end if;
      end if;
      if S.Wire.Head.Processes = Process_Capacity then
         S.Wire.Head.Processes_Dropped :=
           Saturating_Increment (S.Wire.Head.Processes_Dropped);
         --  A reused slot no longer names its old entry.
         S.Map (Slot) := 0;
         return;
      end if;
      Next := S.Wire.Head.Processes + 1;
      S.Wire.Processes (Next) :=
        (Identity      => Identity,
         Spawner       => Spawner,
         Name          => Name,
         Spawn_Started => Started,
         Loaded        => Loaded,
         others        => <>);
      S.Wire.Head.Processes := Next;
      S.Map (Slot) := Next;
   end Note_Spawn;

   procedure Note_Event
     (S     : in out State;
      Slot  : Process_Slot;
      Event : Process_Event;
      Now   : Ticks)
   is
      N : constant Process_Count := S.Map (Slot);
   begin
      Expire (S, Now);
      if not Recording (S) or else N = 0 then
         return;
      end if;
      declare
         E : Process_Entry renames S.Wire.Processes (N);
      begin
         case Event is
            when Resumed =>
               if E.Resumed = Not_Recorded then E.Resumed := Now; end if;
            when First_Run =>
               if E.First_Run = Not_Recorded then E.First_Run := Now; end if;
            when Registered =>
               if E.Registered = Not_Recorded then E.Registered := Now; end if;
            when Received =>
               if E.First_Receive = Not_Recorded then E.First_Receive := Now; end if;
               if E.Ready = Not_Recorded and then E.Registered /= Not_Recorded then
                  E.Ready := Now;
               end if;
         end case;
      end;
   end Note_Event;

   procedure Note_Wait
     (S        : in out State;
      Slot     : Process_Slot;
      Waiter   : Identity_Word;
      Kind     : Wait_Kind;
      Label    : Unsigned_32;
      Target   : Identity_Word;
      Started  : Ticks;
      Finished : Ticks)
   is
      N        : constant Process_Count := S.Map (Slot);
      Duration : constant Ticks := Elapsed (Started, Finished);
      Counted  : Boolean;
   begin
      Expire (S, Finished);
      if not Recording (S) then
         return;
      end if;
      case Kind is
         when Call | Sleep =>
            Counted := True;
         when Receive | Completion | Futex =>
            --  Startup waits only: none once the process is Ready.
            Counted := N /= 0 and then
              (S.Wire.Processes (N).Ready = Not_Recorded
               or else S.Wire.Processes (N).Ready > Started);
         when No_Wait =>
            Counted := False;
      end case;
      if not Counted then
         return;
      end if;
      if N /= 0 then
         declare
            E : Process_Entry renames S.Wire.Processes (N);
         begin
            case Kind is
               when Call =>
                  E.Calls := Saturating_Increment (E.Calls);
                  E.Call_Ticks := Saturating_Add (E.Call_Ticks, Duration);
               when Sleep =>
                  E.Waits := Saturating_Increment (E.Waits);
                  E.Sleep_Ticks := Saturating_Add (E.Sleep_Ticks, Duration);
               when Receive | Completion | Futex =>
                  E.Waits := Saturating_Increment (E.Waits);
                  E.Wait_Ticks := Saturating_Add (E.Wait_Ticks, Duration);
               when No_Wait =>
                  null;
            end case;
         end;
      end if;
      if S.Wire.Head.Minimum_Wait = 0 or else Duration < S.Wire.Head.Minimum_Wait then
         return;
      end if;
      --  The same wait again (a polling loop, a retried call) joins its
      --  first entry.
      for W in reverse 1 .. S.Wire.Head.Waits loop
         if S.Wire.Waits (W).Waiter = Waiter and then S.Wire.Waits (W).Kind = Kind
           and then S.Wire.Waits (W).Target = Target and then S.Wire.Waits (W).Label = Label
         then
            if S.Wire.Waits (W).Repeats < Wait_Repeats'Last then
               S.Wire.Waits (W).Repeats := S.Wire.Waits (W).Repeats + 1;
            end if;
            S.Wire.Waits (W).Duration := Saturating_Add (S.Wire.Waits (W).Duration, Duration);
            return;
         end if;
      end loop;
      if S.Wire.Head.Waits = Wait_Capacity then
         S.Wire.Head.Waits_Dropped := Saturating_Increment (S.Wire.Head.Waits_Dropped);
         return;
      end if;
      declare
         Next : constant Wait_Number := S.Wire.Head.Waits + 1;
      begin
         S.Wire.Waits (Next) :=
           (Kind     => Kind,
            Repeats  => 1,
            Label    => Label,
            Waiter   => Waiter,
            Target   => Target,
            Started  => Started,
            Duration => Duration,
            others   => <>);
         S.Wire.Head.Waits := Next;
      end;
   end Note_Wait;

   procedure Mark_Milestone
     (S      : in out State;
      Item   : Milestone;
      Marker : Identity_Word;
      Now    : Ticks)
   is
   begin
      Expire (S, Now);
      if Recording (S)
        and then S.Wire.Milestones (Slot_Of (Item)).At_Ticks = Not_Recorded
      then
         S.Wire.Milestones (Slot_Of (Item)) := (At_Ticks => Now, Marker => Marker);
      end if;
   end Mark_Milestone;
end Boot_Timeline_Records;
