pragma Ada_2022;

package body Shutdown_Sequences with SPARK_Mode is

   function Next_Pending (Units : Unit_Array; Length : Natural; Tail : Boolean) return Natural is
      Best : Natural := 0;
   begin
      for I in 1 .. Length loop
         if Units (I).State = Pending and then In_Phase (Units (I), Tail)
           and then (Best = 0 or else not Precedes (Units (Best), Units (I)))
         then
            Best := I;
         end if;
         pragma Loop_Invariant (Best <= I);
         pragma Loop_Invariant
           (if Best = 0 then
              (for all J in 1 .. I =>
                 not (Units (J).State = Pending and then In_Phase (Units (J), Tail)))
            else Units (Best).State = Pending and then In_Phase (Units (Best), Tail)
                 and then (for all J in 1 .. I =>
                             (if Units (J).State = Pending and then In_Phase (Units (J), Tail)
                              then Precedes (Units (Best), Units (J)))));
      end loop;
      return Best;
   end Next_Pending;

   function Find (Units : Unit_Array; Length : Natural; Process : Process_Word) return Natural is
   begin
      for I in 1 .. Length loop
         if Units (I).Process = Process then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   function Count (Units : Unit_Array; Length : Natural; State : Unit_State) return Natural is
      Total : Natural := 0;
   begin
      for I in 1 .. Length loop
         if Units (I).State = State then
            Total := Total + 1;
         end if;
         pragma Loop_Invariant (Total <= I);
      end loop;
      return Total;
   end Count;

   --  No unit is open: stop the next unit of the phase, or move to the
   --  phase's end (the filesystem's commit, then devmgr's drivers).
   procedure Advance
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural; Now : Time; Next : out Action)
   with Pre  => Units'First = 1 and then Length <= Units'Last
                and then (for all I in 1 .. Length => Units (I).State not in Open)
                and then (if S.Tail then
                            (for all I in 1 .. Length =>
                               (if Units (I).Stage /= Power_Tail then Settled (Units (I)))))
                and then (if S.Apps_Only then
                            not S.Tail
                            and then (for all I in 1 .. Length => Units (I).Stage = Apps)),
        Post => Consistent (S, Units, Length)
                and then S.Tail = S.Tail'Old
                and then S.Apps_Only = S.Apps_Only'Old
                and then S.Driver_Grace = S.Driver_Grace'Old
                and then S.Storage = S.Storage'Old
                and then S.Drivers = S.Drivers'Old
                and then Next in Send_Stop | Commit_Storage | Answer
                and then (if S.Tail or else S.Apps_Only then Next in Send_Stop | Answer
                          else Next in Send_Stop | Commit_Storage)
                and then (if Next = Send_Stop then S.State = Stopping_Unit
                                  and then S.Current = Next_Pending (Units'Old, Length, S.Tail)
                          elsif Next = Commit_Storage then S.State = Committing
                          else S.State = Finished)
                and then (for all I in 1 .. Length =>
                            Units (I).Process = Units'Old (I).Process
                            and then Units (I).Stage = Units'Old (I).Stage
                            and then Units (I).Key = Units'Old (I).Key)
   is
      K : constant Natural := Next_Pending (Units, Length, S.Tail);
   begin
      if K /= 0 then
         Units (K).State := Stopping;
         Units (K).Stop_At := Now;
         Units (K).Stop_Taken := False;
         S.Current := K;
         S.State := Stopping_Unit;
         S.Deadline := Now + Unsigned_64 (Units (K).Grace);
         Next := Send_Stop;
      elsif S.Apps_Only or else S.Tail then
         --  The apps alone (no storage or driver stage), or the tail done.
         S.Current := 0;
         S.State := Finished;
         Next := Answer;
      else
         S.Current := 0;
         S.State := Committing;
         Next := Commit_Storage;
      end if;
   end Advance;

   --  The open current unit ends with Outcome.
   procedure Decide
     (S : Sequence; Units : in out Unit_Array; Length : Natural; Outcome : Decided; Now : Time)
   with Pre  => Consistent (S, Units, Length) and then S.State in Stopping_Unit | Ending_Unit,
        Post => (for all I in 1 .. Length => Units (I).State not in Open)
                and then Units (S.Current).State = Outcome
                and then (if S.Tail then
                            (for all I in 1 .. Length =>
                               (if Units (I).Stage /= Power_Tail then Settled (Units (I)))))
                and then (for all I in 1 .. Length =>
                            Units (I).Process = Units'Old (I).Process
                            and then Units (I).Stage = Units'Old (I).Stage
                            and then Units (I).Key = Units'Old (I).Key)
   is
   begin
      Units (S.Current).State := Outcome;
      Units (S.Current).Decided_At := Now;
   end Decide;

   --  devmgr's drivers are done (or refused): the Power_Tail phase.
   procedure Enter_Tail
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural; Result : Driver_Result;
      Now : Time; Next : out Action)
   with Pre  => Consistent (S, Units, Length) and then S.State in Asking_Drivers | Stopping_Drivers,
        Post => Consistent (S, Units, Length) and then S.Tail and then S.Drivers = Result
                and then Next in Send_Stop | Answer
   is
   begin
      S.Drivers := Result;
      S.Tail := True;
      Advance (S, Units, Length, Now, Next);
   end Enter_Tail;

   procedure Begin_Stop
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural;
      Driver_Grace : Grace_Ms; Apps_Only : Boolean; Now : Time; Next : out Action)
   is
   begin
      S := (State => Idle, Apps_Only => Apps_Only, Current => 0, Tail => False,
            Deadline => 0, Next_Poll => 0,
            Driver_Grace => Driver_Grace, Storage => CuBit.System_Stops.Not_Asked,
            Drivers => CuBit.System_Stops.Not_Asked);
      Advance (S, Units, Length, Now, Next);
   end Begin_Stop;

   procedure Stop_Sent (S : Sequence; Units : in out Unit_Array; Length : Natural; Taken : Boolean) is
   begin
      Units (S.Current).Stop_Taken := Taken;
   end Stop_Sent;

   procedure Exited
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural;
      Process : Process_Word; Clean : Boolean; Now : Time; Next : out Action)
   is
      K : constant Natural := Find (Units, Length, Process);
   begin
      Next := Nothing;
      if K = 0 then
         return;
      end if;
      case Units (K).State is
         when Pending =>
            --  It ended on its own before its turn.
            Units (K).State := Ended_Early;
            Units (K).Decided_At := Now;
         when Stopping =>
            Decide (S, Units, Length, (if Clean then Stopped_Clean else Exited_Failing), Now);
            Advance (S, Units, Length, Now, Next);
         when Ending =>
            Decide (S, Units, Length, Forced, Now);
            Advance (S, Units, Length, Now, Next);
         when Kept | Decided =>
            null;
      end case;
   end Exited;

   procedure End_Sent
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural;
      Accepted : Boolean; Now : Time; Next : out Action)
   is
   begin
      Next := Nothing;
      if not Accepted then
         Decide (S, Units, Length, Force_Failed, Now);
         Advance (S, Units, Length, Now, Next);
      end if;
   end End_Sent;

   procedure Tick
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural; Now : Time; Next : out Action)
   is
   begin
      Next := Nothing;
      case S.State is
         when Stopping_Unit =>
            if Now >= S.Deadline then
               Units (S.Current).State := Ending;
               S.State := Ending_Unit;
               S.Deadline := Now + End_Deadline_Ms;
               Next := End_Unit;
            end if;
         when Ending_Unit =>
            if Now >= S.Deadline then
               Decide (S, Units, Length, Force_Failed, Now);
               Advance (S, Units, Length, Now, Next);
            end if;
         when Stopping_Drivers =>
            if Now >= S.Deadline then
               Enter_Tail (S, Units, Length, CuBit.System_Stops.Drivers_Timed_Out, Now, Next);
            elsif Now >= S.Next_Poll then
               S.Next_Poll := Now + Driver_Poll_Ms;
               Next := Poll_Drivers;
            end if;
         when Idle | Committing | Asking_Drivers | Finished =>
            null;
      end case;
   end Tick;

   procedure Storage_Done
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural;
      Result : Storage_Result; Now : Time; Next : out Action)
   is
   begin
      S.Storage := Result;
      S.State := Asking_Drivers;
      Next := Ask_Drivers;
   end Storage_Done;


   procedure Drivers_Asked
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural; Accepted : Boolean; Now : Time;
      Next : out Action)
   is
   begin
      if Accepted then
         S.State := Stopping_Drivers;
         S.Deadline := Now + Unsigned_64 (S.Driver_Grace) + Driver_Allowance_Ms;
         S.Next_Poll := Now + Driver_Poll_Ms;
         Next := Nothing;
      else
         Enter_Tail (S, Units, Length, CuBit.System_Stops.Drivers_Refused, Now, Next);
      end if;
   end Drivers_Asked;

   procedure Drivers_Polled
     (S : in out Sequence; Units : in out Unit_Array; Length : Natural; Done : Boolean; Now : Time;
      Next : out Action)
   is
   begin
      if Done then
         Enter_Tail (S, Units, Length, CuBit.System_Stops.Drivers_Stopped, Now, Next);
      elsif Now >= S.Deadline then
         Enter_Tail (S, Units, Length, CuBit.System_Stops.Drivers_Timed_Out, Now, Next);
      else
         S.Next_Poll := Now + Driver_Poll_Ms;
         Next := Nothing;
      end if;
   end Drivers_Polled;

   procedure Lemma_Finished_Settled (S : Sequence; Units : Unit_Array; Length : Natural) is null;
end Shutdown_Sequences;
