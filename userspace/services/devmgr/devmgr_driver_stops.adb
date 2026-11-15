pragma Ada_2022;
with CuBit.Messages;
with CuBit.Child_Exits;
with CuBit.Control_Events;
with CuBit.Handles.Control;
with CuBit.Process_Events;
with Devmgr_Launches;

package body Devmgr_Driver_Stops is
   package DL renames Devmgr_Launches;
   package HC renames CuBit.Handles.Control;
   use type DL.Child;
   use type CuBit.Child_Exits.Termination_Kind;
   use ASCII;
   procedure debugPrint (str : String) renames CuBit.Messages.debugPrint;

   type Track is (Not_Tracked, Running, Ending, Stopped_Clean, Exited_Failing, Forced, Not_Ended);
   Tracks : array (DL.Child range 1 .. DL.Child'Last) of Track := [others => Not_Tracked];
   Current : STP.Driver_Phase := STP.Not_Asked;
   Grace_End, Final_End : Unsigned_64 := 0;
   Grace_Passed : Boolean := False;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   procedure Begin_Stop
     (Grace : STP.Grace_Ms; Except : Process_ID; Keep_Tail : Boolean;
      Now : Unsigned_64; Asked : out Natural; Receipt : out STP.Driver_Receipt)
   is
      Answer : Unsigned_64;
   begin
      Asked := 0;
      if STP."/=" (Current, STP.Not_Asked) then
         Receipt := STP.Driver_Already_Stopping;
         return;
      end if;
      Current := STP.Stopping;
      Grace_End := Now + Unsigned_64 (Grace);
      Final_End := Grace_End + End_Deadline_Ms;
      --  Reverse launch order: later children first (the Stop is one-way,
      --  so they quiesce side by side within the one grace).
      for C in reverse Tracks'Range loop
         if DL.Used (C) and then DL.PID (C) /= No_Process and then DL.PID (C) /= Except
           and then Keep_Tail and then DL.Power_Tail (C)
         then
            debugPrint ("devmgr: shutdown: keeping " & DL.Name_Of (C) & " (Power_Tail: the firmware step)" & LF);
         elsif DL.Used (C) and then DL.PID (C) /= No_Process and then DL.PID (C) /= Except then
            Answer := HC.Send_Control
              (DL.PID (C), CuBit.Control_Events.Control_Kind'Enum_Rep (CuBit.Control_Events.Stop),
               DL.Control (C));
            --  0: queued; Busy: alive with its lane full (ended at the
            --  grace if it does not go); anything else: already gone.
            if Answer = 0 or else Answer = HC.Busy_Word then
               Tracks (C) := Running;
               Asked := Asked + 1;
               debugPrint ("devmgr: shutdown: stopping " & DL.Name_Of (C) & " (grace" &
                           Unsigned_64'Image (Unsigned_64 (Grace)) & " ms)" & LF);
            end if;
         end if;
      end loop;
      Receipt := STP.Accepted;
      Step (Now);
   end Begin_Stop;

   procedure Step (Now : Unsigned_64) is
      Found : Boolean;
      Report : CuBit.Child_Exits.Report;
      Open : Natural := 0;
   begin
      if STP."/=" (Current, STP.Stopping) then
         return;
      end if;
      for C in Tracks'Range loop
         if Tracks (C) in Running | Ending then
            CuBit.Process_Events.Take_Exit (DL.PID (C), Found, Report);
            if Found then
               Tracks (C) :=
                 (if Tracks (C) = Ending then Forced
                  elsif Report.Kind = CuBit.Child_Exits.Exited and then Report.Code = 0 then Stopped_Clean
                  else Exited_Failing);
               debugPrint ("devmgr: shutdown: " & DL.Name_Of (C) & " " &
                           (case Tracks (C) is
                              when Stopped_Clean => "STOPPED_CLEAN",
                              when Forced => "FORCED (ended after its grace)",
                              when others => "EXITED_FAILING") & LF);
            end if;
         end if;
      end loop;
      if not Grace_Passed and then Now >= Grace_End then
         Grace_Passed := True;
         for C in Tracks'Range loop
            if Tracks (C) = Running then
               if HC.Kill (DL.PID (C), DL.Control (C)) then
                  Tracks (C) := Ending;
                  debugPrint ("devmgr: shutdown: " & DL.Name_Of (C) &
                              " did not end within its grace; ending it" & LF);
               else
                  Tracks (C) := Not_Ended;
                  debugPrint ("devmgr: shutdown: " & DL.Name_Of (C) & " could not be ended" & LF);
               end if;
            end if;
         end loop;
      end if;
      for C in Tracks'Range loop
         if Tracks (C) in Running | Ending then
            Open := Open + 1;
         end if;
      end loop;
      if Open = 0 or else Now >= Final_End then
         for C in Tracks'Range loop
            if Tracks (C) in Running | Ending then
               Tracks (C) := Not_Ended;
               debugPrint ("devmgr: shutdown: " & DL.Name_Of (C) & " NOT_ENDED" & LF);
            end if;
         end loop;
         Current := STP.Finished;
         debugPrint ("devmgr: shutdown: drivers stopped (" & Image (Counts_Word) & ")" & LF);
      end if;
   end Step;

   function Phase return STP.Driver_Phase is (Current);

   function Counts_Word return Unsigned_64 is
      Clean, Failing, Ended, Running_Now : Natural := 0;
   begin
      for T of Tracks loop
         case T is
            when Stopped_Clean  => Clean := Clean + 1;
            when Exited_Failing => Failing := Failing + 1;
            when Forced         => Ended := Ended + 1;
            when Running | Ending | Not_Ended => Running_Now := Running_Now + 1;
            when Not_Tracked    => null;
         end case;
      end loop;
      return STP.Counts_Word (Clean, Failing, Ended, Running_Now);
   end Counts_Word;

   function Wake_At return Unsigned_64 is
     (if STP."=" (Current, STP.Stopping)
      then (if Grace_Passed then Final_End else Grace_End)
      else 0);
end Devmgr_Driver_Stops;
