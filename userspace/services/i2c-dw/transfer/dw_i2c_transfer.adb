pragma Ada_2022;
package body DW_I2C_Transfer with SPARK_Mode is
   use type Interfaces.Unsigned_32;
   function Current_Phase (S : State) return Phase is (S.At_Phase);
   function Reason (S : State) return Failure is (S.Error);
   function Outstanding_Reads (S : State) return Count is
     (S.Queued_Reads - S.Drained);
   function Sent (S : State) return Count is
     (S.Queued_Writes + S.Queued_Reads);
   function Received (S : State) return Count is (S.Drained);
   function Pending (S : State) return Boolean is
     (S.In_Flight.Kind /= No_Action);
   function Valid (S : State) return Boolean is
     (S.Queued_Writes <= S.Writes and then S.Queued_Reads <= S.Reads
      and then S.Drained <= S.Queued_Reads
      and then S.Writes <= Count'Last - S.Reads
      and then S.Queued_Reads - S.Drained <= Count (S.RX_Size)
      and then (if Pending (S) then
         S.At_Phase = Active and then S.In_Flight.ID /= No_Ticket
         and then S.In_Flight.ID = S.Sequence
         and then S.In_Flight.Index > 0
         and then (if S.In_Flight.Kind = Receive_Byte then
            S.Drained < S.Queued_Reads
            and then S.In_Flight.Index = S.Drained + 1
         elsif S.In_Flight.Read_Request then
            S.Queued_Writes = S.Writes and then S.Queued_Reads < S.Reads
            and then S.Queued_Reads - S.Drained < Count (S.RX_Size)
            and then S.In_Flight.Index = S.Queued_Reads + 1
         else S.Queued_Writes < S.Writes
            and then S.In_Flight.Index = S.Queued_Writes + 1))
      and then (if S.At_Phase = Idle then
         S.Writes = 0 and then S.Reads = 0 and then not S.Stop_Seen)
      and then (if S.At_Phase = Complete then
         S.Stop_Seen and then S.Queued_Writes = S.Writes
         and then S.Queued_Reads = S.Reads and then S.Drained = S.Reads)
      and then (if S.At_Phase = Failed then S.Error /= None));
   procedure Submit
     (S : in out State; Write_Count, Read_Count : Count; Byte_Limit : Limit;
      TX_Depth, RX_Depth : FIFO_Depth; Now, Deadline : Tick;
      Result : out Admission) is
   begin
      if S.At_Phase /= Idle then Result := Busy;
      elsif Write_Count = 0 and then Read_Count = 0 then Result := Empty_Transfer;
      elsif Write_Count > Byte_Limit or else
        Read_Count > Byte_Limit - Write_Count then Result := Capacity_Refused;
      elsif Now >= Deadline then Result := Deadline_Refused;
      else
         S.Writes := Write_Count; S.Reads := Read_Count;
         S.TX_Size := TX_Depth; S.RX_Size := RX_Depth;
         S.Last_Time := Now; S.Expires := Deadline;
         S.At_Phase := Active; Result := Accepted;
      end if;
   end Submit;
   procedure Fail (S : in out State; Why : Failure)
     with Pre => Valid (S) and then Why /= None,
          Post => Valid (S) and then Current_Phase (S) = Failed
            and then Reason (S) = Why;
   procedure Fail (S : in out State; Why : Failure) is
   begin
      S.At_Phase := Failed; S.Error := Why; S.In_Flight := (others => <>);
   end Fail;
   procedure Poll
     (S : in out State; Now : Tick; Hardware : Observation; Next : out Action) is
      Item : Action;
   begin
      Next := (others => <>);
      if S.At_Phase /= Active or else Pending (S) then return; end if;
      if Now < S.Last_Time then Fail (S, Clock_Reversed); return; end if;
      S.Last_Time := Now;
      if Now >= S.Expires then Fail (S, Deadline_Expired); return; end if;
      if Hardware.Aborted then Fail (S, Controller_Abort); return; end if;
      if Hardware.TX_Free > S.TX_Size or else
        Hardware.RX_Available > S.RX_Size or else
        Count (Hardware.RX_Available) > Outstanding_Reads (S)
      then Fail (S, Invalid_Observation); return; end if;
      S.Stop_Seen := S.Stop_Seen or Hardware.Stop_Detected;
      if S.Stop_Seen and then
        (S.Queued_Writes /= S.Writes or else S.Queued_Reads /= S.Reads)
      then Fail (S, Premature_Stop); return; end if;
      if Hardware.RX_Available > 0 then
         Item.Kind := Receive_Byte; Item.Index := S.Drained + 1;
      elsif S.Stop_Seen and then S.Drained = S.Reads then
         S.At_Phase := Complete; return;
      elsif Hardware.TX_Free = 0 or else S.Stop_Seen then return;
      elsif S.Queued_Writes < S.Writes then
         Item.Kind := Send_Command; Item.Index := S.Queued_Writes + 1;
         Item.Stop := Item.Index = S.Writes and then S.Reads = 0;
      elsif S.Queued_Reads < S.Reads and then
        Outstanding_Reads (S) < Count (S.RX_Size) then
         Item.Kind := Send_Command; Item.Read_Request := True;
         Item.Index := S.Queued_Reads + 1;
         Item.Restart := S.Writes > 0 and then S.Queued_Reads = 0;
         Item.Stop := Item.Index = S.Reads;
      else return;
      end if;
      if S.Sequence = Ticket'Last then Fail (S, Ticket_Exhausted); return; end if;
      S.Sequence := S.Sequence + 1; Item.ID := S.Sequence;
      S.In_Flight := Item; Next := Item;
   end Poll;
   procedure Finish_Access
     (S : in out State; ID : Ticket; Result : Access_Result;
      Matched : out Boolean) is
   begin
      Matched := Pending (S) and then ID /= No_Ticket
        and then ID = S.In_Flight.ID;
      if not Matched then return; end if;
      if Result = Refused then Fail (S, Access_Failed); return;
      elsif Result = Indeterminate then Fail (S, Access_Indeterminate); return;
      end if;
      if S.In_Flight.Kind = Receive_Byte then S.Drained := S.Drained + 1;
      elsif S.In_Flight.Read_Request then S.Queued_Reads := S.Queued_Reads + 1;
      else S.Queued_Writes := S.Queued_Writes + 1;
      end if;
      S.In_Flight := (others => <>);
   end Finish_Access;
   procedure Retire (S : in out State; Controller_Quiescent : Boolean;
                     Retired : out Boolean) is
   begin
      Retired := Controller_Quiescent and then
        (S.At_Phase = Complete or else S.At_Phase = Failed);
      if Retired then
         S.At_Phase := Idle; S.Error := None;
         S.Writes := 0; S.Reads := 0; S.Queued_Writes := 0;
         S.Queued_Reads := 0; S.Drained := 0; S.Stop_Seen := False;
         -- Sequence deliberately survives retirement: stale tickets cannot
         -- complete an action from a later request using the same owner.
      end if;
   end Retire;
   function Encode_Command (Item : Action; Data : Byte) return Word is
      Value : Word := (if Item.Read_Request then Read_Command_Bit else Word (Data));
   begin
      if Item.Stop then Value := Value or Stop_Command_Bit; end if;
      if Item.Restart then Value := Value or Restart_Command_Bit; end if;
      return Value;
   end Encode_Command;
end DW_I2C_Transfer;
