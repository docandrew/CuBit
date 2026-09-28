------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Flow with SPARK_Mode is
   use type TCP_Congestion.Ack_Action;
   use type TCP_Congestion.Duplicate_Action;

   --  When the retransmission timer fires if armed now.
   function Deadline_After (Now : Time; RTO : TCP_RTO.Estimator) return Time is
     (if Now <= Time'Last - Time (RTO.RTO) then Now + Time (RTO.RTO) else Time'Last);

   procedure Open_Active (F : out Flow; P : EP.Chunks.Pool; Me : EP.Chunks.Owner_Id;
                          ISS : Seq; MSS : TCP_Congestion.Segment_Size; Now : Time)
   is
   begin
      F := (E => <>, RTO => <>, CC => <>, others => <>);
      EP.Open_Active (F.E, P, Me, ISS);
      TCP_Congestion.Initialize (F.CC, MSS, ISS);
      F.Armed := True;
      F.Deadline := Deadline_After (Now, F.RTO);
   end Open_Active;

   procedure Open_Passive (F : out Flow; P : EP.Chunks.Pool; Me : EP.Chunks.Owner_Id;
                           MSS : TCP_Congestion.Segment_Size)
   is
   begin
      F := (E => <>, RTO => <>, CC => <>, others => <>);
      EP.Open_Passive (F.E, P, Me);
      TCP_Congestion.Initialize (F.CC, MSS, 0);
   end Open_Passive;

   procedure Send (F : in out Flow; P : EP.Chunks.Pool; Now : Time;
                   Data : out EP.Sends.Byte_Array; First : out Seq;
                   Taken : out EP.Sends.Byte_Count; FIN : out Boolean)
   is
      Allow : constant Natural :=
        TCP_Congestion.Allowance (F.CC, Pipe (F), Natural (F.E.C.Snd_Wnd));
      Limit : constant Natural :=
        Natural'Min (Natural'Min (Allow, Data'Length), F.CC.SMSS);
   begin
      EP.Next_Segment (F.E, P, Limit, Data, First, Taken, FIN);
      if Taken > 0 and then not F.Timing then
         --  Time this segment (it is new: a rewind stops timing).
         F.Timing := True;
         F.Timed_End := First + Seq (Taken);
         F.Timed_At := Now;
      end if;
      if In_Flight (F) and then not F.Armed then
         F.Armed := True;
         F.Deadline := Deadline_After (Now, F.RTO);
      end if;
   end Send;

   procedure Arrive (F : in out Flow; P : in out EP.Chunks.Pool; Now : Time; S : Segment;
                     Payload : EP.Receives.Byte_Array; New_ISS : Seq; O : out Outcome)
   is
      Old_Una : constant Seq := F.E.C.Snd_Una;
      Acked   : Seq;
      A       : TCP_Congestion.Ack_Action;
      D       : TCP_Congestion.Duplicate_Action;
      Old_P   : constant EP.Chunks.Pool := P with Ghost;
   begin
      EP.Chunks.Lemma_Equal (Old_P, P);
      pragma Assert (EP.Chunks.Valid (Old_P));
      EP.Segment_Arrived (F.E, P, S, Payload, New_ISS, O);
      pragma Assert (EP.Sends.Isolated (EP.Sends.Owner_Of (F.E.S), Old_P, P));
      if F.E.C.St not in Established .. Time_Wait then
         if F.E.C.St = Syn_Received and then not F.Armed then
            --  A passive open: our SYN-ACK goes out now.
            F.Armed := True;
            F.Deadline := Deadline_After (Now, F.RTO);
         elsif F.E.C.St in Closed | Listen then
            F.Armed := False;
         end if;
         return;
      end if;
      Acked := Distance (Old_Una, F.E.C.Snd_Una);
      if Acked in 1 .. TCP_Congestion.Maximum_Cwnd then
         --  New data acknowledged.
         F.Retrying := False;
         F.Retries := 0;
         if F.Timing and then Ge (F.E.C.Snd_Una, F.Timed_End) then
            if Now >= F.Timed_At then
               TCP_RTO.Update
                 (F.RTO, TCP_RTO.Sample (Unsigned_64'Min (Now - F.Timed_At, TCP_RTO.Maximum_Sample)));
            end if;
            F.Timing := False;
         end if;
         TCP_Congestion.On_Ack
           (F.CC, Natural (Acked), F.E.C.Snd_Una, Pipe (F), A);
         if A = TCP_Congestion.Retransmit_First then
            --  NewReno partial ACK: resend from the new SND.UNA.
            EP.Timeout (F.E, P);
            F.Timing := False;
         end if;
         if In_Flight (F) then
            F.Armed := True;
            F.Deadline := Deadline_After (Now, F.RTO);
         else
            F.Armed := False;
         end if;
         pragma Assert (F.Armed = In_Flight (F) and then F.Retries = 0);
      elsif Acked = 0 and then S.ACK and then S.Length = 0 and then not S.SYN and then
        not S.FIN and then Outstanding (F) > 0
      then
         --  A duplicate ACK.
         TCP_Congestion.On_Duplicate_Ack
           (F.CC, F.E.C.Snd_Una, F.E.C.Snd_Nxt, Pipe (F), D);
         if D = TCP_Congestion.Fast_Retransmit then
            EP.Timeout (F.E, P);
            F.Timing := False;
         end if;
      end if;
   end Arrive;

   procedure Retransmit_Timeout (F : in out Flow; P : EP.Chunks.Pool; Now : Time) is
   begin
      TCP_Congestion.On_Timeout
        (F.CC, Pipe (F), F.E.C.Snd_Nxt, First => not F.Retrying);
      TCP_RTO.Back_Off (F.RTO);
      EP.Timeout (F.E, P);
      F.Retrying := True;
      F.Timing := False;
      F.Retries := Natural'Min (F.Retries + 1, Maximum_Retries);
      F.Armed := True;
      F.Deadline := Deadline_After (Now, F.RTO);
   end Retransmit_Timeout;

   procedure Path_MTU_Reduced (F : in out Flow; P : EP.Chunks.Pool;
                               SMSS : TCP_Congestion.Segment_Size) is
   begin
      TCP_Congestion.Reduce_Segment_Size (F.CC, SMSS);
      EP.Timeout (F.E, P);
      F.Timing := False;
   end Path_MTU_Reduced;

   --  RTO * 2 ** Probes, capped at Maximum_Persist_Ms.
   function Persist_Wait (F : Flow) return Time is
      Wait : Time := Time (F.RTO.RTO);
   begin
      for K in 1 .. F.Probes loop
         exit when Wait >= Maximum_Persist_Ms;
         Wait := Wait * 2;
         pragma Loop_Invariant (Wait <= 2 * Maximum_Persist_Ms + 2 * Time (F.RTO.RTO));
      end loop;
      return Time'Min (Wait, Maximum_Persist_Ms);
   end Persist_Wait;

   function Later (Now, Wait : Time) return Time is
     (if Now <= Time'Last - Wait then Now + Wait else Time'Last);

   procedure Update_Persist (F : in out Flow; Now : Time) is
   begin
      if Window_Blocked (F) then
         if not F.Persisting then
            F.Persisting := True;
            F.Probes := 0;
            F.Persist_At := Later (Now, Persist_Wait (F));
         end if;
      else
         F.Persisting := False;
      end if;
   end Update_Persist;

   procedure Probe_Answered (F : in out Flow) is
   begin
      F.Probes := 0;
   end Probe_Answered;

   procedure Persist_Timeout (F : in out Flow; Now : Time; Give_Up : out Boolean) is
   begin
      Give_Up := F.Probes >= Maximum_Probes;
      if not Give_Up then
         F.Probes := F.Probes + 1;
         F.Persist_At := Later (Now, Persist_Wait (F));
         if F.Persist_At <= Now then
            F.Persist_At := Time'Last;
         end if;
      end if;
   end Persist_Timeout;
end TCP_Flow;
