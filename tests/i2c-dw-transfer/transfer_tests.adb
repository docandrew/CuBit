pragma Ada_2022;
with Ada.Text_IO;
with DW_I2C_Transfer; use DW_I2C_Transfer;
procedure Transfer_Tests is
   use type Word;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Context : String) is
   begin
      if not OK then raise Program_Error with Context; end if;
      Checks := Checks + 1;
   end Check;
   procedure Matrix (W, R : Count; Depth : FIFO_Depth) is
      S : State;
      A, Duplicate : Action;
      Accepted_As : Admission;
      Matched, Retired : Boolean;
      N : Tick := 1;
      Commands, Bytes_Read : Count := 0;
      H : Observation;
      Expected : Word;
      Hold_Credit : Boolean := False;
   begin
      Submit (S, W, R, 32, Depth, Depth, N, 100_000, Accepted_As);
      Check (Accepted_As = Accepted, "matrix admission");
      while Current_Phase (S) = Active loop
         N := N + 1;
         Check (N < 1000, "progress bound");
         H := (TX_Free => (if N mod 7 = 0 then 0 else Depth),
               RX_Available => 0, Stop_Detected => Commands = W + R,
               Aborted => False);
         if Outstanding_Reads (S) = Count (Depth) and then not Hold_Credit then
            Hold_Credit := True;
         elsif Outstanding_Reads (S) = Count (Depth) or else Commands = W + R then
            H.RX_Available := FIFO_Count (Outstanding_Reads (S));
            Hold_Credit := False;
         end if;
         Poll (S, N, H, A);
         Check (Valid (S), "matrix invariant after poll");
         if A.Kind /= No_Action then
            Check (A.ID /= No_Ticket and then Pending (S), "issued ticket");
            Poll (S, N, H, Duplicate);
            Check (Duplicate.Kind = No_Action, "pending action never reissued");
            Finish_Access (S, No_Ticket, Succeeded, Matched);
            Check (not Matched and then Pending (S), "zero ticket refused");
            if A.Kind = Receive_Byte then
               Bytes_Read := Bytes_Read + 1;
               Check (A.Index = Bytes_Read, "read index");
               Check (not A.Read_Request and then not A.Restart and then not A.Stop,
                      "receive action has no command flags");
            else
               Commands := Commands + 1;
               Check (A.Read_Request = (Commands > W), "command direction");
               Check (A.Index = (if Commands <= W then Commands else Commands - W),
                      "command index");
               Check (A.Stop = (Commands = W + R), "only final command stops");
               Check (A.Restart = (W > 0 and then Commands = W + 1),
                      "only first read after write restarts");
               Expected := (if A.Read_Request then Read_Command_Bit else 16#AB#);
               if Commands = W + R then Expected := Expected + Stop_Command_Bit; end if;
               if W > 0 and then Commands = W + 1 then
                  Expected := Expected + Restart_Command_Bit;
               end if;
               Check (Encode_Command (A, 16#AB#) = Expected, "DATA_CMD encoding");
            end if;
            Finish_Access (S, A.ID, Succeeded, Matched);
            Check (Matched and then not Pending (S), "access completed once");
            Finish_Access (S, A.ID, Succeeded, Matched);
            Check (not Matched, "duplicate completion ignored");
         end if;
         Check (Outstanding_Reads (S) <= Count (Depth), "receive credit bound");
         Check (Valid (S), "matrix invariant after completion");
      end loop;
      Check (Current_Phase (S) = Complete and then Reason (S) = None,
             "stop and drain complete");
      Check (Commands = W + R and then Bytes_Read = R, "exact transfer extents");
      Submit (S, 1, 0, 1, Depth, Depth, N, 100_000, Accepted_As);
      Check (Accepted_As = Busy, "must retire terminal request");
      Retire (S, False, Retired);
      Check (not Retired, "retirement requires quiescence");
      Retire (S, True, Retired);
      Check (Retired and then Current_Phase (S) = Idle and then Valid (S), "retired");
   end Matrix;
   procedure Failure_Case (Which : Positive; Expected : Failure) is
      S : State;
      A : Action;
      Admitted : Admission;
      Matched, Retired : Boolean;
      H : Observation := (1, 0, False, False);
      N : Tick := 11;
   begin
      Submit (S, 1, 1, 2, 1, 1, 10, 20, Admitted);
      Check (Admitted = Accepted, "fault case admitted");
      case Which is
         when 1 => N := 20;
         when 2 => N := 9;
         when 3 => H.Aborted := True;
         when 4 => H.RX_Available := 1;
         when 5 => H.TX_Free := 2;
         when 6 => H.Stop_Detected := True;
         when others => null;
      end case;
      Poll (S, N, H, A);
      if Which = 7 or else Which = 8 then
         Check (A.Kind = Send_Command, "fault access issued");
         Finish_Access (S, A.ID,
           (if Which = 7 then Indeterminate else Refused), Matched);
         Check (Matched, "fault completion matched");
      end if;
      Check (Current_Phase (S) = Failed and then Reason (S) = Expected,
             "typed failure");
      Poll (S, 12, (1, 0, False, False), A);
      Check (A.Kind = No_Action and then not Pending (S), "failure never replays");
      Check (Valid (S), "failed invariant");
      Retire (S, True, Retired);
      Check (Retired, "failed owner recovery");
   end Failure_Case;
   procedure Admission_And_Generations is
      S : State;
      A, B : Action;
      Admitted : Admission;
      Matched, Retired : Boolean;
   begin
      Submit (S, 0, 0, 1, 1, 1, 1, 2, Admitted);
      Check (Admitted = Empty_Transfer, "empty rejected");
      Submit (S, Count'Last, Count'Last, Count'Last, 1, 1, 1, 2, Admitted);
      Check (Admitted = Capacity_Refused, "sum overflow refused");
      Submit (S, 2, 0, 1, 1, 1, 1, 2, Admitted);
      Check (Admitted = Capacity_Refused, "write capacity refused");
      Submit (S, 1, 0, 1, 1, 1, 2, 2, Admitted);
      Check (Admitted = Deadline_Refused, "expired deadline refused");
      Submit (S, Count'Last, 0, Count'Last, 256, 256, 1, Tick'Last, Admitted);
      Check (Admitted = Accepted, "maximum explicit counts accepted");
      Poll (S, 2, (256, 0, False, False), A);
      Finish_Access (S, A.ID, Indeterminate, Matched);
      Retire (S, True, Retired);
      Submit (S, 1, 0, 1, 1, 1, 3, 10, Admitted);
      Poll (S, 4, (1, 0, False, False), B);
      Check (B.ID /= A.ID, "generation survives retirement");
      Finish_Access (S, A.ID, Succeeded, Matched);
      Check (not Matched and then Sent (S) = 0 and then Pending (S),
             "old completion cannot finish new request");
      Finish_Access (S, B.ID, Succeeded, Matched);
      Poll (S, 5, (1, 0, False, False), B);
      Check (B.Kind = No_Action and then Current_Phase (S) = Active,
             "enqueue is not bus completion");
      Poll (S, 6, (1, 0, True, False), B);
      Check (Current_Phase (S) = Complete, "stop completes write");
   end Admission_And_Generations;
begin
   for D in FIFO_Depth range 1 .. 4 loop
      for W in Count range 0 .. 8 loop
         for R in Count range 0 .. 8 loop
            if W + R > 0 then Matrix (W, R, D); end if;
         end loop;
      end loop;
   end loop;
   Matrix (0, 32, 256);
   Matrix (1, 31, 16);
   Matrix (32, 0, 256);
   Failure_Case (1, Deadline_Expired);
   Failure_Case (2, Clock_Reversed);
   Failure_Case (3, Controller_Abort);
   Failure_Case (4, Invalid_Observation);
   Failure_Case (5, Invalid_Observation);
   Failure_Case (6, Premature_Stop);
   Failure_Case (7, Access_Indeterminate);
   Failure_Case (8, Access_Failed);
   Admission_And_Generations;
   Ada.Text_IO.Put_Line ("DesignWare transfer checks" & Checks'Image);
end Transfer_Tests;
