pragma Ada_2022;
with Ada.Text_IO;
with DW_I2C_Transfer;
with DW_I2C_Interrupt_Policy;
procedure Demand_Tests is
   package T renames DW_I2C_Transfer;
   package I renames DW_I2C_Interrupt_Policy;
   use type T.Action_Kind, T.Phase, T.Admission, T.Wake_Requirement, T.Word, T.Count;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   procedure Scenario (Kind : Positive) is
      S : T.State;
      A : T.Action;
      Accepted : T.Admission;
      Matched, Retired : Boolean;
      Writes : constant T.Count := (if Kind = 2 then 0 else 2);
      Reads : constant T.Count := (if Kind = 3 then 0 else 3);
      procedure Expect (Active, TX, RX, Stop : Boolean) is
         Need : constant T.Wake_Requirement := T.Wake_Sources (S);
         Mask : constant I.Mask := I.Wanted (Need, True);
      begin
         Check (Need = (Active, TX, RX, Stop));
         Check (I.Wanted (Need, False) = 0);
         Check (((Mask and I.TX_Empty) /= 0) = (Active and TX));
         Check (((Mask and I.RX_Full) /= 0) = (Active and RX));
         Check (((Mask and I.Stop_Detected) /= 0) = (Active and Stop));
         Check ((Mask and I.Error_Sources) = (if Active then I.Error_Sources else 0));
      end Expect;
      procedure Poll (TX_Free, RX_Ready : T.FIFO_Count; Stop : Boolean := False) is
      begin T.Poll (S, 1, (TX_Free, RX_Ready, Stop, False), A); end Poll;
      procedure Finish is
      begin
         Expect (False, False, False, False);
         T.Finish_Access (S, A.ID, T.Succeeded, Matched); Check (Matched);
      end Finish;
   begin
      Expect (False, False, False, False);
      T.Submit (S, Writes, Reads, 5, 2, 2, 1, 1000, Accepted);
      Check (Accepted = T.Accepted); Expect (True, True, False, True);
      Poll (0, 0); Check (A.Kind = T.No_Action); Expect (True, True, False, True);
      for N in 1 .. Writes loop
         Poll (1, 0); Check (A.Kind = T.Send_Command and not A.Read_Request); Finish;
      end loop;
      if Reads = 0 then
         Expect (True, False, False, True);
         Poll (2, 0); Check (A.Kind = T.No_Action);
         Poll (2, 0, True); Check (T.Current_Phase (S) = T.Complete);
      else
         Expect (True, True, False, True);
         for N in 1 .. 2 loop
            Poll (1, 0); Check (A.Kind = T.Send_Command and A.Read_Request); Finish;
         end loop;
         Expect (True, False, True, True);
         Poll (2, 0); Check (A.Kind = T.No_Action);
         Check (I.Wanted (T.Wake_Sources (S), True) = (I.Error_Sources or I.RX_Full or I.Stop_Detected));
         Poll (2, 1); Check (A.Kind = T.Receive_Byte); Finish;
         Expect (True, True, True, True);
         Poll (1, 0); Check (A.Kind = T.Send_Command and A.Read_Request and A.Stop); Finish;
         Expect (True, False, True, True);
         Poll (2, 1, True); Check (A.Kind = T.Receive_Byte); Finish;
         Expect (True, False, True, False);
         Poll (2, 0); Check (A.Kind = T.No_Action);
         Check (I.Wanted (T.Wake_Sources (S), True) = (I.Error_Sources or I.RX_Full));
         Poll (2, 1); Check (A.Kind = T.Receive_Byte); Finish;
         Poll (2, 0); Check (T.Current_Phase (S) = T.Complete);
      end if;
      Expect (False, False, False, False);
      T.Retire (S, True, Retired); Check (Retired and T.Current_Phase (S) = T.Idle);
      Expect (False, False, False, False);
   end Scenario;
   procedure Terminal_Case (Aborted : Boolean) is
      S : T.State;
      A : T.Action;
      Accepted : T.Admission;
   begin
      T.Submit (S, 1, 1, 2, 1, 1, 1, 10, Accepted); Check (Accepted = T.Accepted);
      T.Poll (S, (if Aborted then 1 else 10), (1, 0, False, Aborted), A);
      Check (T.Current_Phase (S) = T.Failed and I.Wanted (T.Wake_Sources (S), True) = 0);
   end Terminal_Case;
   procedure Boundaries is
      S : T.State;
      Accepted : T.Admission;
   begin
      T.Submit (S, T.Count'Last - 1, 1, T.Limit'Last, T.FIFO_Depth'Last, T.FIFO_Depth'First,
         1, 10, Accepted);
      Check (Accepted = T.Accepted and T.Wake_Sources (S) = (True, True, False, True));
   end Boundaries;
begin
   for Active in Boolean loop
      for TX in Boolean loop
         for RX in Boolean loop
            for Stop in Boolean loop
               declare Need : constant T.Wake_Requirement := (Active, TX, RX, Stop); begin
                  Check (I.Wanted (Need, False) = 0);
                  Check ((I.Wanted (Need, True) and not I.All_Sources) = 0);
                  if not Active then Check (I.Wanted (Need, True) = 0); end if;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   for Kind in 1 .. 3 loop Scenario (Kind); end loop;
   Terminal_Case (False); Terminal_Case (True); Boundaries;
   Ada.Text_IO.Put_Line ("I2C interrupt demand checks" & Checks'Image);
end Demand_Tests;
