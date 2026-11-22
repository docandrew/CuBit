------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Desktop_Status_Machines with SPARK_Mode is

   procedure Step (S : in out Clock_State; E : Clock_Event; Act : out Clock_Action) is
   begin
      Act := None;
      case E is
         when Due =>
            if S = Idle then
               S := Reading;
               Act := Submit_Read;
            end if;
         when Done =>
            S := Idle;
      end case;
   end Step;

   procedure Step (S : in out Audio_State; E : Audio_Event; Act : out Audio_Action) is
      procedure Send_Queued
      with Pre => S.Queued and then S.Phase = Idle,
           Post => not S.Queued and then S.Phase = Setting_Level and then S.Sent = S'Old.Wanted
                   and then S.Wanted = S'Old.Wanted
                   and then Act.Kind = Submit_Set and then Act.Value = S'Old.Wanted
      is
      begin
         S.Phase := Setting_Level;
         S.Sent := S.Wanted;
         S.Queued := False;
         Act := (Kind => Submit_Set, Value => S.Wanted);
      end Send_Queued;
   begin
      Act := (Kind => None, Value => <>);
      case E.Kind is
         when Due =>
            if S.Phase = Idle then
               if S.Queued then
                  Send_Queued;
               else
                  S.Phase := Reading;
                  Act := (Kind => Submit_Get, Value => <>);
               end if;
            end if;
         when Set =>
            S.Wanted := E.Value;
            S.Queued := True;
            if S.Phase = Idle then
               Send_Queued;
            end if;
         when Done =>
            if S.Phase = Setting_Level and then E.Result /= Answered and then not S.Queued then
               --  Not confirmed: send it again at the next due time.
               S.Queued := True;
               S.Wanted := S.Sent;
            end if;
            S.Phase := Idle;
            if S.Queued and then E.Result = Answered then
               Send_Queued;
            end if;
      end case;
   end Step;

   procedure Step (S : in out Loan_State; E : Loan_Event; Act : out Loan_Action) is
   begin
      Act := None;
      case E is
         when Lend_Done =>
            if S = Free then
               S := Lent;
            end if;
         when Lend_Failed =>
            null;
         when Request_Over =>
            if S = Lent then
               S := Retiring;
               Act := Revoke;
            end if;
         when Retired =>
            if S = Retiring then
               S := Free;
            end if;
      end case;
   end Step;

   function Later (Now : Unsigned_64) return Unsigned_64 is
     (if Now <= Unsigned_64'Last - Retry_Ms then Now + Retry_Ms else Unsigned_64'Last);

   procedure Step (S : in out Watch_State; E : Watch_Event; Act : out Watch_Action) is
   begin
      Act := None;
      case E.Kind is
         when Tick =>
            if S.Phase = Waiting and then E.Now >= S.Retry_At then
               S.Phase := Idle;
            end if;
            if S.Phase = Idle then
               case S.Loan is
                  when Free =>
                     S.Loan := Lent;
                     S.Phase := Watching;
                     Act := Lend_And_Submit;
                  when Lent =>
                     S.Phase := Watching;
                     Act := Submit;
                  when Retiring =>
                     null;
               end case;
            end if;
         when Lend_Failed =>
            if S.Phase = Watching and then S.Loan = Lent then
               S.Loan := Free;
            end if;
            S.Phase := Waiting;
            S.Retry_At := Later (E.Now);
            S.Shown := False;
         when Done =>
            if E.Result /= Answered then
               S.Shown := False;
               S.Phase := Waiting;
               S.Retry_At := Later (E.Now);
               if S.Loan = Lent then
                  S.Loan := Retiring;
                  Act := Revoke;
               end if;
            elsif S.Phase = Watching then
               S.Shown := True;
               if S.Loan = Lent then
                  Act := Submit;
               else
                  S.Phase := Idle;
               end if;
            end if;
         when Retired =>
            if S.Loan = Retiring then
               S.Loan := Free;
            end if;
      end case;
   end Step;

   procedure Step (S : in out Switch_State; E : Switch_Event; Act : out Switch_Action) is
   begin
      Act := (others => <>);
      case E.Kind is
         when Flip =>
            if S.Phase = Switching then
               S.Wanted := E.Value;
               S.Queued := E.Value /= S.Sent;
            else
               S.Queued := False;
            end if;
            if S.Phase = Idle and then E.Value /= S.Confirmed then
               S.Phase := Switching;
               S.Sent := E.Value;
               Act := (Submit => True, Value => E.Value);
            end if;
         when Done =>
            if S.Phase = Switching then
               if E.Result = Answered then
                  S.Confirmed := S.Sent;
               end if;
               S.Phase := Idle;
               if S.Queued then
                  S.Queued := False;
                  if S.Wanted /= S.Confirmed then
                     S.Phase := Switching;
                     S.Sent := S.Wanted;
                     Act := (Submit => True, Value => S.Wanted);
                  end if;
               end if;
            end if;
      end case;
   end Step;
end Desktop_Status_Machines;
