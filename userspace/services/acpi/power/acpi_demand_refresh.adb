pragma Ada_2022;
package body ACPI_Demand_Refresh with SPARK_Mode is
   function Valid(S:State) return Boolean is
     (S.Serial<=Maximum_Serial and then S.Finished<=S.Serial
      and then (if S.Active then S.Serial>0 and S.Finished<S.Serial));
   function Pending(S:State) return Boolean is(S.Active);
   function Stopping(S:State) return Boolean is(S.Stopped);
   function Can_Request(S:State;Now:Word;Clock_Known:Boolean) return Boolean is
     (not S.Stopped and then Clock_Known and then (not S.Have_Time or else Now>=S.Last_Time)
      and then (S.Active or else (S.Serial<Maximum_Serial and then
        (not S.Have_Time or else Now-S.Last_Time>=Minimum_Interval))));
   function Ready(S:State;Ticket:Word) return Boolean is
     (Ticket>0 and then Ticket<=S.Serial and then (S.Stopped or Ticket<=S.Finished));
   procedure Request(S:in out State;Now:Word;Clock_Known:Boolean;
      Ticket:out Word;New_Attempt,Accepted:out Boolean) is
   begin
      Ticket:=0;New_Attempt:=False;Accepted:=Can_Request(S,Now,Clock_Known);
      if not Accepted then return;end if;
      if not S.Active then
         S.Serial:=S.Serial+1;S.Active:=True;S.Last_Time:=Now;S.Have_Time:=True;New_Attempt:=True;
      end if;
      Ticket:=S.Serial;
   end Request;
   procedure Complete(S:in out State) is
   begin if S.Active then S.Finished:=S.Serial;S.Active:=False;end if;end Complete;
   procedure Begin_Stop(S:in out State) is begin S.Stopped:=True;end Begin_Stop;
end ACPI_Demand_Refresh;
