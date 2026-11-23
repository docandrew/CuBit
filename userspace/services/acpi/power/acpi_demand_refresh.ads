pragma Ada_2022;
with Interfaces;
generic
   Minimum_Interval : Interfaces.Unsigned_64 := 1_000_000;
   Maximum_Serial : Interfaces.Unsigned_64 := Interfaces.Unsigned_64(Interfaces.Integer_64'Last);
package ACPI_Demand_Refresh with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   type State is private;
   function Valid(S:State) return Boolean with Global=>null;
   function Pending(S:State) return Boolean with Global=>null;
   function Can_Request(S:State;Now:Word;Clock_Known:Boolean) return Boolean with Global=>null;
   function Ready(S:State;Ticket:Word) return Boolean with Global=>null;
   function Stopping(S:State) return Boolean with Global=>null;
   -- A pending attempt coalesces; clock loss/regression still refuses admission.
   -- No timers, callbacks or automatic retry are owned by this policy.
   procedure Request(S:in out State;Now:Word;Clock_Known:Boolean;
      Ticket:out Word;New_Attempt,Accepted:out Boolean)
     with Pre=>Valid(S),Post=>Valid(S)
       and then (if Accepted then Ticket>0 else Ticket=0);
   procedure Complete(S:in out State) with Pre=>Valid(S),Post=>Valid(S);
   procedure Begin_Stop(S:in out State) with Pre=>Valid(S),Post=>Valid(S);
private
   type State is record
      Serial,Finished,Last_Time:Word:=0;
      Active,Have_Time,Stopped:Boolean:=False;
   end record;
end ACPI_Demand_Refresh;
