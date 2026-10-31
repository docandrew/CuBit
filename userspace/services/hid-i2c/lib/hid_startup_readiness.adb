pragma Ada_2022;
package body HID_Startup_Readiness with SPARK_Mode is
   procedure Decide (S : in out State; Now : W.Word; Limit : W.Millisecond;
      Clock_Available, Device_Ready, Stopping : Boolean; Send : out Choice) is
   begin
      Send := No_Report; if S.Reported /= No_Report then return; end if;
      if not Clock_Available or else Stopping or else Now >= Limit then
         Send := Report_Declined;
      elsif Device_Ready then Send := Report_Ready;
      end if;
      S.Reported := Send;
   end Decide;
   procedure Delivery (S : in out State; Acknowledged : Boolean) is
   begin
      if S.Attempt_Completed then return; end if;
      S.Attempt_Completed := True; S.Was_Acknowledged := Acknowledged;
   end Delivery;
   function Must_Stop (S : State) return Boolean is
     (S.Reported = Report_Declined or else
        (S.Attempt_Completed and then not S.Was_Acknowledged));
   function Confirmed_Ready (S : State) return Boolean is
     (S.Reported = Report_Ready and then S.Attempt_Completed and then S.Was_Acknowledged);
   function Wake_Deadline (S : State; Limit : W.Millisecond; Otherwise : W.Word)
      return W.Word is
     (if S.Reported = No_Report then W.Word'Min (Limit, Otherwise) else Otherwise);
end HID_Startup_Readiness;
