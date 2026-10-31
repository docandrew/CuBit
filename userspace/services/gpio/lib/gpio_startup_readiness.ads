pragma Ada_2022;
package GPIO_Startup_Readiness with SPARK_Mode, Pure is
   type Gates is record
      Community_Parked, Registry_Operational, Role_Registered,
      Admission_Clean, Not_Stopping, Clock_Available : Boolean := False;
   end record;
   function Ready (Item : Gates) return Boolean is
     (Item.Community_Parked and then Item.Registry_Operational and then Item.Role_Registered
      and then Item.Admission_Clean and then Item.Not_Stopping and then Item.Clock_Available);
   type Choice is (No_Report, Report_Ready, Report_Not_Present);
   type State is limited private;
   function Attempted (S : State) return Boolean;
   procedure Decide (S : in out State; Item : Gates; Selected : out Choice)
     with Global => null,
       Post => (if Attempted (S)'Old then Selected = No_Report
          elsif Ready (Item) then Selected = Report_Ready else Selected = Report_Not_Present)
         and then Attempted (S);
   -- The caller records this decision BEFORE its single send attempt. Failed
   -- delivery/ACK never permits replay. Inputs must reflect actual owner state,
   -- role registration and sampled clock; the decision itself grants no access.
private
   type State is limited record
      Sent : Boolean := False;
   end record;
   function Attempted (S : State) return Boolean is (S.Sent);
end GPIO_Startup_Readiness;
