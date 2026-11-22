pragma Ada_2022;
package body GPIO_Service_Pump is
   package R renames Admin.Registry;
   function Pending (S : Admin.State) return Boolean is
     (R.Needs_Step or else Admin.Observe (S).At_Phase in
        Admin.Await_Admission | Admin.Await_Retirement | Admin.Drain_Lost_Admission);
   procedure Turn (S : in out Admin.State; Result : out Turn_Report) is
      Found, Worked : Boolean;
   begin
      Result := (others => <>);
      for N in 1 .. Events_Per_Turn loop
         Poll_Event (S, Found); exit when not Found;
         Result.Events := Result.Events + 1;
      end loop;
      for N in 1 .. Requests_Per_Turn loop
         Poll_Request (S, Found); exit when not Found;
         Result.Requests := Result.Requests + 1;
      end loop;
      for N in 1 .. Steps_Per_Turn loop
         exit when not Pending (S);
         R.Step (Worked); Admin.Step (S);
         Result.Steps := Result.Steps + 1;
      end loop;
      Result.Progress_Needed := Pending (S);
      Result.Terminal := R.Terminal;
      Result.Quiescent := R.Quiescent and then Admin.Quiescent (S);
   end Turn;
end GPIO_Service_Pump;
