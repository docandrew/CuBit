with Ada.Text_IO; with Interfaces;
with Intel_GPU_Combo_PHY; with Intel_GPU_Combo_Restore;
procedure Combo_Restore_Tests is
   use Interfaces; use Intel_GPU_Combo_PHY;
   procedure Test (Fail_At : Natural; Mode : Natural := 0) is
      States : array (PHY) of Snapshot := (others => (others => 0));
      Calls, Ends, Writes, Reads : Natural := 0;
      Active : Boolean := False;
      function Step return Boolean is
      begin Calls := Calls + 1; return Calls /= Fail_At; end Step;
      function Begin_Scope return Boolean is
      begin Active := Step; return Active; end Begin_Scope;
      procedure End_Scope is
      begin pragma Assert (Active); Active := False; Ends := Ends + 1; end End_Scope;
      function Held return Boolean is
      begin pragma Assert (Active); return Step; end Held;
      procedure Read_State (Port : PHY; State : out Snapshot; Success : out Boolean) is
      begin
         pragma Assert (Active); Reads := Reads + 1; Success := Step;
         State := States (Port);
         if Mode = 1 and Port = B then State (Comp_3) := 16#1F000000#; end if;
         if Mode = 2 and Reads = 3 then State (CL_5) := 16#10#; end if;
         if Mode = 3 and Reads = 4 then State (Comp_0) := 0; end if;
         if Mode = 5 and Reads = 3 then
            -- Unowned fields changed after preflight: preserve the fresh
            -- value when constructing the write, not the old baseline.
            State (CL_5) := 1;
            State (Comp_0) := 16#00005F23#;
         end if;
         if Mode = 6 and Reads = 3 then State (Comp_3) := 16#01000000#; end if;
      end Read_State;
      procedure Write_Register (Offset, Value : Unsigned_32; Success : out Boolean) is
         Found : Boolean := False;
      begin
         pragma Assert (Active); Writes := Writes + 1; Success := Step;
         -- A failed callback may already have changed hardware.
         for Port in PHY loop
            for F in Misc .. CL_5 loop
               if Write_Offset (Port, F) = Offset then
                  States (Port) (F) := Value; Found := True;
                  if Port = B then
                     pragma Assert (Prepare (A, States (A)).Status = Already_Ready);
                  end if;
               end if;
            end loop;
         end loop;
         pragma Assert (Found);
      end Write_Register;
      procedure Finish_Writes (Success : out Boolean) is
      begin pragma Assert (Active); Success := Step; end Finish_Writes;
      package E is new Intel_GPU_Combo_Restore
        (Begin_Scope, End_Scope, Held, Read_State, Write_Register, Finish_Writes);
      use type E.Outcome;
      R : E.Report;
      Saved : Natural;
      Initial : Plan;
   begin
      if Mode = 4 then
         for Port in PHY loop
            Initial := Prepare (Port, States (Port));
            for I in 1 .. Initial.Count loop
               States (Port) (Initial.Writes (I).Register) := Initial.Writes (I).Value;
            end loop;
         end loop;
      end if;
      E.Execute (False, R);
      pragma Assert (R.Status = E.Rejected and Calls = 0);
      E.Execute (True, R);
      pragma Assert (not Active and Ends = (if Fail_At = 1 then 0 else 1));
      pragma Assert (R.Writes_Attempted = Writes);
      if Mode = 1 then
         pragma Assert (R.Status = E.Invalid_State and Writes = 0);
         pragma Assert (E.Diagnostic (R) = "PHY=B result=invalid-state writes= 0");
      elsif Mode = 2 or Mode = 6 then
         pragma Assert (R.Status = E.Changed and Writes = 0);
         pragma Assert (E.Diagnostic (R) = "PHY=A result=changed writes= 0");
      elsif Mode = 3 then
         pragma Assert (R.Status = E.Verification_Failed and Writes = 9);
         pragma Assert (E.Diagnostic (R) = "PHY=A result=verification-failed writes= 9");
      elsif Mode = 4 then
         pragma Assert (R.Status = E.Ready and Writes = 0 and Reads = 6);
      elsif Mode = 5 then
         pragma Assert (R.Status = E.Ready and Writes = 17);
         pragma Assert (States (A) (CL_5) = 16#11#);
         pragma Assert (States (A) (Comp_0) = 16#80005F23#);
      elsif Fail_At = 0 then
         pragma Assert (R.Status = E.Ready and Writes = 17);
         pragma Assert (Calls = 52);
      else
         pragma Assert (R.Status /= E.Ready and Calls = Fail_At);
      end if;
      Saved := Calls;
      E.Execute (True, R);
      pragma Assert (R.Status = E.Rejected and Calls = Saved);
   end Test;
begin
   Test (0);
   for I in 1 .. 52 loop Test (I); end loop;
   for Mode in 1 .. 6 loop Test (0, Mode); end loop;
   Ada.Text_IO.Put_Line ("combo restore: callback failures, ordering, quarantine PASS");
end Combo_Restore_Tests;
