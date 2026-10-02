with Region_Transition;
with Ada.Text_IO;
procedure Transition_Tests is
   procedure Run (Failure : Natural; Raise_Failure : Boolean) is
      Calls : Natural := 0;
      Log : String (1 .. 8) := [others => ' '];
      Fault : exception;
      procedure Step (Kind : Character; OK : out Boolean) is
      begin
         Calls := Calls + 1;
         Log (Calls) := Kind;
         if Calls = Failure and Raise_Failure then raise Fault; end if;
         OK := Calls /= Failure;
      end Step;
      procedure Revoke (OK : out Boolean) is
      begin Step ('R', OK); end Revoke;
      procedure Invalidate (OK : out Boolean) is
      begin Step ('F', OK); end Invalidate;
      procedure Install (Executable : Boolean; OK : out Boolean) is
      begin Step ((if Executable then 'X' else 'W'), OK); end Install;
      package T is new Region_Transition (Revoke, Invalidate, Install);
      use type T.Phase, T.Permission;
      Object : T.Region;
      OK : Boolean;
   begin
      T.Change (Object, T.Executable, False, OK);
      pragma Assert (not OK and Calls = 0 and T.State (Object) = T.Stable);
      T.Change (Object, T.Writable, True, OK);
      pragma Assert (OK and Calls = 0);
      begin
         T.Change (Object, T.Executable, True, OK);
         pragma Assert (not Raise_Failure or Failure = 0);
      exception
         when Fault => pragma Assert (Raise_Failure and Failure /= 0);
      end;
      if Failure = 0 then
         pragma Assert (OK and Log (1 .. 4) = "RFXF" and T.Current (Object) = T.Executable);
         T.Change (Object, T.Writable, True, OK);
         pragma Assert (OK and Log = "RFXFRFWF" and T.Current (Object) = T.Writable);
      else
         pragma Assert (Calls = Failure and T.State (Object) = T.Quarantined);
         T.Change (Object, T.Writable, True, OK);
         pragma Assert (not OK and Calls = Failure);
      end if;
   end Run;
begin
   Run (0, False);
   for I in 1 .. 4 loop
      Run (I, False);
      Run (I, True);
   end loop;
   Ada.Text_IO.Put_Line ("PASS: ordered RW/RX transitions, denial, callback failures and exceptions");
end Transition_Tests;
