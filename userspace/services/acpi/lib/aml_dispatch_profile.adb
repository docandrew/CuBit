pragma Ada_2022;
package body AML_Dispatch_Profile with SPARK_Mode is
   use type Counter;
   procedure Add (State : in out Context; Into : in out Counter; Amount : Counter) is
   begin
      if Amount > Counter'Last - Into then Into := Counter'Last; State.Overflowed := True;
      else Into := Into + Amount; end if;
   end Add;
   procedure Account (State : in out Context; Now : Counter) is
      Ticks : Counter := State.Items (State.Previous_Site, State.Previous_Class).Interval_Ticks;
   begin
      if Now < State.Last then
         if State.Regressions < Counter'Last then State.Regressions := State.Regressions + 1; end if;
      else
         Add (State, Ticks, Now - State.Last);
         State.Items (State.Previous_Site, State.Previous_Class).Interval_Ticks := Ticks;
      end if;
      State.Last := Now;
   end Account;
   procedure Start (State : in out Context; Now : Counter) is
   begin
      if State.Started then State.Malformed := True; return; end if;
      State.Started := True; State.Recording := True; State.Last := Now;
   end Start;
   procedure Observe (State : in out Context; Where : Site;
      Item : Instruction_Class; Now : Counter) is
      Visits : Counter;
   begin
      if not State.Recording then return; end if;
      Account (State, Now);
      Visits := State.Items (Where, Item).Visits;
      Add (State, Visits, 1);
      State.Items (Where, Item).Visits := Visits;
      State.Previous_Site := Where; State.Previous_Class := Item;
   end Observe;
   procedure Stop (State : in out Context; Now : Counter) is
   begin
      if not State.Recording then return; end if;
      Account (State, Now); State.Recording := False;
   end Stop;
   function Read (State : Context; Where : Site; Item : Instruction_Class) return Counters is
     (State.Items (Where, Item));
   function Active (State : Context) return Boolean is (State.Recording);
   function Complete (State : Context) return Boolean is
     (State.Started and then not State.Recording and then not State.Malformed
      and then not State.Overflowed and then State.Regressions = 0);
   function Clock_Regressions (State : Context) return Counter is (State.Regressions);
   function Saturated (State : Context) return Boolean is (State.Overflowed);
end AML_Dispatch_Profile;
