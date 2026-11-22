package body AML_Clock with SPARK_Mode is
   procedure No_Sample (Value : out Tick; Available : out Boolean) is
   begin Value := 0; Available := False; end No_Sample;
   function Last_Accepted (Clock : State) return Tick is (Clock.Last);
   function Fresh return State is (Last => 0);
   procedure Observe
     (Clock : in out State; Microseconds : Tick; Available : Boolean;
      Value : out Tick; Status : out Sample_Status)
   is
   begin
      Value := 0;
      if not Available then Status := Unavailable; return; end if;
      if Microseconds > Max_Microseconds then Status := Out_Of_Range; return; end if;
      if Microseconds * 10 < Clock.Last then Status := Regressed; return; end if;
      Value := Microseconds * 10;
      Clock.Last := Value;
      Status := Accepted;
   end Observe;
end AML_Clock;
