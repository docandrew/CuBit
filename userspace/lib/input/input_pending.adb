package body Input_Pending with SPARK_Mode is
   procedure Acknowledge (Q : in out Queue) is
   begin
      Q.Head := (Q.Head + 1) mod Capacity;
      Q.Used := Q.Used - 1;
   end Acknowledge;

   procedure Append (Q : in out Queue; Payload : Word; Lost : out Boolean;
                     Observed_Ms : Word := Word'Last) is
   begin
      Lost := Q.Used = Capacity;
      if Lost then
         Q.Head := 0;
         Q.Used := 0;
      end if;
      Q.Last_Sequence := Next_Sequence (Q.Last_Sequence);
      Q.Data ((Q.Head + Q.Used) mod Capacity) :=
        (Payload, Q.Last_Sequence, Lost or Q.Recover_Next, Observed_Ms);
      Q.Recover_Next := False;
      Q.Used := Q.Used + 1;
   end Append;

   procedure Reset (Q : in out Queue) is
   begin
      Q.Used := 0;
      Q.Head := 0;
      Q.Recover_Next := True;
   end Reset;
end Input_Pending;
