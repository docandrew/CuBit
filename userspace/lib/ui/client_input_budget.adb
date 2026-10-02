package body Client_Input_Budget with SPARK_Mode is
   function Open (Now : Tick) return Batch is ((Count => 0, Start => Now));
   function Can_Poll (S : Batch; Now : Tick) return Boolean is
     (S.Count < Poll_Limit and then
      (S.Count = 0 or else (Now >= S.Start and then Now - S.Start < Time_Limit)));
   procedure Charge (S : in out Batch) is
   begin
      S.Count := S.Count + 1;
   end Charge;
end Client_Input_Budget;
