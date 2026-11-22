package body ACPI_Serving_Properties with SPARK_Mode is
   use type P.Mode,P.Reason,P.Snapshot;
   procedure Observer_Requires_All_Facts(Facts:P.Eligibility;Retired:Boolean) is
      Run:P.Session;Accepted:Boolean;
   begin
      P.Select_Observer(Run,Facts,Retired,Accepted);
      pragma Assert(Accepted=(Retired and then P.Eligible(Facts)));
      pragma Assert(P.Observe(Run).Consumed);
      pragma Assert(P.Observe(Run).Selected=Accepted);
      pragma Assert(P.Observe(Run).Current=(if Accepted then P.Observer_Only else P.Not_Serving));
      pragma Assert(P.Observe(Run).Why=(if Accepted then P.Runtime_Not_Admitted else P.None));
   end Observer_Requires_All_Facts;
   procedure Consumer_Duty_Blocks_Observer(Facts:P.Eligibility;Retired:Boolean) is
      Run:P.Session;Accepted:Boolean;Before:P.Snapshot;
   begin
      P.Select_Observer(Run,Facts,Retired,Accepted);
      pragma Assert(not Accepted and then not P.Observe(Run).Selected);
      Before:=P.Observe(Run);
      P.Select_Runtime(Run,True,Accepted);
      pragma Assert(not Accepted and then P.Observe(Run)=Before);
      P.Select_Observer(Run,(others=>True),True,Accepted);
      pragma Assert(not Accepted and then P.Observe(Run)=Before);
   end Consumer_Duty_Blocks_Observer;
   procedure Retirement_Is_Required(Facts:P.Eligibility) is
      Run:P.Session;Accepted:Boolean;Before:P.Snapshot;
   begin
      P.Select_Observer(Run,Facts,False,Accepted);
      pragma Assert(not Accepted and then not P.Observe(Run).Selected);
      Before:=P.Observe(Run);
      P.Select_Observer(Run,(others=>True),True,Accepted);
      pragma Assert(not Accepted and then P.Observe(Run)=Before);
   end Retirement_Is_Required;
   procedure Runtime_Requires_Admission(Admitted:Boolean) is
      Run:P.Session;Accepted:Boolean;Before:P.Snapshot;
   begin
      P.Select_Runtime(Run,Admitted,Accepted);
      pragma Assert(Accepted=Admitted);
      pragma Assert(P.Observe(Run).Consumed and then P.Observe(Run).Selected=Admitted);
      pragma Assert(P.Observe(Run).Current=(if Admitted then P.Local_Runtime else P.Not_Serving));
      pragma Assert(P.Observe(Run).Why=P.None);
      Before:=P.Observe(Run);
      P.Select_Observer(Run,(others=>True),True,Accepted);
      pragma Assert(not Accepted and then P.Observe(Run)=Before);
   end Runtime_Requires_Admission;
   procedure Observer_Choice_Is_Final(Facts:P.Eligibility) is
      Run:P.Session;Accepted:Boolean;Before:P.Snapshot;
   begin
      P.Select_Observer(Run,Facts,True,Accepted);
      pragma Assert(Accepted and then P.Observe(Run).Current=P.Observer_Only);
      Before:=P.Observe(Run);
      P.Select_Runtime(Run,True,Accepted);
      pragma Assert(not Accepted and then P.Observe(Run)=Before);
      P.Select_Observer(Run,Facts,True,Accepted);
      pragma Assert(not Accepted and then P.Observe(Run)=Before);
   end Observer_Choice_Is_Final;
end ACPI_Serving_Properties;
