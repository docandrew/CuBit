pragma Ada_2022;
package body ACPI_Query_Client_Lifetime with SPARK_Mode is
   function Observe(Object:State)return Snapshot is(Object.View);
   function Valid(Object:State)return Boolean is
     ((if Object.View.Current in Idle|Loan_Granted then not Object.View.Expected_Payload and not Object.View.Valid_Payload)
      and then (if Object.View.Current=Awaiting_Retirement then not Object.View.Valid_Payload)
      and then (if Object.View.Current=Payload_Readable then Object.View.Expected_Payload and not Object.View.Valid_Payload)
      and then (if Object.View.Valid_Payload then Object.View.Current=Result_Ready and Object.View.Expected_Payload));
   function Has_Loan(Object:State)return Boolean is(Object.View.Current in Loan_Granted|Awaiting_Retirement);
   function May_Read_Payload(Object:State)return Boolean is(Object.View.Current=Payload_Readable);
   function Payload_Available(Object:State)return Boolean is(Object.View.Current=Result_Ready and Object.View.Valid_Payload);
   procedure Begin_Grant(Object:in out State;Grant_Created:Boolean;Accepted:out Boolean) is
   begin Accepted:=Object.View.Current=Idle;if Accepted then Object.View:=(Current=>(if Grant_Created then Loan_Granted else Result_Ready),others=>False);end if;end;
   procedure Record_Reply(Object:in out State;Payload_Expected:Boolean;Accepted:out Boolean) is
   begin Accepted:=Object.View.Current=Loan_Granted;if Accepted then Object.View:=(Awaiting_Retirement,Payload_Expected,False);end if;end;
   procedure Observe_Retirement(Object:in out State;Confirmed:Boolean;Progressed:out Boolean) is
   begin Progressed:=Object.View.Current=Awaiting_Retirement and Confirmed;if Progressed then Object.View.Current:=(if Object.View.Expected_Payload then Payload_Readable else Result_Ready);end if;end;
   procedure Finish_Payload(Object:in out State;Payload_Valid:Boolean;Accepted:out Boolean) is
   begin Accepted:=Object.View.Current=Payload_Readable;if Accepted then Object.View:=(Result_Ready,True,Payload_Valid);end if;end;
   procedure Consume(Object:in out State;Accepted:out Boolean) is
   begin Accepted:=Object.View.Current=Result_Ready;if Accepted then Object.View:=(Idle,False,False);end if;end;
end ACPI_Query_Client_Lifetime;
