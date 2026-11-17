pragma Ada_2022;
package ACPI_Query_Client_Lifetime with Pure, SPARK_Mode is
   pragma Unevaluated_Use_Of_Old(Allow);
   type Phase is (Idle, Loan_Granted, Awaiting_Retirement, Payload_Readable, Result_Ready);
   type Snapshot is record
      Current:Phase:=Idle;
      Expected_Payload,Valid_Payload:Boolean:=False;
   end record;
   type State is limited private with Default_Initial_Condition =>
      Observe(State)=(Idle,False,False);
   function Observe(Object:State)return Snapshot with Global=>null;
   function Valid(Object:State)return Boolean with Global=>null;
   function Has_Loan(Object:State)return Boolean with Global=>null,
     Post=>Has_Loan'Result=(Observe(Object).Current in Loan_Granted|Awaiting_Retirement);
   function May_Read_Payload(Object:State)return Boolean with Global=>null,
     Post=>May_Read_Payload'Result=(Observe(Object).Current=Payload_Readable);
   function Payload_Available(Object:State)return Boolean with Global=>null,
     Post=>Payload_Available'Result=(Observe(Object).Current=Result_Ready and Observe(Object).Valid_Payload);
   -- Grant_Created is the actual kernel creation result. Reply and retirement
   -- observations must refer to that exact owned reference; booleans supplied
   -- by a peer/request are not evidence. This policy itself confers no authority.
   procedure Begin_Grant(Object:in out State;Grant_Created:Boolean;Accepted:out Boolean)
     with Global=>null,Always_Terminates,Pre=>Valid(Object),Post=>Valid(Object),
       Contract_Cases=>
       (Observe(Object).Current=Idle=>Accepted and then Observe(Object)=
          (Current=>(if Grant_Created then Loan_Granted else Result_Ready),others=>False),
        others=>not Accepted and then Observe(Object)=Observe(Object)'Old);
   procedure Record_Reply(Object:in out State;Payload_Expected:Boolean;Accepted:out Boolean)
     with Global=>null,Always_Terminates,Pre=>Valid(Object),Post=>Valid(Object),
       Contract_Cases=>
       (Observe(Object).Current=Loan_Granted=>Accepted and then Observe(Object)=
          (Awaiting_Retirement,Payload_Expected,False),
        others=>not Accepted and then Observe(Object)=Observe(Object)'Old);
   -- IPC completion, timeout and revoke acceptance do not establish retirement.
   -- Only an affirmative kernel owner retirement observation permits progress.
   procedure Observe_Retirement(Object:in out State;Confirmed:Boolean;Progressed:out Boolean)
     with Global=>null,Always_Terminates,Pre=>Valid(Object),Post=>Valid(Object),
       Contract_Cases=>
       (Observe(Object).Current=Awaiting_Retirement and Confirmed=>Progressed and then
          Observe(Object)=(Current=>(if Observe(Object)'Old.Expected_Payload then Payload_Readable else Result_Ready),
            Expected_Payload=>Observe(Object)'Old.Expected_Payload,Valid_Payload=>False),
        others=>not Progressed and then Observe(Object)=Observe(Object)'Old);
   procedure Finish_Payload(Object:in out State;Payload_Valid:Boolean;Accepted:out Boolean)
     with Global=>null,Always_Terminates,Pre=>Valid(Object),Post=>Valid(Object),
       Contract_Cases=>
       (Observe(Object).Current=Payload_Readable=>Accepted and then
          Observe(Object)=(Result_Ready,True,Payload_Valid),
        others=>not Accepted and then Observe(Object)=Observe(Object)'Old);
   procedure Consume(Object:in out State;Accepted:out Boolean)
     with Global=>null,Always_Terminates,Pre=>Valid(Object),Post=>Valid(Object),
       Contract_Cases=>
       (Observe(Object).Current=Result_Ready=>Accepted and then Observe(Object)=(Idle,False,False),
        others=>not Accepted and then Observe(Object)=Observe(Object)'Old);
private
   type State is limited record View:Snapshot;end record;
end ACPI_Query_Client_Lifetime;
