pragma Ada_2022;
with ACPI_Query_Client_Results;with ACPI_Passive_Query.Pages;with ACPI_Query_Transport;with ACPI_Query_Client_Lifetime;
generic
   Maximum_Bytes:Positive:=4096;
   type Loan_Reference is private;
   No_Reference:Loan_Reference;
   with procedure Store_Request(Data:ACPI_Passive_Query.Pages.Byte_Array;Accepted:out Boolean);
   with procedure Create(Loan:out Loan_Reference;Accepted:out Boolean);
   with procedure Call(Loan:Loan_Reference;Extent,Deadline:ACPI_Query_Transport.Word;
      Reply:out ACPI_Query_Transport.Packet;Answered:out Boolean);
   with procedure Revoke(Loan:Loan_Reference;Accepted:out Boolean);
   with function Retirement_Confirmed(Loan:Loan_Reference)return Boolean;
   with procedure Copy_Response(Data:out ACPI_Passive_Query.Pages.Byte_Array;Accepted:out Boolean);
package ACPI_Query_Client_Session with SPARK_Mode=>Off is
   package T renames ACPI_Query_Transport;
   package P renames ACPI_Passive_Query.Pages;
   subtype Buffer is P.Byte_Array(0..Maximum_Bytes-1);
   subtype Outcome is ACPI_Query_Client_Results.Outcome;
   subtype Report is ACPI_Query_Client_Results.Report;
   -- One serialized exchange per instance. Store_Request must write private
   -- stable aligned storage before Create. Keep that storage alive unchanged
   -- until Cleanup_Pending=False. Call must go only down the service layering.
   -- All kernel observations refer to this instance's exact owned reference.
   procedure Start(Request_Data:P.Byte_Array;Deadline:T.Word;Accepted:out Boolean);
   procedure Progress(Did_Work:out Boolean);
   function Status return ACPI_Query_Client_Lifetime.Phase;
   function Cleanup_Pending return Boolean;
   -- A page is copied only after affirmative owner retirement, never merely
   -- after IPC completion/timeout or accepted revocation. No request is replayed.
   procedure Take(Value:out Report;Data:out Buffer;Taken:out Boolean);
end ACPI_Query_Client_Session;
