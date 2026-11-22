pragma Ada_2022;
with ACPI_Query_Transport;
package ACPI_Query_Client_Results with Pure,SPARK_Mode is
   type Outcome is(No_Result,Pending,Invalid_Request,Store_Failed,Create_Refused,
      Call_Failed,Malformed_Reply,Transport_Refused,Copy_Failed,Invalid_Payload,Completed);
   type Report is record
      Status:Outcome:=No_Result;
      Has_Transport:Boolean:=False;
      Transport:ACPI_Query_Transport.Reply_Details;
   end record;
end ACPI_Query_Client_Results;
