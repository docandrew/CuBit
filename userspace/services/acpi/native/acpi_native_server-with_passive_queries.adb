pragma Ada_2022;
with ACPI_Native_Passive_Queries;
package body ACPI_Native_Server.With_Passive_Queries is
   procedure Advance(Server:in out ACPI_Requests.State;Result:out Owner_Outcome) is
      Worked:Boolean;
   begin
      ACPI_Native_Passive_Queries.Advance(Server,Worked);
      Advance_Normal(Server,Result);
      if Worked and then Result=Owner_Idle then Result:=Owner_Pending;end if;
   end Advance;
   procedure Maintain(Deadline:out Interfaces.Unsigned_64;May_Continue:out Boolean) is
      Query_Deadline:Interfaces.Unsigned_64;Queries_Healthy:Boolean;
   begin
      Maintain_Normal(Deadline,May_Continue);
      ACPI_Native_Passive_Queries.Maintain(Query_Deadline,Queries_Healthy);
      Deadline:=Interfaces.Unsigned_64'Min(Deadline,Query_Deadline);
      May_Continue:=May_Continue and Queries_Healthy;
   end Maintain;
end ACPI_Native_Server.With_Passive_Queries;
