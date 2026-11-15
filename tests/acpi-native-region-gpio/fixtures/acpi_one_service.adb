pragma Ada_2022;
with AML_Decode;
package body ACPI_One_Service is
 package body Root is
  package body Fields is
   procedure Read(Register:ACPI_EC_Types.Read_Register;Value:out ACPI_EC_Types.Byte;Result:out ACPI_EC_Types.Access_Result) is
    pragma Unreferenced(Register);
   begin Prep_Model.Physical_Calls:=Prep_Model.Physical_Calls+1;Value:=0;Result:=ACPI_EC_Types.Refused;end;
   procedure Write(Register:ACPI_EC_Types.Write_Register;Value:ACPI_EC_Types.Byte;Result:out ACPI_EC_Types.Access_Result) is
    pragma Unreferenced(Register,Value);
   begin Prep_Model.Physical_Calls:=Prep_Model.Physical_Calls+1;Result:=ACPI_EC_Types.Refused;end;
   procedure Clock(Now:out ACPI_EC_Types.Microseconds;Available:out Boolean) is
   begin Prep_Model.Physical_Calls:=Prep_Model.Physical_Calls+1;Now:=0;Available:=False;end;
   procedure Enter_Command(For_Query,For_Write:Boolean;Deadline:ACPI_EC_Types.Microseconds;Accepted:out Boolean) is
    pragma Unreferenced(For_Query,For_Write,Deadline);
   begin Prep_Model.Physical_Calls:=Prep_Model.Physical_Calls+1;Accepted:=False;end;
   procedure Leave_Command(Released:out Boolean) is
   begin Prep_Model.Physical_Calls:=Prep_Model.Physical_Calls+1;Released:=False;end;
  end Fields;
  package body Backing is
   package body Regions is
    package body Cache is
     function Observe(State:Context) return Observation is
      pragma Unreferenced(State);begin return(Regions=>0);end;
     function Read_Profile(State:Context;Index:Positive) return Profile is
      pragma Unreferenced(State,Index);begin return(others=><>);end;
    end Cache;
   end Regions;
  end Backing;
  package body Evaluation is
   procedure Prepare_Fields(Service:ACPI_Service.State;Accepted:out Boolean) is
   begin Prep_Model.Prepare_Calls:=Prep_Model.Prepare_Calls+1;Accepted:=ACPI_Service.Valid(Service) and then Prep_Model.Prepare_OK and then not Prep_Model.Fields_Pending;end;
   procedure Evaluate_Named(Service:aliased in out ACPI_Service.State;Item:ACPI_Service.Node_Descriptor;Budget:Natural;
    Result:out ACPI_Service.Values.Result;Status:out ACPI_Service.Values.Access_Status) is
   begin
    ACPI_Service.Evaluate_Named(Service,Item,Budget,Result,Status);
    if ACPI_Service.Values.Node_Name(Item)="_INI" then
     Prep_Model.Initializer_Evaluations:=Prep_Model.Initializer_Evaluations+1;
     if Prep_Model.Lose_Handler_After_Prefix and then Prep_Model.Initializer_Evaluations=3 then Prep_Model.EC_Ready:=False;end if;
    end if;
   end;
   procedure Invoke_Retained(Service:aliased in out ACPI_Service.State;Node:ACPI_Service.Namespace.Node_ID;
    Args:AML_Execute.Arguments;Argument_Count:ACPI_Service.Values.Argument_Count;Budget:Natural;
    Result:out ACPI_Service.Values.Result;Status:out ACPI_Service.Values.Access_Status) is
    use type AML_Decode.Integer_Value;
   begin
    if Args(1)=1 then Prep_Model.Connect_Calls:=Prep_Model.Connect_Calls+1;
    else Prep_Model.Disconnect_Calls:=Prep_Model.Disconnect_Calls+1;end if;
    ACPI_Service.Invoke_Retained(Service,Node,Args,Argument_Count,Budget,Result,Status);
   end;
  end Evaluation;
 end Root;
end ACPI_One_Service;
