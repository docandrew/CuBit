pragma Ada_2022;
-- Hosted physical-owner model; actual Core and native Regions body are selected.
with Interfaces;with Prep_Model;with ACPI_Service;with AML_Execute;
with AML_Field_Notes;with ACPI_EC_Types;with ACPI_Local_EC_Commands;with Platform_Pool_ABI;
package ACPI_One_Service is
 subtype Word is Interfaces.Unsigned_64;
 package Root is
  package Fields is
   type Field_Phase is(Owner_Fresh,Owner_Ready,Owner_Failed,Owner_Closed);
   type Field_Report is record
    Stage:Field_Phase:=Owner_Ready;
    Module_Attempted,Module_Completed,Startup_Finished,IO_Pending:Boolean:=False;
    Registers:Natural:=0;
   end record;
   type Uncertainty is record Present:Boolean:=False;end record;
   function Control_Idle return Boolean is(not Prep_Model.Busy);
   function Observe return Field_Report is((Stage=>Owner_Ready,Module_Attempted=>True,Module_Completed=>True,IO_Pending=>Prep_Model.Fields_Pending,others=><>));
   function Uncertain return Uncertainty is((Present=>Prep_Model.Uncertain));
   type Field_Sequence is new Word;
   type Field_Trace_Report is record Sequence:Field_Sequence:=0;Saturated:Boolean:=False;Item:AML_Field_Notes.Event;end record;
   function Field_Observation return Field_Trace_Report is((others=><>));
   type Preparation_Outcome is(Not_Prepared,Prepared);
   type Preparation_Report is record
    Status:Preparation_Outcome:=Prepared;
    Module_Completed,Anchor_Present,Core_Valid,Context_Current:Boolean:=True;
    Owner_Ready,Scope_Ready,Mux_Ready,Hardware_Admitting:Boolean:=True;
    Binding_Limit:Natural:=0;
    Metadata_Bytes,Allocated_Metadata_Bytes:Word:=0;
    Failed_Node:ACPI_Service.Namespace.Node_ID:=ACPI_Service.Namespace.Root;
    Access_Failure:ACPI_Service.Values.Access_Status:=ACPI_Service.Values.Available;
    Region_Entries,Read_Fields,Write_Fields,Missing_Fields:Natural:=0;
   end record;
   function Preparation_Observation return Preparation_Report is((others=><>));
   type EC_Boot_Status is(EC_Not_Inspected,EC_Absent,EC_Multiple,EC_Inspection_Limit,
    EC_Description_Refused,EC_Registers_Refused,EC_Path_Refused,EC_Port_Outside_Grant,EC_Ready);
   type EC_Boot_Report is record Status:EC_Boot_Status:=EC_Ready;end record;
   function EC_Boot_Observation return EC_Boot_Report is((others=><>));
   function EC_Handler_Available return Boolean is(Prep_Model.EC_Ready);
   procedure Read(Register:ACPI_EC_Types.Read_Register;Value:out ACPI_EC_Types.Byte;Result:out ACPI_EC_Types.Access_Result);
   procedure Write(Register:ACPI_EC_Types.Write_Register;Value:ACPI_EC_Types.Byte;Result:out ACPI_EC_Types.Access_Result);
   procedure Clock(Now:out ACPI_EC_Types.Microseconds;Available:out Boolean);
   procedure Enter_Command(For_Query,For_Write:Boolean;Deadline:ACPI_EC_Types.Microseconds;Accepted:out Boolean);
   procedure Leave_Command(Released:out Boolean);
   function Command_Quiet return Boolean is(not Prep_Model.Busy);
   function Backing_Owned return Boolean is(False);
   package EC_Commands is new ACPI_Local_EC_Commands(Read,Write,Clock,Enter_Command,Leave_Command,Command_Quiet,Backing_Owned);
  end Fields;
  package Backing is
   package Regions is
    package Cache is
     package ABI renames Platform_Pool_ABI;
     subtype Word is Interfaces.Unsigned_64;
     type Outcome is(Not_Ready,Kernel_Refused);
     type Context is null record;
     type Observation is record Regions:Natural:=0;end record;
     type Geometry is record Address,Length:Word:=0;end record;
     type Profile is record Available:Boolean:=False;Geometry:Cache.Geometry;Result:Outcome:=Not_Ready;Raw_Status:Word:=0;end record;
     function Observe(State:Context) return Observation;
     function Read_Profile(State:Context;Index:Positive) return Profile;
    end Cache;
   end Regions;
   Memory:Regions.Cache.Context;
  end Backing;
  package Hardware is
   type Observation is record Lock_Ready:Boolean:=True;end record;
   function Observe return Observation is((Lock_Ready=>Prep_Model.Lock_Ready));
  end Hardware;
  package Shutdown is
   type Observation is record Working:Boolean:=False;end record;
   function Observe return Observation is((others=><>));
  end Shutdown;
  package Evaluation is
   procedure Prepare_Fields(Service:ACPI_Service.State;Accepted:out Boolean);
   procedure Evaluate_Named(Service:aliased in out ACPI_Service.State;Item:ACPI_Service.Node_Descriptor;Budget:Natural;
    Result:out ACPI_Service.Values.Result;Status:out ACPI_Service.Values.Access_Status);
   procedure Invoke_Retained(Service:aliased in out ACPI_Service.State;Node:ACPI_Service.Namespace.Node_ID;
    Args:AML_Execute.Arguments;Argument_Count:ACPI_Service.Values.Argument_Count;Budget:Natural;
    Result:out ACPI_Service.Values.Result;Status:out ACPI_Service.Values.Access_Status);
  end Evaluation;
 end Root;
 package Runtime is
  package Status is type Phase is(Fresh,Active,Stopped);end Status;
  function Current return Status.Phase is(Status.Fresh);
  procedure Invoke_Retained(Service:aliased in out ACPI_Service.State;Node:ACPI_Service.Namespace.Node_ID;
   Args:AML_Execute.Arguments;Argument_Count:ACPI_Service.Values.Argument_Count;Budget:Natural;
   Result:out ACPI_Service.Values.Result;Status:out ACPI_Service.Values.Access_Status) renames Root.Evaluation.Invoke_Retained;
 end Runtime;
end ACPI_One_Service;
