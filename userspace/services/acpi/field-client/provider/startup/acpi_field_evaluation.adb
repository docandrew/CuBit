pragma Ada_2022;
package body ACPI_Field_Evaluation is
   use type Bindings.Outcome;
   procedure Allocate(Service:Core.State;IO:Provider.Context;
      Capacity_Ceiling:Positive;Byte_Ceiling:Interfaces.Unsigned_64;
      Run:out Context_Access;Report:out Allocation_Report) is
      Capacity:Positive;Counted:Boolean;
      use type Interfaces.Unsigned_64,System.Address;
   begin
      Run:=null;Report:=(others=><>);
      Bindings.Required_Capacity(Service,IO,Capacity,Counted);
      if not Counted then return;end if;
      Report.Capacity:=Capacity;
      if Capacity>Capacity_Ceiling then Report.Status:=Capacity_Exceeded;return;end if;
      declare
         subtype Shape is Context(Capacity);
         Bits:constant Interfaces.Unsigned_64:=Shape'Object_Size;
         Bytes:constant Interfaces.Unsigned_64:=Bits/System.Storage_Unit+(if Bits mod System.Storage_Unit=0 then 0 else 1);
      begin
         Report.Bytes:=Bytes;
         if Bytes>Byte_Ceiling then Report.Status:=Capacity_Exceeded;return;end if;
         declare Address:constant System.Address:=Reserve_Cache(Bytes,Interfaces.Unsigned_64'Max(16,Shape'Alignment));
         begin
         if Address=System.Null_Address then Report.Status:=Allocation_Refused;return;end if;
         declare
            pragma Warnings(Off,"default initialization of * may modify overlaid storage");
            Value:aliased Shape with Address=>Address;
            pragma Warnings(On,"default initialization of * may modify overlaid storage");
         begin Run:=Value'Unchecked_Access;end;
         end;
         Report.Allocated_Bytes:=Bytes;Report.Status:=Allocated;
      end;
   end Allocate;
   function Observe (Run : Context) return Bindings.Report is
     (Bindings.Observe (Run.Cache));
   procedure Prepare(Run:in out Context;Service:Core.State;IO:in out Provider.Context;Accepted:out Boolean) is
      Report:Bindings.Report;
   begin
      if Observe(Run).Status=Bindings.Not_Attempted then Bindings.Build(Run.Cache,Service,IO,Report);end if;
      Accepted:=Bindings.Current(Run.Cache,Service,IO);
   end Prepare;
   procedure Evaluate_Named
     (Run : in out Context; Service : aliased in out Core.State;
      IO : in out Provider.Context; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
   is
      Accepted : Boolean;
      procedure Evaluate(Items:Bindings.Fields.Binding_Set) is
      begin Bindings.Fields.Evaluate_Named (Service, IO, Items, Item, Budget, Result, Status); end Evaluate;
      procedure Borrow is new Bindings.With_Bindings(Evaluate);
   begin
      Result := (AML_Execute.Unsupported_Value, 0);
      Status := Core.Values.Wrong_Phase;
      Prepare (Run, Service, IO, Accepted);
      if not Accepted then return; end if;
      if Provider.Pending (IO) then Status := Core.Values.Busy; return; end if;
      Borrow(Run.Cache,Service,IO,Accepted);
   end Evaluate_Named;
   procedure Invoke_Retained
     (Run : in out Context; Service : aliased in out Core.State;
      IO : in out Provider.Context; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
   is
      Accepted : Boolean;
      procedure Evaluate(Items:Bindings.Fields.Binding_Set) is
      begin Bindings.Fields.Invoke_Retained
        (Service, IO, Items, Node, Args, Natural (Argument_Count), Budget, Result, Status); end Evaluate;
      procedure Borrow is new Bindings.With_Bindings(Evaluate);
   begin
      Result := (AML_Execute.Unsupported_Value, 0);
      Status := Core.Values.Wrong_Phase;
      Prepare (Run, Service, IO, Accepted);
      if not Accepted then return; end if;
      if Provider.Pending (IO) then Status := Core.Values.Busy; return; end if;
      Borrow(Run.Cache,Service,IO,Accepted);
   end Invoke_Retained;
   procedure Invoke_With_Arguments
     (Run : in out Context; Service : aliased in out Core.State;
      IO : in out Provider.Context; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
   is
      Accepted : Boolean;
      procedure Evaluate(Items:Bindings.Fields.Binding_Set) is
      begin Bindings.Fields.Invoke_With_Arguments
        (Service, IO, Items, Node, Args, Argument_Count, Budget, Result, Status); end Evaluate;
      procedure Borrow is new Bindings.With_Bindings(Evaluate);
   begin
      Result := (AML_Execute.Unsupported_Value, 0);
      Status := Core.Values.Wrong_Phase;
      Prepare (Run, Service, IO, Accepted);
      if not Accepted then return; end if;
      if Provider.Pending (IO) then Status := Core.Values.Busy; return; end if;
      Borrow(Run.Cache,Service,IO,Accepted);
   end Invoke_With_Arguments;
end ACPI_Field_Evaluation;
