pragma Ada_2022;
with ACPI_Hosted_Heap;with ACPI_Service_Core;with AML_Delays;with AML_Objects;
with ACPI_Field_Hosted_Provider;with ACPI_Region_Policy;with ACPI_FADT.Transactions;
with AML_Field_Access;with AML_Field_Targets;with AML_Execute;with Ada.Text_IO;
with Ada.Command_Line;with Ada.Streams;with Ada.Streams.Stream_IO;with Firmware_Tables;
with Interfaces;use Interfaces;
with System;with ACPI_Fallible_Heap;
with ACPI_Field_Provider_View;with ACPI_Field_Evaluation;with AML_Identity;
with AML_Field_Layout;with AML_Register_Views;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Capacity_Tests is
 package C is new ACPI_Service_Core(AML_Delays.Unavailable_Provider,Namespace_Node_Capacity=>128);
 package T renames ACPI_FADT.Transactions;package A renames AML_Field_Access;package IO renames Ada.Streams.Stream_IO;
 use type C.Stage_Status,C.Freeze_Status,C.Values.Access_Status,C.Namespace.Lookup_Status,AML_Execute.Execution_Status;
 type Hardware is record Reads:Natural:=0;end record;
 procedure Transact(H:in out Hardware;Space:ACPI_Region_Policy.Address_Space;Address:Unsigned_64;Width:T.Access_Width;
 For_Write:Boolean;Input:Unsigned_64;Output:out Unsigned_64;Completed:out Boolean) is
 pragma Unreferenced(Space,Width,Input);
 begin H.Reads:=H.Reads+1;Output:=16#A5#;Completed:=not For_Write and Address in 16#1000#..16#1007#;end Transact;
 package P is new ACPI_Field_Hosted_Provider(Hardware,Transact);
 use type AML_Identity.Identity;
 function Ready(IO:P.Context) return Boolean is (P.Identity(IO)/=AML_Identity.No_Identity and then not P.Pending(IO));
 function Count(IO:P.Context) return Natural is pragma Unreferenced(IO);begin return 8;end Count;
 procedure Export(IO:P.Context;Ordinal:Positive;Target:out A.Target;Accepted:out Boolean) is
 begin Accepted:=Ready(IO) and Ordinal<=8;Target:=A.No_Target;
 if Accepted then Target:=(P.Identity(IO),7,1,A.Register_ID(Ordinal));end if;end Export;
 procedure Select_Target(IO:in out P.Context;Geometry:AML_Field_Layout.Register_Geometry;For_Write:Boolean;Target:out A.Target;Accepted:out Boolean) is
 Candidate:A.Target;Have,Allowed:Boolean;Found:AML_Field_Layout.Register_Geometry;
 begin Target:=A.No_Target;Accepted:=False;for I in 1..8 loop Export(IO,I,Candidate,Have);
 if Have then P.Describe(IO,Candidate,Found,Allowed);
 if Allowed and then AML_Register_Views.Same_Transaction(Found,Geometry) then
 P.Authorize(IO,Candidate,For_Write,Accepted);if Accepted then Target:=Candidate;end if;return;end if;end if;end loop;end Select_Target;
 package V is new ACPI_Field_Provider_View(P.Context,Ready,P.Identity,P.Pending,Count,Export,Select_Target,P.Describe,P.Authorize,P.Dispatch,P.Reconciled);
 Fail_Allocation:Boolean:=False;Reserve_Calls:Natural:=0;
 function Reserve(Bytes,Alignment:Unsigned_64) return System.Address is
 begin Reserve_Calls:=Reserve_Calls+1;if Fail_Allocation then return System.Null_Address;end if;
 return ACPI_Fallible_Heap.Reserve(Bytes,Alignment);end Reserve;
 package E is new ACPI_Field_Evaluation(C,V,Reserve_Cache=>Reserve);
 use type E.Context_Access,E.Allocation_Outcome;
 Heap_Run:E.Context_Access;Allocation:E.Allocation_Report;
 package B renames E.Bindings;
 use type B.Outcome;
 package F is new C.Field_IO(P.Context,P.Pending,P.Describe,P.Dispatch,P.Reconciled,Authorize_Target=>P.Authorize);
 S:aliased C.State(1,4096,4096,1,Value_Object_Capacity=>AML_Objects.Max_Objects,Value_Byte_Capacity=>AML_Objects.Max_Bytes,Value_Element_Capacity=>AML_Objects.Max_Elements);
 Provider:P.Context;
 -- Capacity exceeds the generic default, but is a runtime caller input.
 Capacity:constant Positive:=Positive'Value(Ada.Command_Line.Argument(2));
 Items:F.Binding_Set(Capacity);Small:F.Binding_Set(32);
 File:IO.File_Type;Raw:Ada.Streams.Stream_Element_Array(1..4096);Last:Ada.Streams.Stream_Element_Offset;
 Data:Firmware_Tables.Bytes(1..4096);Staged:C.Stage_Status;Frozen:C.Freeze_Status;Report:C.Execution_Report;
 Members:C.Namespace.Initialization_Report;Status:C.Values.Access_Status;Found:C.Namespace.Lookup_Result;
 Desc:C.Values.Field_Description;Key:C.Values.Field_Key;Node:C.Node_Descriptor;Value:C.Values.Result;
 Targets:AML_Field_Targets.Vector;OK:Boolean;Checks:Natural:=0;
 Config:ACPI_Region_Policy.Configuration;
 procedure Check(Good:Boolean;Why:String) is begin Checks:=Checks+1;if not Good then raise Program_Error with Why;end if;end Check;
 function Name(I:Natural) return String is
 Hex:constant String:="0123456789ABCDEF";
 begin return "F0"&Hex(I/16+1)&Hex(I mod 16+1);end Name;
 begin
 IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));IO.Read(File,Raw,Last);IO.Close(File);
 for I in 1..Natural(Last) loop Data(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
 C.Stage_Table(S,1,C.DSDT,Data(1..Natural(Last)),Staged);Check(Staged=C.Staged,"stage");C.Freeze_Catalog(S,Frozen);Check(Frozen=C.Catalog_Frozen_OK,"freeze");
 C.Execute_Frozen(S,10000,Report,Status);Check(Status=C.Values.Available,"modules");C.Initialize_Members(S,Members,Status);Check(Status=C.Values.Available,"members");
 for Cell in 0..7 loop
 Config:=(Tag=>17,Base=>16#1000#,Length=>8,Space=>ACPI_Region_Policy.Memory_Space,Readable=>True,Writable=>False,
 Widths=>[T.Byte_Access=>True,others=>False],Register_Authority=>(Unsigned_64(Cell+1),True,False,False,0),Register_Offset=>Unsigned_64(Cell),Register_Width=>T.Byte_Access);
 P.Install(Provider,7,Config,True,OK);Check(OK,"named register installed");end loop;
 if Ada.Command_Line.Argument_Count=3 then
   declare Need:Positive;Counted,Prepared:Boolean;begin
      B.Required_Capacity(S,Provider,Need,Counted);Check(Counted and Need=72,"nine input regions times eight provisioned targets");
      E.Allocate(S,Provider,128,64*1024*1024,Heap_Run,Allocation);
      Check(Allocation.Status=E.Allocated and Heap_Run/=null and Allocation.Capacity=72,"bank-region metadata uses counted owner size");
      E.Prepare(Heap_Run.all,S,Provider,Prepared);
      Check(Prepared and E.Observe(Heap_Run.all).Region_Entries=72,"real builder publishes region-register entries beyond32");
      Check(P.Observe_Hardware(Provider).Reads=0,"bank metadata setup performs no register IO");
      declare Released:Boolean;begin C.Release_Storage(S,Released);Check(Released,"bank owner core released");end;
      Ada.Text_IO.Put_Line("Runtime binding capacity checks"&Checks'Image);return;
   end;
 end if;
 declare
   Needed:Positive;Counted,Prepared:Boolean;Run:E.Context(Capacity);
   Borrow_Run:B.Context(Capacity);Other:P.Context;Built:B.Report;
   Before:Natural:=P.Observe_Hardware(Provider).Reads;
   Outer_Calls,Inner_Calls:Natural:=0;Nested_Accepted:Boolean:=True;
   procedure Inner(Items:B.Fields.Binding_Set) is pragma Unreferenced(Items);begin Inner_Calls:=Inner_Calls+1;end Inner;
   procedure Nested is new B.With_Bindings(Inner);
   procedure Outer(Items:B.Fields.Binding_Set) is
   begin Check(Items.Capacity=Capacity,"borrowed exact storage extent");Outer_Calls:=Outer_Calls+1;
      Nested(Borrow_Run,S,Provider,Nested_Accepted);end Outer;
   procedure Borrow is new B.With_Bindings(Outer);
   Small_Output:B.Fields.Binding_Set(1);
 begin
   B.Required_Capacity(S,Provider,Needed,Counted);Check(Counted and Needed=64,"capacity derives from loaded namespace fields");
   Fail_Allocation:=True;
   E.Allocate(S,Provider,128,64*1024*1024,Heap_Run,Allocation);
   Check(Heap_Run=null and Allocation.Status=E.Allocation_Refused and Reserve_Calls=1,"real allocator refusal returns typed outcome");
   Fail_Allocation:=False;
   E.Allocate(S,Provider,32,64*1024*1024,Heap_Run,Allocation);
   Check(Heap_Run=null and Allocation.Status=E.Capacity_Exceeded and Reserve_Calls=1,"capacity ceiling refuses before reservation");
   E.Allocate(S,Provider,128,1,Heap_Run,Allocation);
   Check(Heap_Run=null and Allocation.Status=E.Capacity_Exceeded and Reserve_Calls=1,"byte budget refuses before reservation");
   E.Allocate(S,Provider,128,64*1024*1024,Heap_Run,Allocation);
   Check(Heap_Run/=null and Allocation.Status=E.Allocated and Allocation.Capacity=Needed and Heap_Run.Capacity=Needed
      and Allocation.Bytes=Allocation.Allocated_Bytes and Allocation.Bytes>0 and Reserve_Calls=2,"real input-sized owner allocation");
   E.Prepare(Heap_Run.all,S,Provider,Prepared);Check(Prepared and E.Observe(Heap_Run.all).Read_Fields=64,"allocated owner prepares all64 bindings");
   C.Find_Object(S,C.Namespace.Root,"F03F",Found,Status);C.Describe_Node(S,Found.Node,Node,Status);
   E.Evaluate_Named(Heap_Run.all,S,Provider,Node,1000,Value,Status);
   Check(Status=C.Values.Available and Value.Status=AML_Execute.Returned and Value.Number=1,"allocated native-style owner reads binding64");
   -- The remaining checks use separate caller-constrained cache instances.
   -- Keep the allocated owner reachable until this test process exits.
   Before:=P.Observe_Hardware(Provider).Reads;
   E.Prepare(Run,S,Provider,Prepared);Check(Prepared=(Capacity>=Needed),"real input-sized cache preparation boundary");
   Check(P.Observe_Hardware(Provider).Reads=Before,"metadata build performs no data IO");
   if Capacity>=Needed then
      Check(E.Observe(Run).Read_Fields=64,"full cache publishes all64 fields");
      for I in 0..63 loop
         C.Find_Object(S,C.Namespace.Root,Name(I),Found,Status);C.Describe_Node(S,Found.Node,Node,Status);
         E.Evaluate_Named(Run,S,Provider,Node,1000,Value,Status);
         Check(Status=C.Values.Available and Value.Status=AML_Execute.Returned and Value.Number=Shift_Right(Unsigned_64(16#A5#),I mod 8) mod 2,"real cached borrowed evaluation beyond32");
      end loop;
      B.Build(Borrow_Run,S,Provider,Built);Check(Built.Status=B.Built,"second independent metadata context built");
      Borrow(Borrow_Run,S,Provider,OK);Check(OK and Outer_Calls=1 and Inner_Calls=0 and not Nested_Accepted,"same-cache reentry refused before action");
      Borrow(Borrow_Run,S,Other,OK);Check(not OK and Outer_Calls=1,"foreign provider cannot borrow cache");
      B.Copy_Bindings(Borrow_Run,S,Provider,Small_Output,OK);Check(not OK,"mismatched extent refuses without discriminant exception");
   else
      Check(E.Observe(Run).Status=B.Binding_Refused,"insufficient explicit extent has typed binding refusal");
      E.Prepare(Run,S,Provider,Prepared);Check(not Prepared and P.Observe_Hardware(Provider).Reads=Before,"failed builder does not retry or publish partial set");
   end if;
 end;
 F.Clear(Items);Check(Items.Capacity=Capacity,"clear preserves runtime extent");
 for I in 0..63 loop
 C.Find_Object(S,C.Namespace.Root,Name(I),Found,Status);Check(Status=C.Values.Available and Found.Status=C.Namespace.Found,"field lookup");
 C.Read_Field_Descriptor(S,Found.Node,Desc,Key,Status);Check(Status=C.Values.Available,"field key");
 Targets:=[others=>A.No_Target];Targets(1):=(P.Identity(Provider),7,1,A.Register_ID(I/8+1));
 F.Add_Read_Binding(S,Provider,Key,Targets,1,Items,Status);
 Check((Status=C.Values.Available)=(I<Capacity),"actual runtime capacity boundary");
 F.Add_Read_Binding(S,Provider,Key,Targets,1,Small,Status);Check((Status=C.Values.Available)=(I<32),"explicit small capacity still refuses entry33");
 if I<Capacity then C.Describe_Node(S,Found.Node,Node,Status);Check(Status=C.Values.Available,"current descriptor");
 F.Evaluate_Named(S,Provider,Items,Node,1000,Value,Status);
 Check(Status=C.Values.Available and Value.Status=AML_Execute.Returned and Value.Number=Shift_Right(Unsigned_64(16#A5#),I mod 8) mod 2,"every binding including33..64 reads through real evaluator");end if;
 end loop;
 F.Clear(Items);Check(Items.Capacity=Capacity,"clear after populated set preserves extent");
 declare Released:Boolean;begin C.Release_Storage(S,Released);Check(Released,"owner released");end;
 Ada.Text_IO.Put_Line("Runtime binding capacity checks"&Checks'Image);
end Capacity_Tests;
