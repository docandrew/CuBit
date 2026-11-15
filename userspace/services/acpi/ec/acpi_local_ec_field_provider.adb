pragma Ada_2022;
with AML_Identity.Issuer;
with AML_Register_Views;
with AML_Decode;
with ACPI_FADT.Transactions;
package body ACPI_Local_EC_Field_Provider with SPARK_Mode is
   use type A.Endpoint_ID,A.Register_ID,A.Counter,AML_Identity.Identity;
   use type AML_Decode.Byte,T.Microseconds,T.Work_Budget,T.Outcome,Commands.Phase,Commands.Start_Result;
   Provider_Epoch : constant A.Counter := 1;
   function Pending (IO : Context) return Boolean is
     (IO.Working or else IO.Captured.Present or else not Commands.Quiescent);
   function Is_Ready (IO : Context) return Boolean is
     (IO.Stage=Ready and then not Pending(IO) and then Commands.Observe.Stage=Commands.Ready);
   function Identity (IO : Context) return AML_Identity.Identity is (IO.Issuer);
   function Register_Count (IO : Context) return Natural is (if Is_Ready(IO) then IO.Count else 0);
   function Uncertain (IO : Context) return A.Observation is (IO.Captured);
   function Completed_Fault (IO : Context) return A.Observation is (IO.Fault);
   function Target_At (IO : Context; Cell : T.Cell_Offset) return A.Target is
     ((IO.Issuer,IO.Symbol,Provider_Epoch,A.Register_ID(Cell)+1));
   function Matches (IO : Context; Item : A.Target) return Boolean is
     (Is_Ready(IO) and then IO.Issuer/=AML_Identity.No_Identity and then Item.Provider=IO.Issuer
      and then Item.Endpoint=IO.Symbol and then Item.Epoch=Provider_Epoch
      and then Item.Register in 1 .. A.Register_ID(T.Cell_Offset'Last)+1
      and then IO.Used(T.Cell_Offset(Item.Register-1)));
   function Geometry_At (Cell : T.Cell_Offset) return L.Register_Geometry is
     ((Region=>(Space=>L.Embedded_Control_Space,Base=>0,Length=>L.EC_Last_Cell+1,PCI=>(Known=>False)),
       Byte_Offset=>A.Counter(Cell),Width=>ACPI_FADT.Transactions.Byte_Access));
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean) is
   begin
      Accepted:=False;if IO.Stage/=Fresh then return;end if;IO.Stage:=Failed;
      if Symbol=A.No_Endpoint or else Work=0 or else not Commands.Quiescent then return;end if;
      AML_Identity.Issuer.Issue(IO.Issuer,Accepted);
      if Accepted then IO.Symbol:=Symbol;IO.Stage:=Ready;end if;
   end Initialize;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean) is
      Seen : Cell_Count := 0;
   begin
      Target:=A.No_Target;Accepted:=False;if not Is_Ready(IO) or else Ordinal>IO.Count then return;end if;
      for Cell in T.Cell_Offset loop
         if IO.Used(Cell) then Seen:=Seen+1;if Seen=Ordinal then Target:=Target_At(IO,Cell);Accepted:=True;return;end if;end if;
      end loop;
   end Export_Target;
   procedure Describe (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry:=(others=><>);Accepted:=Matches(IO,Target);
      if Accepted then Geometry:=Geometry_At(T.Cell_Offset(Target.Register-1));end if;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
   begin
      Accepted:=Matches(IO,Target) and then Cell_Allowed(T.Cell_Offset(Target.Register-1),For_Write);
   end Authorize;
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean) is
      Cell : T.Cell_Offset;
   begin
      Target:=A.No_Target;Accepted:=False;
      if not Is_Ready(IO) or else Geometry.Region.Space/=L.Embedded_Control_Space
        or else not AML_Register_Views.Valid_Register(Geometry) then return;end if;
      Cell:=T.Cell_Offset(AML_Register_Views.First_Address(Geometry));
      if not Cell_Allowed(Cell,For_Write) then return;end if;
      if not IO.Used(Cell) then IO.Used(Cell):=True;IO.Count:=IO.Count+1;end if;
      Target:=Target_At(IO,Cell);Accepted:=True;
   end Select_Target;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Allowed, Good, Stopped : Boolean;
      Until_Time : T.Microseconds;
      Began : Commands.Start_Result;
      Token : Commands.Token_ID;
      Done : Commands.Completion;
   begin
      Reply:=(others=><>);Authorize(IO,Item.Destination,Item.For_Write,Allowed);
      if not Allowed or else (not Item.For_Write and then Item.Value/=0) or else Item.Value>A.Counter(T.Byte'Last) then return;end if;
      IO.Working:=True;Deadline(Until_Time,Good);
      if not Good or else Until_Time=0 then IO.Working:=False;return;end if;
      Commands.Start((if Item.For_Write then T.Write_Cell else T.Read_Cell),
         T.Cell_Offset(Item.Destination.Register-1),T.Byte(Item.Value),Until_Time,Work,Began,Token);
      if Began/=Commands.Started then IO.Working:=False;return;end if;
      -- Each engine Step spends original Work or terminates for exhaustion;
      -- one extra step observes the exhausted budget. No callback can renew it.
      for Step in 1 .. Natural(Work)+1 loop
         exit when Commands.Observe.Stage/=Commands.Running;
         Commands.Advance;
         if Commands.Observe.Stage=Commands.Running and then Commands.Wake_At>0 then
            Pause(Commands.Wake_At,Good);
            if not Good then Commands.Request_Stop(Stopped);end if;
         end if;
      end loop;
      Commands.Collect(Done);
      if not Done.Present then
         IO.Captured:=(True,IO.Issuer,Item,A.Operation_ID(Token));
         Reply:=(A.Indeterminate,IO.Issuer,0,A.Operation_ID(Token));
      elsif Done.Recovery then
         IO.Captured:=(True,IO.Issuer,Item,A.Operation_ID(Done.Token));
         Reply:=(A.Indeterminate,IO.Issuer,0,A.Operation_ID(Done.Token));
      elsif Done.Hardware=T.Succeeded and then Done.Guard_Cleared and then not Done.Stopping then
         Reply:=(A.Completed,IO.Issuer,A.Counter(Done.Value),A.No_Operation);
      elsif Done.Effects_Possible then
         IO.Fault:=(True,IO.Issuer,Item,A.No_Operation);
         Reply:=(Status=>A.Failed_Completed,Provider=>IO.Issuer,others=><>);
      end if;
      IO.Working:=False;
   end Dispatch;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced(IO,Expected);
   begin Accepted:=False;end Reconciled;
   procedure Close (IO : in out Context) is
   begin if not IO.Working then IO.Stage:=Closed;end if;end Close;
end ACPI_Local_EC_Field_Provider;
