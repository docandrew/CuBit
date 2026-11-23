pragma Ada_2022;
with ACPI_FADT.Transactions;
with AML_Identity.Issuer;

package body ACPI_Local_CMOS_Provider with SPARK_Mode is
   use type A.Endpoint_ID, A.Register_ID, A.Counter, AML_Identity.Identity;
   use type I.Byte, I.Decision_Kind, T.Phase, T.Initialization;
   use type T.Start_Result, T.Finish_Result, T.Completion;
   use type ACPI_FADT.Transactions.Access_Width;
   Provider_Epoch : constant A.Counter := 1;

   function Pending (IO : Context) return Boolean is
     (IO.Working or else IO.Captured.Present
      or else T.Current (IO.Owner) in T.In_Flight | T.Uncertain);
   function Is_Ready (IO : Context) return Boolean is
     (IO.Stage = Ready and then T.Valid (IO.Owner)
      and then not Pending (IO) and then T.Current (IO.Owner) = T.Ready);
   function Identity (IO : Context) return AML_Identity.Identity is (IO.Issuer);
   function Register_Count (IO : Context) return Natural is (if Is_Ready (IO) then IO.Count else 0);
   function Uncertain (IO : Context) return A.Observation is (IO.Captured);
   function Completed_Fault (IO : Context) return A.Observation is (IO.Fault);
   function Target_At (IO : Context; Cell : I.Byte) return A.Target is
     ((IO.Issuer, IO.Symbol, Provider_Epoch, A.Register_ID (Cell) + 1));
   function Matches (IO : Context; Item : A.Target) return Boolean is
     (Is_Ready (IO) and then IO.Issuer /= AML_Identity.No_Identity
      and then Item.Provider = IO.Issuer and then Item.Endpoint = IO.Symbol
      and then Item.Epoch = Provider_Epoch
      and then Item.Register in 1 .. A.Register_ID (IO.Last_Cell) + 1
      and then IO.Used (I.Byte (Item.Register - 1)));
   function Geometry_At (IO : Context; Cell : I.Byte) return L.Register_Geometry is
     ((Region => (Space => CMOS_Space, Base => 0, Length => A.Counter (IO.Last_Cell) + 1,
         PCI => (Known => False)),
       Byte_Offset => A.Counter (Cell), Width => ACPI_FADT.Transactions.Byte_Access));

   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean) is
      Profile : P.Profile;
      Century : I.Byte;
      Known, NMI, Standard, Extended : Boolean;
      Initialized : T.Initialization;
      Stopped : Boolean;
   begin
      Accepted := False;
      if IO.Stage /= Fresh or else not T.Valid (IO.Owner) then return; end if;
      IO.Stage := Failed;
      if Symbol = A.No_Endpoint then return; end if;
      Read_Configuration (Profile, Century, Known, NMI, Standard, Extended);
      T.Initialize (IO.Owner, Profile, Century, Known, NMI, Standard, Extended, Initialized);
      if Initialized /= T.Initialized or else not P.Supported (Profile) then return; end if;
      AML_Identity.Issuer.Issue (IO.Issuer, Accepted);
      if not Accepted then T.Stop (IO.Owner, Stopped); return; end if;
      IO.Last_Cell := P.Last_Cell (Profile);
      IO.Century := Century;
      IO.Symbol := Symbol;
      IO.Stage := Ready;
   end Initialize;

   procedure Export_Target (IO : Context; Ordinal : Positive;
      Target : out A.Target; Accepted : out Boolean)
   is
      Seen : Natural range 0 .. 256 := 0;
   begin
      Target := A.No_Target; Accepted := False;
      if not Is_Ready (IO) or else Ordinal > IO.Count then return; end if;
      for Cell in I.Byte loop
         if IO.Used (Cell) then
            if Seen = 256 then return; end if;
            Seen := Seen + 1;
            if Seen = Ordinal then Target := Target_At (IO, Cell); Accepted := True; return; end if;
         end if;
      end loop;
   end Export_Target;

   procedure Describe (IO : Context; Target : A.Target;
      Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (others => <>); Accepted := Matches (IO, Target);
      if Accepted then Geometry := Geometry_At (IO, I.Byte (Target.Register - 1)); end if;
   end Describe;

   procedure Authorize (IO : Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
   begin
      Accepted := Matches (IO, Target)
        and then (not For_Write or else not I.Read_Only (I.Byte (Target.Register - 1), IO.Century))
        and then Access_Allowed (For_Write);
   end Authorize;

   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean) is
   begin
      Target := A.No_Target; Accepted := False;
      if not Is_Ready (IO) or else Geometry.Region.Space /= CMOS_Space
        or else Geometry.Region.PCI.Known
        or else Geometry.Width /= ACPI_FADT.Transactions.Byte_Access
        or else not Access_Allowed (For_Write)
      then return; end if;
      declare
         Decision : constant I.Decision := I.Resolve
           (Geometry.Region.Base, Geometry.Region.Length, Geometry.Byte_Offset,
            IO.Last_Cell, IO.Century, For_Write);
      begin
         if Decision.Kind /= I.Accepted then return; end if;
         if not IO.Used (Decision.Cell) then
            if IO.Count = 256 then return; end if;
            IO.Used (Decision.Cell) := True; IO.Count := IO.Count + 1;
         end if;
         Target := Target_At (IO, Decision.Cell); Accepted := True;
      end;
   end Select_Target;

   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Allowed : Boolean;
      Plan : T.Transfer;
      Started : T.Start_Result;
      Finished : T.Finish_Result;
      Value : I.Byte := 0;
      Outcome : T.Completion := T.Unknown;
   begin
      Reply := (others => <>);
      Authorize (IO, Item.Destination, Item.For_Write, Allowed);
      if not Allowed or else (not Item.For_Write and then Item.Value /= 0)
        or else Item.Value > A.Counter (I.Byte'Last)
      then return; end if;
      IO.Working := True;
      T.Begin_Access (IO.Owner, A.Counter (Item.Destination.Register - 1), 1, 0,
        Item.For_Write, I.Byte (Item.Value), Plan, Started);
      if Started /= T.Started then IO.Working := False; return; end if;
      Transfer_Byte (Plan, Value, Outcome);
      T.Finish_Access (IO.Owner, Plan.Ticket, Outcome, Finished);
      if Finished /= T.Finished or else Outcome = T.Unknown then
         IO.Captured := (True, IO.Issuer, Item, A.Operation_ID (Plan.Ticket));
         Reply := (A.Indeterminate, IO.Issuer, 0, A.Operation_ID (Plan.Ticket));
      elsif Outcome = T.Completed then
         Reply := (A.Completed, IO.Issuer, A.Counter (Value), A.No_Operation);
      else
         IO.Fault := (True, IO.Issuer, Item, A.No_Operation);
         Reply := (Status => A.Failed_Completed, Provider => IO.Issuer, others => <>);
      end if;
      IO.Working := False;
   end Dispatch;

   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO, Expected);
   begin Accepted := False; end Reconciled;
   procedure Close (IO : in out Context) is
      Accepted : Boolean;
   begin
      if IO.Working or else not T.Valid (IO.Owner) then return; end if;
      T.Stop (IO.Owner, Accepted);
      if Accepted then IO.Stage := Closed; end if;
   end Close;
end ACPI_Local_CMOS_Provider;
