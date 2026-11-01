pragma Ada_2022;
with AML_Identity.Issuer;
with AML_Register_Views;
package body ACPI_Field_Combined_Provider is
   use type A.Counter, A.Endpoint_ID, A.Target, A.Completion, AML_Identity.Identity;
   use type L.Register_Geometry, Scalar.Startup_State;
   Epoch : constant A.Counter := 1;
   function Status (IO : Context) return Startup_State is (IO.Stage);
   function Identity (IO : Context) return AML_Identity.Identity is (IO.Token);
   function Pending (IO : Context) return Boolean is
     (IO.Working or else IO.Captured.Present or else Scalar.Pending (IO.Registers) or else Memory.Pending (IO.NVS));
   function Uncertain (IO : Context) return A.Observation is (IO.Captured);
   function Completed_Fault (IO : Context) return A.Observation is (IO.Fault);
   function Live (IO : Context) return Boolean is
     (IO.Stage = Ready and then not Pending (IO) and then Scalar.Status (IO.Registers) = Scalar.Ready);
   function At_Index (IO : Context; Index : Target_Index) return A.Target is
     ((IO.Token, IO.Symbol, Epoch, A.Register_ID (Index)));
   function Matches (IO : Context; Target : A.Target) return Boolean is
     (Target.Provider = IO.Token and then IO.Token /= AML_Identity.No_Identity
      and then Target.Endpoint = IO.Symbol and then Target.Epoch = Epoch
      and then Target.Register in 1 .. A.Register_ID (IO.Count));
   procedure Add
     (IO : in out Context; Path : Lane; Inner : A.Target; Geometry : L.Register_Geometry;
      Target : out A.Target; Accepted : out Boolean) is
   begin
      Target := A.No_Target; Accepted := False;
      for I in 1 .. IO.Count loop
         if IO.Rows (I).Path = Path and then IO.Rows (I).Inner = Inner then
            if IO.Rows (I).Geometry /= Geometry then return; end if;
            Target := At_Index (IO, I); Accepted := True; return;
         end if;
      end loop;
      if Inner = A.No_Target or else IO.Count = Maximum_Targets then return; end if;
      IO.Count := IO.Count + 1; IO.Rows (IO.Count) := (Path, Inner, Geometry);
      Target := At_Index (IO, IO.Count); Accepted := True;
   end Add;
   procedure Initialize
     (IO : in out Context; Destination : Peer; Symbol : A.Endpoint_ID;
      Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant; Accepted : out Boolean) is
      Good : Boolean;
      Inner, Ignored : A.Target;
      Geometry : L.Register_Geometry;
   begin
      Accepted := False; if IO.Stage /= Fresh then return; end if;
      IO.Stage := Initializing; IO.Working := True;
      Scalar.Initialize (IO.Registers, Destination, Symbol, Deadline, Good);
      if not Good then IO.Stage := Failed; IO.Working := False; return; end if;
      Memory.Initialize (IO.NVS, Destination, Symbol, Deadline, Good);
      if not Good then IO.Stage := Failed; IO.Working := False; return; end if;
      AML_Identity.Issuer.Issue (IO.Token, Good);
      if not Good then IO.Stage := Failed; IO.Working := False; return; end if;
      IO.Symbol := Symbol;
      for I in 1 .. Scalar.Register_Count (IO.Registers) loop
         Scalar.Export_Target (IO.Registers, I, Inner, Good);
         if not Good then IO.Stage := Failed; IO.Working := False; return; end if;
         Scalar.Describe (IO.Registers, Inner, Geometry, Good);
         if not Good then IO.Stage := Failed; IO.Working := False; return; end if;
         Add (IO, Named_Register, Inner, Geometry, Ignored, Good);
         if not Good then IO.Stage := Failed; IO.Working := False; return; end if;
      end loop;
      IO.Working := False; IO.Stage := Ready; Accepted := True;
   end Initialize;
   function Register_Count (IO : Context) return Natural is (if Live (IO) then IO.Count else 0);
   procedure Export_Target
     (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean) is
   begin
      Target := A.No_Target; Accepted := Live (IO) and then Ordinal <= IO.Count;
      if Accepted then Target := At_Index (IO, Ordinal); end if;
   end Export_Target;
   type Named_Match is (Absent, Unique, Ambiguous, Unavailable);
   procedure Match_Named
     (IO : Context; Geometry : L.Register_Geometry; Inner : out A.Target;
      Canonical : out L.Register_Geometry; Match : out Named_Match) is
      Candidate : A.Target;
      Description : L.Register_Geometry;
      Good : Boolean;
   begin
      Inner := A.No_Target; Canonical := (others => <>); Match := Absent;
      for I in 1 .. Scalar.Register_Count (IO.Registers) loop
         Scalar.Export_Target (IO.Registers, I, Candidate, Good);
         if not Good then Match := Unavailable; return; end if;
         Scalar.Describe (IO.Registers, Candidate, Description, Good);
         if not Good then Match := Unavailable; return; end if;
         if AML_Register_Views.Same_Transaction (Description, Geometry) then
            if Match = Unique then Inner := A.No_Target; Match := Ambiguous; return; end if;
            Inner := Candidate; Canonical := Description; Match := Unique;
         end if;
      end loop;
   end Match_Named;
   procedure Select_Target
     (IO : in out Context; Geometry : L.Register_Geometry; For_Write : Boolean;
      Target : out A.Target; Accepted : out Boolean) is
      Inner : A.Target;
      Canonical : L.Register_Geometry;
      Match : Named_Match;
      Good : Boolean;
   begin
      Target := A.No_Target; Accepted := False; if not Live (IO) then return; end if;
      Match_Named (IO, Geometry, Inner, Canonical, Match);
      if Match = Unique then
         Scalar.Authorize (IO.Registers, Inner, For_Write, Good);
         if Good then Add (IO, Named_Register, Inner, Canonical, Target, Accepted); end if;
      elsif Match = Absent and then not For_Write then
         Memory.Install (IO.NVS, Geometry, Inner, Good);
         if Good then
            Memory.Describe (IO.NVS, Inner, Canonical, Good);
            if Good then Add (IO, NVS_Data, Inner, Canonical, Target, Accepted); end if;
         end if;
      end if;
   end Select_Target;
   procedure Resolve_Module_Target
     (IO : in out Context; Geometry : L.Register_Geometry;
      Target : in out A.Target; Accepted : in out Boolean) is
      Inner : A.Target;
      Canonical : L.Register_Geometry;
      Match : Named_Match;
      Good : Boolean;
   begin
      Target := A.No_Target; Accepted := False; if not Live (IO) then return; end if;
      Match_Named (IO, Geometry, Inner, Canonical, Match);
      if Match = Unique then
         Add (IO, Named_Register, Inner, Canonical, Target, Accepted);
      elsif Match = Absent then
         Memory.Install (IO.NVS, Geometry, Inner, Good);
         if Good then
            Memory.Describe (IO.NVS, Inner, Canonical, Good);
            if Good then Add (IO, NVS_Data, Inner, Canonical, Target, Accepted); end if;
         end if;
      end if;
   end Resolve_Module_Target;
   procedure Describe
     (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean) is
      Row : Association;
   begin
      Geometry := (others => <>); Accepted := False;
      if not Live (IO) or else not Matches (IO, Target) then return; end if;
      Row := IO.Rows (Target_Index (Target.Register));
      case Row.Path is
         when Named_Register => Scalar.Describe (IO.Registers, Row.Inner, Geometry, Accepted);
         when NVS_Data => Memory.Describe (IO.NVS, Row.Inner, Geometry, Accepted);
      end case;
      Accepted := Accepted and then Geometry = Row.Geometry;
      if not Accepted then Geometry := (others => <>); end if;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
      Row : Association;
   begin
      Accepted := False;
      if not Live (IO) or else not Matches (IO, Target) or else IO.Issued = A.Counter'Last then return; end if;
      Row := IO.Rows (Target_Index (Target.Register));
      case Row.Path is
         when Named_Register => Scalar.Authorize (IO.Registers, Row.Inner, For_Write, Accepted);
         when NVS_Data => Memory.Authorize (IO.NVS, Row.Inner, For_Write, Accepted);
      end case;
   end Authorize;
   procedure Describe_Scoped_Read
     (IO : Context; Target : A.Target;
      ID : out ACPI_Locked_NVS_Protocol.Record_ID;
      Datum : out ACPI_Locked_NVS_Protocol.Geometry; Accepted : out Boolean) is
      package D renames ACPI_Locked_NVS_Protocol;
   begin
      ID := D.No_Record; Datum := D.Empty_Geometry; Accepted := False;
      if not Live (IO) or else not Matches (IO, Target) then return; end if;
      declare Row : Association renames IO.Rows (Target_Index (Target.Register)); begin
         if Row.Path /= NVS_Data then return; end if;
         Memory.Describe_Scoped_Read (IO.NVS, Row.Inner, ID, Datum, Accepted);
      end;
   end Describe_Scoped_Read;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Row : Association;
      Lower : A.Response;
      Good : Boolean;
      Window_Start, Before, After, Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant;
      use type ACPI_Inventory_Protocol.Wire.Time_Instant;
   begin
      Reply := (others => <>); Authorize (IO, Item.Destination, Item.For_Write, Good);
      if not Good or else (not Item.For_Write and then Item.Value /= 0) then return; end if;
      Row := IO.Rows (Target_Index (Item.Destination.Register));
      IO.Working := True; IO.Issued := IO.Issued + 1;
      Read_Window (Window_Start, Deadline); Read_Clock (Before, Good);
      if not Good or else Deadline = ACPI_Inventory_Protocol.Wire.No_Finite_Deadline
        or else Before < Window_Start or else Before >= Deadline
      then IO.Working := False; IO.Stage := Failed; return; end if;
      case Row.Path is
         when Named_Register => Scalar.Dispatch (IO.Registers, (Row.Inner, Item.For_Write, Item.Value), Lower);
         when NVS_Data => Memory.Dispatch (IO.NVS, (Row.Inner, Item.For_Write, Item.Value), Lower);
      end case;
      Read_Clock (After, Good);
      IO.Working := False;
      if Lower.Status = A.Completed and then
        (not Good or else After < Before or else After >= Deadline)
      then Lower.Status := A.Failed_Completed; end if;
      if Lower.Status /= A.Denied and then Lower.Provider /= Row.Inner.Provider then Lower.Status := A.Indeterminate; end if;
      case Lower.Status is
         when A.Completed => Reply := (A.Completed, IO.Token, Lower.Value, A.No_Operation);
         when A.Failed_Completed =>
            IO.Stage := Failed; IO.Fault := (True, IO.Token, Item, A.No_Operation);
            Reply := (A.Failed_Completed, IO.Token, 0, A.No_Operation);
         when A.Indeterminate =>
            IO.Stage := Failed; IO.Captured := (True, IO.Token, Item, A.Operation_ID (IO.Issued));
            Reply := (A.Indeterminate, IO.Token, 0, IO.Captured.Operation);
         when A.Denied => IO.Stage := Failed;
      end case;
   end Dispatch;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO, Expected);
   begin Accepted := False; end Reconciled;
end ACPI_Field_Combined_Provider;
