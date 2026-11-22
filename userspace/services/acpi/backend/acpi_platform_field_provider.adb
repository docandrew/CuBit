pragma Ada_2022;
with AML_Identity.Issuer;
with AML_Register_Views;
with ACPI_Firmware_Data_Identities;
with AML_Decode;
package body ACPI_Platform_Field_Provider is
   use type Maps.Word, Maps.Outcome, Maps.Data_Status;
   use type AML_Identity.Identity, A.Endpoint_ID, A.Register_ID;
   use type L.Register_Geometry, Store.Outcome;
   use type AML_Decode.Byte;
   function Identity (IO : Context) return AML_Identity.Identity is (IO.Token);
   function Pending (IO : Context) return Boolean is
     (IO.Working or else IO.Captured.Present or else Maps.Pending (Memory));
   function Is_Ready (IO : Context) return Boolean is
     (IO.Stage = Ready and then not Pending (IO) and then Maps.Admitting (Memory));
   function Register_Count (IO : Context) return Natural is
     (if Is_Ready (IO) then IO.Ready_Count else 0);
   function Capacity (IO : Context) return Natural is (Store.Capacity (IO.Rows));
   function Uncertain (IO : Context) return A.Observation is (IO.Captured);
   function Completed_Fault (IO : Context) return A.Observation is (IO.Fault);
   function Target_At (IO : Context; Index : Positive) return A.Target is
     ((IO.Token,IO.Symbol,IO.Epoch,A.Register_ID (Index)));
   function Index_Of (IO : Context; Target : A.Target) return Natural is
   begin
      if not Is_Ready (IO) or else Target.Provider /= IO.Token or else Target.Endpoint /= IO.Symbol
        or else Target.Epoch /= IO.Epoch or else Target.Register = A.No_Register
        or else A.Counter (Target.Register) > A.Counter (Store.Length (IO.Rows)) then return 0; end if;
      declare Index : constant Positive := Positive (Target.Register); begin
         return (if Store.Read (IO.Rows,Index).Available then Index else 0);
      end;
   end Index_Of;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean) is
      Epoch : ACPI_Firmware_Data_Identities.F.Identifier;
   begin
      Accepted := False;
      if IO.Stage /= Fresh then return; end if;
      IO.Stage := Failed;
      if Symbol = A.No_Endpoint or else not Maps.Admitting (Memory) then return; end if;
      AML_Identity.Issuer.Issue (IO.Token,Accepted);
      if not Accepted then return; end if;
      ACPI_Firmware_Data_Identities.Issue (Epoch,Accepted);
      if not Accepted then return; end if;
      IO.Symbol := Symbol; IO.Epoch := A.Counter (Epoch); IO.Stage := Ready;
   end Initialize;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean) is
      Seen : Natural := 0;
   begin
      Target := A.No_Target; Accepted := False;
      if not Is_Ready (IO) or else Ordinal > IO.Ready_Count then return; end if;
      for I in 1 .. Store.Length (IO.Rows) loop
         if Store.Read (IO.Rows,I).Available then
            Seen := Seen + 1;
            if Seen = Ordinal then Target := Target_At (IO,I); Accepted := True; return; end if;
         end if;
      end loop;
   end Export_Target;
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean) is
      pragma Unreferenced (For_Write);
   begin
      Target := A.No_Target; Accepted := False;
      if not Is_Ready (IO) then return; end if;
      for I in 1 .. Store.Length (IO.Rows) loop
         declare Row : constant Association := Store.Read (IO.Rows,I); begin
            if Row.Available and then Row.Geometry = Geometry then
               Target := Target_At (IO,I); Accepted := True; return;
            end if;
         end;
      end loop;
   end Select_Target;
   procedure Resolve (IO : in out Context; Item : Declaration;
      Geometry : L.Register_Geometry; Deadline_Microseconds : Maps.Word;
      Target : out A.Target; Accepted : out Boolean)
   is
      Origin : Maps.Source;
      Validated : Boolean;
      Row : Association := (Geometry => Geometry,others => <>);
      Index : Natural;
      Stored : Store.Outcome;
      Result : Maps.Map_Result;
   begin
      Target := A.No_Target; Accepted := False;
      if not Is_Ready (IO) or else Geometry.Region.Space /= L.Memory_Space
        or else Deadline_Microseconds in 0 | Maps.No_Finite_Deadline
        or else not AML_Register_Views.Valid_Register (Geometry)
        or else not Maps.Layouts.Valid ((Geometry.Region.Base,Geometry.Region.Length)) then return; end if;
      IO.Working := True;
      Validate_Declaration (Item,Geometry,Origin,Validated);
      if not Validated then IO.Working := False; return; end if;
      declare Envelope : constant Maps.Layouts.P.Envelope := Maps.Layouts.Envelope
         ((Geometry.Region.Base,Geometry.Region.Length)); begin
         if not Compatible_Envelope (Envelope.First_Page,Envelope.Pages) then IO.Working := False; return; end if;
      end;
      for I in 1 .. Store.Length (IO.Rows) loop
         Row := Store.Read (IO.Rows,I);
         if Row.Geometry = Geometry then
            Accepted := Row.Available;
            if Accepted then Target := Target_At (IO,I); end if;
            IO.Working := False; return;
         end if;
      end loop;
      Row := (Geometry => Geometry,others => <>);
      Store.Append (IO.Rows,Row,Index,Stored);
      if Stored /= Store.Stored then IO.Working := False; return; end if;
      Maps.Map_Pool_Region_For_AML (Memory,Origin,(Geometry.Region.Base,Geometry.Region.Length),Deadline_Microseconds,Result);
      if Result.Status in Maps.Mapped | Maps.Reused then
         Row.Region := Result.Region; Row.Available := True;
         Store.Replace (IO.Rows,Index,Row); IO.Ready_Count := IO.Ready_Count + 1;
         Target := Target_At (IO,Index); Accepted := True;
      end if;
      IO.Working := False;
   end Resolve;
   procedure Describe (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean) is
      Index : constant Natural := Index_Of (IO,Target);
   begin
      Geometry := (others => <>); Accepted := Index /= 0;
      if Accepted then Geometry := Store.Read (IO.Rows,Index).Geometry; end if;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
      pragma Unreferenced (For_Write);
   begin Accepted := Index_Of (IO,Target) /= 0; end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Index : constant Natural := Index_Of (IO,Item.Destination);
      Result : Maps.Data_Result;
      Row : Association;
   begin
      Reply := (others => <>); if Index = 0 then return; end if;
      Row := Store.Read (IO.Rows,Index); IO.Working := True;
      Maps.Access_Region (Memory,Row.Region,Row.Geometry.Byte_Offset,Row.Geometry.Width,Item.For_Write,Item.Value,Result);
      case Result.Status is
         when Maps.Denied => null;
         when Maps.Completed => Reply := (A.Completed,IO.Token,Result.Value,A.No_Operation);
         when Maps.Failed_Completed =>
            IO.Fault := (True,IO.Token,Item,A.No_Operation); IO.Stage := Failed;
            Reply := (A.Failed_Completed,IO.Token,0,A.No_Operation);
         when Maps.Indeterminate =>
            IO.Captured := (True,IO.Token,Item,A.Operation_ID (Result.Operation)); IO.Stage := Failed;
            Reply := (A.Indeterminate,IO.Token,0,A.Operation_ID (Result.Operation));
      end case;
      IO.Working := False;
   end Dispatch;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO,Expected);
   begin Accepted := False; end Reconciled;
   procedure Close (IO : in out Context) is
   begin if not IO.Working then IO.Stage := Closed; end if; end Close;
end ACPI_Platform_Field_Provider;
