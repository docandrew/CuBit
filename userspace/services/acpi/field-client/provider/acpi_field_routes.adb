pragma Ada_2022;
with AML_Identity.Issuer;
with ACPI_FADT.Transactions;
package body ACPI_Field_Routes is
   use type A.Target, A.Counter, A.Register_ID, A.Endpoint_ID, A.Completion;
   use type AML_Identity.Identity, L.Register_Geometry, Store.Outcome;
   Epoch : constant A.Counter := 1;
   function Identity (Owner : Context) return AML_Identity.Identity is (Owner.Token);
   function Pending (Owner : Context) return Boolean is (Owner.Working or else Owner.Stopped);
   function Ready (Owner : Context) return Boolean is (Owner.Initialized and then not Pending (Owner));
   function Register_Count (Owner : Context) return Natural is
      (if Ready (Owner) then Store.Length (Owner.Rows) else 0);
   function Observe_Capacity (Owner : Context) return ACPI_Capacity_Diagnostics.Report is
      (Store.Capacity_Failure (Owner.Rows, ACPI_Capacity_Diagnostics.Combined_Targets));
   function Uncertain (Owner : Context) return A.Observation is (Owner.Captured);
   function Completed_Fault (Owner : Context) return A.Observation is (Owner.Fault);
   function At_Index (Owner : Context; Index : Positive) return A.Target is
      ((Owner.Token, Owner.Symbol, Epoch, A.Register_ID (Index)));
   function Matches (Owner : Context; Target : A.Target) return Boolean is
      (Ready (Owner) and then Target.Provider = Owner.Token
       and then Target.Endpoint = Owner.Symbol and then Target.Epoch = Epoch
       and then Target.Register in 1 .. A.Register_ID (Store.Length (Owner.Rows)));
   procedure Initialize (Owner : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Owner.Consumed then return; end if;
      Owner.Consumed := True;
      if Symbol = A.No_Endpoint then return; end if;
      AML_Identity.Issuer.Issue (Owner.Token, Accepted);
      if Accepted then Owner.Symbol := Symbol; Owner.Initialized := True; end if;
   end Initialize;
   procedure Adopt (Owner : in out Context; IO : User_Context; Path : Lane;
      Inner : A.Target; Geometry : L.Register_Geometry;
      Target : out A.Target; Accepted : out Boolean) is
      Canonical : L.Register_Geometry;
      Slot : Natural;
      Stored : Store.Outcome;
      Good : Boolean;
   begin
      Target := A.No_Target; Accepted := False;
      if not Ready (Owner) or else Inner.Provider = AML_Identity.No_Identity
        or else Inner.Endpoint = A.No_Endpoint or else Inner.Register = A.No_Register then return; end if;
      -- Revalidate even a cached association; geometry never creates authority.
      Describe_Inner (IO, Path, Inner, Canonical, Good);
      if not Good or else Canonical /= Geometry then return; end if;
      for I in 1 .. Store.Length (Owner.Rows) loop
         declare Row : constant Association := Store.Read (Owner.Rows, I); begin
            if Row.Path = Path and then Row.Inner = Inner then
               if Row.Geometry = Geometry then Target := At_Index (Owner, I); Accepted := True; end if;
               return;
            end if;
         end;
      end loop;
      Store.Append (Owner.Rows, (Path, Inner, Geometry), Slot, Stored);
      if Stored = Store.Stored then Target := At_Index (Owner, Slot); Accepted := True; end if;
   end Adopt;
   procedure Export_Target (Owner : Context; Ordinal : Positive;
      Target : out A.Target; Accepted : out Boolean) is
   begin
      Target := A.No_Target;
      Accepted := Ready (Owner) and then Ordinal <= Store.Length (Owner.Rows);
      if Accepted then Target := At_Index (Owner, Ordinal); end if;
   end Export_Target;
   procedure Describe (Owner : Context; IO : User_Context; Target : A.Target;
      Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (others => <>); Accepted := False;
      if not Matches (Owner, Target) then return; end if;
      declare Row : constant Association := Store.Read (Owner.Rows, Positive (Target.Register)); begin
         Describe_Inner (IO, Row.Path, Row.Inner, Geometry, Accepted);
         Accepted := Accepted and then Geometry = Row.Geometry;
         if not Accepted then Geometry := (others => <>); end if;
      end;
   end Describe;
   procedure Authorize (Owner : Context; IO : User_Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
      Geometry : L.Register_Geometry;
   begin
      Describe (Owner, IO, Target, Geometry, Accepted);
      if not Accepted or else Owner.Issued = A.Counter'Last then Accepted := False; return; end if;
      declare Row : constant Association := Store.Read (Owner.Rows, Positive (Target.Register)); begin
         Authorize_Inner (IO, Row.Path, Row.Inner, For_Write, Accepted);
      end;
   end Authorize;
   function Fits (Value : A.Counter; Geometry : L.Register_Geometry) return Boolean is
      (if ACPI_FADT.Transactions.Octets (Geometry.Width) = 8 then True
       else Value < 2 ** (8 * ACPI_FADT.Transactions.Octets (Geometry.Width)));
   procedure Dispatch (Owner : in out Context; IO : in out User_Context;
      Item : A.Request; Reply : out A.Response) is
      Lower : A.Response;
      Good : Boolean;
   begin
      Reply := (others => <>);
      Authorize (Owner, IO, Item.Destination, Item.For_Write, Good);
      if not Good or else (not Item.For_Write and then Item.Value /= 0) then return; end if;
      declare Row : constant Association := Store.Read (Owner.Rows, Positive (Item.Destination.Register)); begin
         if Item.For_Write and then not Fits (Item.Value, Row.Geometry) then return; end if;
         -- Allocate the outer receipt before a potentially effectful callback.
         Owner.Working := True; Owner.Issued := Owner.Issued + 1;
         Dispatch_Inner (IO, Row.Path, (Row.Inner, Item.For_Write, Item.Value), Lower);
         Owner.Working := False;
         if Lower.Status /= A.Denied and then Lower.Provider /= Row.Inner.Provider then
            Lower.Status := A.Indeterminate;
         end if;
         if Lower.Status = A.Completed and then
           ((Item.For_Write and then Lower.Value /= 0) or else not Fits (Lower.Value, Row.Geometry)) then
            Lower.Status := A.Failed_Completed;
         end if;
         case Lower.Status is
            when A.Completed => Reply := (A.Completed, Owner.Token, Lower.Value, A.No_Operation);
            when A.Failed_Completed =>
               Owner.Stopped := True; Owner.Fault := (True, Owner.Token, Item, A.No_Operation);
               Reply := (A.Failed_Completed, Owner.Token, 0, A.No_Operation);
            when A.Indeterminate =>
               Owner.Stopped := True;
               Owner.Captured := (True, Owner.Token, Item, A.Operation_ID (Owner.Issued));
               Reply := (A.Indeterminate, Owner.Token, 0, Owner.Captured.Operation);
            when A.Denied => Owner.Stopped := True;
         end case;
      end;
   end Dispatch;
   procedure Reconciled (Owner : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (Owner, Expected);
   begin Accepted := False; end Reconciled;
end ACPI_Field_Routes;
