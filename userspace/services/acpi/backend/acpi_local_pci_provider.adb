pragma Ada_2022;
with AML_Identity.Issuer;
with AML_Register_Views;
with AML_Decode;
with ACPI_PCI_Protocol;
with ACPI_Inventory_Protocol;
package body ACPI_Local_PCI_Provider is
   package Q renames ACPI_PCI_Protocol;
   use type A.Counter,A.Endpoint_ID,AML_Identity.Identity,R.Word,Q.Outcome,R.Outcome;
   use type L.Register_Geometry,AML_Decode.Byte,G.Outcome,P.Bus_Key,G.Bus_Window;
   use type Buses.Outcome,Store.Outcome;
   use type P.Segment_Number,P.Bus_Number,P.T.Access_Width;
   Provider_Epoch : constant A.Counter := 1;
   function Pending (IO : Context) return Boolean is (IO.Working or else IO.Captured.Present or else Backing_Pending);
   function Is_Ready (IO : Context) return Boolean is (IO.Stage = Ready and then not Pending (IO));
   function Identity (IO : Context) return AML_Identity.Identity is (IO.Issuer);
   function Register_Count (IO : Context) return Natural is (if Is_Ready (IO) then Store.Length (IO.Rows) else 0);
   function Uncertain (IO : Context) return A.Observation is (IO.Captured);
   function Completed_Fault (IO : Context) return A.Observation is (IO.Fault);
   function Target_At (IO : Context; I : Positive) return A.Target is ((IO.Issuer,IO.Symbol,Provider_Epoch,A.Register_ID (I)));
   function Matches (IO : Context; Target : A.Target) return Boolean is
     (Is_Ready (IO) and then IO.Issuer /= AML_Identity.No_Identity and then Target.Provider = IO.Issuer
      and then Target.Endpoint = IO.Symbol and then Target.Epoch = Provider_Epoch
      and then Target.Register in 1 .. A.Register_ID (Store.Length (IO.Rows)));
   function Valid (Window : R.Window_Description; Key : P.Bus_Key) return Boolean is
     (Window.Status = Q.Ready and then Window.Epoch /= 0 and then Window.ID /= 0 and then G.Valid (Window.Geometry)
      and then Window.Geometry.Segment = Key.Segment and then Window.Geometry.Bus = Key.Bus);
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean) is
   begin
      Accepted := False; if IO.Stage /= Fresh then return; end if; IO.Stage := Failed;
      if Symbol = A.No_Endpoint or else Backing_Pending then return; end if;
      AML_Identity.Issuer.Issue (IO.Issuer,Accepted);
      if Accepted then IO.Symbol := Symbol; IO.Stage := Ready; end if;
   end Initialize;
   procedure Get_Window (IO : in out Context; Key : P.Bus_Key; Allow_Setup : Boolean;
      Window : out R.Window_Description; Accepted : out Boolean) is
      Row : Bus := (Key => Key, others => <>);
      Slot : Buses.Count;
      Stored : Buses.Outcome;
      Setup,Data : R.Word;
      Good : Boolean;
   begin
      Window := (others => <>); Accepted := False;
      for I in 1 .. Buses.Length (IO.Windows) loop
         Row := Buses.Read (IO.Windows,I);
         if Row.Key = Key then
            if Row.Stage /= Retained then return; end if;
            Inspect (Row.Window.Epoch,Row.Window.ID,Window);
            Accepted := Valid (Window,Key) and then Window.Epoch = Row.Window.Epoch and then Window.ID = Row.Window.ID
              and then Window.Geometry = Row.Window.Geometry;
            if not Accepted then IO.Stage := Failed; end if;
            return;
         end if;
      end loop;
      if not Allow_Setup or else IO.Setup_Closed or else not Setup_Allowed then return; end if;
      Deadlines (Setup,Data,Good); if not Good or else Setup in 0 | R.Word'Last then return; end if;
      Row := (Key => Key, others => <>);
      Buses.Append (IO.Windows,Row,Slot,Stored); if Stored /= Buses.Stored then return; end if;
      -- The row is visible as Requested before any acquisition callback.
      Prepare (Key,Setup,Window); Row.Stage := Refused;
      if Valid (Window,Key) then
         Accepted := True;
         for I in 1 .. Buses.Length (IO.Windows) loop
            if I /= Slot and then Buses.Read (IO.Windows,I).Stage = Retained then
               declare Other : constant R.Window_Description := Buses.Read (IO.Windows,I).Window; begin
                  if (Other.Epoch = Window.Epoch and then Other.ID = Window.ID)
                    or else (if Other.Geometry.First <= Window.Geometry.First then
                      Window.Geometry.First-Other.Geometry.First < Other.Geometry.Bytes
                      else Other.Geometry.First-Window.Geometry.First < Window.Geometry.Bytes)
                  then Accepted := False; IO.Stage := Failed; end if;
               end;
            end if;
         end loop;
         if Accepted then Row.Stage := Retained; Row.Window := Window; end if;
      end if;
      Buses.Replace (IO.Windows,Slot,Row);
   end Get_Window;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean) is
   begin
      Target := A.No_Target; Accepted := Is_Ready (IO) and then Ordinal <= Store.Length (IO.Rows);
      if Accepted then Target := Target_At (IO,Ordinal); end if;
   end Export_Target;
   procedure Resolve (IO : in out Context; Geometry : L.Register_Geometry;
      Allow_Setup : Boolean; Target : out A.Target; Match : out Match_Kind; Accepted : out Boolean) is
      Window : R.Window_Description;
      Key : P.Bus_Key;
      Config : P.Region;
      Offset : R.Word := 0;
      Good,Selected : Boolean := False;
      Plan : G.View;
      Slot : Store.Count;
      Stored : Store.Outcome;
   begin
      Target := A.No_Target; Match := Unavailable; Accepted := False;
      if not Is_Ready (IO) or else not AML_Register_Views.Valid_Register (Geometry) then return; end if;
      IO.Working := True;
      if Geometry.Region.Space = L.PCI_Config_Space and then Geometry.Region.PCI.Known then
         Match := Reserved;
         Config := (Geometry.Region.PCI.Device,Geometry.Region.Base,Geometry.Region.Length);
         Key := (Config.Device.Segment,Config.Device.Bus); Offset := Geometry.Byte_Offset;
         if Geometry.Width /= P.T.Qword_Access then
            Get_Window (IO,Key,Allow_Setup,Window,Good);
            if Good then Plan := G.Plan_Config (Window.Geometry,Config,Offset,Geometry.Width); Selected := Plan.Status = G.Read_Ready; end if;
         end if;
      elsif Geometry.Region.Space = L.Memory_Space and then not Geometry.Region.PCI.Known then
         Match := Absent;
         for I in 1 .. Buses.Length (IO.Windows) loop
            declare B : constant Bus := Buses.Read (IO.Windows,I); begin
               if B.Stage = Retained and then Geometry.Region.Base <= B.Window.Geometry.First+(B.Window.Geometry.Bytes-1)
                 and then B.Window.Geometry.First <= Geometry.Region.Base+(Geometry.Region.Length-1) then
                  if Match /= Absent then Match := Reserved; Selected := False; exit; end if;
                  Match := Reserved;
                  declare Alias_View : constant G.Translation := G.Plan_Alias (B.Window.Geometry,Geometry.Region.Base,
                     Geometry.Region.Length,Geometry.Byte_Offset,Geometry.Width); begin
                     if Alias_View.Status = G.Read_Ready then
                        Get_Window (IO,B.Key,False,Window,Good);
                        if Good then Config := Alias_View.Region; Offset := Alias_View.Offset; Selected := True; end if;
                     end if;
                  end;
               end if;
            end;
         end loop;
         if Match = Absent then
            Good := False; Resolve_Alias (Geometry,Key,Good);
            if Good then
               Match := Reserved;
               Get_Window (IO,Key,Allow_Setup,Window,Good);
               if Good then
                  declare Alias_View : constant G.Translation := G.Plan_Alias (Window.Geometry,Geometry.Region.Base,
                     Geometry.Region.Length,Geometry.Byte_Offset,Geometry.Width); begin
                     if Alias_View.Status = G.Read_Ready then Config := Alias_View.Region; Offset := Alias_View.Offset; Selected := True; end if;
                  end;
               end if;
            end if;
         end if;
      end if;
      if not Selected or else IO.Stage /= Ready then IO.Working := False; return; end if;
      Match := Whole_Window;
      for I in 1 .. Store.Length (IO.Rows) loop
         if Store.Read (IO.Rows,I).Geometry = Geometry then Target := Target_At (IO,I); Accepted := True; IO.Working := False; return; end if;
      end loop;
      Store.Append (IO.Rows,(Geometry,Window,Config,Offset),Slot,Stored);
      if Stored = Store.Stored then Target := Target_At (IO,Slot); Accepted := True; end if;
      IO.Working := False;
   end Resolve;
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean) is
      Match : Match_Kind;
   begin
      Target := A.No_Target; Accepted := False; if For_Write then return; end if;
      Resolve (IO,Geometry,False,Target,Match,Accepted);
   end Select_Target;
   procedure Describe (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (others => <>); Accepted := Matches (IO,Target);
      if Accepted then Geometry := Store.Read (IO.Rows,Positive (Target.Register)).Geometry; end if;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
   begin Accepted := not For_Write and then Matches (IO,Target) and then IO.Sequence < A.Counter'Last; end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Good : Boolean;
      Setup,Deadline : R.Word;
      Result : R.Result;
      function Guard return Boolean is (Access_Allowed (False));
   begin
      Reply := (others => <>); Authorize (IO,Item.Destination,Item.For_Write,Good);
      if not Good or else Item.Value /= 0 then return; end if;
      IO.Working := True; Deadlines (Setup,Deadline,Good);
      if not Good or else Deadline in 0 | R.Word'Last then IO.Working := False; return; end if;
      IO.Sequence := IO.Sequence+1;
      declare Row : constant Association := Store.Read (IO.Rows,Positive (Item.Destination.Register)); begin
         Read_Data (Row.Window.Epoch,Row.Window.ID,Row.Region,Row.Offset,Row.Geometry.Width,Deadline,Guard'Access,Result);
         if Result.Status = R.Read_Complete and then Result.Value > ACPI_Inventory_Protocol.Width_Mask (Row.Geometry.Width) then
            Result.Status := R.Completion_Unknown;
         end if;
      end;
      IO.Working := False;
      case Result.Status is
         when R.Read_Complete => Reply := (A.Completed,IO.Issuer,Result.Value,A.No_Operation);
         when R.Completion_Unknown =>
            IO.Stage := Failed; IO.Captured := (True,IO.Issuer,Item,A.Operation_ID (IO.Sequence));
            Reply := (A.Indeterminate,IO.Issuer,0,IO.Captured.Operation);
         when R.Guard_Refused | R.Invalid_Geometry | R.Invalid_Mapping | R.Deadline_Expired => IO.Stage := Failed;
         when others =>
            IO.Stage := Failed; IO.Fault := (True,IO.Issuer,Item,A.No_Operation);
            Reply := (A.Failed_Completed,IO.Issuer,0,A.No_Operation);
      end case;
   end Dispatch;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO,Expected);
   begin Accepted := False; end Reconciled;
   procedure Close_Setup (IO : in out Context) is
   begin if not IO.Working then IO.Setup_Closed := True; end if; end Close_Setup;
   procedure Close (IO : in out Context) is
   begin if not IO.Working then IO.Stage := Closed; IO.Setup_Closed := True; end if; end Close;
end ACPI_Local_PCI_Provider;
