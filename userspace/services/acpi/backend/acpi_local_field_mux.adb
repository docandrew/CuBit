pragma Ada_2022;
with AML_Register_Views;
with AML_Decode;
package body ACPI_Local_Field_Mux is
   use type A.Target, L.Register_Geometry, L.PCI_Route, A.Counter;
   use type Word, AML_Decode.Byte,Named_NVS.Match_Kind,PCI.Match_Kind;
   function Pending (IO : Context) return Boolean is
     (IO.Working or else Routes.Pending (IO.Routing) or else Named.Pending (IO.Lower.Registers)
      or else PCI.Pending (IO.Lower.Configuration_Data) or else Named_NVS.Pending (IO.Lower.Data) or else Pool.Pending (IO.Lower.Platform));
   function Is_Ready (IO : Context) return Boolean is
     (IO.Stage = Ready and then not Pending (IO) and then Named.Is_Ready (IO.Lower.Registers)
      and then PCI.Is_Ready (IO.Lower.Configuration_Data) and then Named_NVS.Is_Ready (IO.Lower.Data)
      and then (not IO.Has_Pool or else Pool.Is_Ready (IO.Lower.Platform)));
   function Identity (IO : Context) return AML_Identity.Identity is (Routes.Identity (IO.Routing));
   function Pool_Available (IO : Context) return Boolean is (IO.Has_Pool and then Pool.Is_Ready (IO.Lower.Platform));
   function Register_Count (IO : Context) return Natural is
     (if Is_Ready (IO) then Routes.Register_Count (IO.Routing) else 0);
   function Uncertain (IO : Context) return A.Observation is (Routes.Uncertain (IO.Routing));
   function Completed_Fault (IO : Context) return A.Observation is (Routes.Completed_Fault (IO.Routing));
   procedure Inner_Describe (IO : Lower_Context; Path : Lane; Target : A.Target;
      Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin
      case Path is
         when Named_Register => Named.Describe (IO.Registers,Target,Geometry,Accepted);
         when Configuration => PCI.Describe (IO.Configuration_Data,Target,Geometry,Accepted);
         when Named_Data => Named_NVS.Describe (IO.Data,Target,Geometry,Accepted);
         when Platform_Memory => Pool.Describe (IO.Platform,Target,Geometry,Accepted);
      end case;
   end Inner_Describe;
   procedure Inner_Authorize (IO : Lower_Context; Path : Lane; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
   begin
      case Path is
         when Named_Register => Named.Authorize (IO.Registers,Target,For_Write,Accepted);
         when Configuration => PCI.Authorize (IO.Configuration_Data,Target,For_Write,Accepted);
         when Named_Data => Named_NVS.Authorize (IO.Data,Target,For_Write,Accepted);
         when Platform_Memory => Pool.Authorize (IO.Platform,Target,For_Write,Accepted);
      end case;
   end Inner_Authorize;
   procedure Inner_Dispatch (IO : in out Lower_Context; Path : Lane; Item : A.Request; Reply : out A.Response) is
   begin
      Reply := (others => <>);
      if not Access_Allowed (Item.For_Write) then return; end if;
      case Path is
         when Named_Register => Named.Dispatch (IO.Registers,Item,Reply);
         when Configuration => PCI.Dispatch (IO.Configuration_Data,Item,Reply);
         when Named_Data => Named_NVS.Dispatch (IO.Data,Item,Reply);
         when Platform_Memory => Pool.Dispatch (IO.Platform,Item,Reply);
      end case;
   end Inner_Dispatch;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean) is
      Inner,Outer : A.Target;
      Geometry : L.Register_Geometry;
      Good : Boolean;
   begin
      Accepted := False; if IO.Stage /= Fresh then return; end if;
      IO.Stage := Initializing; IO.Working := True;
      Named.Initialize (IO.Lower.Registers,Symbol,Good);
      if not Good then IO.Stage := Failed; IO.Working := False; return; end if;
      PCI.Initialize (IO.Lower.Configuration_Data,Symbol,Good);
      if not Good then IO.Stage := Failed; IO.Working := False; return; end if;
      Named_NVS.Initialize (IO.Lower.Data,Symbol,Good);
      if not Good then IO.Stage := Failed; IO.Working := False; return; end if;
      -- No pool capability must not disable independent named registers.
      Pool.Initialize (IO.Lower.Platform,Symbol,IO.Has_Pool);
      if Pool.Pending (IO.Lower.Platform) then IO.Stage := Failed; IO.Working := False; return; end if;
      Routes.Initialize (IO.Routing,Symbol,Good);
      if not Good then IO.Stage := Failed; IO.Working := False; return; end if;
      for I in 1 .. Named.Register_Count (IO.Lower.Registers) loop
         Named.Export_Target (IO.Lower.Registers,I,Inner,Good);
         if Good then Named.Describe (IO.Lower.Registers,Inner,Geometry,Good); end if;
         if Good then Routes.Adopt (IO.Routing,IO.Lower,Named_Register,Inner,Geometry,Outer,Good); end if;
         if not Good then IO.Stage := Failed; IO.Working := False; return; end if;
      end loop;
      IO.Stage := Ready; IO.Working := False; Accepted := True;
   end Initialize;
   type Match is (Absent, Exact, Reserved, Unavailable);
   procedure Match_Named (IO : Lower_Context; Geometry : L.Register_Geometry;
      Target : out A.Target; Canonical : out L.Register_Geometry; Result : out Match) is
      Item : A.Target;
      View : L.Register_Geometry;
      Good : Boolean;
      use type L.Region_Geometry;
   begin
      Target := A.No_Target; Canonical := (others => <>); Result := Absent;
      if not AML_Register_Views.Valid_Register (Geometry) then Result := Unavailable; return; end if;
      for I in 1 .. Named.Register_Count (IO.Registers) loop
         Named.Export_Target (IO.Registers,I,Item,Good);
         if Good then Named.Describe (IO.Registers,Item,View,Good); end if;
         if not Good or else not AML_Register_Views.Valid_Register (View) then Result := Unavailable; return; end if;
         if View.Region.Space = Geometry.Region.Space and then View.Region.PCI = Geometry.Region.PCI
           and then AML_Register_Views.First_Address (View) <= AML_Register_Views.Last_Address (Geometry)
           and then AML_Register_Views.First_Address (Geometry) <= AML_Register_Views.Last_Address (View)
         then
            if Result /= Absent or else not AML_Register_Views.Same_Transaction (View,Geometry) then
               Result := Reserved; Target := A.No_Target; return;
            end if;
            Target := Item; Canonical := View; Result := Exact;
         end if;
      end loop;
   end Match_Named;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean) is
   begin Target := A.No_Target; Accepted := False; if Is_Ready (IO) then Routes.Export_Target (IO.Routing,Ordinal,Target,Accepted); end if; end Export_Target;
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean) is
      Inner : A.Target;
      Canonical : L.Register_Geometry;
      Found : Match;
      Claim : Named_NVS.Match_Kind := Named_NVS.Absent;
      PCI_Claim : PCI.Match_Kind := PCI.Absent;
      Good : Boolean;
   begin
      Target := A.No_Target; Accepted := False; if not Is_Ready (IO) then return; end if;
      IO.Working := True; Match_Named (IO.Lower,Geometry,Inner,Canonical,Found);
      case Found is
         when Exact =>
            Named.Authorize (IO.Lower.Registers,Inner,For_Write,Good);
            if Good then Routes.Adopt (IO.Routing,IO.Lower,Named_Register,Inner,Canonical,Target,Accepted); end if;
         when Absent =>
            PCI.Resolve (IO.Lower.Configuration_Data,Geometry,False,Inner,PCI_Claim,Good);
            if PCI_Claim = PCI.Whole_Window and then Good then
               PCI.Authorize (IO.Lower.Configuration_Data,Inner,For_Write,Good);
               if Good then Routes.Adopt (IO.Routing,IO.Lower,Configuration,Inner,Geometry,Target,Accepted); end if;
            end if;
            if PCI_Claim = PCI.Absent and then Geometry.Region.Space = L.Memory_Space then
               Named_NVS.Resolve (IO.Lower.Data,Geometry,Inner,Claim,Good);
               if Claim = Named_NVS.Whole_Record and then Good then
                  Named_NVS.Authorize (IO.Lower.Data,Inner,For_Write,Good);
                  if Good then Routes.Adopt (IO.Routing,IO.Lower,Named_Data,Inner,Geometry,Target,Accepted); end if;
               end if;
            end if;
            if PCI_Claim = PCI.Absent and then Claim = Named_NVS.Absent and then IO.Has_Pool then
               Pool.Select_Target (IO.Lower.Platform,Geometry,For_Write,Inner,Good);
               if Good then Routes.Adopt (IO.Routing,IO.Lower,Platform_Memory,Inner,Geometry,Target,Accepted); end if;
            end if;
         when Reserved | Unavailable => null;
      end case;
      IO.Working := False;
   end Select_Target;
   procedure Resolve (IO : in out Context; Item : Declaration; Geometry : L.Register_Geometry;
      Deadline_Microseconds : Word; Target : out A.Target; Accepted : out Boolean) is
      Inner : A.Target;
      Canonical : L.Register_Geometry;
      Found : Match;
      Claim : Named_NVS.Match_Kind := Named_NVS.Absent;
      PCI_Claim : PCI.Match_Kind := PCI.Absent;
      Good, Reading, Writing : Boolean;
   begin
      Target := A.No_Target; Accepted := False; if not Is_Ready (IO) then return; end if;
      IO.Working := True; Match_Named (IO.Lower,Geometry,Inner,Canonical,Found);
      if Found = Exact then
         Named.Authorize (IO.Lower.Registers,Inner,False,Reading);
         Named.Authorize (IO.Lower.Registers,Inner,True,Writing);
         if Reading or Writing then Routes.Adopt (IO.Routing,IO.Lower,Named_Register,Inner,Canonical,Target,Accepted); end if;
      elsif Found = Absent then
         PCI.Resolve (IO.Lower.Configuration_Data,Geometry,True,Inner,PCI_Claim,Good);
         if PCI_Claim = PCI.Whole_Window and then Good then
            Routes.Adopt (IO.Routing,IO.Lower,Configuration,Inner,Geometry,Target,Accepted);
         end if;
         if PCI_Claim = PCI.Absent and then Geometry.Region.Space = L.Memory_Space then
            Named_NVS.Resolve (IO.Lower.Data,Geometry,Inner,Claim,Good);
            if Claim = Named_NVS.Whole_Record and then Good then
               Routes.Adopt (IO.Routing,IO.Lower,Named_Data,Inner,Geometry,Target,Accepted);
            end if;
         end if;
         if PCI_Claim = PCI.Absent and then Claim = Named_NVS.Absent and then IO.Has_Pool then
            Pool.Resolve (IO.Lower.Platform,Item,Geometry,Deadline_Microseconds,Inner,Good);
            if Good then Routes.Adopt (IO.Routing,IO.Lower,Platform_Memory,Inner,Geometry,Target,Accepted); end if;
         end if;
      end if;
      IO.Working := False;
   end Resolve;
   procedure Resolve_PCI (IO : in out Context; Geometry : L.Register_Geometry;
      Allow_Setup : Boolean; Target : out A.Target; Accepted : out Boolean) is
      Inner : A.Target;
      Claim : PCI.Match_Kind;
      Good : Boolean;
   begin
      Target := A.No_Target; Accepted := False;
      if not Is_Ready (IO) or else Geometry.Region.Space /= L.PCI_Config_Space then return; end if;
      IO.Working := True;
      PCI.Resolve (IO.Lower.Configuration_Data,Geometry,Allow_Setup,Inner,Claim,Good);
      if Good and then Claim = PCI.Whole_Window then
         Routes.Adopt (IO.Routing,IO.Lower,Configuration,Inner,Geometry,Target,Accepted);
      end if;
      IO.Working := False;
   end Resolve_PCI;
   procedure Close_Setup (IO : in out Context) is
   begin if not IO.Working then PCI.Close_Setup (IO.Lower.Configuration_Data); end if; end Close_Setup;
   procedure Describe (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin Geometry := (others => <>); Accepted := False; if Is_Ready (IO) then Routes.Describe (IO.Routing,IO.Lower,Target,Geometry,Accepted); end if; end Describe;
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
   begin Accepted := False; if Is_Ready (IO) then Routes.Authorize (IO.Routing,IO.Lower,Target,For_Write,Accepted); end if; end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
   begin
      Reply := (others => <>); if not Is_Ready (IO) then return; end if;
      IO.Working := True; Routes.Dispatch (IO.Routing,IO.Lower,Item,Reply); IO.Working := False;
   end Dispatch;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin Routes.Reconciled (IO.Routing,Expected,Accepted); end Reconciled;
   procedure Close (IO : in out Context) is
   begin if not IO.Working then IO.Stage := Closed; PCI.Close (IO.Lower.Configuration_Data); Named_NVS.Close (IO.Lower.Data); Pool.Close (IO.Lower.Platform); end if; end Close;
end ACPI_Local_Field_Mux;
