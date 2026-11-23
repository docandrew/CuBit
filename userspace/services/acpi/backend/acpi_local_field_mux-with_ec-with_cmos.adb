pragma Ada_2022;
with AML_Decode;
with AML_Register_Views;
package body ACPI_Local_Field_Mux.With_EC.With_CMOS is
   use type AML_Decode.Byte;
   function Pending (IO : Context) return Boolean is
     (IO.Working or else Links.Pending(IO.Routing) or else ACPI_Local_Field_Mux.With_EC.Pending(IO.Lower.Base) or else CMOS.Pending(IO.Lower.Clock));
   function Is_Ready (IO : Context) return Boolean is
     (IO.Initialized and then not IO.Closed and then not Pending(IO)
      and then ACPI_Local_Field_Mux.With_EC.Is_Ready(IO.Lower.Base) and then (not IO.Has_CMOS or else CMOS.Is_Ready(IO.Lower.Clock)));
   function Identity (IO : Context) return AML_Identity.Identity is (Links.Identity(IO.Routing));
   function CMOS_Available (IO : Context) return Boolean is
     (Is_Ready(IO) and then IO.Has_CMOS and then CMOS.Is_Ready(IO.Lower.Clock));
   function Pool_Available (IO : Context) return Boolean is (ACPI_Local_Field_Mux.With_EC.Pool_Available(IO.Lower.Base));
   function Register_Count (IO : Context) return Natural is (if Is_Ready(IO) then Links.Register_Count(IO.Routing) else 0);
   function Uncertain (IO : Context) return A.Observation is (Links.Uncertain(IO.Routing));
   function Completed_Fault (IO : Context) return A.Observation is (Links.Completed_Fault(IO.Routing));
   procedure Describe_Inner (IO : Lower_Context; Path : Lane; Target : A.Target;
      Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin
      case Path is
         when Ordinary=>ACPI_Local_Field_Mux.With_EC.Describe(IO.Base,Target,Geometry,Accepted);
         when CMOS_Controller=>CMOS.Describe(IO.Clock,Target,Geometry,Accepted);
      end case;
   end Describe_Inner;
   -- Keep the lower descriptor as authority; a different enclosing AML
   -- region may describe exactly the same scalar byte transaction.
   procedure Adopt_View (IO : in out Context; Path : Lane; Inner : A.Target;
      Requested : L.Register_Geometry; Target : out A.Target; Accepted : out Boolean) is
      Canonical : L.Register_Geometry;
      Good : Boolean;
   begin
      Target := A.No_Target; Accepted := False;
      Describe_Inner (IO.Lower,Path,Inner,Canonical,Good);
      if Good and then AML_Register_Views.Same_Transaction (Requested,Canonical) then
         Links.Adopt (IO.Routing,IO.Lower,Path,Inner,Canonical,Target,Accepted);
      end if;
   end Adopt_View;
   procedure Authorize_Inner (IO : Lower_Context; Path : Lane; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
   begin
      case Path is
         when Ordinary=>ACPI_Local_Field_Mux.With_EC.Authorize(IO.Base,Target,For_Write,Accepted);
         when CMOS_Controller=>CMOS.Authorize(IO.Clock,Target,For_Write,Accepted);
      end case;
   end Authorize_Inner;
   procedure Dispatch_Inner (IO : in out Lower_Context; Path : Lane; Item : A.Request; Reply : out A.Response) is
   begin
      Reply:=(others=><>);if not Access_Allowed(Item.For_Write) then return;end if;
      case Path is
         when Ordinary=>ACPI_Local_Field_Mux.With_EC.Dispatch(IO.Base,Item,Reply);
         when CMOS_Controller=>CMOS.Dispatch(IO.Clock,Item,Reply);
      end case;
   end Dispatch_Inner;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean) is
      Inner,Outer:A.Target;Geometry:L.Register_Geometry;Good:Boolean;
   begin
      Accepted:=False;if IO.Initialized or else IO.Closed or else IO.Working then return;end if;
      IO.Working:=True;IO.Closed:=True;
      ACPI_Local_Field_Mux.With_EC.Initialize(IO.Lower.Base,Symbol,Good);
      if Good then CMOS.Initialize(IO.Lower.Clock,Symbol,IO.Has_CMOS);end if;
      if CMOS.Pending(IO.Lower.Clock) then IO.Working:=False;return;end if;
      if Good then Links.Initialize(IO.Routing,Symbol,Good);end if;
      if not Good then IO.Working:=False;return;end if;
      for I in 1 .. ACPI_Local_Field_Mux.With_EC.Register_Count(IO.Lower.Base) loop
         ACPI_Local_Field_Mux.With_EC.Export_Target(IO.Lower.Base,I,Inner,Good);
         if Good then ACPI_Local_Field_Mux.With_EC.Describe(IO.Lower.Base,Inner,Geometry,Good);end if;
         if Good then Links.Adopt(IO.Routing,IO.Lower,Ordinary,Inner,Geometry,Outer,Good);end if;
         if not Good then IO.Working:=False;return;end if;
      end loop;
      IO.Initialized:=True;IO.Closed:=False;IO.Working:=False;Accepted:=True;
   end Initialize;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean) is
   begin Target:=A.No_Target;Accepted:=False;if Is_Ready(IO) then Links.Export_Target(IO.Routing,Ordinal,Target,Accepted);end if;end Export_Target;
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean) is
      Inner:A.Target;Good:Boolean;Path:Lane;
   begin
      Target:=A.No_Target;Accepted:=False;if not Is_Ready(IO) then return;end if;IO.Working:=True;
      if Geometry.Region.Space=L.System_CMOS_Space then
         Path:=CMOS_Controller;Good:=False;
         if IO.Has_CMOS then CMOS.Select_Target(IO.Lower.Clock,Geometry,For_Write,Inner,Good);end if;
      else Path:=Ordinary;ACPI_Local_Field_Mux.With_EC.Select_Target(IO.Lower.Base,Geometry,For_Write,Inner,Good);end if;
      if Good then Adopt_View(IO,Path,Inner,Geometry,Target,Accepted);end if;IO.Working:=False;
   end Select_Target;
   procedure Resolve (IO : in out Context; Item : Declaration; Geometry : L.Register_Geometry;
      Deadline_Microseconds : Word; Target : out A.Target; Accepted : out Boolean) is
      Inner:A.Target;Good:Boolean;Path:Lane;
   begin
      Target:=A.No_Target;Accepted:=False;if not Is_Ready(IO) then return;end if;IO.Working:=True;
      if Geometry.Region.Space=L.System_CMOS_Space then
         Path:=CMOS_Controller;Good:=False;
         if IO.Has_CMOS then
            CMOS.Select_Target(IO.Lower.Clock,Geometry,False,Inner,Good);
            if not Good then CMOS.Select_Target(IO.Lower.Clock,Geometry,True,Inner,Good);end if;
         end if;
      else Path:=Ordinary;ACPI_Local_Field_Mux.With_EC.Resolve(IO.Lower.Base,Item,Geometry,Deadline_Microseconds,Inner,Good);end if;
      if Good then Adopt_View(IO,Path,Inner,Geometry,Target,Accepted);end if;IO.Working:=False;
   end Resolve;
   procedure Resolve_PCI (IO : in out Context; Geometry : L.Register_Geometry;
      Allow_Setup : Boolean; Target : out A.Target; Accepted : out Boolean) is
      Inner:A.Target;Good:Boolean;
   begin
      Target:=A.No_Target;Accepted:=False;if not Is_Ready(IO) then return;end if;IO.Working:=True;
      ACPI_Local_Field_Mux.With_EC.Resolve_PCI(IO.Lower.Base,Geometry,Allow_Setup,Inner,Good);
      if Good then Links.Adopt(IO.Routing,IO.Lower,Ordinary,Inner,Geometry,Target,Accepted);end if;IO.Working:=False;
   end Resolve_PCI;
   procedure Close_Setup (IO : in out Context) is
   begin if not IO.Working then ACPI_Local_Field_Mux.With_EC.Close_Setup(IO.Lower.Base);end if;end Close_Setup;
   procedure Describe (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin Geometry:=(others=><>);Accepted:=False;if Is_Ready(IO) then Links.Describe(IO.Routing,IO.Lower,Target,Geometry,Accepted);end if;end Describe;
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
   begin Accepted:=False;if Is_Ready(IO) then Links.Authorize(IO.Routing,IO.Lower,Target,For_Write,Accepted);end if;end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
   begin Reply:=(others=><>);if not Is_Ready(IO) then return;end if;IO.Working:=True;Links.Dispatch(IO.Routing,IO.Lower,Item,Reply);IO.Working:=False;end Dispatch;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin Links.Reconciled(IO.Routing,Expected,Accepted);end Reconciled;
   procedure Close (IO : in out Context) is
   begin if not IO.Working and then not Pending(IO) then IO.Closed:=True;ACPI_Local_Field_Mux.With_EC.Close(IO.Lower.Base);CMOS.Close(IO.Lower.Clock);end if;end Close;
end ACPI_Local_Field_Mux.With_EC.With_CMOS;
