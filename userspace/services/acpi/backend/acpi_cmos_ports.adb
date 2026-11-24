package body ACPI_CMOS_Ports is
   package B renames ACPI_Backend_Binding_Types;
   use type B.Resource_Kind, P.Device_Kind, P.Route_Kind, P.Port_Pair;
   use type I.Byte, I.Word, T.Completion, Interfaces.Unsigned_16;

   function Pending (IO : Context) return Boolean is (IO.Working or else IO.Faulted);
   function Is_Ready (IO : Context) return Boolean is (IO.Configured and then not Pending (IO));
   function Exact_Grant (Actual : B.Description; Profile : P.Profile) return Boolean is
     (Actual.Current and then Actual.Kind = B.Port_Resource
      and then Actual.Base = 16#70#
      and then Actual.Length =
        (if Profile.Device = P.Intel_Compatible and then Profile.Extended_Enabled then 4 else 2)
      and then Actual.Readable and then Actual.Writable and then not Actual.Delegable);

   procedure Initialize
     (IO : in out Context; Held : Port_Handle; Profile : P.Profile;
      Century : I.Byte; Known_NMI, NMI_Disabled : Boolean; Accepted : out Boolean)
   is
      Actual : B.Description;
   begin
      Accepted := False;
      if IO.Attempted then return; end if;
      IO.Attempted := True;
      if not Known_NMI or else not P.Supported (Profile)
        or else Century > P.Last_Cell (Profile)
      then return; end if;
      IO.Working := True;
      Inspect (Held, Actual);
      IO.Working := False;
      if not Exact_Grant (Actual, Profile) then return; end if;
      IO.Held := Held; IO.Device := Profile; IO.Century := Century;
      IO.NMI_Disabled := NMI_Disabled; IO.Configured := True; Accepted := True;
   end Initialize;

   procedure Configuration
     (IO : Context; Profile : out P.Profile; Century : out I.Byte;
      Known_NMI, NMI_Disabled, Standard_Granted, Extended_Granted : out Boolean) is
   begin
      Profile := IO.Device; Century := IO.Century;
      Known_NMI := Is_Ready (IO); NMI_Disabled := IO.NMI_Disabled;
      Standard_Granted := Is_Ready (IO);
      Extended_Granted := Is_Ready (IO) and then IO.Device.Device = P.Intel_Compatible
        and then IO.Device.Extended_Enabled;
   end Configuration;

   procedure Transfer_Byte
     (IO : in out Context; Plan : T.Transfer; Value : out I.Byte;
      Outcome : out T.Completion)
   is
      Actual : B.Description;
      Cell : constant I.Byte := T.Logical_Cell (Plan);
      Select_Port : Interfaces.Unsigned_16;
      Raw : I.Word := 0;
      Step : T.Completion := T.Unknown;
      procedure Perform is
      begin
         Inspect (IO.Held, Actual);
         if not Exact_Grant (Actual, IO.Device) then return; end if;
         declare
            Route : constant P.Route := P.Locate (IO.Device, Cell);
         begin
            if Route.Kind /= P.Ready or else Route.Pair /= Plan.Pair
              or else (Plan.Selector and 16#7F#) /= Route.Index
            then return; end if;
         end;
         Select_Port := (if Plan.Pair = P.Standard_Pair then 16#70# else 16#72#);
         Write_Byte (IO.Held, Select_Port, Plan.Selector, Step);
         if Step /= T.Completed then Outcome := Step; return; end if;
         -- No other callback or hardware access between selection and data.
         if Plan.For_Write then
            Write_Byte (IO.Held, Select_Port + 1, Plan.Value, Step);
            Outcome := Step;
         else
            Read_Byte (IO.Held, Select_Port + 1, Raw, Step);
            Outcome := Step;
            if Step = T.Completed then
               if Raw > I.Word (I.Byte'Last) then Outcome := T.Refused;
               else Value := I.Byte (Raw); end if;
            end if;
         end if;
      end Perform;
   begin
      Value := 0; Outcome := T.Refused;
      if not Is_Ready (IO) or else Plan.Ticket = 0
        or else (((Plan.Selector and 16#80#) /= 0) /= IO.NMI_Disabled)
        or else (Plan.For_Write and then I.Read_Only (Cell, IO.Century))
      then return; end if;
      IO.Working := True;
      Perform;
      if Outcome = T.Unknown then IO.Faulted := True; end if;
      IO.Working := False;
   end Transfer_Byte;
end ACPI_CMOS_Ports;
