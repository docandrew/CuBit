pragma Ada_2022;
with Ada.Streams; with Ada.Streams.Stream_IO;
with ACPI_Service_Core; with AML_Delays; with AML_Objects;
with AML_Execute; with AML_Names; with Firmware_Tables;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with ACPI_Backend_Binding_Types;
with ACPI_Backend_Catalog;
with ACPI_Local_Register_Provider;
with ACPI_Platform_Region_Cache;
with ACPI_Platform_Field_Provider;
with ACPI_Local_Field_Mux;
with Empty_PCI_Fixture;
with ACPI_Backend_NVS_Records;
with ACPI_Local_NVS_Provider;
with ACPI_Firmware_Data_Layouts;
with Platform_Pool_ABI;
with ACPI_Local_Field_Mux.With_EC;
with ACPI_EC_Types;
with ACPI_Local_EC_Commands;
with ACPI_Local_EC_Field_Provider;
with ACPI_Local_Field_Mux.With_EC.With_CMOS;
with ACPI_Local_CMOS_Provider;
with ACPI_CMOS_Transactions;
with ACPI_CMOS_Ports;
with GNAT.Source_Info;
procedure CMOS_AML_Tests is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package F renames ACPI_Firmware_Data_Layouts;
   package B renames ACPI_Backend_Binding_Types;
   subtype Word is Unsigned_64;
   use type A.Target,A.Observation,A.Completion,AML_Identity.Identity,B.Outcome;
   Base : constant Word := 16#FED0_0000#;
   VA : constant Word := 16#6000_0000_0000#;
   Scope : constant Word := 16#F123_4567_0000_0001#;
   Mode : constant String := Ada.Command_Line.Argument (2);
   Physical_Held : Boolean := False;
   Begins, Ends : Natural := 0;
   Checks, Map_Calls, Named_Effects, Pool_Effects, Validations : Natural := 0;
   Gate : constant Boolean := True;
   Reenter : constant Boolean := Mode = "reentry";
   Value : constant Word := 16#42#;
   procedure Nested;
   procedure Check (Good : Boolean; Location : String := GNAT.Source_Info.Source_Location) is
   begin Checks := Checks+1; if not Good then raise Program_Error with Mode & Checks'Image & " at " & Location; end if; end Check;
   procedure Inspect (Cap : Natural; Info : out B.Description) is
   begin Check (Cap = 1); Info := (True,B.Memory_Resource,Base,8192,True,True,True); end Inspect;
   procedure Named_Map (Cap : Natural; Physical,Virtual,Pages : Word; Read_Only : Boolean; Mapped : out Boolean) is
      pragma Unreferenced (Cap,Physical,Virtual,Pages,Read_Only);
   begin Mapped := True; end Named_Map;
   procedure Clock (Now : out B.Tick; Available : out Boolean) is
   begin Now := 1; Available := True; end Clock;
   procedure Named_IO (Address : Word; Width : F.T.Access_Width; Writing : Boolean; Input : Word;
      Output : out Word; Completed : out Boolean) is
      pragma Unreferenced (Address,Width,Input);
   begin Check (not Writing); Named_Effects := Named_Effects+1; Output := Value; Completed := True; if Reenter then Nested; end if; end Named_IO;
   procedure Ports (Cap : Natural; Port : Unsigned_16; Width : F.T.Access_Width; Writing : Boolean; Input : Word;
      Output : out Word; Completed : out Boolean) is
      pragma Unreferenced (Cap,Port,Width,Writing,Input);
   begin Output := 0; Completed := False; Check (False); end Ports;
   package Catalog is new ACPI_Backend_Catalog
      (Natural,0,Natural,0,Inspect,Inspect,Named_Map,Clock,Named_IO,Ports,1,4,1,VA,VA+8192,2);
   Hardware : Catalog.Context;
   package Named is new ACPI_Local_Register_Provider (Catalog,Hardware,Scope,4);
   function Pool_Cap (Cap : Natural) return Boolean is (Cap = 2);
   function Exclusive (First,Limit : Word) return Boolean is (First = VA+16#10000# and Limit = VA+16#20000#);
   procedure Pool_Clock (Now : out Word; Available : out Boolean) is
   begin Now := 1; Available := True; end Pool_Clock;
   function Allowed (Writing : Boolean) return Boolean is
      pragma Unreferenced (Writing);
   begin return Gate; end Allowed;
   procedure Pool_Map (Cap : Natural; Physical,Virtual,Pages : Word; Read_Only : Boolean;
      Status : out Word; Why : out Platform_Pool_ABI.Refusal_Record) is
   begin
      Check (Cap = 2 and Physical = Base and Virtual = VA+16#10000# and Pages = 2 and not Read_Only);
      Map_Calls := Map_Calls+1; Status := 0; Why := [others => 0]; if Reenter then Nested; end if;
   end Pool_Map;
   procedure Pool_IO (Address : Word; Width : F.T.Access_Width; Writing : Boolean; Input : Word;
      Output : out Word; Completed : out Boolean) is
      pragma Unreferenced (Address,Width,Writing,Input);
   begin Pool_Effects := Pool_Effects+1; Output := 0; Completed := Mode /= "unknown"; if Reenter then Nested; end if; end Pool_IO;
   package Cache is new ACPI_Platform_Region_Cache (Natural,0,Pool_Cap,Exclusive,Pool_Map,Pool_Clock,Allowed,Pool_IO,4,8);
   Memory : Cache.Context;
   type Declaration is null record;
   procedure Validate (Item : Declaration; Geometry : L.Register_Geometry;
      Origin : out Cache.Source; Accepted : out Boolean) is
      pragma Unreferenced (Item,Geometry);
   begin
      Validations := Validations+1; Origin := ("TEST",Scope,0,3); Accepted := True; if Reenter then Nested; end if;
   end Validate;
   function Compatible (First_Page,Pages : Word) return Boolean is
   begin return First_Page = Base/F.Page_Bytes and Pages = 2; end Compatible;
   package Pool is new ACPI_Platform_Field_Provider (Cache,Memory,Declaration,Validate,Compatible);
   function No_NVS (Window : Catalog.W.Window_ID) return Boolean is
      pragma Unreferenced (Window);
   begin return False; end No_NVS;
   procedure No_NVS_Read (Virtual : Word; Width : F.T.Access_Width; Value : out Word; Completed : out Boolean) is
      pragma Unreferenced (Virtual,Width);
   begin Value := 0; Completed := False; Check (False); end No_NVS_Read;
   package Records is new ACPI_Backend_NVS_Records (Catalog,Hardware,No_NVS,No_NVS_Read,1);
   Backing : Records.Context;
   Records_Result : Records.Outcome;
   use type Records.Outcome;
   package Data is new ACPI_Local_NVS_Provider (Records,Backing,Scope,Allowed);
   package Empty_PCI is new Empty_PCI_Fixture (Allowed);
   package T_EC renames ACPI_EC_Types;
   package ECT renames T_EC;
   Hardware_Calls : Natural := 0;
   procedure Read (Register : ECT.Read_Register; Value : out ECT.Byte; Result : out ECT.Access_Result) is
      pragma Unreferenced (Register);
   begin Hardware_Calls := Hardware_Calls + 1; Value := 0; Result := ECT.Refused; end Read;
   procedure Write (Register : ECT.Write_Register; Value : ECT.Byte; Result : out ECT.Access_Result) is
      pragma Unreferenced (Register, Value);
   begin Hardware_Calls := Hardware_Calls + 1; Result := ECT.Refused; end Write;
   procedure EC_Clock (Now : out ECT.Microseconds; Available : out Boolean) is
   begin Now := 10; Available := True; end EC_Clock;
   procedure Enter_Command (For_Query, For_Write : Boolean; Deadline : ECT.Microseconds; Accepted : out Boolean) is
      pragma Unreferenced (For_Query, For_Write, Deadline);
   begin Hardware_Calls := Hardware_Calls + 1; Accepted := False; end Enter_Command;
   procedure Leave_Command (Released : out Boolean) is
   begin Hardware_Calls := Hardware_Calls + 1; Released := True; end Leave_Command;
   function Quiet return Boolean is (True);
   function Owned return Boolean is (True);
   package Commands is new ACPI_Local_EC_Commands (Read, Write, EC_Clock, Enter_Command, Leave_Command, Quiet, Owned);
   function Cell_Allowed (Cell : ECT.Cell_Offset; For_Write : Boolean) return Boolean is
      pragma Unreferenced (Cell);
   begin return not For_Write; end Cell_Allowed;
   procedure Deadline (Until_Time : out ECT.Microseconds; Available : out Boolean) is
   begin Until_Time := 100; Available := True; end Deadline;
   procedure Pause (Target : ECT.Microseconds; Available : out Boolean) is
      pragma Unreferenced (Target);
   begin Hardware_Calls := Hardware_Calls + 1; Available := False; end Pause;
   package Embedded is new ACPI_Local_EC_Field_Provider (Commands, Cell_Allowed, Deadline, Pause, 16);
   package Base_Mux is new ACPI_Local_Field_Mux (Declaration,Named,Pool,Data,Empty_PCI.Provider,Allowed);
   package EC_Mux is new Base_Mux.With_EC (Embedded);
   package CT renames ACPI_CMOS_Transactions;
   use type CT.I.Byte, CT.Completion, Unsigned_16;
   CMOS_Transfers, CMOS_Selects, CMOS_Writes : Natural := 0;
   Selected : CT.I.Byte := 0;
   type CMOS_Cells is array (CT.I.Byte) of CT.I.Byte;
   Cells : CMOS_Cells := [others => 16#23#];
   procedure CMOS_Inspect (Held : Natural; Info : out B.Description) is
   begin Check (Held = 3); Info := (True,B.Port_Resource,16#70#,2,True,True,False); end CMOS_Inspect;
   procedure CMOS_Read (Held : Natural; Port : Unsigned_16; Output : out Word; Outcome : out CT.Completion) is
   begin
      Check (Held = 3 and Port = 16#71# and CMOS_Selects = CMOS_Transfers + CMOS_Writes + 1);
      Check (Physical_Held);
      CMOS_Transfers := CMOS_Transfers + 1; Output := Word (Cells (Selected));
      Outcome := (if Mode = "cmos-unknown" then CT.Unknown else CT.Completed);
      if Mode = "cmos-reentry" then Nested; end if;
   end CMOS_Read;
   procedure CMOS_Write (Held : Natural; Port : Unsigned_16; Input : CT.I.Byte; Outcome : out CT.Completion) is
   begin
      Check (Physical_Held);
      Check (Held = 3 and Port in 16#70# | 16#71#); Outcome := CT.Completed;
      if Port = 16#70# then
         Check ((Input and 16#80#) /= 0); Selected := Input and 16#7F#;
         CMOS_Selects := CMOS_Selects + 1;
      else
         Check (Selected > 13 and Selected /= 50 and CMOS_Selects = CMOS_Transfers + CMOS_Writes + 1);
         Cells (Selected) := Input; CMOS_Writes := CMOS_Writes + 1;
      end if;
   end CMOS_Write;
   package Port_Interface is new ACPI_CMOS_Ports (Natural,0,CMOS_Inspect,CMOS_Read,CMOS_Write);
   Physical : Port_Interface.Context;
   procedure Configuration (Profile : out CT.P.Profile; Century : out CT.I.Byte;
      Known_NMI,NMI_Disabled,Standard_Granted,Extended_Granted : out Boolean) is
   begin Port_Interface.Configuration (Physical,Profile,Century,Known_NMI,NMI_Disabled,Standard_Granted,Extended_Granted); end Configuration;
   procedure CMOS_Transfer (Plan : CT.Transfer; Output : out CT.I.Byte; Outcome : out CT.Completion) is
   begin Port_Interface.Transfer_Byte (Physical,Plan,Output,Outcome); end CMOS_Transfer;
   package CMOS_Provider is new ACPI_Local_CMOS_Provider (Configuration,Allowed,CMOS_Transfer);
   package Mux is new EC_Mux.With_CMOS (CMOS_Provider);
   IO : Mux.Context;
   Bound : B.Report; Added : Catalog.Add_Report; OK : Boolean;
   use type Catalog.Add_Code;
   procedure Nested is begin Check (False); end Nested;
   procedure Begin_Field (Context : in out Mux.Context; Requires_Lock,For_Write : Boolean; Accepted : in out Boolean) is
      pragma Unreferenced (For_Write);
   begin
      Accepted := False;
      if not Mux.Is_Ready (Context) or else Mode = "lock-denied" then return; end if;
      Check (Requires_Lock and not Physical_Held);
      Physical_Held := True; Begins := Begins + 1; Accepted := True;
   end Begin_Field;
   procedure End_Field (Context : in out Mux.Context; Accepted : in out Boolean) is
   begin
      Check (Physical_Held); Ends := Ends + 1;
      if Mux.Uncertain (Context).Present then Accepted := False;
      else Physical_Held := False; Accepted := True; end if;
   end End_Field;
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,Namespace_Node_Capacity => 64,Retained_Result_Capacity => 4);
   package V renames C.Values; package N renames C.Namespace;
   package Fields is new C.Field_IO
     (IO_Context => Mux.Context,Pending => Mux.Pending,Describe_Target => Mux.Describe,
      Dispatch => Mux.Dispatch,Completion_Confirmed => Mux.Reconciled,
      Authorize_Target => Mux.Authorize,Begin_Field => Begin_Field,End_Field => End_Field,Allow_Locked_Fields => True);
   Bindings : Fields.Binding_Set (8);
   package Streams renames Ada.Streams.Stream_IO;
   function Read_Table return Firmware_Tables.Bytes is
      File : Streams.File_Type;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      Streams.Open (File,Streams.In_File,Ada.Command_Line.Argument (1));
      declare Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Streams.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset; Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         Streams.Read (File,Raw,Last); Streams.Close (File); Check (Last = Raw'Last);
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Table : constant Firmware_Tables.Bytes := Read_Table;
   Service : aliased C.State (1,Table'Length,Table'Length,1,Value_Object_Capacity => 256,Value_Byte_Capacity => 2048,Value_Element_Capacity => 128);
   Stage : C.Stage_Status; Freeze : C.Freeze_Status; Executed : C.Execution_Report;
   Members : N.Initialization_Report; Status : V.Access_Status; Result : V.Result;
   use type C.Stage_Status,C.Freeze_Status,V.Access_Status,V.Module_Outcome,N.Lookup_Status,AML_Execute.Execution_Status;
   function Find (Name : AML_Names.Segment) return C.Node_Descriptor is
      Found : N.Lookup_Result; Item : C.Node_Descriptor;
   begin
      C.Find_Object (Service,N.Root,Name,Found,Status); Check (Status = V.Available and then Found.Status = N.Found);
      C.Describe_Node (Service,Found.Node,Item,Status); Check (Status = V.Available); return Item;
   end Find;
   procedure Bind (Name : AML_Names.Segment; Cell : Word) is
      Item : constant C.Node_Descriptor := Find (Name);
      Key : V.Field_Key; Description : V.Field_Description; Target : A.Target;
   begin
      C.Read_Field_Descriptor (Service,V.Position (Item),Description,Key,Status); Check (Status = V.Available);
      Mux.Select_Target (IO,((L.System_CMOS_Space,0,63,(Known => False)),Cell,F.T.Byte_Access),False,Target,OK);
      Check (OK = (Mode /= "no-cmos"));
      if OK then
         Fields.Add_Binding (Service,IO,Key,Target,Bindings,Status);
         Check ((Status = V.Available) = (Mode /= "bad-access"));
      end if;
   end Bind;
   Method : C.Node_Descriptor;
begin
   Port_Interface.Initialize (Physical,3,(CT.P.PC_AT_64,False),50,Mode /= "no-cmos",True,OK);
   Check (OK = (Mode /= "no-cmos"));
   Catalog.Bind_Memory (Hardware,(1,Catalog.P.Memory_Space,Base,8192,True,True,True),1,100,Bound); Check (Bound.Status = B.Bound);
   Catalog.Authorize_Scope (Hardware,Scope,OK); Check (OK);
   for I in 1 .. (if Mode = "ambiguous" then 2 else 1) loop
      Catalog.Add (Hardware,1,
         (Tag => Scope,Base => Base,Length => 1,Space => Catalog.P.Memory_Space,Readable => True,Writable => False,
          Widths => [F.T.Byte_Access => True,others => False],Register_Authority => (Word (I),True,False,False,0),
          Register_Offset => 0,Register_Width => F.T.Byte_Access),Catalog.W.Exact_Writes_Admitted,Added);
      Check (Added.Status = Catalog.Added);
   end loop;
   Catalog.Seal (Hardware,OK); Check (OK);
   if Mode /= "no-pool" then Cache.Initialize (Memory,2,VA+16#10000#,VA+16#20000#,OK); Check (OK); end if;
   Records.Configure (Backing,Scope,Records_Result); Check (Records_Result = Records.Configured);
   Records.Seal (Backing,Records_Result); Check (Records_Result = Records.Sealed);
   Mux.Initialize (IO,16#F000_0000_0000_0001#,OK); Check (OK);

   C.Stage_Table (Service,1,C.DSDT,Table,Stage); Check (Stage = C.Staged);
   C.Freeze_Catalog (Service,Freeze); Check (Freeze = C.Catalog_Frozen_OK);
   C.Execute_Frozen (Service,1000,Executed,Status);
   Check (Status = V.Available and then Executed.Modules.Outcome = V.Modules_Completed);
   C.Initialize_Members (Service,Members,Status); Check (Status = V.Available);
   Fields.Clear (Bindings); Bind ("HOR_",4); Bind ("RWB0",20); Bind ("CENT",50);
   Method := Find ((if Mode = "write" then "WRT0" elsif Mode = "protected" then "WRO0" elsif Mode = "century" then "WRC0" else "RDTM"));
   Fields.Invoke_Retained (Service,IO,Bindings,V.Position (Method),[0 => 16#A5#,others => 0],
     (if Mode in "write" | "protected" | "century" then 1 else 0),1000,Result,Status);
   Check (Status = V.Available and C.Valid (Service) and C.Retained_Results (Service) = 0);
   Ada.Text_IO.Put_Line ("CMOS AML status=" & Result.Status'Image & " reads=" & CMOS_Transfers'Image & " writes=" & CMOS_Writes'Image & " selects=" & CMOS_Selects'Image);
   if Mode = "cmos-unknown" then
      Check (Result.Status = AML_Execute.Field_Indeterminate and Mux.Pending (IO) and Mux.Uncertain (IO).Present and Fields.Uncertain (Service).Present);
      Check (Physical_Held and CMOS_Transfers = 1 and CMOS_Writes = 0);
      Mux.Close (IO); Check (Mux.Pending (IO) and Mux.Uncertain (IO).Present);
   elsif Mode in "protected" | "century" | "no-cmos" | "lock-denied" | "bad-access" then
      Check (Result.Status = (if Mode in "protected" | "century" then AML_Execute.Unsupported else AML_Execute.Unsupported_Value) and CMOS_Transfers = 0 and CMOS_Writes = 0 and CMOS_Selects = 0);
      Check (not Physical_Held and not Mux.Uncertain (IO).Present);
   else
      Check (Result.Status = AML_Execute.Returned and Result.Number = (if Mode = "write" then 16#A5# else 16#23#));
      Check (not Physical_Held and Begins = Ends and Begins > 0);
      Check (CMOS_Transfers = 1 and CMOS_Writes = (if Mode = "write" then 1 else 0));
   end if;
   Check (Hardware_Calls = 0 and Named_Effects = 0 and Pool_Effects = 0);
   Ada.Text_IO.Put_Line ("CMOS AML " & Mode & " checks" & Checks'Image & " PASS");
end CMOS_AML_Tests;
