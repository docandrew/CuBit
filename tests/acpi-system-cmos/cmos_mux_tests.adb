pragma Ada_2022;
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
procedure CMOS_Mux_Tests is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package F renames ACPI_Firmware_Data_Layouts;
   package B renames ACPI_Backend_Binding_Types;
   subtype Word is Unsigned_64;
   use type A.Target,A.Observation,A.Completion,AML_Identity.Identity,B.Outcome;
   Base : constant Word := 16#FED0_0000#;
   VA : constant Word := 16#6000_0000_0000#;
   Scope : constant Word := 16#F123_4567_0000_0001#;
   Mode : constant String := Ada.Command_Line.Argument (1);
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
   Result : Records.Outcome;
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
      CMOS_Transfers := CMOS_Transfers + 1; Output := Word (Cells (Selected));
      Outcome := (if Mode = "cmos-unknown" then CT.Unknown else CT.Completed);
      if Mode = "cmos-reentry" then Nested; end if;
   end CMOS_Read;
   procedure CMOS_Write (Held : Natural; Port : Unsigned_16; Input : CT.I.Byte; Outcome : out CT.Completion) is
   begin
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
   IO,Foreign : Mux.Context;
   Named_Target,Pool_Target,T : A.Target;
   Geometry : L.Register_Geometry := (Region => (L.Memory_Space,Base,8192,(Known => False)),others => <>);
   Back : L.Register_Geometry;
   Reply : A.Response;
   Bound : B.Report;
   Added : Catalog.Add_Report;
   OK : Boolean;
   use type Catalog.Add_Code;
   Before : Natural;
   procedure Nested is
      Good : Boolean;
      Target : A.Target;
      R : A.Response;
   begin
      Mux.Resolve (IO,(null record),Geometry,100,Target,Good); Check (not Good and Target = A.No_Target);
      Mux.Dispatch (IO,(Named_Target,False,0),R); Check (R.Status = A.Denied);
      Mux.Close (IO); Check (Mux.Pending (IO));
   end Nested;
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
   Records.Configure (Backing,Scope,Result); Check (Result = Records.Configured);
   Records.Seal (Backing,Result); Check (Result = Records.Sealed);
   Mux.Initialize (IO,16#F000_0000_0000_0001#,OK); Check (OK);
   declare
      EC_Geometry : constant L.Register_Geometry :=
        ((L.Embedded_Control_Space,0,256,(Known => False)),16,F.T.Byte_Access);
      EC_Target : A.Target;
   begin
      Mux.Resolve (IO,(null record),EC_Geometry,100,EC_Target,OK); Check (OK);
      Mux.Describe (IO,EC_Target,Back,OK);
      Check (OK and then Back.Region.Length = 256 and then Back.Byte_Offset = 16);
      Mux.Authorize (IO,EC_Target,True,OK); Check (not OK);
      Check (Hardware_Calls = 0);
   end;
   declare
      CMOS_Geometry : constant L.Register_Geometry :=
        ((L.System_CMOS_Space,0,63,(Known => False)),4,F.T.Byte_Access);
      CMOS_Target : A.Target;
   begin
      Mux.Resolve (IO,(null record),CMOS_Geometry,100,CMOS_Target,OK);
      Check (OK = (Mode /= "no-cmos"));
      if Mode /= "no-cmos" then
      Mux.Describe (IO,CMOS_Target,Back,OK); Check (OK and Back.Region.Length = 64);
      Mux.Dispatch (IO,(CMOS_Target,False,0),Reply);
      if Mode = "cmos-unknown" then
         Check (Reply.Status = A.Indeterminate and Mux.Pending (IO) and Mux.Uncertain (IO).Present);
         declare Receipt : constant A.Observation := Mux.Uncertain (IO); Count : constant Natural := CMOS_Transfers; begin
            Mux.Close (IO); Check (Mux.Uncertain (IO) = Receipt and Mux.Pending (IO));
            Mux.Reconciled (IO,Receipt,OK); Check (not OK);
            Mux.Dispatch (IO,(CMOS_Target,False,0),Reply); Check (Reply.Status = A.Denied and CMOS_Transfers = Count);
         end;
         Ada.Text_IO.Put_Line ("CMOS MUX " & Mode & " checks" & Checks'Image & " PASS"); return;
      end if;
      Check (Reply.Status = A.Completed and Reply.Value = 16#23# and CMOS_Transfers = 1);
      Mux.Authorize (IO,CMOS_Target,True,OK); Check (not OK);
      declare Writable : constant L.Register_Geometry := ((L.System_CMOS_Space,20,1,(Known => False)),0,F.T.Byte_Access); begin
         Mux.Select_Target (IO,Writable,True,T,OK); Check (OK);
         Mux.Dispatch (IO,(T,True,16#A5#),Reply); Check (Reply.Status = A.Completed and Cells (20) = 16#A5# and CMOS_Writes = 1);
      end;
      end if;
   end;
   Geometry.Region.Length := 1; -- exact ordinary descriptor on this named base
   Mux.Resolve (IO,(null record),Geometry,100,Named_Target,OK);
   if Mode = "ambiguous" then
      Check (not OK and Map_Calls = 0 and Validations = 0);
   else
      Check (OK and Named_Target.Provider = Mux.Identity (IO) and Map_Calls = 0 and Validations = 0);
      Mux.Authorize (IO,Named_Target,True,OK); Check (not OK);
      Mux.Select_Target (IO,Geometry,True,T,OK); Check (not OK and Map_Calls = 0);
      Mux.Dispatch (IO,(Named_Target,False,0),Reply); Check (Reply.Status = A.Completed and Reply.Value = Value and Named_Effects = 1);
      -- A wider alias must not escape a named read-only byte through the pool.
      Geometry.Width := F.T.Word_Access;
      Mux.Resolve (IO,(null record),Geometry,100,T,OK); Check (not OK and Map_Calls = 0 and Validations = 0);
      Geometry.Width := F.T.Byte_Access; Geometry.Region.Length := 8192; Geometry.Byte_Offset := 1;
      Mux.Resolve (IO,(null record),Geometry,100,Pool_Target,OK);
      if Mode = "no-pool" then Check (not OK and Mux.Is_Ready (IO) and not Mux.Pool_Available (IO));
      else
         Check (OK and Map_Calls = 1 and Pool_Target.Provider = Named_Target.Provider);
         Mux.Export_Target (IO,Mux.Register_Count (IO),T,OK); Check (OK and T = Pool_Target);
         Mux.Initialize (Foreign,77,OK); Check (OK);
         Mux.Dispatch (Foreign,(Pool_Target,False,0),Reply); Check (Reply.Status = A.Denied and Pool_Effects = 0);
         Mux.Dispatch (IO,(Pool_Target,False,0),Reply);
         if Mode = "unknown" then
            Check (Reply.Status = A.Indeterminate and Mux.Uncertain (IO).Present and Mux.Pending (IO));
            Before := Pool_Effects; Mux.Dispatch (IO,(Named_Target,False,0),Reply); Check (Reply.Status = A.Denied and Pool_Effects = Before);
         else
            Check (Reply.Status = A.Completed and Pool_Effects = 1);
            Mux.Close (IO); Mux.Dispatch (IO,(Named_Target,False,0),Reply); Check (Reply.Status = A.Denied and Named_Effects = 1);
         end if;
      end if;
   end if;
   Ada.Text_IO.Put_Line ("CMOS MUX " & Mode & " checks" & Checks'Image & " PASS");
end CMOS_Mux_Tests;
