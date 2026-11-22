pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with AML_Objects;
with AML_Decode; use AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_Firmware_Data_Frames;
with ACPI_Firmware_Data_Startup;
with ACPI_Firmware_Data_Transport;
with ACPI_Field_Startup_Owner;
with Locked_Test_Core;
with Locked_Owner_Model;
with Root_Test_Support; use Root_Test_Support;
procedure Firmware_Owner_Tests is
   package C renames Locked_Test_Core;
   package M renames Locked_Owner_Model;
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package F renames ACPI_Firmware_Data_Frames;
   type Scenario is (Normal,Begin_Refused,Unknown_Read,End_Refused,Unknown_Publication,NoLock_Normal);
   Choice : constant Scenario := Scenario'Val (Natural'Value (Ada.Command_Line.Argument (2)));
   function Locked_Field return Boolean is (Choice /= NoLock_Normal);
   Checks, Starts : Natural := 0;
   Publishes : Natural renames M.Data_Publishes;
   Begins : Natural renames M.Data_Begins;
   Ends : Natural renames M.Data_Ends;
   Reads : Natural renames M.Data_Reads;
   use type F.IPC.Time_Instant;
   use type Interfaces.Unsigned_32, Interfaces.Unsigned_64;
   use type Bytes, Byte;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status, C.Namespace.Region_Space_ID;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   function Original (Item : C.Namespace.Operation_Region_Attributes; Geometry : L.Register_Geometry) return Boolean is
      (Item.Origin.Known and then C.Namespace.Has_Definition (Item.Origin.Definition)
       and then Item.Syntax.Address_Is_Literal and then Item.Syntax.Length_Is_Literal
       and then Item.Space = 0 and then Item.Address = Geometry.Region.Base and then Item.Length = Geometry.Region.Length);
   type Channel is limited record Target : M.Peer := 0; Backend : M.Channel_State; end record;
   procedure Initialize_Link (Connection : in out Channel; Target : M.Peer; Accepted : out Boolean) is
      use type M.Peer;
   begin
      Check (Connection.Target = 0 and then Target = M.Destination);
      Connection.Target := Target; Accepted := True;
   end Initialize_Link;
   function Present (Item : Interfaces.Unsigned_64) return Boolean is
      (Item /= 0);
   procedure Dispose (Item : in out Interfaces.Unsigned_64; Closed : out Boolean) is
   begin Check (Item = 0); Closed := True; end Dispose;
   procedure Exchange_Data (Connection : in out Channel; Deadline : F.IPC.Time_Instant;
      Request : F.IPC.Packet; Reply : out F.IPC.Packet; Received : out Interfaces.Unsigned_64;
      Delivered : out F.IPC.Delivery) is
   begin
      Received := 0;
      M.Data_Exchange (Connection.Backend,Connection.Target,Deadline,Request,Reply,Delivered);
   end Exchange_Data;
   package RPC is new ACPI_Firmware_Data_Transport
      (Channel,Interfaces.Unsigned_64,0,Present,M.IPC_Clock,Exchange_Data,Dispose);
   package Firmware is new ACPI_Firmware_Data_Startup
      (M.Peer,0,Channel,RPC,Initialize_Link,M.IPC_Clock,C.Namespace.Operation_Region_Attributes,Original);
   procedure Initialize_FW (Destination : M.Peer; Symbol : A.Endpoint_ID;
      Deadline : M.W.Time_Instant; Accepted : in out Boolean) is
   begin Starts := Starts + 1; Firmware.Initialize (Destination,Symbol,Deadline,Accepted); end Initialize_FW;
   package Owner is new ACPI_Field_Startup_Owner
      (C,M.Channel_State,M.Peer,0,M.IPC_Clock,M.Exchange,
       Maximum_Registers => 2,Maximum_Bindings => 4,Maximum_Confirmations => 2,
       Read_Lease_Clock => M.Lease_Clock,Enable_Firmware_Data => True,
       Initialize_Firmware => Initialize_FW,Inspect_Firmware => Firmware.Inspect,
       Describe_Firmware => Firmware.Describe,Authorize_Firmware => Firmware.Authorize,Dispatch_Firmware => Firmware.Dispatch,
       Resolve_Firmware => Firmware.Resolve,Begin_Firmware => Firmware.Begin_Field,End_Firmware => Firmware.End_Field,Cleanup_Firmware => Firmware.Cleanup);
   function Table return Bytes is
      Code : constant Bytes := [16#5B#,16#80#] & Enc ("LFCN") & [0,16#0B#,0,16#50#,16#0A#,2]
         & Block ([16#5B#,16#81#],Enc ("LFCN") & [Byte'(if Locked_Field then 16#11# else 1)] & Enc ("FLD0") & [8])
         & [8] & Enc ("BOOT") & [0] & [16#70#] & Enc ("FLD0") & Enc ("BOOT")
         & Method ("READ",[16#A4#] & Enc ("FLD0"));
      Data : Bytes (1 .. 36 + Code'Length) := [others => 0]; Sum : Byte := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); Data (9) := 2;
      for I in 0 .. 3 loop Data (5 + I) := Byte ((Data'Length / 256 ** I) mod 256); end loop;
      Data (37 .. Data'Last) := Code; for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
   Service : aliased C.State (1,4096,4096,1,AML_Objects.Max_Objects,AML_Objects.Max_Bytes,AML_Objects.Max_Elements);
   Staged : C.Stage_Status; Frozen : C.Freeze_Status; Status : C.Values.Access_Status;
   Report : C.Execution_Report; Result : C.Values.Result; Good : Boolean;
   Found : C.Namespace.Lookup_Result; Node : C.Node_Descriptor;
   Members : C.Namespace.Initialization_Report;
   Before : Natural;
begin
   Check (M.Mode = 0); M.Configure;
   Owner.Begin_Startup (M.Destination,1_000,Good); Check (Good and then Starts = 1);
   C.Stage_Table (Service,1,C.DSDT,Table,Staged); Check (Staged = C.Staged);
   C.Freeze_Catalog (Service,Frozen); Check (Frozen = C.Catalog_Frozen_OK);
   Owner.Execute_Module_Startup (Service,10_000,Report,Status);
   Ada.Text_IO.Put_Line ("FW owner " & Choice'Image & " " & Report.Modules.Outcome'Image
      & " publish" & Publishes'Image & " begin" & Begins'Image & " read" & Reads'Image
      & " end" & Ends'Image & " acquire" & M.Acquires'Image & " release" & M.Releases'Image);
   Check (Status = C.Values.Available and then Publishes = 1 and then M.Loads = 0 and then M.Legacy_Reads = 0);
   if Choice in Normal | NoLock_Normal then
      Check (Report.Modules.Outcome = C.Values.Modules_Completed and then Begins = 1 and then Reads = 1 and then Ends = 1);
      C.Find_Object (Service,C.Namespace.Root,"BOOT",Found,Status);
      C.Observe_Named_Value (Service,Found.Node,Result,Status);
      Check (Status = C.Values.Available and then Result.Number = 16#5A#);
      C.Initialize_Members (Service,Members,Status); Check (Status = C.Values.Available);
      Owner.Prepare_Fields (Service,Good); Check (Good);
      C.Find_Object (Service,C.Namespace.Root,"READ",Found,Status); C.Describe_Node (Service,Found.Node,Node,Status);
      Owner.Evaluate_Named (Service,Node,1_000,Result,Status);
      Check (Status = C.Values.Available and then Result.Status = AML_Execute.Returned and then Result.Number = 16#5A#);
      Check (Reads = 2 and then Publishes = 1 and then M.Acquires = (if Locked_Field then 2 else 0) and then M.Acquires = M.Releases);
   else
      Check (Report.Modules.Outcome = C.Values.Module_Rejected);
      Check (Reads = (if Choice in Unknown_Read | End_Refused then 1 else 0));
      Check (M.Acquires = (if Choice = Unknown_Publication then 0 else 1) and then M.Acquires = M.Releases);
      if Choice = Unknown_Read then Check (Owner.Uncertain.Present); end if;
   end if;
   Check (M.Word = 0 and then M.Acquires = M.Releases);
   Before := Reads; Owner.Execute_Module_Startup (Service,10_000,Report,Status);
   Check (Status /= C.Values.Available and then Reads = Before);
   Owner.Close_Startup; Check (M.Word = 0 and then M.Acquires = M.Releases and then Reads = Before);
   Ada.Text_IO.Put_Line ("Concrete-session startup owner checks" & Checks'Image);
end Firmware_Owner_Tests;
