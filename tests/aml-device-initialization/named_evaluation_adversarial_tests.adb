pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with AML_Decode;
with AML_Execute;
with AML_Table_Backing;
with AML_Delays;
with Firmware_Tables;
with ACPI_Service_Core;
procedure Named_Evaluation_Adversarial_Tests is
   procedure Clock (Value : out AML_Decode.Integer_Value; Available : out Boolean);
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Read_Microseconds => Clock,
      Retained_Result_Capacity => 4);
   package V renames C.Values;
   package N renames C.Namespace;
   subtype Bytes is Firmware_Tables.Bytes;
   use type Firmware_Tables.Bytes;
   use type V.Access_Status;
   use type V.Phase;
   use type V.Module_Outcome;
   use type V.Collection_Count;
   use type N.Load_Status;
   use type N.Lookup_Status;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Install_Status;
   use type AML_Execute.Execution_Status;
   Global_Service : aliased C.State (1, 4096, 4096, 1);
   Clock_Item : C.Node_Descriptor := V.No_Node;
   Callback_Status : V.Access_Status := V.Available;
   Callback_Charged : Natural := Natural'Last;
   Callback_Count : Natural := 0;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Message; end if; end Check;
   procedure Clock (Value : out AML_Decode.Integer_Value; Available : out Boolean) is
      Result : V.Result;
   begin
      C.Evaluate_Named (Global_Service, Clock_Item, 10, Result, Callback_Status);
      Callback_Charged := Result.Charged; Callback_Count := Callback_Count + 1;
      Value := 1; Available := True;
   end Clock;
   function Enc (Text : String) return Bytes is
      Result : Bytes (1 .. Text'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Result;
   end Enc;
   function String_Operand (Text : String) return Bytes is ([16#0D#] & Enc (Text) & [0]);
   function Pkg_Length (Value : Natural) return Bytes is
   begin
      if Value < 64 then return [Unsigned_8 (Value)]; end if;
      if Value < 4_096 then return [Unsigned_8 (16#40# + Value mod 16), Unsigned_8 (Value / 16)]; end if;
      if Value < 1_048_576 then
         return [Unsigned_8 (16#80# + Value mod 16), Unsigned_8 ((Value / 16) mod 256), Unsigned_8 (Value / 4_096)];
      end if;
      raise Program_Error with "fixture package too large";
   end Pkg_Length;
   function Block (Opcode, Payload : Bytes) return Bytes is
      Width : Positive := 1;
   begin
      loop
         declare
            Length : constant Bytes := Pkg_Length (Payload'Length + Width);
         begin
            if Length'Length = Width then return Opcode & Length & Payload; end if;
            Width := Length'Length;
         end;
      end loop;
   end Block;
   function Method (Name : String; Code : Bytes) return Bytes is
     (Block ([16#14#], Enc (Name) & [0] & Code));
   function Table (Signature : String; Revision : Unsigned_8; Payload : Bytes;
                   OEM : String := "OEM001"; Model : String := "TABLE001") return Bytes is
      Raw : Bytes (1 .. Firmware_Tables.Table_Header_Size + Payload'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Raw (1 .. 4) := Enc (Signature);
      for I in 1 .. 4 loop Raw (4 + I) := Unsigned_8 ((Raw'Length / 256 ** (I - 1)) mod 256); end loop;
      Raw (9) := Revision;
      Raw (11 .. 16) := Enc (OEM); Raw (17 .. 24) := Enc (Model);
      Raw (37 .. Raw'Last) := Payload;
      for Item of Raw loop Sum := Sum + Item; end loop;
      Raw (10) := 0 - Sum;
      return Raw;
   end Table;

   function Name (Part : String; Value : Bytes) return Bytes is ([16#08#] & Enc (Part) & Value);

   procedure Prepare (Service : aliased in out C.State; Code : Bytes; Revision : Unsigned_8 := 2) is
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Execution : C.Execution_Report;
      Access_Result : C.Values.Access_Status;
      Members : C.Namespace.Initialization_Report;
   begin
      C.Stage_Table (Service, 1, C.DSDT, Table ("DSDT", Revision, Code), Staged);
      Check (Staged = C.Staged, "stage");
      C.Freeze_Catalog (Service, Frozen);
      Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (Service, 1000, Execution, Access_Result);
      Check (Access_Result = C.Values.Available and then
        Execution.Modules.Outcome = C.Values.Modules_Completed, "execute");
      C.Initialize_Members (Service, Members, Access_Result);
      Check (Access_Result = C.Values.Available, "members");
      pragma Assert (C.Namespace_Load_Phase (Service) = C.Values.Ready);
   end Prepare;

   procedure Locate (Service : C.State; Part : String; Item : out C.Node_Descriptor) is
      Node : N.Node_ID;
      Status : V.Access_Status;
   begin
      C.Find_Child (Service, N.Root, Part, Node, Status);
      Check (Status = V.Available, "find " & Part);
      C.Describe_Node (Service, Node, Item, Status);
      Check (Status = V.Available, "describe " & Part);
   end Locate;
   function Integer_Code (Number : Natural) return Bytes is
   begin
      if Number <= 255 then return [16#0A#,Unsigned_8 (Number)]; end if;
      if Number <= 65535 then return [16#0B#,Unsigned_8 (Number mod 256),Unsigned_8 (Number / 256)]; end if;
      return [16#0C#,Unsigned_8 (Number mod 256),Unsigned_8 ((Number / 256) mod 256),
        Unsigned_8 ((Number / 65536) mod 256),Unsigned_8 (Number / 16777216)];
   end Integer_Code;
   function Buffer_Code (Length : Natural) return Bytes is
     (Block ([16#11#], Integer_Code (Length) & [16#A5#]));
begin
   Prepare (Global_Service, Name ("DATA", [1]) & Method ("BUSY", [16#A4#,16#5B#,16#33#]));
   Locate (Global_Service, "DATA", Clock_Item);
   declare
      Item : C.Node_Descriptor;
      Value : V.Result;
      Status : V.Access_Status;
      Other : aliased C.State (1, 4096, 4096, 1);
   begin
      Locate (Global_Service, "BUSY", Item);
      C.Evaluate_Named (Global_Service, Item, 100, Value, Status);
      Check (Status = V.Available and then Value.Status = AML_Execute.Returned
        and then Value.Number = 10, "outer real timer method");
      Check (Callback_Count = 1 and then Callback_Status = V.Busy and then Callback_Charged = 0,
        "reentry denied uncharged during actual invocation");
      C.Evaluate_Named (Global_Service, Clock_Item, 1, Value, Status);
      Check (Status = V.Available and then Value.Status = AML_Execute.Returned
        and then C.Retained_Results (Global_Service) = 0, "busy cleared after real callback");
      Prepare (Other, Name ("DATA", [1]));
      C.Evaluate_Named (Other, Clock_Item, 1, Value, Status);
      Check (Status = V.Invalid_Value and then Value.Charged = 0
        and then C.Retained_Results (Other) = 0, "foreign descriptor denied before allocation");
   end;
   declare
      A : V.Arena;
      Input : aliased AML_Table_Backing.State (1,1);
      Item : V.Node_Descriptor;
      Status : V.Access_Status;
      Loaded : N.Load_Status;
      Members : N.Initialization_Report;
      Value : V.Result;
   begin
      V.Initialize (A, Status); Check (Status = V.Available, "stale initialize");
      V.Load (A, Name ("DATA", [1]), AML_Decode.Bits_64, Loaded, Status);
      Check (Status = V.Available and then Loaded = N.Loaded, "stale load");
      V.Seal (A, Members, Status); Check (Status = V.Available, "stale seal");
      V.Describe_Node (A, 1, Item, Status); Check (Status = V.Available, "stale descriptor");
      V.Reset (A, Status); Check (Status = V.Available, "stale reset");
      V.Load (A, Name ("DATA", [1]), AML_Decode.Bits_64, Loaded, Status);
      Check (Status = V.Available and then Loaded = N.Loaded, "stale reload");
      V.Seal (A, Members, Status); Check (Status = V.Available, "stale reseal");
      V.Evaluate_Named (A, Input, Item, AML_Decode.Bits_64, 1, Value, Status);
      Check (Status = V.Invalid_Value and then Value.Charged = 0 and then V.Retained_Count (A) = 0,
        "same index after reset rejects stale descriptor");
   end;
   -- Static provider metadata is not access authority; no missing/default path.
   declare
      Service : aliased C.State (1, 4096, 4096, 1);
      Installed : C.Install_Status;
      Members : N.Initialization_Report;
      Status : V.Access_Status;
      Item : C.Node_Descriptor;
      Value : V.Result;
      Code : constant Bytes := [16#5B#,16#80#] & Enc ("REG0") & [0,0,1] &
        Block ([16#5B#,16#81#], Enc ("REG0") & [1] & Enc ("_STA") & Pkg_Length (8));
   begin
      C.Install (Service, 1, C.DSDT, Table ("DSDT", 2, Code), Installed);
      Check (Installed = C.Installed, "provider metadata install");
      C.Initialize_Members (Service, Members, Status); Check (Status = V.Available, "provider metadata seal");
      Locate (Service, "_STA", Item);
      C.Evaluate_Named (Service, Item, 1, Value, Status);
      Check (Status = V.Available and then Value.Status = AML_Execute.Unsupported_Value
        and then Value.Charged = 1 and then C.Retained_Results (Service) = 0,
        "unavailable provider is explicit read error not missing or zero");
   end;
   -- External named field and ordinary method field reads share exact width rules.
   for Revision in Unsigned_8 range 1 .. 2 loop
      declare
         Service : aliased C.State (1, 4096, 4096, 1);
         Item, Reader : C.Node_Descriptor;
         Value : V.Result;
         Status : V.Access_Status;
         Handle : V.Value_Handle;
         Data : Bytes (1 .. 5);
         Copied : Natural;
         Expected : constant Bytes := Enc ("OEM00");
         Number : Unsigned_64 := 0;
         Code : constant Bytes := [16#5B#,16#88#] & Enc ("REG0") & String_Operand ("DSDT") &
           String_Operand ("") & String_Operand ("") &
           Block ([16#5B#,16#81#], Enc ("REG0") & [1,0] & Pkg_Length (80) & Enc ("_STA") & Pkg_Length (40)) &
           Method ("READ", [16#A4#] & Enc ("_STA"));
      begin
         Prepare (Service, Code, Revision);
         Locate (Service, "_STA", Item); Locate (Service, "READ", Reader);
         for I in Expected'Range loop Number := Number + Unsigned_64 (Expected (I)) * 2 ** (8 * (I - 1)); end loop;
         for Via_Method in Boolean loop
            C.Evaluate_Named (Service, (if Via_Method then Reader else Item), 100, Value, Status);
            Check (Status = V.Available, "width evaluation access");
            if Revision = 1 then
               Check (Value.Status = AML_Execute.Object_Returned, "40-bit field is buffer in revision1");
               Handle := Value.Handle;
               C.Read_Result_Bytes (Service, Handle, 0, Data, Copied, Status);
               Check (Status = V.Available and then Copied = 5 and then Data = Expected, "full width buffer");
               C.Release_Result (Service, Handle, Status); Check (Status = V.Available, "width release");
            else
               Check (Value.Status = AML_Execute.Returned and then Value.Number = Number,
                 "40-bit field integer in revision2");
            end if;
            Check (C.Retained_Results (Service) = 0, "width result cleanup");
         end loop;
      end;
   end loop;
   -- Hosted-constructed AML, with real collection and full byte comparisons.
   declare
      A : V.Arena;
      Code : constant Bytes := Name ("B000", Buffer_Code (16384)) &
        Name ("B001", Buffer_Code (16384)) & Name ("B002", Buffer_Code (16384)) &
        [16#5B#,16#13#] & Enc ("B000") & [0] & Integer_Code (65536) & Enc ("WIDE") &
        Method ("ALOC", [16#A4#] & Buffer_Code (16384));
      Raw : constant Bytes := Table ("DSDT", 2, Code);
      Input : aliased AML_Table_Backing.State (1, Raw'Length);
      Modules : V.Module_Report;
      Members : N.Initialization_Report;
      Status : V.Access_Status;
      Found : N.Lookup_Result;
      Wide, Allocator : V.Node_Descriptor;
      Value : V.Result;
      Held : V.Value_Handle;
      Before, After : V.Collection_Statistics;
      procedure Check_Bytes (Handle : V.Value_Handle; Length : Positive) is
         Data : Bytes (1 .. Length);
         Copied : Natural;
      begin
         V.Read_Bytes (A, Handle, 0, Data, Copied, Status);
         Check (Status = V.Available and then Copied = Length, "pressure full read");
         Check (Data (1) = 16#A5# and then
           (for all I in 2 .. Data'Last => Data (I) = 0), "pressure complete content");
      end Check_Bytes;
   begin
      Input.Count := 1; Input.Tables (1) := (0, Raw'Length); Input.Data.Bytes := Raw;
      V.Initialize (A, Status); Check (Status = V.Available, "pressure initialize");
      V.Execute_Modules (A, Input, 1000, Modules, Status);
      Check (Status = V.Available and then Modules.Outcome = V.Modules_Completed, "pressure module");
      V.Seal_With_Tables (A, Input, Members, Status); Check (Status = V.Available, "pressure seal");
      V.Find_Object (A, N.Root, "WIDE", Found, Status);
      Check (Status = V.Available and then Found.Status = N.Found, "pressure find field");
      V.Describe_Node (A, Found.Node, Wide, Status); Check (Status = V.Available, "pressure describe field");
      V.Find_Object (A, N.Root, "ALOC", Found, Status);
      Check (Status = V.Available and then Found.Status = N.Found, "pressure find allocator");
      V.Describe_Node (A, Found.Node, Allocator, Status); Check (Status = V.Available, "pressure describe allocator");
      V.Evaluate_Named (A, Input, Wide, AML_Decode.Bits_64, 1, Value, Status);
      Check (Status = V.Available and then Value.Status = AML_Execute.Object_Returned, "pressure first wide");
      Held := Value.Handle; Check_Bytes (Held, 8192);
      V.Release (A, Held, Status); Check (Status = V.Available, "pressure release first wide");
      Before := V.Reclamation_Metrics (A);
      V.Evaluate_Named (A, Input, Allocator, AML_Decode.Bits_64, 100, Value, Status);
      Check (Status = V.Available and then Value.Status = AML_Execute.Object_Returned, "pressure allocate after collection");
      Held := Value.Handle; Check_Bytes (Held, 16384); After := V.Reclamation_Metrics (A);
      Check (After.Completed > Before.Completed and then After.Freed_Bytes >= Before.Freed_Bytes + 8192,
        "pressure collection actually reclaimed wide result");
      V.Evaluate_Named (A, Input, Wide, AML_Decode.Bits_64, 1, Value, Status);
      Check (Status = V.Available and then Value.Status = AML_Execute.Value_Limit and then
        V.Retained_Count (A) = 1, "pressure failure releases temporary reservation");
      Check_Bytes (Held, 16384);
      V.Release (A, Held, Status); Check (Status = V.Available, "pressure release allocator");
      Before := V.Reclamation_Metrics (A);
      V.Evaluate_Named (A, Input, Wide, AML_Decode.Bits_64, 1, Value, Status);
      Check (Status = V.Available and then Value.Status = AML_Execute.Object_Returned, "pressure recovery clears busy");
      Held := Value.Handle; Check_Bytes (Held, 8192); After := V.Reclamation_Metrics (A);
      Check (After.Completed > Before.Completed and then After.Freed_Bytes >= Before.Freed_Bytes + 16384,
        "pressure recovery actual collection");
      V.Release (A, Held, Status);
      Check (Status = V.Available and then V.Retained_Count (A) = 0, "pressure final cleanup");
   end;
   Ada.Text_IO.Put_Line ("named evaluation adversarial checks" & Checks'Image);
end Named_Evaluation_Adversarial_Tests;
