pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with AML_Execute;
with ACPI_Service; use ACPI_Service;
with ACPI_Service_Core;
with AML_Delays;
with Firmware_Tables;
procedure Module_Core_Tests is
   subtype Bytes is Firmware_Tables.Bytes;
   use type Firmware_Tables.Bytes;
   use type Values.Access_Status;
   use type Values.Module_Outcome;
   use type Values.Preparation_Outcome;
   use type Values.Audit_Value;
   use type Namespace.Bind_Status;
   use type AML_Execute.Execution_Status;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Message; end if; end Check;
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
   function Field (Region_Name, Name : String; Offset, Bits : Natural) return Bytes is
     (Block ([16#5B#, 16#81#], Enc (Region_Name) & [1] &
       (if Offset = 0 then Bytes'[1 .. 0 => 0] else Bytes'[0] & Pkg_Length (Offset)) & Enc (Name) & Pkg_Length (Bits)));
   function Method (Name : String; Code : Bytes) return Bytes is
     (Block ([16#14#], Enc (Name) & [0] & Code));
   function Read_Method (Name, Target : String) return Bytes is
     (Method (Name, [16#A4#] & Enc (Target)));
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
   procedure Stage (Service : in out State; ID : Positive; Kind : Table_Kind; Data : Bytes) is
      Result : Stage_Status;
   begin
      Stage_Table (Service, ID, Kind, Data, Result);
      Check (Result = Staged, "stage");
   end Stage;
   procedure Expect (Service : aliased in out State; Part : String; Expected : Unsigned_64) is
      Node : Namespace.Node_ID;
      Access_Result : Values.Access_Status;
      Result : AML_Execute.Execution_Result;
   begin
      Find_Child (Service, Namespace.Root, Part, Node, Access_Result);
      Check (Access_Result = Values.Available and then Node /= Namespace.Root, "find " & Part);
      Invoke_Scalar (Service, Node, [others => 0], 0, 1000, Result);
      Check (Result.Status = AML_Execute.Returned and then Result.Value = Expected, "invoke " & Part);
   end Expect;
   procedure Check_Bytes (Service : State; Index : Positive; Expected : Bytes) is
   begin
      Check (Table_Info (Service, Index).Extent = Expected'Length, "catalog extent");
      Check ((for all I in Expected'Range => Table_Byte (Service, Index, I - Expected'First) = Expected (I)), "catalog bytes");
   end Check_Bytes;
begin
   for Revision in Unsigned_8 range 1 .. 2 loop
      declare
         Service : aliased State (3, 8192, 4096, Load_Capacity => 3);
         Access_Result : Values.Access_Status;
         Report : Execution_Report;
         Initialization : Namespace.Initialization_Report;
         Frozen : Freeze_Status;
         Rejected : Stage_Status;
         Installed_Result : Install_Status;
         Node : Namespace.Node_ID;
         Bound : Namespace.Bind_Status;
         Code : constant Bytes := Name ("CNT0", [0]) & Name ("SEEN", [0]) &
           Method ("SSEL", [16#75#] & Enc ("CNT0") & [16#A4#] & String_Operand ("SSDT")) &
           [16#5B#,16#88#] & Enc ("REG0") & Enc ("SSEL") & String_Operand ("") & String_Operand ("") &
           Field ("REG0", "FLD0", 0, 32) & [16#06#] & Enc ("FLD0") & Enc ("ALS0") &
           [16#70#] & Enc ("CNT0") & Enc ("SEEN") & Read_Method ("READ", "ALS0") &
           Read_Method ("RCNT", "CNT0") & Read_Method ("RSEE", "SEEN") & Method ("WIDE", [16#A4#,16#FF#]);
         First : constant Bytes := Table ("DSDT", Revision, Code);
         Later : constant Bytes := Table ("SSDT", Revision, [16#70#,16#0A#,42] & Enc ("CNT0"));
      begin
         declare Before : constant Values.Audit_Value := Audit (Service); begin
            Execute_Frozen (Service, 2000, Report, Access_Result);
            Check (Access_Result = Values.Wrong_Phase
              and then Report.Phase = Execution_Not_Started
              and then Report.Preparation = Values.Preparation_Not_Attempted
              and then Audit (Service) = Before and then Current_Import_Phase (Service) = Undecided, "undecided execute unchanged");
         end;
         Initialize_Members (Service, Initialization, Access_Result);
         Check (Access_Result = Values.Wrong_Phase, "uninitialized initialization denied");
         Stage_Table (Service, 87, Description, Table ("TEST", 2, []), Rejected);
         Check (Rejected = Stage_Wrong_Order and then Table_Count (Service) = 0, "no DSDT description denied");
         Stage_Table (Service, 87, SSDT, Later, Rejected);
         Check (Rejected = Stage_Wrong_Order, "SSDT before DSDT denied");
         Stage (Service, 501, DSDT, First);
         Execute_Frozen (Service, 2000, Report, Access_Result);
         Check (Access_Result = Values.Wrong_Phase and then Current_Import_Phase (Service) = Catalog_Staging, "staging execute denied");
         Stage (Service, 9001, SSDT, Later);
         Freeze_Catalog (Service, Frozen); Check (Frozen = Catalog_Frozen_OK, "freeze");
         Initialize_Members (Service, Initialization, Access_Result);
         Check (Access_Result = Values.Wrong_Phase, "frozen initialization denied");
         Execute_Frozen (Service, 2000, Report, Access_Result);
         Check (Access_Result = Values.Available
        and then Report.Phase = Module_Execution_Reported
        and then Report.Preparation = Values.Firmware_Prepared and then Report.Modules.Outcome = Values.Modules_Completed and then Report.Modules.Completed = 2 and then Report.Failed_Table_ID = 0 and then Current_Import_Phase (Service) = Catalog_Executed, "execute frozen");
         Check (Table_Info (Service, 1).ID = 501 and then Table_Info (Service, 2).ID = 9001, "catalog IDs preserved");
         Check_Bytes (Service, 1, First); Check_Bytes (Service, 2, Later);
         declare Before : constant Values.Audit_Value := Audit (Service); begin
            Execute_Frozen (Service, 2000, Report, Access_Result);
            Check (Access_Result = Values.Wrong_Phase
              and then Report.Phase = Execution_Not_Started
              and then Report.Preparation = Values.Preparation_Not_Attempted
              and then Audit (Service) = Before, "execution exactly once");
            Declare_Table_Region (Service, Namespace.Root, "BADR", (Name => "SSDT", others => <>), Node, Bound);
            Check (Bound = Namespace.Binding_Invalid and then Audit (Service) = Before, "executed direct declarations denied");
            Install (Service, 13, Description, Table ("TEST", 2, []), Installed_Result);
            Check (Installed_Result = Wrong_Order and then Audit (Service) = Before, "legacy mixing denied");
         end;
         Initialize_Members (Service, Initialization, Access_Result);
         Check (Access_Result = Values.Available and then Current_Import_Phase (Service) = Catalog_Executed, "seal executed keeps terminal phase");
         Expect (Service, "READ", 16#54445353#); Expect (Service, "RCNT", 42); Expect (Service, "RSEE", 1);
         Expect (Service, "WIDE", (if Revision = 1 then 16#FFFF_FFFF# else Unsigned_64'Last));
      end;
   end loop;
   declare
      Service : State (2, 4096, 2048, Load_Capacity => 2);
      Access_Result : Values.Access_Status;
      Report : Execution_Report;
      Frozen : Freeze_Status;
      Initialization : Namespace.Initialization_Report;
      Node : Namespace.Node_ID;
      Bound : Namespace.Bind_Status;
      Staged_Result : Stage_Status;
      Installed_Result : Install_Status;
      First : constant Bytes := Table ("DSDT", 2, Name ("KEEP", [1]));
      Later : constant Bytes := Table ("SSDT", 2, Name ("DROP", [1]) & [16#A4#,1]);
   begin
      Stage (Service, 71, DSDT, First); Stage (Service, 4097, SSDT, Later);
      Freeze_Catalog (Service, Frozen); Check (Frozen = Catalog_Frozen_OK, "failure freeze");
      Execute_Frozen (Service, 1000, Report, Access_Result);
      Check (Access_Result = Values.Available
        and then Report.Phase = Module_Execution_Reported
        and then Report.Preparation = Values.Firmware_Prepared and then Report.Modules.Outcome = Values.Module_Rejected and then Report.Modules.Completed = 1 and then Report.Modules.Failed_Table = 2 and then Report.Failed_Table_ID = 4097 and then Current_Import_Phase (Service) = Catalog_Failed, "failed module maps actual ID");
      Check (Observe (Service).Objects = Values.Firmware_Seed_Count + 1, "prior module retained and failed module rolled back");
      Check_Bytes (Service, 1, First); Check_Bytes (Service, 2, Later);
      declare Before : constant Values.Audit_Value := Audit (Service); begin
         Execute_Frozen (Service, 1000, Report, Access_Result);
         Check (Access_Result = Values.Wrong_Phase
              and then Report.Phase = Execution_Not_Started
              and then Report.Preparation = Values.Preparation_Not_Attempted
              and then Audit (Service) = Before, "failed execution cannot retry");
         Initialize_Members (Service, Initialization, Access_Result);
         Check (Access_Result = Values.Wrong_Phase
              and then Report.Phase = Execution_Not_Started
              and then Report.Preparation = Values.Preparation_Not_Attempted
              and then Audit (Service) = Before, "failed catalog cannot seal");
         Declare_Table_Region (Service, Namespace.Root, "BADR", (Name => "SSDT", others => <>), Node, Bound);
         Check (Bound = Namespace.Binding_Invalid and then Node = Namespace.Root and then Audit (Service) = Before, "failed direct region denied");
         Declare_Table_Field (Service, Namespace.Root, "BADF", Namespace.Root, 0, 8, Node, Bound);
         Check (Bound = Namespace.Binding_Invalid and then Node = Namespace.Root and then Audit (Service) = Before, "failed direct field denied");
         Stage_Table (Service, 88, Description, Table ("TEST", 2, []), Staged_Result);
         Check (Staged_Result = Stage_Wrong_Order and then Audit (Service) = Before, "failed stage denied");
         Install (Service, 88, Description, Table ("TEST", 2, []), Installed_Result);
         Check (Installed_Result = Wrong_Order and then Audit (Service) = Before and then Current_Import_Phase (Service) = Catalog_Failed, "failed legacy install denied");
         Check_Bytes (Service, 1, First); Check_Bytes (Service, 2, Later);
      end;
   end;
   declare
      package Small is new ACPI_Service_Core
        (AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 5);
      use type Small.Stage_Status;
      use type Small.Freeze_Status;
      use type Small.Values.Access_Status;
      use type Small.Values.Preparation_Outcome;
      use type Small.Values.Module_Report;
      use type Small.Execution_Phase_Result;
      use type Small.Import_Phase;
      use type Small.State_Model;
      Service : Small.State (1, 4096, 4096, 1);
      Staged_Result : Small.Stage_Status;
      Frozen : Small.Freeze_Status;
      Access_Result : Small.Values.Access_Status;
      Report : Small.Execution_Report;
      Raw : constant Bytes := Table ("DSDT", 2, []);
   begin
      Small.Stage_Table (Service, 501, Small.DSDT, Raw, Staged_Result);
      Check (Staged_Result = Small.Staged and then Small.Observe (Service).Objects = 0,
        "small namespace stages empty catalog normally");
      Small.Freeze_Catalog (Service, Frozen);
      Check (Frozen = Small.Catalog_Frozen_OK and then Small.Observe (Service).Objects = 0,
        "small namespace freezes without seeds");
      Small.Execute_Frozen (Service, 100, Report, Access_Result);
      Check (Access_Result = Small.Values.Available
        and then Report.Phase = Small.Preparation_Failed
        and then Report.Preparation = Small.Values.Namespace_Capacity_Exceeded
        and then Report.Modules = Small.Values.Module_Report'(others => <>)
        and then Report.Failed_Table_ID = 0
        and then Small.Observe (Service).Objects = 0
        and then Small.Observe (Service).Value_Objects = 0
        and then Small.Current_Import_Phase (Service) = Small.Catalog_Failed,
        "preparation quota is typed atomic failure without AML execution");
      Check ((for all I in Raw'Range => Small.Table_Byte (Service, 1, I - Raw'First) = Raw (I)),
        "preparation failure preserves admitted bytes");
      declare Before : constant Small.State_Model := Small.Model (Service) with Ghost; begin
         Small.Execute_Frozen (Service, 100, Report, Access_Result);
         Check (Access_Result = Small.Values.Wrong_Phase
           and then Report.Phase = Small.Execution_Not_Started
           and then Report.Preparation = Small.Values.Preparation_Not_Attempted
           and then Small.Current_Import_Phase (Service) = Small.Catalog_Failed
           and then Small.Observe (Service).Objects = 0, "preparation failure cannot replay");
         pragma Assert (Small.Model (Service) = Before);
      end;
   end;
   Ada.Text_IO.Put_Line ("AML module Core checks:" & Checks'Image);
end Module_Core_Tests;
