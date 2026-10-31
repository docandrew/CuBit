with Ada.Text_IO;
with ACPI_Bootstrap; use ACPI_Bootstrap;
with ACPI_Service;
with Firmware_Tables;
with Migration_Tables;
procedure Bootstrap_Catalog_Tests is
   subtype Bytes is Firmware_Tables.Bytes;
   use type Bytes;
   use type Firmware_Tables.Byte;
   use type ACPI_Service.Install_Status;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   function Enc (Text : String) return Bytes is
      Value : Bytes (1 .. Text'Length);
   begin
      for I in Value'Range loop Value (I) := Character'Pos (Text (Text'First + (I - 1))); end loop;
      return Value;
   end Enc;
   function Str (Text : String) return Bytes is ([16#0D#] & Enc (Text) & [0]);
   function Name (Text : String; Value : Bytes) return Bytes is ([8] & Enc (Text) & Value);
   function Method (Text : String; Code : Bytes) return Bytes is
   begin
      if Code'Length > 57 then raise Program_Error; end if;
      return [16#14#, Firmware_Tables.Byte (Code'Length + 6)] & Enc (Text) & [0] & Code;
   end Method;
   -- Fail the whole module if the comparison is unequal. Root Return is forbidden.
   function Require_Equal (Left, Right : Bytes) return Bytes is
      Predicate : constant Bytes := [16#92#, 16#93#] & Left & Right;
   begin
      if Predicate'Length > 60 then raise Program_Error; end if;
      return [16#A0#, Firmware_Tables.Byte (Predicate'Length + 3)] & Predicate & [16#A4#, 0];
   end Require_Equal;
   procedure Import (S : in out State; ID : Positive; Kind : ACPI_Service.Table_Kind; Data : Bytes) is
      Result : ACPI_Service.Install_Status;
   begin Import_Table (S, ID, Kind, Data, Result); Check (Result = ACPI_Service.Installed); end Import;
   Prefix : constant Bytes := Migration_Tables.Make (Name ("KEEP", [1]));
   Later_Good : constant Bytes := Migration_Tables.Make (Name ("NEXT", [1]), True);
   Later_Bad : constant Bytes := Migration_Tables.Make (Name ("DROP", [1]) & [16#A4#, 0], True);
   Prefix_Fuel : Natural;
begin
   declare
      Control : State (1, 256, 128, Load_Capacity => 1);
      Accepted : Boolean;
   begin
      Start (Control, 1, Accepted); Check (Accepted);
      Import (Control, 7, ACPI_Service.DSDT, Prefix);
      Finish (Control);
      Check (Current (Control) = Complete and Observe (Control).Objects = ACPI_Service.Values.Firmware_Seed_Count + 1);
      Prefix_Fuel := Completion (Control).Execution.Modules.Charged;
      Check (Prefix_Fuel > 0 and Prefix_Fuel <= Default_Startup_Budget);
   end;
   for Exhausted in Boolean loop
      declare
         S : State (2, 256, 128, Load_Capacity => 2);
         Accepted : Boolean;
         Later : constant Bytes := (if Exhausted then Later_Good else Later_Bad);
      begin
         Start (S, 2, Accepted); Check (Accepted);
         Import (S, 7, ACPI_Service.DSDT, Prefix);
         Import (S, 901, ACPI_Service.SSDT, Later);
         Check (Observe (S).Objects = 0);
         Finish (S, (if Exhausted then Startup_Budget (Prefix_Fuel) else Default_Startup_Budget));
         Check (Current (S) = Failed and Completion (S).Stage = Execution_Failed);
         Check (Completion (S).Execution.Modules.Completed = 1
           and Completion (S).Execution.Modules.Failed_Table = 2
           and Completion (S).Execution.Failed_Table_ID = 901);
         Check (Observe (S).Objects = ACPI_Service.Values.Firmware_Seed_Count + 1 and not Members_Initialized (S));
         if Exhausted then Check (Completion (S).Execution.Modules.Charged <= Prefix_Fuel); end if;
         for I in Later'Range loop Check (Table_Byte (S, 2, I - 1) = Later (I)); end loop;
         declare
            Saved : constant Finish_Report := Completion (S);
            Prior : constant State_Model := Model (S) with Ghost;
         begin
            Finish (S); Check (Completion (S) = Saved);
            pragma Assert (Model (S) = Prior);
         end;
      end;
   end loop;
   -- The later immutable description catalog is available to eager selector calls
   -- while its non-AML payload must never be executed as a module.
   declare
      Code : constant Bytes := Name ("CNT0", [0]) &
        Method ("SSEL", [16#75#] & Enc ("CNT0") & [16#A4#] & Str ("TEST")) &
        [16#5B#, 16#88#] & Enc ("REG0") & Enc ("SSEL") & Str ("") & Str ("") &
        [16#5B#, 16#81#, 11] & Enc ("REG0") & [1] & Enc ("FLD0") & [32] &
        Require_Equal (Enc ("CNT0"), [1]) &
        Require_Equal (Enc ("FLD0"), [16#0C#, 16#54#, 16#45#, 16#53#, 16#54#]);
      First : constant Bytes := Migration_Tables.Make (Code);
      Description : Bytes := Migration_Tables.Make ([16#FE#]);
      Sum : Firmware_Tables.Byte := 0;
      S : State (2, 1024, 512, Load_Capacity => 2);
      Accepted : Boolean;
   begin
      Description (1 .. 4) := Enc ("TEST"); Description (10) := 0;
      for B of Description loop Sum := Sum + B; end loop;
      Description (10) := 0 - Sum;
      Start (S, 2, Accepted); Check (Accepted);
      Import (S, 5, ACPI_Service.DSDT, First);
      Import (S, 909, ACPI_Service.Description, Description);
      Check (Observe (S).Objects = 0 and Completion (S).Stage = Not_Attempted);
      Finish (S);
      Check (Current (S) = Complete and Completion (S).Stage = Finished);
      Check (Completion (S).Execution.Modules.Completed = 1);
      Check (Table_Info (S, 2).ID = 909 and Table_Byte (S, 2, 36) = 16#FE#);
      Check (Pending_Members (S) = 0 and Members_Initialized (S));
   end;
   Ada.Text_IO.Put_Line ("Bootstrap catalog checks" & Checks'Image);
end Bootstrap_Catalog_Tests;
