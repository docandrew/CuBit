pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service; use ACPI_Service;
with ACPI_Test_Results;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Static_Table_Service_Tests is
   subtype Bytes is Firmware_Tables.Bytes;
   use type Firmware_Tables.Bytes;
   use type AML_Decode.Status;
   use type AML_Execute.Execution_Status;
   use type Values.Access_Status;
   use type Values.Audit_Value;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Message; end if;
   end Check;
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
   function Region (Name, Signature : String; OEM : String := ""; Model : String := "") return Bytes is
     ([16#5B#, 16#88#] & Enc (Name) & String_Operand (Signature) & String_Operand (OEM) & String_Operand (Model));
   function Field (Region_Name, Name : String; Offset, Bits : Natural) return Bytes is
     (Block ([16#5B#, 16#81#], Enc (Region_Name) & [1] &
       (if Offset = 0 then Bytes'[1 .. 0 => 0] else Bytes'[0] & Pkg_Length (Offset)) & Enc (Name) & Pkg_Length (Bits)));
   function Method (Name : String; Code : Bytes) return Bytes is
     (Block ([16#14#], Enc (Name) & [0] & Code));
   function Read_Method (Name, Target : String) return Bytes is
     (Method (Name, [16#A4#] & Enc (Target)));
   function Type_Method (Name, Target : String) return Bytes is
     (Method (Name, [16#A4#, 16#8E#] & Enc (Target)));
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
   procedure Install_OK (Service : in out State; ID : Positive; Kind : Table_Kind; Raw : Bytes) is
      Status : Install_Status;
   begin
      Install (Service, ID, Kind, Raw, Status);
      Check (Status = Installed, "install admitted table");
   end Install_OK;
   procedure Seal_OK (Service : in out State) is
      Report : Namespace.Initialization_Report;
      Status : Values.Access_Status;
   begin
      Initialize_Members (Service, Report, Status);
      Check (Status = Values.Available and then Report.Missing = 0 and then Report.Unsupported = 0,
        "all deferred members initialized");
      Check (Pending_Members (Service) = 0, "initialization drains pending queue");
   end Seal_OK;
   procedure Expect_Integer (Service : aliased in out State; Name : String; Expected : Unsigned_64) is
      Held : ACPI_Test_Results.Holder;
      Result : Values.Result;
   begin
      ACPI_Test_Results.Invoke (Service, Held,
        ACPI_Test_Results.Child (Service, Namespace.Root, Name), [others => 0], 0, 1_000, Result);
      Check (Result.Status = AML_Execute.Returned and then Result.Number = Expected, "integer result " & Name);
      Check (Retained_Results (Service) = 0, "scalar result has no retained pin");
      ACPI_Test_Results.Drop (Service, Held);
   end Expect_Integer;
begin
   for Revision in Unsigned_8 range 1 .. 2 loop
      declare
         Service : aliased State (8, 32_768, 8_192);
         Code : constant Bytes := Region ("SELF", "DSDT") & Field ("SELF", "SIG0", 0, 32) &
           [16#06#] & Enc ("SIG0") & Enc ("ALS1") &
           [16#06#] & Enc ("ALS1") & Enc ("ALS2") &
           Type_Method ("TYPE", "ALS2") & Read_Method ("READ", "ALS2");
      begin
         Install_OK (Service, 1, DSDT, Table ("DSDT", Revision, Code));
         Seal_OK (Service);
         Expect_Integer (Service, "TYPE", 5);
         Check (Observe (Service).Value_Objects = 0 and then Observe (Service).Value_Bytes = 0,
           "static alias ObjectType does not allocate");
         Expect_Integer (Service, "READ", 16#5444_5344#);
      end;
   end loop;
   declare
      Service : aliased State (8, 32_768, 8_192);
      Code : constant Bytes :=
        Region ("WILD", "TEST") & Field ("WILD", "FIR0", 36 * 8, 8) &
        Region ("OEMR", "TEST", "OEM002") & Field ("OEMR", "OEMF", 36 * 8, 8) &
        Region ("MODR", "TEST", "", "TABLE002") & Field ("MODR", "MODF", 36 * 8, 8) &
        Region ("EXAR", "TEST", "OEM002", "TABLE002") & Field ("EXAR", "EXAF", 36 * 8, 8) &
        Read_Method ("ONE0", "FIR0") & Read_Method ("TWO0", "OEMF") &
        Read_Method ("THR0", "MODF") & Read_Method ("FOU0", "EXAF");
   begin
      Install_OK (Service, 1, DSDT, Table ("DSDT", 2, Code));
      Install_OK (Service, 2, Description, Table ("TEST", 2, [16#A5#]));
      Install_OK (Service, 3, Description, Table ("TEST", 2, [16#B6#], "OEM002", "TABLE002"));
      Install_OK (Service, 4, Description, Table ("TEST", 2, [16#C7#], "OEM002", "TABLE002"));
      Seal_OK (Service);
      Expect_Integer (Service, "ONE0", 16#A5#);
      Expect_Integer (Service, "TWO0", 16#B6#);
      Expect_Integer (Service, "THR0", 16#B6#);
      Expect_Integer (Service, "FOU0", 16#B6#);
   end;
   declare
      Service : aliased State (8, 32_768, 8_192);
      Report : Namespace.Initialization_Report;
      Status : Values.Access_Status;
      Field_Node : Namespace.Node_ID;
   begin
      Install_OK (Service, 1, DSDT, Table ("DSDT", 2,
        Region ("MISS", "NONE") & Field ("MISS", "BADF", 0, 8) &
        [16#06#] & Enc ("BADF") & Enc ("ALS1") &
        [16#06#] & Enc ("ALS1") & Enc ("ALS2") & Type_Method ("TYPE", "ALS2")));
      Initialize_Members (Service, Report, Status);
      Check (Status = Values.Available and then Report.Missing = 2 and then Report.Bound = 0
        and then Report.Unsupported = 0, "missing region and dependent field both reported");
      Find_Child (Service, Namespace.Root, "BADF", Field_Node, Status);
      Check (Status = Values.Available and then
        Read_Namespace_Field (Service, Field_Node).Status /= AML_Decode.Accepted,
        "missing table produces no readable phantom field");
      Expect_Integer (Service, "TYPE", 5);
      Check (Observe (Service).Value_Objects = 0 and then Observe (Service).Value_Bytes = 0
        and then Pending_Members (Service) = 0, "missing input does not materialize values");
   end;
   declare
      Service : aliased State (8, 32_768, 8_192);
      Report : Namespace.Initialization_Report;
      Status : Values.Access_Status;
      Field_Node : Namespace.Node_ID;
   begin
      Install_OK (Service, 1, DSDT, Table ("DSDT", 2,
        Region ("TEST", "TEST") & Field ("TEST", "BADF", 37 * 8, 1) &
        [16#06#] & Enc ("BADF") & Enc ("ALS1") &
        [16#06#] & Enc ("ALS1") & Enc ("ALS2") & Type_Method ("TYPE", "ALS2")));
      Install_OK (Service, 2, Description, Table ("TEST", 2, [16#A5#]));
      Initialize_Members (Service, Report, Status);
      Check (Status = Values.Available and then Report.Unsupported = 1 and then Report.Bound = 1
        and then Report.Missing = 0, "out-of-extent field reported while region binds");
      Find_Child (Service, Namespace.Root, "BADF", Field_Node, Status);
      Check (Status = Values.Available and then
        Read_Namespace_Field (Service, Field_Node).Status /= AML_Decode.Accepted,
        "invalid extent produces no readable phantom field");
      Expect_Integer (Service, "TYPE", 5);
      Check (Observe (Service).Value_Objects = 0 and then Observe (Service).Value_Bytes = 0,
        "invalid extent allocates no value");
   end;
   declare
      Service : aliased State (8, 32_768, 8_192);
      Before : Values.Audit_Value;
      Prior_Metrics : Metrics;
      Prior_Pending : Natural;
      Installed_Result : Install_Status;
      Status : Values.Access_Status;
      Node : Namespace.Node_ID;
      Initial_DSDT : constant Bytes := Table ("DSDT", 2, [8] & Enc ("KEEP") & [16#0A#, 42]);
      Initial_Description : constant Bytes := Table ("TEST", 2, [16#A5#]);
      Good : constant Bytes := Region ("BADR", "TEST") & Field ("BADR", "BADF", 36 * 8, 8) &
        Read_Method ("READ", "BADF");
   begin
      Install_OK (Service, 1, DSDT, Initial_DSDT);
      Install_OK (Service, 2, Description, Table ("TEST", 2, [16#A5#]));
      Before := Audit (Service); Prior_Metrics := Observe (Service); Prior_Pending := Pending_Members (Service);
      Install (Service, 3, SSDT, Table ("SSDT", 2, Good & [16#5B#]), Installed_Result);
      Check (Installed_Result = Invalid_AML and then Audit (Service) = Before,
        "malformed trailing AML rolls back namespace atomically");
      Check (Observe (Service).Tables = Prior_Metrics.Tables and then
        Observe (Service).Bytes = Prior_Metrics.Bytes and then
        Observe (Service).Objects = Prior_Metrics.Objects and then
        Observe (Service).Value_Objects = Prior_Metrics.Value_Objects and then
        Pending_Members (Service) = Prior_Pending, "failed install preserves catalog and deferred queue");
      Check ((for all I in Initial_DSDT'Range => Table_Byte (Service, 1, I - 1) = Initial_DSDT (I))
        and then (for all I in Initial_Description'Range =>
          Table_Byte (Service, 2, I - 1) = Initial_Description (I)),
        "failed install preserves every previously admitted table byte");
      Find_Child (Service, Namespace.Root, "BADR", Node, Status);
      Check (Status = Values.Available and then Node = Namespace.Root, "no failed region binding");
      Find_Child (Service, Namespace.Root, "BADF", Node, Status);
      Check (Status = Values.Available and then Node = Namespace.Root, "no failed field binding");
      Install_OK (Service, 3, SSDT, Table ("SSDT", 2, Good));
      Seal_OK (Service);
      Expect_Integer (Service, "READ", 16#A5#);
   end;
   Ada.Text_IO.Put_Line ("Static table service checks" & Checks'Image);
end Static_Table_Service_Tests;
