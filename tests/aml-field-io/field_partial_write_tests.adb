pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service_Core;
with ACPI_Field_Hosted_Provider;
with ACPI_Region_Policy;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Plans;
with AML_Field_Targets;
with Root_Test_Support; use Root_Test_Support;
with Field_Write_Oracle;
procedure Field_Partial_Write_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 32);
   package T renames ACPI_FADT.Transactions;
   package A renames AML_Field_Access;
   package E renames AML_Execute;
   subtype Bytes is AML_Decode.Bytes;
   use type Bytes;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Namespace.Lookup_Status;
   use type E.Execution_Status;
   use type A.Observation;
   use type T.Access_Width;
   use type Field_Write_Oracle.Memory;
   use type ACPI_Region_Policy.Address_Space;
   subtype Byte_Index is Natural range 0 .. 11;
   type Memory is array (Byte_Index) of Unsigned_8;
   Initial : constant Memory := [16#A5#,16#5A#,16#3C#,16#C3#,16#96#,16#69#,
      16#F0#,16#0F#,16#33#,16#CC#,16#87#,16#78#];
   type Hardware is record
      Data : Memory := Initial;
      Reads, Writes : Natural := 0;
      Last_Space : ACPI_Region_Policy.Address_Space := ACPI_Region_Policy.Memory_Space;
   end record;
   Read_Unknown, Write_Unknown, Read_Fault : Integer range -1 .. Byte_Index'Last := -1;
   Read_Revokes : Integer range -1 .. Byte_Index'Last := -1;
   Writes_Revoked : Boolean := False;
   procedure Transact (H : in out Hardware; Space : ACPI_Region_Policy.Address_Space;
      Address : Unsigned_64; Width : T.Access_Width; For_Write : Boolean;
      Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      Offset : constant Natural := Natural (Address - 16#1000#);
   begin
      H.Last_Space := Space;
      Output := 0; Completed := True;
      if For_Write then
         H.Writes := H.Writes + 1;
         for I in 0 .. T.Octets (Width) - 1 loop
            H.Data (Offset + I) := Unsigned_8 (Shift_Right (Input, I * 8) and 255);
         end loop;
         if Offset = Write_Unknown then Write_Unknown := -1; Completed := False; end if;
      else
         H.Reads := H.Reads + 1;
         for I in 0 .. T.Octets (Width) - 1 loop
            Output := Output or Shift_Left (Unsigned_64 (H.Data (Offset + I)), I * 8);
         end loop;
         if Offset = Read_Revokes then Writes_Revoked := True; end if;
         if Offset = Read_Unknown then Read_Unknown := -1; Completed := False;
         elsif Offset = Read_Fault then Output := 2 ** (8 * T.Octets (Width)); end if;
      end if;
   end Transact;
   package P is new ACPI_Field_Hosted_Provider (Hardware, Transact);
   procedure Authorize (IO : P.Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
   begin
      P.Authorize (IO, Target, For_Write, Accepted);
      Accepted := Accepted and then not (For_Write and then Writes_Revoked);
   end Authorize;
   package F is new C.Field_IO
     (P.Context, P.Pending, P.Describe, P.Dispatch, P.Reconciled, Authorize_Target => Authorize);
   type Source_Kind is (Zero_Value, Ones_Value, Pattern_Value, Buffer_Value,
      String_Value, Empty_Buffer, Empty_String);
   type Rule is (Preserve, Write_Ones, Write_Zeros);
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why & Checks'Image; end if;
   end Check;
   function Table (Revision : Unsigned_8; Width : T.Access_Width; Update : Rule;
      First, Length : Positive; Any_Field, IO_Space : Boolean) return Bytes is
      Flag : constant Unsigned_8 := Unsigned_8
        ((if Any_Field then 0 else T.Access_Width'Pos (Width) + 1) + Rule'Pos (Update) * 32);
      Encoded_Length : constant Bytes := (if Length < 64 then [Unsigned_8 (Length)]
         else [Unsigned_8 (16#40# + Length mod 16), Unsigned_8 (Length / 16)]);
      Payload : constant Bytes := [16#5B#,16#80#] & Enc ("REG0")
        & [Unsigned_8'(if IO_Space then 1 else 0),16#0C#,0,16#10#,0,0,16#0A#,12]
        & Block ([16#5B#,16#81#], Enc ("REG0") & [Flag,0,Unsigned_8 (First)]
           & Enc ("FLD0") & Encoded_Length)
        & Block ([16#14#], Enc ("WRIT") & [1,16#70#,16#68#] & Enc ("FLD0") & [16#A4#,0])
        & Method ("BUFR", [16#70#] & Block ([16#11#], [16#0A#,8,16#C7#,16#34#,16#12#,16#5A#,16#96#,16#69#,16#A5#,16#3C#])
           & Enc ("FLD0") & [16#A4#,0])
        & Method ("STRG", [16#70#,16#0D#] & Enc ("Az2!") & [0] & Enc ("FLD0") & [16#A4#,0])
        & Method ("EBUF", [16#70#] & Block ([16#11#], [0]) & Enc ("FLD0") & [16#A4#,0])
        & Method ("ESTR", [16#70#,16#0D#,0] & Enc ("FLD0") & [16#A4#,0]);
      Data : Bytes (1 .. 36 + Payload'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); Data (9) := Revision;
      for I in 0 .. 3 loop
         Data (5 + I) := Unsigned_8 (Shift_Right (Unsigned_32 (Data'Length), I * 8) and 255);
      end loop;
      Data (37 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
   procedure Exercise (Revision : Unsigned_8; Width : T.Access_Width;
      Update : Rule; Source : Source_Kind; Policy_Mode : Natural := 0; Fault_Mode : Natural := 0;
      Any_Field : Boolean := False; IO_Space : Boolean := False) is
      S : aliased C.State (1, 4096, 4096, 1);
      IO : P.Context;
      Items, Read_Only : F.Binding_Set;
      Targets : AML_Field_Targets.Vector := [others => A.No_Target];
      IDs : constant array (Positive range 1 .. 9) of A.Register_ID :=
        [16#8000_0000_0000_0041#,16#FFFF_FFFF_FFFF_FF97#,16#71#,
         16#A001#,16#B002#,16#C003#,16#D004#,16#E005#,16#F006#];
      First : constant Positive := (case Width is
         when T.Byte_Access => 7, when T.Word_Access => 11, when others => 27);
      Length : constant Positive := (if Any_Field then 64 else (case Width is
         when T.Byte_Access => 12, when T.Word_Access => 23, when others => 37));
      Count : constant AML_Field_Plans.Datum_Count :=
        (if Any_Field then 9 elsif Width = T.Dword_Access then 2 else 3);
      Unit_Bits : constant Positive := 8 * T.Octets (Width);
      Begin_Bit : constant Natural := First / Unit_Bits * Unit_Bits;
      End_Bit : constant Natural := (First + Length - 1) / Unit_Bits * Unit_Bits + Unit_Bits - 1;
      Raw : constant Unsigned_64 := (case Source is when Zero_Value | Empty_Buffer | Empty_String => 0,
         when Ones_Value => Unsigned_64'Last, when Pattern_Value => 16#5A12_34C7#,
         when Buffer_Value => 16#3CA5_6996_5A12_34C7#, when String_Value => 16#2132_7A41#);
      Input : constant Unsigned_64 := (if Revision = 1 and then Source in Zero_Value .. Pattern_Value
         then Raw and 16#FFFF_FFFF# else Raw);
      Expected : Memory := Initial;
      Configuration : ACPI_Region_Policy.Configuration;
      Status : C.Values.Access_Status;
      Key : C.Values.Field_Key;
      Description : C.Values.Field_Description;
      Found : C.Namespace.Lookup_Result;
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Report : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Result : E.Execution_Result;
      OK, Expected_Bit : Boolean;
      Saved : A.Observation;
      Before, Observed : Hardware;
      Method_Name : constant String := (case Source is when Buffer_Value => "BUFR",
         when String_Value => "STRG", when Empty_Buffer => "EBUF", when Empty_String => "ESTR", when others => "WRIT");
      Expected_Reads : Natural := 0;
      procedure Run (Set : F.Binding_Set; Want : E.Execution_Status) is
      begin
         C.Find_Object (S, C.Namespace.Root,
            [Method_Name (1),Method_Name (2),Method_Name (3),Method_Name (4)], Found, Status);
         Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "method");
         F.Invoke_Scalar (S, IO, Set, Found.Node, [0 => Raw, others => 0],
            (if Method_Name = "WRIT" then 1 else 0), 1000, Result);
         Check (Result.Status = Want, "write status" & Result.Status'Image);
         Check (C.Retained_Results (S) = 0, "no result leak");
      end Run;
   begin
      Read_Unknown := -1; Write_Unknown := -1; Read_Fault := -1;
      Read_Revokes := -1; Writes_Revoked := False;
      C.Stage_Table (S, 1, C.DSDT, Table (Revision, Width, Update, First, Length, Any_Field, IO_Space), Staged);
      Check (Staged = C.Staged, "stage");
      C.Freeze_Catalog (S, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (S, 10000, Report, Status);
      Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed, "module");
      C.Initialize_Members (S, Members, Status); Check (Status = C.Values.Available, "members");
      C.Find_Object (S, C.Namespace.Root, "FLD0", Found, Status);
      Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "field");
      C.Read_Field_Descriptor (S, Found.Node, Description, Key, Status);
      Check (Status = C.Values.Available, "descriptor");
      for Position in AML_Field_Plans.Datum_Position range 1 .. Count loop
         Configuration := (Tag => 17, Base => 16#1000#, Length => 12,
            Space => (if IO_Space then ACPI_Region_Policy.IO_Space else ACPI_Region_Policy.Memory_Space),
            Readable => Policy_Mode /= 1,
            Writable => Policy_Mode /= 2, Widths => [others => False],
            Register_Authority => (Resource_ID => Unsigned_64 (IDs (Positive (Position))),
               Readable => Policy_Mode /= 1, Writable => Policy_Mode /= 2,
               Delegable => False, Write_Mask => (if Policy_Mode = 2 then 0 elsif Policy_Mode = 3 then 1
                 else 2 ** Unit_Bits - 1)),
            Register_Offset => Unsigned_64 ((Natural (Position) - 1) * T.Octets (Width)), Register_Width => Width);
         Configuration.Widths (Width) := True;
         P.Install (IO, 7, Configuration, Policy_Mode /= 4, OK); Check (OK, "install named register");
         Targets (Position) := (P.Identity (IO),7,1,IDs (Positive (Position)));
      end loop;
      F.Add_Read_Binding (S, IO, Key, Targets, Count, Read_Only, Status);
      Check (Status = C.Values.Available, "read-only association");
      Run (Read_Only, E.Unsupported); Check (P.Observe_Hardware (IO).Reads = 0 and then
         P.Observe_Hardware (IO).Writes = 0, "read binding cannot enable writes");
      F.Add_Write_Binding (S, IO, Key, Targets, Count, Items, Status);
      if Policy_Mode in 2 .. 4 or else (Policy_Mode = 1 and then Update = Preserve) then
         Check (Status /= C.Values.Available, "denied permission preflight");
         Check (P.Observe_Hardware (IO).Reads = 0 and then P.Observe_Hardware (IO).Writes = 0, "denied bind no effects");
         return;
      end if;
      Check (Status = C.Values.Available, "write association");
      for Bit in Begin_Bit .. End_Bit loop
         if Bit in First .. First + Length - 1 then
            Expected_Bit := (Shift_Right (Input, Bit - First) and 1) /= 0;
         else Expected_Bit := (case Update is when Preserve =>
            (Shift_Right (Unsigned_64 (Initial (Bit / 8)), Bit mod 8) and 1) /= 0,
            when Write_Ones => True, when Write_Zeros => False); end if;
         if Expected_Bit then Expected (Bit / 8) := Expected (Bit / 8) or Shift_Left (Unsigned_8'(1), Bit mod 8);
         else Expected (Bit / 8) := Expected (Bit / 8) and not Shift_Left (Unsigned_8'(1), Bit mod 8); end if;
      end loop;
      if Fault_Mode = 1 then Read_Unknown := 0;
      elsif Fault_Mode = 2 then Write_Unknown := 1;
      elsif Fault_Mode = 3 then Read_Fault := 2;
      elsif Fault_Mode = 4 then
         P.Revoke_Register (IO, Targets (Count), OK); Check (OK, "revoke target");
      elsif Fault_Mode = 5 then Read_Revokes := 0;
      elsif Fault_Mode = 6 then Read_Revokes := 2;
      end if;
      Run (Items, (if Fault_Mode in 1 .. 2 then E.Field_Indeterminate
         elsif Fault_Mode in 3 .. 6 then E.Unsupported else E.Returned));
      Observed := P.Observe_Hardware (IO);
      if Fault_Mode = 0 then
         Check (Observed.Data = Expected, "independent bit model");
         Check (Observed.Last_Space = (if IO_Space then ACPI_Region_Policy.IO_Space
            else ACPI_Region_Policy.Memory_Space), "callback address space");
         if not IO_Space and then Policy_Mode = 0 and then Source <= String_Value then
            Check (Field_Write_Oracle.Memory (Observed.Data) = Field_Write_Oracle.Expected
              ((((Natural (Revision) - 1) * 4 + (if Any_Field then 3 else T.Access_Width'Pos (Width))) * 3
                + Rule'Pos (Update)) * 5 + Source_Kind'Pos (Source)), "cached normal ACPICA bytes");
         end if;
         if Update = Preserve then
            for I in 0 .. Natural (Count) - 1 loop
               if I * Unit_Bits < First or else (I + 1) * Unit_Bits - 1 > First + Length - 1 then
                  Expected_Reads := Expected_Reads + 1;
               end if;
            end loop;
         end if;
         Check (Observed.Writes = Natural (Count) and then Observed.Reads = Expected_Reads, "update rule access count");
      elsif Fault_Mode = 4 then Check (Observed.Reads = 0 and then Observed.Writes = 0, "revoked final target before prefix");
      elsif Fault_Mode = 1 then Check (Observed.Reads = 1 and then Observed.Writes = 0 and then Observed.Data = Initial, "unknown read before writes");
      elsif Fault_Mode = 5 then Check (Observed.Reads = 1 and then Observed.Writes = 0
         and then Observed.Data = Initial, "write permission rechecked after read");
      else
         Check (Observed.Writes = 2 and then Observed.Data (0 .. 1) = Expected (0 .. 1)
            and then Observed.Data (2 .. 11) = Initial (2 .. 11), "completed write prefix preserved");
      end if;
      if Fault_Mode in 1 .. 2 then
         Saved := F.Uncertain (S); Check (Saved.Present and then P.Pending (IO), "exact unknown saved");
         Check (Saved.Item.For_Write = (Fault_Mode = 2), "unknown operation direction");
         Before := Observed; Run (Items, E.Field_Indeterminate);
         Check (P.Observe_Hardware (IO) = Before, "no automatic retry");
         P.Confirm_Completion (IO, Saved, True, OK); Check (OK, "terminal evidence");
         F.Reconcile (S, IO, Saved, Status); Check (Status = C.Values.Available, "namespace reconciliation");
         Check (P.Observe_Hardware (IO) = Before, "reconcile no replay");
      end if;
   end Exercise;
begin
   for Revision in Unsigned_8 range 1 .. 2 loop
      for Width in T.Access_Width range T.Byte_Access .. T.Dword_Access loop
         for Update in Rule loop
            for Source in Source_Kind loop Exercise (Revision, Width, Update, Source); end loop;
            for Mode in 1 .. 4 loop Exercise (Revision, Width, Update, Pattern_Value, Policy_Mode => Mode); end loop;
         end loop;
      end loop;
   end loop;
   for Fault in 1 .. 6 loop Exercise (2, T.Byte_Access, Preserve, Pattern_Value, Fault_Mode => Fault); end loop;
   for Revision in Unsigned_8 range 1 .. 2 loop
      for Update in Rule loop
         for Source in Source_Kind loop
            Exercise (Revision, T.Byte_Access, Update, Source, Any_Field => True);
            Exercise (Revision, T.Byte_Access, Update, Source, Any_Field => True, IO_Space => True);
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Partial Field write checks" & Checks'Image);
end Field_Partial_Write_Tests;
