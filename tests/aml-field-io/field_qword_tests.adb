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
with Qword_Oracle;
procedure Field_Qword_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 32);
   package T renames ACPI_FADT.Transactions;
   package A renames AML_Field_Access;
   package E renames AML_Execute;
   subtype Bytes is AML_Decode.Bytes;
   use type Bytes;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Values.Value_Kind;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Namespace.Lookup_Status;
   use type E.Execution_Status;
   use type T.Access_Width;
   use type ACPI_Region_Policy.Address_Space;
   use type Qword_Oracle.Memory;
   type Memory is array (Natural range 0 .. 15) of Unsigned_8;
   Initial : constant Memory := [16#A5#,16#5A#,16#3C#,16#C3#,16#96#,16#69#,16#F0#,16#0F#,
      16#33#,16#CC#,16#87#,16#78#,16#58#,16#65#,16#72#,16#8F#];
   type Hardware is record
      Data : Memory := Initial;
      Reads, Writes : Natural := 0;
   end record;
   Unknown_Read, Unknown_Write, Revoke_On_Read : Integer range -1 .. 8 := -1;
   Writes_Revoked : Boolean := False;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why & Checks'Image; end if;
   end Check;
   procedure Transact (H : in out Hardware; Space : ACPI_Region_Policy.Address_Space;
      Address : Unsigned_64; Width : T.Access_Width; For_Write : Boolean;
      Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      Offset : constant Natural := Natural (Address - 16#1000#);
   begin
      Check (Space = ACPI_Region_Policy.Memory_Space and then Width = T.Qword_Access
         and then Offset in 0 | 8, "explicit memory Qword transaction");
      Output := 0; Completed := True;
      if For_Write then
         H.Writes := H.Writes + 1;
         for I in 0 .. 7 loop H.Data (Offset + I) := Unsigned_8 (Shift_Right (Input, I * 8) and 255); end loop;
         if Offset = Unknown_Write then Unknown_Write := -1; Completed := False; end if;
      else
         H.Reads := H.Reads + 1;
         for I in 0 .. 7 loop Output := Output or Shift_Left (Unsigned_64 (H.Data (Offset + I)), I * 8); end loop;
         if Offset = Unknown_Read then Unknown_Read := -1; Completed := False; end if;
         if Offset = Revoke_On_Read then Writes_Revoked := True; end if;
      end if;
   end Transact;
   package P is new ACPI_Field_Hosted_Provider (Hardware, Transact);
   procedure Authorize (IO : P.Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
   begin
      P.Authorize (IO, Target, For_Write, Accepted);
      Accepted := Accepted and then not (For_Write and then Writes_Revoked);
   end Authorize;
   package F is new C.Field_IO
     (P.Context, P.Pending, P.Describe, P.Dispatch, P.Reconciled, Authorize_Target => Authorize);
   type Source_Kind is (Zero_Value, Ones_Value, Pattern_Value, Wide_Value,
      Buffer_Value, String_Value, Empty_Buffer, Empty_String);
   type Rule is (Preserve, Write_Ones, Write_Zeros);
   function Table (Revision : Unsigned_8; First : Natural; Update : Rule) return Bytes is
      Flag : constant Unsigned_8 := 4 + Unsigned_8 (Rule'Pos (Update) * 32);
      Payload : constant Bytes := [16#5B#,16#80#] & Enc ("REG0") & [0,16#0C#,0,16#10#,0,0,16#0A#,16]
        & Block ([16#5B#,16#81#], Enc ("REG0") & [Flag]
           & (if First = 0 then Bytes'[] else Bytes'[0,Unsigned_8 (First)]) & Enc ("FLD0") & [16#40#,4])
        & Method ("READ", [16#A4#] & Enc ("FLD0"))
        & Block ([16#14#], Enc ("WRIT") & [1,16#70#,16#68#] & Enc ("FLD0") & [16#A4#,0])
        & Method ("BUFR", [16#70#] & Block ([16#11#], [16#0A#,8,16#C7#,16#34#,16#12#,16#5A#,16#96#,16#69#,16#A5#,16#3C#]) & Enc ("FLD0") & [16#A4#,0])
        & Method ("STRG", [16#70#,16#0D#] & Enc ("Az2!") & [0] & Enc ("FLD0") & [16#A4#,0])
        & Method ("EBUF", [16#70#] & Block ([16#11#], [0]) & Enc ("FLD0") & [16#A4#,0])
        & Method ("ESTR", [16#70#,16#0D#,0] & Enc ("FLD0") & [16#A4#,0]);
      Data : Bytes (1 .. 36 + Payload'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); Data (9) := Revision;
      for I in 0 .. 3 loop Data (5 + I) := Unsigned_8 (Shift_Right (Unsigned_32 (Data'Length), I * 8) and 255); end loop;
      Data (37 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
   procedure Exercise (Revision : Unsigned_8; First : Natural; Update : Rule;
      Source : Source_Kind; Policy : Natural := 0; Fault : Natural := 0) is
      S : aliased C.State (1,4096,4096);
      IO : P.Context;
      Items, Read_Only : F.Binding_Set;
      Targets : AML_Field_Targets.Vector := [others => A.No_Target];
      Count : constant AML_Field_Plans.Datum_Count := (if First = 0 then 1 else 2);
      IDs : constant array (Positive range 1 .. 2) of A.Register_ID :=
        [16#FFFF_FFFF_FFFF_FF97#,16#8000_0000_0000_0041#];
      Raw : constant Unsigned_64 := (case Source is
         when Zero_Value | Empty_Buffer | Empty_String => 0,
         when Ones_Value => Unsigned_64'Last, when Pattern_Value => 16#5A12_34C7#,
         when Wide_Value => 16#FEDC_BA98_7654_3210#,
         when Buffer_Value => 16#3CA5_6996_5A12_34C7#, when String_Value => 16#2132_7A41#);
      Input : constant Unsigned_64 := (if Revision = 1 and then Source <= Wide_Value
         then Raw and 16#FFFF_FFFF# else Raw);
      Name : constant String := (case Source is when Buffer_Value => "BUFR", when String_Value => "STRG",
         when Empty_Buffer => "EBUF", when Empty_String => "ESTR", when others => "WRIT");
      Expected : Memory := Initial;
      Config : ACPI_Region_Policy.Configuration;
      Status : C.Values.Access_Status;
      Key : C.Values.Field_Key;
      Description : C.Values.Field_Description;
      Found : C.Namespace.Lookup_Result;
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Report : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Result : E.Execution_Result;
      Saved : A.Observation;
      Before, After : Hardware;
      OK, Bit_Value : Boolean;
      procedure Write_Value (Set : F.Binding_Set; Want : E.Execution_Status) is
      begin
         C.Find_Object (S,C.Namespace.Root,[Name (1),Name (2),Name (3),Name (4)],Found,Status);
         Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "write method");
         F.Invoke_Scalar (S,IO,Set,Found.Node,[0 => Raw, others => 0],(if Name = "WRIT" then 1 else 0),1000,Result);
         Check (Result.Status = Want, "write outcome" & Result.Status'Image);
         Check (C.Retained_Results (S) = 0,"write root cleanup");
      end Write_Value;
      procedure Read_Value (Image : Memory) is
         Number : Unsigned_64 := 0;
         Named : C.Values.Result;
         View : C.Values.Value_Description;
         Data : Bytes (1 .. 8);
         Copied : Natural;
         Handle : C.Values.Value_Handle;
      begin
         for I in 0 .. 63 loop
            if (Shift_Right (Unsigned_64 (Image ((First + I) / 8)),(First + I) mod 8) and 1) /= 0 then
               Number := Number or Shift_Left (Unsigned_64'(1),I);
            end if;
         end loop;
         C.Find_Object (S,C.Namespace.Root,"READ",Found,Status);
         Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found,"read method");
         F.Invoke_Retained (S,IO,Read_Only,Found.Node,[others => 0],0,1000,Named,Status);
         Check (Status = C.Values.Available,"read access");
         if Revision = 1 then
            Check (Named.Status = E.Object_Returned,"32bit AML returns Buffer");
            C.Describe_Result (S,Named.Handle,View,Status);
            Check (Status = C.Values.Available and then View.Kind = C.Values.Buffer_Description and then View.Length = 8,"eight byte Buffer");
            C.Read_Result_Bytes (S,Named.Handle,0,Data,Copied,Status);
            Check (Status = C.Values.Available and then Copied = 8,"copy Buffer");
            for I in 0 .. 7 loop Check (Data (I + 1) = Unsigned_8 (Shift_Right (Number,I * 8) and 255),"read byte value"); end loop;
            Handle := Named.Handle; C.Release_Result (S,Handle,Status);
            Check (Status = C.Values.Available,"release Buffer");
         else
            Check (Named.Status = E.Returned and then Named.Number = Number,"64bit read value");
         end if;
         Check (C.Retained_Results (S) = 0,"read root cleanup");
      end Read_Value;
   begin
      Unknown_Read := -1; Unknown_Write := -1; Revoke_On_Read := -1; Writes_Revoked := False;
      C.Stage_Table (S,1,C.DSDT,Table (Revision,First,Update),Staged); Check (Staged = C.Staged,"stage");
      C.Freeze_Catalog (S,Frozen); Check (Frozen = C.Catalog_Frozen_OK,"freeze");
      C.Execute_Frozen (S,10000,Report,Status);
      Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed,"module");
      C.Initialize_Members (S,Members,Status); Check (Status = C.Values.Available,"members");
      C.Find_Object (S,C.Namespace.Root,"FLD0",Found,Status);
      Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found,"field");
      C.Read_Field_Descriptor (S,Found.Node,Description,Key,Status); Check (Status = C.Values.Available,"descriptor");
      for Position in AML_Field_Plans.Datum_Position range 1 .. Count loop
         Config := (Tag => 17,Base => 16#1000#,Length => 16,Space => ACPI_Region_Policy.Memory_Space,
            Readable => Policy /= 1,Writable => Policy /= 2,Widths => [others => False],
            Register_Authority => (Resource_ID => Unsigned_64 (IDs (Positive (Position))),Readable => Policy /= 1,
               Writable => Policy /= 2,Delegable => False,Write_Mask => (if Policy = 2 then 0 elsif Policy = 3 then 1 else Unsigned_64'Last)),
            Register_Offset => Unsigned_64 ((Natural (Position) - 1) * 8),Register_Width => T.Qword_Access);
         Config.Widths (T.Qword_Access) := True;
         P.Install (IO,7,Config,Policy /= 4,OK); Check (OK,"named Qword install");
         Targets (Position) := (P.Identity (IO),7,1,IDs (Positive (Position)));
      end loop;
      F.Add_Read_Binding (S,IO,Key,Targets,Count,Read_Only,Status); Check (Status = C.Values.Available,"read association");
      Write_Value (Read_Only,E.Unsupported); Check (P.Observe_Hardware (IO).Reads = 0 and then P.Observe_Hardware (IO).Writes = 0,"read association never authorizes write");
      F.Add_Write_Binding (S,IO,Key,Targets,Count,Items,Status);
      if Policy in 2 .. 4 or else (Policy = 1 and then Update = Preserve and then First /= 0) then
         Check (Status /= C.Values.Available,"permission preflight denial");
         Check (P.Observe_Hardware (IO).Reads = 0 and then P.Observe_Hardware (IO).Writes = 0,"denied no effects"); return;
      end if;
      Check (Status = C.Values.Available,"write association");
      for Bit in 0 .. Natural (Count) * 64 - 1 loop
         Bit_Value := (if Bit in First .. First + 63 then (Shift_Right (Input,Bit - First) and 1) /= 0
            else (case Update is when Preserve => (Shift_Right (Unsigned_64 (Initial (Bit / 8)),Bit mod 8) and 1) /= 0,
               when Write_Ones => True, when Write_Zeros => False));
         if Bit_Value then Expected (Bit / 8) := Expected (Bit / 8) or Shift_Left (Unsigned_8'(1),Bit mod 8);
         else Expected (Bit / 8) := Expected (Bit / 8) and not Shift_Left (Unsigned_8'(1),Bit mod 8); end if;
      end loop;
      if Policy = 0 and then Fault = 0 then Read_Value (Initial); end if;
      Before := P.Observe_Hardware (IO);
      case Fault is
         when 1 => Unknown_Read := 0;
         when 2 => Unknown_Write := 8;
         when 4 => P.Revoke_Register (IO,Targets (Count),OK); Check (OK,"revoke last target");
         when 5 => Revoke_On_Read := 0;
         when 6 => Revoke_On_Read := 8;
         when others => null;
      end case;
      Write_Value (Items,(if Fault in 1 .. 2 then E.Field_Indeterminate elsif Fault /= 0 then E.Unsupported else E.Returned));
      After := P.Observe_Hardware (IO);
      if Fault = 0 then
         Check (After.Data = Expected,"independent bit model");
         if Policy = 0 and then Source not in Wide_Value | Empty_Buffer | Empty_String then
            Check (Qword_Oracle.Memory (After.Data) = Qword_Oracle.Expected
              ((((Natural (Revision) - 1) * 3 + (if First = 0 then 0 elsif First = 7 then 1 else 2)) * 3
                + Rule'Pos (Update)) * 5 + (if Source <= Pattern_Value then Source_Kind'Pos (Source)
                  else Source_Kind'Pos (Source) - 1)),"normal ACPICA byte values");
         end if;
         Check (After.Writes = Before.Writes + Natural (Count) and then
            After.Reads = Before.Reads + (if Update = Preserve and then First /= 0 then 2 else 0),"update access count");
         if Policy = 0 then Read_Value (Expected); end if;
      elsif Fault = 4 then Check (After = Before,"final target preflight before prefix");
      elsif Fault in 1 | 5 then Check (After.Writes = Before.Writes and then After.Data = Initial,"stop before first write");
      elsif Fault = 2 then Check (After.Writes = 2 and then After.Data = Expected,"unknown second write retains effects");
      else
         Check (After.Writes = 1 and then After.Data (0 .. 7) = Expected (0 .. 7)
            and then After.Data (8 .. 15) = Initial (8 .. 15),"prefix retained on permission change");
      end if;
      if Fault in 1 .. 2 then
         Saved := F.Uncertain (S); Check (Saved.Present and then P.Pending (IO),"unknown receipt");
         Check (Saved.Item.For_Write = (Fault = 2),"unknown direction");
         Before := After; Write_Value (Items,E.Field_Indeterminate);
         Check (P.Observe_Hardware (IO) = Before,"no retry");
         P.Confirm_Completion (IO,Saved,True,OK); Check (OK,"trusted completion");
         F.Reconcile (S,IO,Saved,Status); Check (Status = C.Values.Available and then P.Observe_Hardware (IO) = Before,"reconcile without replay");
      end if;
   end Exercise;
   Offsets : constant array (Positive range 1 .. 3) of Natural := [0,7,63];
begin
   for Revision in Unsigned_8 range 1 .. 2 loop
      for First of Offsets loop
         for Update in Rule loop
            for Source in Source_Kind loop Exercise (Revision,First,Update,Source); end loop;
         end loop;
      end loop;
   end loop;
   for Update in Rule loop
      for Policy in 1 .. 4 loop Exercise (2,7,Update,Pattern_Value,Policy => Policy); end loop;
      Exercise (2,0,Update,Pattern_Value,Policy => 1);
   end loop;
   for Fault in 1 .. 6 loop
      if Fault /= 3 then Exercise (2,7,Preserve,Pattern_Value,Fault => Fault); end if;
   end loop;
   Ada.Text_IO.Put_Line ("Qword Field checks" & Checks'Image);
end Field_Qword_Tests;
