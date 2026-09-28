with Ada.Text_IO;
with CCL.VM; use CCL.VM;
with CCL.Types;
with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Format;

procedure Import_Result_Tests is
   use type CCL.Types.Type_Reference;
   use type CCL.Types.Definition_Result;
   use type Interfaces.Unsigned_32;
   use type CCL.Catalog.Intern_Result;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Catalog.Link_Result;
   use type CCL.Format.Format_Error;
   Candidate : Program;
   Verified : Validated_Program;
   Error : Validation_Error;
   Machine : Machine_State;
   Outcome : Execution_Result;
   Response : Value;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "import result check" & Checks'Image; end if;
   end Check;
begin
   Candidate.Length := 3;
   Candidate.Imports_Length := 1;
   Candidate.Code (0) := (Op => Push_Integer, others => <>);
   Candidate.Code (1) := (Op => Invoke_Import, others => <>);
   Candidate.Code (2) := (Op => Halt, others => <>);
   for Kind in Scalar_Kind loop
      Candidate.Imports (0).Result := Kind;
      Verify (Candidate, Verified, Error); Check (Error = Valid);
      for Fault in 0 .. 4 loop
         Initialize (Verified, 32, Machine);
         Continue_Execution (Verified, Machine, Outcome);
         Check (Outcome.Status = Waiting_For_Host and not Outcome.Has_Value);
         Response := (if Kind = Integer_Value then Integer_Constant (42) else Boolean_Constant (True));
         case Fault is
            when 0 => null;
            when 1 => Response.Type_Tag := 1;
            when 2 => Response.Copyable := False;
            when 3 => Response.Data_Type := CCL.Types.Integer_Type;
            when others => Response.Kind := (if Kind = Integer_Value then Boolean_Value else Integer_Value);
         end case;
         Complete_Host_Call (Verified, Machine, Response, True);
         Check (Is_Well_Formed (Verified, Machine));
         Continue_Execution (Verified, Machine, Outcome);
         if Fault = 0 then
            Check (Outcome.Status = Completed and Outcome.Has_Value);
            Check (Outcome.Result_Value = Response);
         else
            Check (Outcome.Status = Invalid_Bytecode and not Outcome.Has_Value);
         end if;
      end loop;
   end loop;
   declare
      Types : CCL.Types.Registry;
      Root, Other : CCL.Types.Type_Reference;
      Defined : CCL.Types.Definition_Result;
      Expected : Value;
      Before : Machine_Snapshot;
   begin
      CCL.Types.Define (Types,
        (Identifier => CCL.Types.Named ("Reading"), Form => CCL.Types.Sum, Count => 3,
         Parts => [1 => (CCL.Types.Named ("Value"), CCL.Types.Integer_Type),
                   2 => (CCL.Types.Named ("Unavailable"), CCL.Types.Unit_Type),
                   3 => (CCL.Types.Named ("Flag"), CCL.Types.Boolean_Type), others => <>]),
         Root, Defined);
      Check (Defined = CCL.Types.Defined);
      declare
         D : CCL.Types.Description := CCL.Types.Describe (Types, Root);
      begin
         D.Identifier := CCL.Types.Named ("DifferentReading");
         CCL.Types.Define (Types, D, Other, Defined); Check (Defined = CCL.Types.Defined);
      end;
      Candidate := (others => <>);
      Candidate.Data_Types := Types;
      Candidate.Imports_Length := 1;
      Candidate.Imports (0) := (Argument => Variant_Value, Result => Variant_Value,
        Argument_Data_Type => Root, Result_Data_Type => Root, others => <>);
      for Choice in 1 .. 3 loop
         Expected := (Kind => Variant_Value, Data_Type => Root, Alternative => Choice, others => <>);
         if Choice = 2 then
            Candidate.Length := 3;
            Candidate.Code (0) := (Op => Make_Variant, Data_Type => Root, Alternative => Choice, others => <>);
            Candidate.Code (1) := (Op => Invoke_Import, others => <>);
            Candidate.Code (2) := (Op => Halt, others => <>);
         else
            Expected.Integer := (if Choice = 1 then 42 else 0);
            Expected.Boolean := Choice = 3;
            Candidate.Length := 4;
            Candidate.Code (0) := (Op => (if Choice = 1 then Push_Integer else Push_Boolean),
              Immediate => (if Choice = 1 then 42 else 1), others => <>);
            Candidate.Code (1) := (Op => Make_Variant, Data_Type => Root, Alternative => Choice, others => <>);
            Candidate.Code (2) := (Op => Invoke_Import, others => <>);
            Candidate.Code (3) := (Op => Halt, others => <>);
         end if;
         Verify (Candidate, Verified, Error); Check (Error = Valid);
         for Fault in 0 .. 6 loop
            Initialize (Verified, 32, Machine);
            Continue_Execution (Verified, Machine, Outcome);
            Check (Outcome.Status = Waiting_For_Host and Outcome.Request_Argument = Expected);
            Before := Snapshot (Machine);
            Continue_Execution_For (Verified, Machine, 8, Outcome);
            Check (Outcome.Status = Waiting_For_Host and Snapshot (Machine) = Before);
            Response := Expected;
            case Fault is
               when 0 => null;
               when 1 => Response.Data_Type := Other;
               when 2 => Response.Alternative := 4;
               when 3 => Response.Copyable := False;
               when 4 => Response.Type_Tag := 1;
               when 5 => Response.Kind := Integer_Value;
               when others => Response.Data_Type := CCL.Types.Invalid_Type;
            end case;
            Complete_Host_Call (Verified, Machine, Response, True);
            Check (Is_Well_Formed (Verified, Machine));
            Continue_Execution (Verified, Machine, Outcome);
            Check (Outcome.Status = (if Fault = 0 then Completed else Invalid_Bytecode));
            Check (Outcome.Has_Value = (Fault = 0));
            if Fault = 0 then Check (Outcome.Result_Value = Expected); end if;
         end loop;
         Candidate.Imports (0).Argument_Data_Type := Other;
         Verify (Candidate, Verified, Error); Check (Error = Type_Mismatch);
         Candidate.Imports (0).Argument_Data_Type := Root;
      end loop;
      declare
         Links : CCL.Catalog.Linkage_Table;
         Index : Import_Index;
         Interned : CCL.Catalog.Intern_Result;
         Bytes : CCL.Format.Byte_Array;
         Size : CCL.Format.Module_Length;
         Format_Error : CCL.Format.Format_Error;
      begin
         CCL.Catalog.Intern (Links,
           (Interface_Digest => [1, 2, 3, 4], Interface_Major => 1,
            Parameters => 1,
            Import => (Argument => CCL.Host_Values.Object_Value,
                       Result => CCL.Host_Values.Object_Value,
                       Argument_Schema => [11, 12, 13, 14],
                       Result_Schema => [11, 12, 13, 14], others => <>), others => <>),
           Index, Interned);
         Check (Interned = CCL.Catalog.Linkage_Added);
         CCL.Format.Encode (Candidate, Links, (Fuel => 32, Memory => 4096, In_Flight => 1),
           Bytes, Size, Format_Error, Error);
         Check (Error = Valid and Format_Error = CCL.Format.Format_Valid and Size > 0);
      end;
      Candidate.Imports (0).Result_Data_Type := CCL.Types.Invalid_Type;
      Verify (Candidate, Verified, Error); Check (Error = Invalid_Data_Type);
      Candidate.Imports (0).Result := Integer_Value;
      Candidate.Imports (0).Result_Data_Type := Root;
      Verify (Candidate, Verified, Error); Check (Error = Invalid_Data_Type);
      Candidate.Imports (0).Result := Variant_Value;
      Candidate.Imports (0).Ownership_Argument := True;
      Verify (Candidate, Verified, Error); Check (Error = Invalid_Import);
   end;
   declare
      Links : CCL.Catalog.Linkage_Table;
      Grants : CCL.Catalog.Granted_Bindings;
      Op : constant CCL.Catalog.Resolved_Operation :=
        (Interface_Digest => [1, 2, 3, 4], Interface_Major => 1, Parameters => 1, others => <>);
      Index : Import_Index;
      Interned : CCL.Catalog.Intern_Result;
      Granted : CCL.Catalog.Grant_Result;
      Linked : CCL.Catalog.Link_Result;
   begin
      Candidate := (others => <>);
      Candidate.Imports_Length := 1;
      CCL.Catalog.Intern (Links, Op, Index, Interned);
      Check (Interned = CCL.Catalog.Linkage_Added);
      CCL.Catalog.Install (Grants, Op, 7, Granted); Check (Granted = CCL.Catalog.Grant_Added);
      Candidate.Imports (0).Local := 1;
      CCL.Catalog.Link_Program (Grants, Links, Candidate, Linked);
      Check (Linked = CCL.Catalog.Import_Contract_Mismatch and Candidate.Imports (0).Binding = 0);
      Candidate.Imports (0).Local := 0;
      CCL.Catalog.Link_Program (Grants, Links, Candidate, Linked);
      Check (Linked = CCL.Catalog.Link_Valid and Candidate.Imports (0).Binding = 7);
   end;
   Ada.Text_IO.Put_Line ("VM import result admission: PASS" & Checks'Image & " checks");
end Import_Result_Tests;
