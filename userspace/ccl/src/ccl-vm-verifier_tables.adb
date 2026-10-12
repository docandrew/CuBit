with CCL.Ownership.Bytecode;

package body CCL.VM.Verifier_Tables with SPARK_Mode => On is
   use type CCL.Ownership.Ownership_Mode;
   use type CCL.Ownership.Bytecode.Verification_Error;
   use type CCL.Imports.Transfer_Mode;

   procedure Check_Tables (Candidate : Program; Error : out Validation_Error) is
      Types : CCL.Types.Registry renames Candidate.Data_Types;
      D : CCL.Types.Description;
   begin
      Error := Valid;

      --  Every text constant lies inside the pool's text.
      for I in 1 .. Candidate.Constants_Length loop
         if not In_Pool (Candidate.Constants (I - 1)) then
            Error := Invalid_Constant;
            return;
         end if;
      end loop;

      -- A discovery snapshot may describe more types than this VM can execute.
      -- Registry construction/CCLB decoding already validate the definitions.
      -- Enforce executable shapes at each use (locals, match tables,
      -- constructors and comparisons), not on unrelated metadata.
      for I in 1 .. Candidate.Imports_Length loop
         declare
            Op : constant Import_Declaration := Candidate.Imports (I - 1);
         begin
            --  Text crosses an import as its characters, within the contract's
            --  limit; a limit means nothing for any other kind.
            if not (Known_Value_Type (Types, Op.Argument, Op.Argument_Data_Type) or else
                    (Op.Argument = Text_Value and then Op.Argument_Data_Type = CCL.Types.Invalid_Type)) or else
              not (Known_Value_Type (Types, Op.Result, Op.Result_Data_Type) or else
                   (Op.Result = Text_Value and then Op.Result_Data_Type = CCL.Types.Invalid_Type)) or else
              (Op.Argument /= Text_Value and then Op.Argument_Text_Limit /= 0) or else
              (Op.Result /= Text_Value and then Op.Result_Text_Limit /= 0)
            then Error := Invalid_Data_Type; return; end if;
            if Op.Result = Resource_Value then
               if not Declared_Type (Op.Result_Type_Tag, Candidate.Types_Length) or else
                 Candidate.Types (Op.Result_Type_Tag).Mode = CCL.Ownership.Unrestricted
               then Error := Invalid_Ownership; return; end if;
            elsif Op.Result_Type_Tag /= 0 then Error := Invalid_Ownership; return;
            end if;
            if Has_Receiver (Op) and then
              (not Known_Value_Type (Types, Resource_Value, Op.Receiver_Data_Type) or else
               not Op.Ownership_Argument or else Op.Transfer = CCL.Imports.Copy_Argument or else
               Op.Argument = Resource_Value)
            then Error := Invalid_Import; return; end if;
            -- Opaque resources can only be passed by a checked local borrow or
            -- move, never copied as a data argument.
            if Op.Argument = Resource_Value and then
              (not Op.Ownership_Argument or else Op.Transfer = CCL.Imports.Copy_Argument)
            then Error := Invalid_Import; return; end if;
            if Op.Ownership_Argument and then not
              (Scalar_Import (Op) or else
               ((Has_Receiver (Op) or else Op.Argument = Resource_Value) and then Op.Result /= Resource_Value)) then
               Error := Invalid_Import; return;
            end if;
         end;
      end loop;

      for L in 1 .. Candidate.Locals_Length loop
         declare
            Local : constant CCL.Ownership.Binding_Id := Local_At (L - 1);
            Kind : constant Value_Kind := Candidate.Local_Kinds (Local);
            Ref : constant CCL.Types.Type_Reference := Candidate.Local_Data_Types (Local);
         begin
            --  Text and characters may live in a compiler-created (dynamic)
            --  local, never in an initial local the host supplies: hosts cannot
            --  forge descriptors or out-of-range codes.
            if not Known_Value_Type (Types, Kind, Ref) and then
              not (L - 1 >= Candidate.Locals_Length - Candidate.Dynamic_Locals_Length and then
                   ((Kind in Text_Value | Character_Value and then Ref = CCL.Types.Invalid_Type) or else
                    (Kind = List_Value and then Supported_List (Types, Ref)) or else
                    (Kind = Object_Value and then Node_Type (Types, Ref)) or else
                    (Kind = Function_Value and then CCL.Types.Is_Function (Types, Ref)) or else
                    (Kind = Integer_Value and then CCL.Types.Is_Handle (Types, Ref))))
            then Error := Invalid_Data_Type; return; end if;
            if Kind = Resource_Value and then
              (not Declared_Type (Candidate.Local_Types (Local), Candidate.Types_Length) or else
               Candidate.Types (Candidate.Local_Types (Local)).Mode = CCL.Ownership.Unrestricted)
            then Error := Invalid_Ownership; return; end if;
         end;
      end loop;

      for M in 1 .. Candidate.Matches_Length loop
         if CCL.Types.Describe (Types, Candidate.Matches (M - 1).Data_Type).Form /= CCL.Types.Sum or else
           not CCL.Objects.Storable (Types, Candidate.Matches (M - 1).Data_Type)
         then Error := Invalid_Match; return; end if;
         D := CCL.Types.Describe (Types, Candidate.Matches (M - 1).Data_Type);
         for A in D.Count + 1 .. CCL.Types.Maximum_Components loop
            if Candidate.Matches (M - 1).Targets (A) /= 0 then
               Error := Invalid_Match; return;
            end if;
         end loop;
      end loop;

      for F in 1 .. Candidate.Functions_Length loop
         declare
            Decl : constant Function_Declaration := Candidate.Functions (F - 1);
         begin
            if Decl.Entry_PC = 0 or else Program_Length (Decl.Entry_PC) >= Candidate.Length or else
              (F > 1 and then Decl.Entry_PC <= Candidate.Functions (F - 2).Entry_PC) or else
              Decl.Captures > Decl.Count or else
              not Function_Data (Types, Decl.Result, Decl.Result_Data_Type)
            then Error := Invalid_Function; return; end if;
            for P in 1 .. Decl.Count loop
               if not Function_Data (Types, Decl.Kinds (P), Decl.Data_Types (P)) then
                  Error := Invalid_Function; return;
               end if;
            end loop;
         end;
      end loop;
   end Check_Tables;

   --  A + B, or one past the stack when that is deeper.
   function Sum (A, B : Demand) return Demand is
     (Demand (Natural'Min (MAX_STACK_DEPTH + 1, Natural (A) + Natural (B))));

   function Stack_Fits
     (Own_Max : Region_Demands; Base_Max : Call_Demands; Calls : Call_Presence) return Boolean
   is
      Need : Region_Demands := [others => 0];
   begin
      --  Callees have lower indexes, so demands combine in index order.
      for R in Region_Index loop
         Need (R) := Own_Max (R);
         for C in Function_Index loop
            if C < R and then Calls (R, C) then
               Need (R) := Demand'Max (Need (R), Sum (Base_Max (R, C), Need (C)));
            end if;
         end loop;
      end loop;
      return Need (No_Region) <= MAX_STACK_DEPTH;
   end Stack_Fits;

   procedure Check_Ownership
     (Candidate : Program; Switches : Switch_Table; Error : out Validation_Error)
   is
      package B renames CCL.Ownership.Bytecode;
      Main_End : constant Program_Length := Region_End (Candidate, No_Region);
      Ownership_Candidate : B.Program;
      Ownership_Result : B.Verification_Result;
   begin
      --  Only the main body can hold locals or ownership operations.
      Ownership_Candidate.Length := B.Code_Length (Main_End);
      Ownership_Candidate.Locals_Length := B.Binding_Count (Candidate.Locals_Length);
      Ownership_Candidate.Dynamic_Locals_Length := B.Binding_Count (Candidate.Dynamic_Locals_Length);
      for Position in 1 .. Candidate.Locals_Length loop
         Ownership_Candidate.Local_Types (Local_At (Position - 1)) := Candidate.Local_Types (Local_At (Position - 1));
      end loop;
      Ownership_Candidate.Types := Candidate.Types;
      for PC in Instruction_Index loop
         exit when Program_Length (PC) >= Main_End;
         declare
            Ins : Instruction renames Candidate.Code (PC);
         begin
            Ownership_Candidate.Code (B.Code_Index (PC)) :=
              (case Ins.Op is
                  when Halt => (Op => B.Halt, others => <>),
                  when Jump => (Op => B.Jump, Target => B.Code_Index (Ins.Target), others => <>),
                  when Jump_If_False => (Op => B.Jump_If, Target => B.Code_Index (Ins.Target), others => <>),
                  when Switch_Variant => Switches (PC),
                  when Initialize_Local => (Op => B.Initialize_Local, Local => Ins.Local, others => <>),
                  when Copy_Local => (Op => B.Copy_Local, Local => Ins.Local, others => <>),
                  when Move_Local => (Op => B.Move_Local, Local => Ins.Local, others => <>),
                  when Drop_Local => (Op => B.Drop_Local, Local => Ins.Local, others => <>),
                  when Borrow_Local_RO => (Op => B.Borrow_Local_RO, Local => Ins.Local, others => <>),
                  when Return_Local_RO => (Op => B.Return_Local_RO, Local => Ins.Local, others => <>),
                  when Borrow_Local_RW => (Op => B.Borrow_Local_RW, Local => Ins.Local, others => <>),
                  when Return_Local_RW => (Op => B.Return_Local_RW, Local => Ins.Local, others => <>),
                  when Apply_Local_Disposition =>
                    (Op => B.Apply_Local_Disposition, Local => Ins.Local, Verb => Ins.Verb, others => <>),
                  when Invoke_Import =>
                    (if Candidate.Imports (Ins.Import).Ownership_Argument then
                       (Op => B.Import_Local,
                        Local => Candidate.Imports (Ins.Import).Local,
                        Import_Mode =>
                          (case Candidate.Imports (Ins.Import).Transfer is
                              when CCL.Imports.Copy_Argument => B.Copy_Argument,
                              when CCL.Imports.Move_Argument => B.Move_Argument,
                              when CCL.Imports.Borrowed_RO_Argument => B.Borrowed_RO_Argument,
                              when CCL.Imports.Borrowed_RW_Argument => B.Borrowed_RW_Argument),
                        Success_Verb => Candidate.Imports (Ins.Import).Success_Verb,
                        Failure_Verb => Candidate.Imports (Ins.Import).Failure_Verb,
                        others => <>)
                     else (Op => B.No_Ownership_Op, others => <>)),
                  when others => (Op => B.No_Ownership_Op, others => <>));
         end;
      end loop;
      B.Verify (Ownership_Candidate, Ownership_Result);
      Error := (if Ownership_Result.Error /= B.Bytecode_Valid then Invalid_Ownership else Valid);
   end Check_Ownership;
end CCL.VM.Verifier_Tables;
