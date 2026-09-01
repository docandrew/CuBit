package body CCL.Ownership.Bytecode with
   SPARK_Mode => On
is
   type Abstract_State is record
      Seen : Boolean := False;
      Env  : Environment;
   end record;

   type State_Table is array (Code_Index) of Abstract_State;

   function Next_Index (Item : Code_Index) return Code_Index is
     (Item + 1);

   procedure Merge
     (States : in out State_Table;
      Target : Code_Index;
      Source : Environment;
      Result : in out Verification_Result)
   is
      Joined : Environment;
      Error  : Ownership_Error;
   begin
      if Result.Error /= Bytecode_Valid then
         null;
      elsif not States (Target).Seen then
         States (Target).Seen := True;
         States (Target).Env := Source;
      else
         Join (States (Target).Env, Source, Joined, Error);
         if Error /= Ownership_Valid then
            Result.Error := Ownership_Join_Failure;
            Result.Ownership_Error := Error;
            Result.Position := Target;
         else
            States (Target).Env := Joined;
         end if;
      end if;
   end Merge;

   procedure Verify
     (Candidate : Program;
      Result    : out Verification_Result)
   is
      States : State_Table := [others => (others => <>)];
      Initial : Environment;
      Current : Environment;
      Success_State : Environment;
      Failure_State : Environment;
      Error   : Ownership_Error;
      Falls_Through : Boolean;
      Item : Instruction;
      Initial_Locals_Length : Binding_Count;
   begin
      Result := (others => <>);
      if Candidate.Length = 0 then
         Result.Error := Empty_Program;
         return;
      end if;

      if Candidate.Dynamic_Locals_Length > Candidate.Locals_Length then
         Result.Error := Invalid_Local;
         return;
      end if;
      Initial_Locals_Length :=
        Candidate.Locals_Length - Candidate.Dynamic_Locals_Length;

      Initialize (Initial);
      if Initial_Locals_Length > 0 then
         for Local in 0 .. Initial_Locals_Length - 1 loop
            Declare_Binding
              (Initial, Local, Candidate.Local_Types (Local), Error);
            if Error /= Ownership_Valid then
               Result.Error := Ownership_Failure;
               Result.Ownership_Error := Error;
               Result.Position := 0;
               return;
            end if;
         end loop;
      end if;
      States (0) := (Seen => True, Env => Initial);

      for PC in Code_Index loop
         exit when Natural (PC) >= Candidate.Length;
         exit when Result.Error /= Bytecode_Valid;
         Result.Position := PC;
         if not States (PC).Seen then
            Result.Error := Unreachable_Instruction;
            exit;
         end if;

         Current := States (PC).Env;
         Item := Candidate.Code (PC);
         Falls_Through := True;

         if Item.Op not in No_Ownership_Op | Halt | Jump | Jump_If and then
           Natural (Item.Local) >= Candidate.Locals_Length
         then
            Result.Error := Invalid_Local;
         else
            case Item.Op is
               when No_Ownership_Op =>
                  Error := Ownership_Valid;
               when Halt =>
                  Check_Scope (Current, Candidate.Types, Error);
                  if Error /= Ownership_Valid then
                     Result.Error := Ownership_Failure;
                     Result.Ownership_Error := Error;
                  end if;
                  Falls_Through := False;
               when Initialize_Local =>
                  if Natural (Item.Local) < Initial_Locals_Length or else
                    Candidate.Types (Candidate.Local_Types (Item.Local)).Mode /=
                      Unrestricted
                  then
                     Result.Error := Ownership_Failure;
                     Error := Binding_Already_Declared;
                  else
                     Declare_Binding
                       (Current, Item.Local, Candidate.Local_Types (Item.Local),
                        Error);
                  end if;
               when Copy_Local =>
                  Copy_Value (Current, Candidate.Types, Item.Local, Error);
               when Move_Local =>
                  Move_Value (Current, Item.Local, Error);
               when Drop_Local =>
                  Drop_Value (Current, Candidate.Types, Item.Local, Error);
               when Borrow_Local_RO =>
                  Borrow_RO (Current, Item.Local, Error);
               when Return_Local_RO =>
                  Return_RO (Current, Item.Local, Error);
               when Borrow_Local_RW =>
                  Borrow_RW (Current, Item.Local, Error);
               when Return_Local_RW =>
                  Return_RW (Current, Item.Local, Error);
               when Apply_Local_Disposition =>
                  Apply_Disposition
                    (Current, Candidate.Types, Item.Local, Item.Verb, Error);
               when Import_Local =>
                  case Item.Import_Mode is
                     when Copy_Argument =>
                        Copy_Value
                          (Current, Candidate.Types, Item.Local, Error);
                     when Move_Argument =>
                        Success_State := Current;
                        Failure_State := Current;
                        Apply_Disposition
                          (Success_State, Candidate.Types, Item.Local,
                           Item.Success_Verb, Error);
                        if Error = Ownership_Valid then
                           Apply_Disposition
                             (Failure_State, Candidate.Types, Item.Local,
                              Item.Failure_Verb, Error);
                        end if;
                        if Error = Ownership_Valid then
                           Join
                             (Success_State, Failure_State, Current, Error);
                        end if;
                     when Borrowed_RO_Argument =>
                        Borrow_RO (Current, Item.Local, Error);
                        if Error = Ownership_Valid then
                           Return_RO (Current, Item.Local, Error);
                        end if;
                     when Borrowed_RW_Argument =>
                        Borrow_RW (Current, Item.Local, Error);
                        if Error = Ownership_Valid then
                           Return_RW (Current, Item.Local, Error);
                        end if;
                  end case;
               when Jump | Jump_If =>
                  Error := Ownership_Valid;
                  if Natural (Item.Target) >= Candidate.Length then
                     Result.Error := Invalid_Target;
                  elsif Natural (Item.Target) <= Natural (PC) then
                     Result.Error := Backward_Jump;
                  else
                     Merge (States, Item.Target, Current, Result);
                     if Item.Op = Jump then
                        Falls_Through := False;
                     end if;
                  end if;
            end case;

            if Result.Error = Bytecode_Valid and then
              Error /= Ownership_Valid
            then
               Result.Error := Ownership_Failure;
               Result.Ownership_Error := Error;
            end if;
         end if;

         if Result.Error = Bytecode_Valid and then Falls_Through then
            if PC = Code_Index'Last then
               Result.Error := Missing_Halt;
            elsif Natural (PC) + 1 >= Candidate.Length then
               Result.Error := Missing_Halt;
            else
               Merge
                 (States, Next_Index (PC), Current, Result);
            end if;
         end if;
      end loop;
   end Verify;
end CCL.Ownership.Bytecode;
