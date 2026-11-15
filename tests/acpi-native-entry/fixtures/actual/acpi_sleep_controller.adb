pragma Ada_2022;
with AML_Execute;
package body ACPI_Sleep_Controller with SPARK_Mode is
   use type Microseconds, Core.Context_Key;
   use type Methods.Method_Phase, Methods.Outcome, Objects.Query_Outcome, Objects.Register_Model;
   use type AML_Execute.Execution_Status;
   procedure Invalidate (Item : in out State) is
   begin Item.Types := Empty_Types; Item.Context := Core.No_Context; end Invalidate;
   function Current_Types (Item : State; Service : Core.State) return Copied_Types is
     (if Item.Types.Available and then Item.Context /= Core.No_Context
        and then Core.Current_Context (Service, Item.Context)
      then Item.Types else Empty_Types);
   procedure Prepare_S5 (Item : in out State; Service : aliased in out Core.State;
      Registers : Objects.Register_Model; Budget : Natural;
      Deadline_Microseconds : Microseconds; Result : out Report) is
      Anchor : Core.Context_Key := Core.No_Context;
      Clock_Seen : Boolean := False;
      Allowed : Boolean;
      procedure Gate (OK : out Boolean) is
         Now : Microseconds;
         Available : Boolean;
      begin
         OK := False;
         if not Core.Current_Context (Service, Anchor)
           or else Core.Retained_Results (Service) /= 0 or else not May_Continue (Service)
         then Result.Status := Recovery_Required; return; end if;
         Read_Clock (Now, Available);
         if not Available then Result.Status := Clock_Unavailable; return; end if;
         if Clock_Seen and then Now < Result.Last_Clock_Microseconds then
            Result.Status := Clock_Reversed; return;
         end if;
         Clock_Seen := True; Result.Last_Clock_Microseconds := Now;
         if Now >= Deadline_Microseconds then Result.Status := Window_Expired; return; end if;
         OK := True;
      end Gate;
      function Unsafe (Value : Methods.Preparation_Report) return Boolean is
        (Value.Status in Methods.Cleanup_Failed | Methods.Provider_Contract_Error | Methods.Identity_Changed
          or else Value.Execution in AML_Execute.Field_Indeterminate | AML_Execute.Fatal_Requested);
   begin
      Invalidate (Item);
      Result := (others => <>);
      for Method in Methods.Method_Phase loop
         Result.Method_Results (Method) := (Target => Methods.S5, Phase => Method, others => <>);
      end loop;
      if Deadline_Microseconds = 0 or else Deadline_Microseconds = Microseconds'Last then
         Result.Status := Invalid_Window; return;
      end if;
      if Budget = 0 then Result.Status := No_Budget; return; end if;
      Anchor := Core.Capture_Context (Service);
      if Anchor = Core.No_Context then return; end if;
      Gate (Allowed); if not Allowed then return; end if;
      for Method in Methods.Method_Phase loop
         Result.Reached := (if Method = Methods.Transition_To_State then Transition_Method else Preparation_Method);
         Gate (Allowed); if not Allowed then return; end if;
         Methods.Prepare (Service, Methods.S5, Budget - Result.Charged,
           Result.Method_Results (Method), Phase => Method);
         declare Value : Methods.Preparation_Report renames Result.Method_Results (Method); begin
            if Value.Charged > Budget - Result.Charged then
               Result.Status := Recovery_Required; return;
            end if;
            Result.Charged := Result.Charged + Value.Charged;
            if Unsafe (Value) then Result.Status := Recovery_Required; return; end if;
            if Value.Status not in Methods.Not_Declared | Methods.Method_Completed and then not Result.Method_Failed then
               Result.Method_Failed := True; Result.First_Failure := Method;
            end if;
         end;
         Gate (Allowed); if not Allowed then return; end if;
      end loop;
      Result.Reached := Sleep_Data;
      Gate (Allowed); if not Allowed then return; end if;
      Objects.Query (Service, Objects.S5, Registers, Budget - Result.Charged, Result.Data_Result);
      if Result.Data_Result.Charged > Budget - Result.Charged then Result.Status := Recovery_Required; return; end if;
      Result.Charged := Result.Charged + Result.Data_Result.Charged;
      if Result.Data_Result.Outcome in Objects.Cleanup_Failed | Objects.Provider_Contract_Error | Objects.Identity_Changed
        or else Result.Data_Result.Execution in AML_Execute.Field_Indeterminate | AML_Execute.Fatal_Requested
      then Result.Status := Recovery_Required; return; end if;
      Result.Reached := Final_Check;
      Gate (Allowed); if not Allowed then return; end if;
      if Result.Data_Result.Outcome /= Objects.Types_Available
        or else not Result.Data_Result.A_Available
        or else (Registers = Objects.PM1A_And_B and then not Result.Data_Result.B_Available)
      then Result.Status := No_Sleep_Data; return; end if;
      Item.Types := (Available => True, Registers => Registers,
        Type_A => Result.Data_Result.Type_A, Type_B => Result.Data_Result.Type_B);
      Item.Context := Anchor;
      Result.Status := (if Result.Method_Failed then Prepared_With_Method_Failure else Prepared);
      Result.Reached := Complete;
   end Prepare_S5;
end ACPI_Sleep_Controller;
