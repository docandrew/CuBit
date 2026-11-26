with AML_Runtime_Error_Policy;
package body AML_Runtime_Error_Invocation with SPARK_Mode is
   use type AML_Runtime_Error_Policy.Resolution_Action;
   function Begin_Call
     (Original : Error_Code; Enabled, Tracking, Active, Complete : Boolean;
      Calls_Left, Max_Depth, Charged, Budget : Natural) return State is
   begin
      return (Step => (if Enabled and Tracking and not Active and Complete then Need_Thread else Done),
        Depth => Calls_Left, Depth_Limit => Max_Depth, Initial_Charged => Charged,
        Charged => Charged, Budget => Budget, Original => Original, Selected => Original,
        Tag => 0, Choice => Keep_Original);
   end Begin_Call;
   procedure Supply_Thread (Item : in out State; Tag : AML_Decode.Integer_Value) is
   begin
      Item.Tag := Tag; Item.Step := (if Tag = 0 then Done else Need_Handler);
   end Supply_Thread;
   procedure Supply_Handler (Item : in out State; Found : Boolean) is
      Plan : AML_Runtime_Error_Policy.Admission_Plan;
   begin
      if not Found then Item.Step := Done; return; end if;
      Plan := AML_Runtime_Error_Policy.Admit
        (True, True, False, True, True, Item.Tag, Item.Depth, Item.Charged, Item.Budget);
      case Plan.Action is
         when AML_Runtime_Error_Policy.Keep_Original => Item.Step := Done;
         when AML_Runtime_Error_Policy.Depth_Exhausted =>
            Item.Choice := Depth_Exhausted; Item.Step := Done;
         when AML_Runtime_Error_Policy.Fuel_Exhausted =>
            Item.Choice := Fuel_Exhausted; Item.Step := Done;
         when AML_Runtime_Error_Policy.Invoke_Handler =>
            Item.Charged := Plan.Next_Charged; Item.Step := Need_Stage;
      end case;
   end Supply_Handler;
   procedure Supply_Stage (Item : in out State; Ready, Propagate_Failure : Boolean) is
   begin
      if Ready then Item.Step := Need_Child;
      else
         if Propagate_Failure then Item.Choice := Propagate_Stage_Failure; end if;
         Item.Step := Need_Cleanup;
      end if;
   end Supply_Stage;
   function Child_Budget (Item : State) return Natural is (Item.Budget - Item.Charged);
   function Child_Depth (Item : State) return Natural is (Item.Depth - 1);
   procedure Supply_Child
     (Item : in out State; Integer_Return, Propagate_Failure : Boolean;
      Value : AML_Decode.Integer_Value; Charged : Natural) is
   begin
      Item.Charged := Item.Charged + Charged;
      if Propagate_Failure then Item.Choice := Propagate_Child_Failure;
      elsif Integer_Return then
         declare Decision : constant AML_Runtime_Error_Policy.Resolution :=
           AML_Runtime_Error_Policy.Resolve (Item.Original, True, Value);
         begin
            if Decision.Action = AML_Runtime_Error_Policy.Continue_Statement then
               Item.Choice := Continue_Statement;
            else Item.Choice := Replace_Exception; Item.Selected := Error_Code (Decision.Code);
            end if;
         end;
      end if;
      Item.Step := Need_Cleanup;
   end Supply_Child;
   procedure Cleaned (Item : in out State) is
   begin Item.Step := Done; end Cleaned;
   function Finish (Item : State) return Outcome is
   begin
      case Item.Choice is
         when Keep_Original => return (Keep_Original, Item.Charged, Item.Original);
         when Replace_Exception => return (Replace_Exception, Item.Charged, Item.Selected);
         when Continue_Statement => return (Continue_Statement, Item.Charged);
         when Fuel_Exhausted => return (Fuel_Exhausted, Item.Charged);
         when Depth_Exhausted => return (Depth_Exhausted, Item.Charged);
         when Propagate_Stage_Failure => return (Propagate_Stage_Failure, Item.Charged);
         when Propagate_Child_Failure => return (Propagate_Child_Failure, Item.Charged);
      end case;
   end Finish;
   function Message_Source (Code : Error_Code) return AML_Byte_Sources.Storage is
      Message : constant AML_Decode.Bytes := AML_Runtime_Exceptions.Message (Code);
   begin return (Length => Message'Length, Bytes => Message); end Message_Source;
end AML_Runtime_Error_Invocation;
