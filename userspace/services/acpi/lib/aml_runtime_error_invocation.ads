with AML_Byte_Sources;
with AML_Decode;
with AML_Runtime_Exceptions;
package AML_Runtime_Error_Invocation with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   subtype Error_Code is AML_Runtime_Exceptions.Status_Code range
     1 .. AML_Runtime_Exceptions.Status_Code'Last;
   type Phase is (Need_Thread, Need_Handler, Need_Stage, Need_Child, Need_Cleanup, Done);
   type Action is (Keep_Original, Continue_Statement, Replace_Exception,
                   Fuel_Exhausted, Depth_Exhausted,
                   Propagate_Stage_Failure, Propagate_Child_Failure);
   type Outcome (Kind : Action := Keep_Original) is record
      Charged : Natural := 0;
      case Kind is
         when Keep_Original | Replace_Exception => Code : Error_Code;
         when others => null;
      end case;
   end record;
   type State is private;
   function Valid (Item : State) return Boolean;
   function Current (Item : State) return Phase;
   function Remaining (Item : State) return Natural;
   function Maximum_Depth (Item : State) return Natural;
   function Budget_Limit (Item : State) return Natural;
   function Initial_Charge (Item : State) return Natural;
   function Begin_Call
     (Original : Error_Code; Enabled, Tracking, Active, Complete : Boolean;
      Calls_Left, Max_Depth, Charged, Budget : Natural) return State
     with Pre => Charged <= Budget and then Calls_Left <= Max_Depth,
       Post => Valid (Begin_Call'Result)
         and then Maximum_Depth (Begin_Call'Result) = Max_Depth
         and then Budget_Limit (Begin_Call'Result) = Budget
         and then Initial_Charge (Begin_Call'Result) = Charged;
   procedure Supply_Thread (Item : in out State; Tag : AML_Decode.Integer_Value)
     with Pre => Valid (Item) and then Current (Item) = Need_Thread,
       Post => Valid (Item) and then Remaining (Item) < Remaining (Item'Old)
         and then Maximum_Depth (Item) = Maximum_Depth (Item'Old)
         and then Budget_Limit (Item) = Budget_Limit (Item'Old)
         and then Initial_Charge (Item) = Initial_Charge (Item'Old);
   procedure Supply_Handler (Item : in out State; Found : Boolean)
     with Pre => Valid (Item) and then Current (Item) = Need_Handler,
       Post => Valid (Item) and then Remaining (Item) < Remaining (Item'Old)
         and then Maximum_Depth (Item) = Maximum_Depth (Item'Old)
         and then Budget_Limit (Item) = Budget_Limit (Item'Old)
         and then Initial_Charge (Item) = Initial_Charge (Item'Old);
   procedure Supply_Stage (Item : in out State; Ready, Propagate_Failure : Boolean)
     with Pre => Valid (Item) and then Current (Item) = Need_Stage
         and then not (Ready and Propagate_Failure),
       Post => Valid (Item) and then Remaining (Item) < Remaining (Item'Old)
         and then Maximum_Depth (Item) = Maximum_Depth (Item'Old)
         and then Budget_Limit (Item) = Budget_Limit (Item'Old)
         and then Initial_Charge (Item) = Initial_Charge (Item'Old);
   function Child_Budget (Item : State) return Natural
     with Pre => Valid (Item) and then Current (Item) = Need_Child;
   function Child_Depth (Item : State) return Natural
     with Pre => Valid (Item) and then Current (Item) = Need_Child,
       Post => Child_Depth'Result <= Maximum_Depth (Item);
   procedure Supply_Child
     (Item : in out State; Integer_Return, Propagate_Failure : Boolean;
      Value : AML_Decode.Integer_Value; Charged : Natural)
     with Pre => Valid (Item) and then Current (Item) = Need_Child
         and then Charged <= Child_Budget (Item)
         and then not (Integer_Return and Propagate_Failure),
       Post => Valid (Item) and then Remaining (Item) < Remaining (Item'Old)
         and then Maximum_Depth (Item) = Maximum_Depth (Item'Old)
         and then Budget_Limit (Item) = Budget_Limit (Item'Old)
         and then Initial_Charge (Item) = Initial_Charge (Item'Old);
   procedure Cleaned (Item : in out State)
     with Pre => Valid (Item) and then Current (Item) = Need_Cleanup,
       Post => Valid (Item) and then Current (Item) = Done
         and then Maximum_Depth (Item) = Maximum_Depth (Item'Old)
         and then Budget_Limit (Item) = Budget_Limit (Item'Old)
         and then Initial_Charge (Item) = Initial_Charge (Item'Old);
   function Finish (Item : State) return Outcome
     with Pre => Valid (Item) and then Current (Item) = Done,
       Post => Finish'Result.Charged in Initial_Charge (Item) .. Budget_Limit (Item);
   function Message_Source (Code : Error_Code) return AML_Byte_Sources.Storage
     with Post => AML_Byte_Sources.Corresponds
       (Message_Source'Result, 0, AML_Runtime_Exceptions.Message (Code));
private
   type State is record
      Step : Phase := Done;
      Depth, Depth_Limit, Initial_Charged, Charged, Budget : Natural := 0;
      Original, Selected : Error_Code := 1;
      Tag : AML_Decode.Integer_Value := 0;
      Choice : Action := Keep_Original;
   end record;
   function Current (Item : State) return Phase is (Item.Step);
   function Remaining (Item : State) return Natural is (Phase'Pos (Done) - Phase'Pos (Item.Step));
   function Maximum_Depth (Item : State) return Natural is (Item.Depth_Limit);
   function Budget_Limit (Item : State) return Natural is (Item.Budget);
   function Initial_Charge (Item : State) return Natural is (Item.Initial_Charged);
   function Valid (Item : State) return Boolean is
     (Item.Initial_Charged <= Item.Charged and then Item.Charged <= Item.Budget
      and then Item.Depth <= Item.Depth_Limit
      and then (if Item.Step in Need_Thread | Need_Handler | Need_Stage | Need_Child then Item.Choice = Keep_Original)
      and then (if Item.Step in Need_Handler | Need_Stage | Need_Child | Need_Cleanup then Item.Tag /= 0)
      and then (if Item.Step in Need_Stage | Need_Child | Need_Cleanup then Item.Depth > 0));
end AML_Runtime_Error_Invocation;
