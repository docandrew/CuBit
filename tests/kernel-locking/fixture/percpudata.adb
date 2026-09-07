with Ada.Task_Attributes;
with Interrupt_State;
package body PerCPUData is
    package IDs is new Ada.Task_Attributes (Natural, 0);
    package States is new Ada.Task_Attributes
      (Interrupt_State.State, Interrupt_State.Initial_State);
    use type Interrupt_State.Result;
    procedure Set_CPU (CPU : Natural) is
    begin
        IDs.Set_Value (CPU);
    end Set_CPU;
    function getCPUNumber return Natural is (IDs.Value);
    function Depth return Natural is (Interrupt_State.Depth (States.Value));
    procedure pushCLI is
        S : Interrupt_State.State := States.Value;
        Result : Interrupt_State.Result;
    begin
        Interrupt_State.Enter (S, Depth = 0, Result);
        pragma Assert (Result = Interrupt_State.Success);
        States.Set_Value (S);
    end pushCLI;
    procedure popCLI is
        S : Interrupt_State.State := States.Value;
        Result : Interrupt_State.Result;
        Enable : Boolean;
    begin
        Interrupt_State.Leave (S, False, Enable, Result);
        pragma Assert (Result = Interrupt_State.Success);
        States.Set_Value (S);
        pragma Assert (Enable = (Depth = 0));
    end popCLI;
end PerCPUData;
