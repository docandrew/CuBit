pragma Ada_2022;
with ACPI_Sleep_Objects;
generic
   with package Driver is new ACPI_Sleep_Objects (Core => ACPI_Service, others => <>);
package ACPI_Bootstrap.Sleep_States with SPARK_Mode is
   Maximum_States : constant := 6;
   subtype State_Count is Natural range 0 .. Maximum_States;
   type Outcome is (Not_Ready, Observed, Budget_Exhausted, Object_Failed, Recovery_Required);
   type Attempt_Map is array (Driver.System_State) of Boolean;
   type Observations is array (Driver.System_State) of Driver.Query_Report;
   type Report is record
      Status : Outcome := Not_Ready;
      Examined : State_Count := 0;
      Attempted : Attempt_Map := [others => False];
      Items : Observations;
      Charged : Natural := 0;
      Recovery_Needed : Boolean := False;
   end record;
   -- Internal serialized owner observation,not an invocation wire selector.
   -- Registers is the owner's descriptive FADT model,never IO authority.
   -- Completed device startup is required. Query canonical ROOT S0..S5 in
   -- order with one total budget,including one host work unit per attempt.
   -- Stop on first failure; preserve earlier historical observations and keep
   -- unattempted states explicit. Missing required objects do not authorize a
   -- transition. No notification enable/ACK,cache publication or register IO.
   -- The owner must stop later AML on recovery/cleanup/provider uncertainty.
   procedure Observe (Session : in out State; Registers : Driver.Register_Model;
      Budget : Natural; Result : out Report)
     with Pre => Valid (Session), Post => Valid (Session) and then Result.Charged <= Budget;
end ACPI_Bootstrap.Sleep_States;
