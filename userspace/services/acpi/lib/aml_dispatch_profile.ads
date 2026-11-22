pragma Ada_2022;
with Interfaces;
-- Diagnostic interval attribution only, never execution fuel or authority.
package AML_Dispatch_Profile with Pure, SPARK_Mode is
   subtype Counter is Interfaces.Unsigned_64;
   type Site is (Load_Phase, Statement_Dispatch, Operand_Dispatch);
   type Instruction_Class is
     (Table_Load, Name_Reference_Or_Call, Integer_Literal, String_Literal,
      Buffer_Or_Package, Arithmetic, Logical, Store_Or_Reference,
      Namespace_Declaration, Control_Flow, Synchronization, Table_Operation, Other);
   type Counters is record
      Visits, Interval_Ticks : Counter := 0;
   end record;
   type Context is private;
   -- Fresh one-shot context. Repeated start marks quality incomplete, never
   -- resets history. No callback, clock, allocation, IPC or interpreter state.
   procedure Start (State : in out Context; Now : Counter);
   -- Charge the interval since the previous observation to its prior label.
   -- These are dispatch visits, not retired instructions or execution fuel.
   -- Intervals include preemption, external work and profiling overhead.
   procedure Observe (State : in out Context; Where : Site;
      Item : Instruction_Class; Now : Counter);
   procedure Stop (State : in out Context; Now : Counter);
   function Read (State : Context; Where : Site; Item : Instruction_Class) return Counters;
   function Active (State : Context) return Boolean;
   function Complete (State : Context) return Boolean;
   function Clock_Regressions (State : Context) return Counter;
   function Saturated (State : Context) return Boolean;
private
   type Entries is array (Site, Instruction_Class) of Counters;
   type Context is record
      Started, Recording, Malformed, Overflowed : Boolean := False;
      Last, Regressions : Counter := 0;
      Previous_Site : Site := Load_Phase;
      Previous_Class : Instruction_Class := Table_Load;
      Items : Entries;
   end record;
end AML_Dispatch_Profile;
