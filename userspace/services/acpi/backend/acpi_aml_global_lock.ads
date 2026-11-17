pragma Ada_2022;
with Interfaces;
with AML_Frame_Handles;
with AML_Mutexes;
with AML_Global_Locks;
with ACPI_Local_Field_Scope;
generic
   with package Scope is new ACPI_Local_Field_Scope (<>);
   Transaction : in out Scope.Context;
   with procedure Read_Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean);
   with procedure Inspect_Window (Deadline : out Interfaces.Unsigned_64; Available : out Boolean);
   with procedure Wait_Until (Target : Interfaces.Unsigned_64; Completed : out Boolean);
   Maximum_Wait_Slices : Positive;
   Slice_Microseconds : Interfaces.Unsigned_64;
-- One adapter/context per retained Transaction. Actuals return normally and
-- cannot reenter AML. The same Scope protects Fields/EC: no second lock owner.
-- Wait_Until uses the existing owner clock domain and finite window, never a
-- newly opened deadline. Its calls are bounded and serialized by this owner.
package ACPI_AML_Global_Lock with SPARK_Mode is
   package G renames AML_Global_Locks;
   type Reason is (None, Invalid_Domain, Reentered, Already_Owned,
     Provider_Unavailable, Invalid_Window, Window_Changed, Clock_Unavailable,
     Clock_Reversed, Timeout_Elapsed, Wait_Unavailable, Work_Exhausted,
     Physical_Refused, Physical_Indeterminate);
   type Attempt_Count is new Natural range 0 .. Maximum_Wait_Slices;
   type Report is record
      Working, Cleanup_Required, Clock_Broken : Boolean := False;
      Attempts : Attempt_Count := 0;
      Last_Reason : Reason := None;
      Last_Result : G.Outcome := G.Unavailable;
   end record;
   type Context is limited private;
   function Observe (State : Context) return Report with Global => null;
   -- Domain is authenticated by the interpreter, not physical authority.
   -- Each accepted first acquisition retains its original domain and token.
   procedure Operate
     (State : in out Context; Domain : AML_Frame_Handles.Invocation_Domain;
      Action : G.Operation; Timeout : AML_Mutexes.Timeout;
      Outcome : in out G.Outcome);
   -- Outside AML: only the retained original domain is used for cleanup.
   -- Does not retire another Field/EC frame or reexecute any AML operation.
   procedure Maintain (State : in out Context; Outcome : out G.Outcome);
private
   type Context is limited record
      Info : Report;
      Seen_Clock : Boolean := False;
      Last_Clock : Interfaces.Unsigned_64 := 0;
      Owner : AML_Frame_Handles.Invocation_Domain := AML_Frame_Handles.No_Domain;
      Token : Scope.AML_Borrow := Scope.No_AML_Borrow;
   end record;
end ACPI_AML_Global_Lock;
