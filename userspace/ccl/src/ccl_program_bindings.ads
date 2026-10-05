with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL_Stream_Table;

--  The programs this front end may launch, as CCL interfaces generated from
--  their descriptions (CCL.Interfaces.Programs), over the platform's
--  CCL_Launcher: ld.run starts a program with typed parameters, each connector
--  accessor answers with that run's stream, and Pump feeds the streams from
--  the programs' outlets and completes their outcomes.
package CCL_Program_Bindings is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Streams : in out CCL_Stream_Table.Table;
      Reply : out CCL.Host_Values.Call_Result);
   --  Deliver what the running programs wrote and how they ended. Arrived:
   --  at least one stream advanced.
   procedure Pump (Streams : in out CCL_Stream_Table.Table; Arrived : out Boolean);

   --  What the runs started since the last call offer, for a front end
   --  that gives each its own card (docs/ccl-launch-parameters.md, "Every
   --  outlet gets a card"): each outlet (its qualified name, whether its
   --  elements are Integers, else text, and the session stream that carries
   --  it), and each run's outcome (the session task). Program is the
   --  interface name; Launched, Pid and Generation make the Run value.
   MAXIMUM_NAME : constant := 48;
   type Started_Kind is (Outlet_Started, Outcome_Started);
   type Started_Outlet is record
      Kind : Started_Kind := Outlet_Started;
      Program : String (1 .. MAXIMUM_NAME) := [others => ' '];
      Program_Length : Natural range 0 .. MAXIMUM_NAME := 0;
      Launched : String (1 .. MAXIMUM_NAME) := [others => ' '];
      Launched_Length : Natural range 0 .. MAXIMUM_NAME := 0;
      Pid, Generation : Interfaces.Integer_64 := 0;
      Outlet : String (1 .. MAXIMUM_NAME) := [others => ' '];
      Outlet_Length : Natural range 0 .. MAXIMUM_NAME := 0;
      Integers : Boolean := False;
      Stream : Interfaces.Integer_64 := 0;
   end record;
   MAXIMUM_STARTED : constant := 16;
   type Started_Array is array (1 .. MAXIMUM_STARTED) of Started_Outlet;
   procedure Take_Started (Items : out Started_Array; Count : out Natural);
end CCL_Program_Bindings;
