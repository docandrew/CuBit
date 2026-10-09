with Interfaces;
with CCL.Catalog;
with CCL.Objects;
with CuBit.Program_Descriptions;

--  A program the console may launch, as a CCL interface generated from its
--  description (docs/ccl-launch-parameters.md, "Connectors, not stdio"). For
--  ld.app, the interface ld:
--    Ld_Parameters    a record of its typed parameters: a file parameter
--                     is an Input_File, Output_File, Input_Directory or
--                     Output_Directory (each its own type), a flag is a
--                     Boolean, text is a String; a parameter that takes many
--                     values or may be absent is a List of them.
--    ld.run           Ld_Parameters -> Run: renders and starts the program.
--    ld.<outlet>        Run -> Stream: one accessor per outlet, named by
--                     its qualified name, so (ld.unix.stderr r).
--    ld.outcome       Run -> Task<Run_Outcome>: how it ended, once it has:
--                     Finished with its Unix_Exit (the exit code), or
--                     Stopped (killed, faulted, or its main thread ended:
--                     no result). Every program's result is Unix_Exit
--                     until a manifest can declare its own result type.
--    ld.outlets       Run -> List<Outlet_State>: each outlet, the type of
--                     its stream (Stream<String>), how many elements arrived
--                     and were lost, whether it ended.
--  The types are CCL source, checked by the CCL type checker when the
--  interface is published, like every other interface's; the digest and
--  keys are SHA-256 of that source (SPARKTLSCrypto), as for the precomputed
--  ones. The shared types (the file kinds and Run) have keys from the
--  shared source alone, so every program's interface publishes the same.
package CCL.Interfaces.Programs is
   use Standard.Interfaces;
   package PD renames CuBit.Program_Descriptions;

   SHARED_SOURCE : constant String :=
     "(type Input_File (record (name String))) " &
     "(type Output_File (record (name String))) " &
     "(type Input_Directory (record (name String))) " &
     "(type Output_Directory (record (name String))) " &
     "(type Run (record (program String) (pid Integer))) " &
     "(type Outlet_Signal (enum Stream Level Edge)) " &
     "(type Unix_Exit (record (code Integer))) " &
     "(type Run_Outcome (variant (Finished Unix_Exit) (Stopped))) " &
     "(type Outlet_State (record (name String) (stream String) (signal Outlet_Signal) (arrived Integer) " &
     "(lost Integer) (ended Boolean)))";
   --  Run's fields, in order.
   RUN_FIELDS : constant := 2;
   OUTLET_STATE_FIELDS : constant := 6;
   --  ld.outlets: a run's outlets and how far each got (discovery and the
   --  stream graph), at the last binding number of the program.
   OUTLETS_OPERATION : constant String := "outlets";
   OUTLETS_NUMBER : constant := 63;
   FILE_FIELDS : constant := 1;
   --  ld.outcome, at the binding number before ld.outlets.
   OUTCOME_OPERATION : constant String := "outcome";
   OUTCOME_NUMBER : constant := 62;
   UNIX_EXIT_FIELDS : constant := 1;
   --  Run_Outcome's alternatives, numbered from 1 in declaration order.
   type Outcome_Alternative is (Finished, Stopped);

   MAXIMUM_PROGRAMS : constant := 16;
   --  Operations per program: run, its outlets, outlets and outcome.
   MAXIMUM_OPERATIONS : constant := 3 + PD.Maximum_Connectors;
   subtype Program_Index is Natural range 0 .. MAXIMUM_PROGRAMS - 1;
   FIRST_BINDING : constant Unsigned_32 := 16#000A_0000#;
   BINDINGS_PER_PROGRAM : constant := 64;

   --  Binding for operation Number (0: run, 1 .. Connector_Total: the
   --  outlets in declaration order, OUTCOME_NUMBER, OUTLETS_NUMBER) of
   --  program Index.
   function Binding_Of (Index : Program_Index; Number : Natural) return Unsigned_32 is
     (FIRST_BINDING + Unsigned_32 (Index) * BINDINGS_PER_PROGRAM + Unsigned_32 (Number))
   with Pre => Number < BINDINGS_PER_PROGRAM;
   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding in FIRST_BINDING .. FIRST_BINDING + MAXIMUM_PROGRAMS * BINDINGS_PER_PROGRAM - 1);
   function Program_Of (Binding : Unsigned_32) return Program_Index is
     (Program_Index ((Binding - FIRST_BINDING) / BINDINGS_PER_PROGRAM))
   with Pre => Handles (Binding);
   function Number_Of (Binding : Unsigned_32) return Natural is
     (Natural ((Binding - FIRST_BINDING) mod BINDINGS_PER_PROGRAM))
   with Pre => Handles (Binding);

   --  The interface name for a launch-table name, from its last component:
   --  "ld.app" -> "ld", "toolchain/bin/gcc" -> "gcc".
   function Interface_Name (Program : String) return String;
   --  The record type of its parameters: "ld.app" -> "Ld_Parameters",
   --  "toolchain/bin/gcc" -> "Gcc_Parameters".
   function Parameters_Type (Program : String) return String;

   type Contracts is record
      Parameters, Run, Input_File, Output_File, Input_Directory, Output_Directory,
      Outlet_State, Outlet_States, Run_Outcome : CCL.Objects.Binding;
   end record;

   --  Generate, check and publish the interface of Program (its launch-table
   --  name) from its decoded description, and grant its operations at the
   --  bindings of Index. Accepted False: the source did not check, or the
   --  catalog refused it (Error).
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings;
      Index : Program_Index; Program : String; Description : PD.Signature;
      Bound : out Contracts; Error : out CCL.Catalog.Catalog_Error);

   --  The generated source for Program (for tests and the console's help).
   function Type_Source (Program : String; Description : PD.Signature) return String;

   --  SHA-256 of Text, then Suffix, as four big-endian words (the
   --  precomputed keys' layout).
   function Key_Of (Text : String; Suffix : String := "") return CCL.Objects.Schema_Key;
end CCL.Interfaces.Programs;
