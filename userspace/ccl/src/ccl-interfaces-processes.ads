with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  What is running (the console's :ps): proc.list. The types are CCL source
--  (TYPE_SOURCE), checked by the CCL type checker when the interface is published;
--  this package holds no Ada description of them. The keys are SHA-256 of
--  TYPE_SOURCE, then "#" and the type's name (tests/ccl-console checks them).
package CCL.Interfaces.Processes with SPARK_Mode is

   --  What Publish adds to a catalog: one interface and this many data
   --  schemas. CCL_Host_Environment sums these to check the catalog's
   --  capacity at compile time; tests/ccl-install's probe checks a real run.
   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 2;
   --  Declared types Publish adds to the catalog's registry (its named
   --  types and the list types it specializes).
   PUBLISHED_TYPES : constant := 5;
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Run_State (enum Ready Running Sleeping Waiting Waiting_Event Sending Receiving " &
     "Waiting_Reply Waiting_Completion Suspended Futex_Waiting Vm_Halted Unknown)) (type " &
     "Bytes (range 0 " &
     "9223372036854775807)) (type Milliseconds (range 0 9223372036854775807)) (type Process " &
     "(record (pid Integer) (name String) (identity String) (state Run_State) (memory Bytes) " &
     "(launcher Integer) (age Milliseconds)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#43DD_AB8D_12A4_4386#,
      16#41B9_F1FC_E62A_F9C7#,
      16#2840_A7F8_0072_A307#,
      16#0A20_DF24_45B5_2450#];
   PROCESS_KEY : constant CCL.Objects.Schema_Key :=
     [16#D16A_9DAB_91E4_1556#,
      16#FCC1_40D4_ECED_6D71#,
      16#0384_84E4_F92D_4171#,
      16#914A_A108_091A_77C2#];
   PROCESSES_KEY : constant CCL.Objects.Schema_Key :=
     [16#80C9_C7B7_46D8_CBAD#,
      16#592B_3693_EBF1_43F9#,
      16#8250_02C0_9D3F_A991#,
      16#39C6_90E4_592D_3CC8#];

   --  The kernel's states (process.ads ProcessState) after INVALID, in order,
   --  then Unknown: a state this console does not know yet is listed as
   --  Unknown, never dropped (tests/ccl-console/check_run_state.py keeps the
   --  list in step with the kernel's).
   type Run_State is
     (Ready, Running, Sleeping, Waiting, Waiting_Event, Sending, Receiving,
      Waiting_Reply, Waiting_Completion, Suspended, Futex_Waiting, Vm_Halted,
      Unknown);
   subtype Kernel_Run_State is Run_State range Ready .. Vm_Halted;
   PROCESS_FIELDS : constant := 7;
   --  A Process takes its product cell, pid, name, identity, two cells of
   --  state and three Integers; a listing's count cell comes first. The
   --  result image (CCL.Objects.Maximum_Cells) then holds 55 processes:
   --  the named ceiling of one (proc.list) (more running is a refusal
   --  that says so, never a silent prefix).
   PROCESS_CELLS : constant := 1 + 1 + 1 + 1 + 2 + 3;
   MAXIMUM_LISTED : constant := (CCL.Objects.Maximum_Cells - 1) / PROCESS_CELLS;

   type Operation is (List);
   function Name (Item : Operation) return String is (case Item is when List => "list");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Processes);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Process, Processes : CCL.Objects.Binding;
   end record;
   --  Check TYPE_SOURCE and bind Process and List<Process>.
   procedure Define_Types (Bound : out Contracts; Accepted : out Boolean);
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Processes;
