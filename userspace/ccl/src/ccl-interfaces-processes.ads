with Interfaces;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  What is running (the console's :ps): proc.list. The types are CCL source
--  (TYPE_SOURCE), checked by the CCL type checker when the interface is published;
--  this package holds no Ada description of them. The keys are SHA-256 of
--  TYPE_SOURCE, then "#" and the type's name (tests/ccl-console checks them).
package CCL.Interfaces.Processes with SPARK_Mode is
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Run_State (enum Ready Running Sleeping Waiting Waiting_Event Sending Receiving " &
     "Waiting_Reply Waiting_Completion Suspended Futex_Waiting)) (type Bytes (range 0 " &
     "9223372036854775807)) (type Milliseconds (range 0 9223372036854775807)) (type Process " &
     "(record (pid Integer) (name String) (identity String) (state Run_State) (memory Bytes) " &
     "(launcher Integer) (age Milliseconds)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#95CC_CEB2_A09D_0CC1#,
      16#59B2_230E_AB40_9E7D#,
      16#5B2A_DCC6_9F86_D807#,
      16#9A9B_6126_94DB_8C93#];
   PROCESS_KEY : constant CCL.Objects.Schema_Key :=
     [16#058F_54C1_12F4_D711#,
      16#B5BF_7977_6988_EC07#,
      16#C821_C4C2_F889_5A81#,
      16#411A_D91E_C493_2FE0#];
   PROCESSES_KEY : constant CCL.Objects.Schema_Key :=
     [16#13A5_B65D_5A58_EA2C#,
      16#B910_ED66_8F3D_76F1#,
      16#DB2F_7683_2BF6_378C#,
      16#EABC_BAEA_6940_7D12#];

   --  The kernel's states (process.ads ProcessState) after INVALID, in order.
   type Run_State is
     (Ready, Running, Sleeping, Waiting, Waiting_Event, Sending, Receiving,
      Waiting_Reply, Waiting_Completion, Suspended, Futex_Waiting);
   PROCESS_FIELDS : constant := 7;
   --  A Process takes its product cell, pid, name, identity, two cells of
   --  state and three Integers; a listing's count cell comes first. The
   --  result image (CCL.Objects.Maximum_Cells) then holds 28 processes.
   PROCESS_CELLS : constant := 1 + 1 + 1 + 1 + 2 + 3;
   MAXIMUM_LISTED : constant := (CCL.Objects.Maximum_Cells - 1) / PROCESS_CELLS;

   type Operation is (List);
   function Name (Item : Operation) return String is (case Item is when List => "list");
   FIRST_BINDING : constant Unsigned_32 := 16#0009_0001#;
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
