with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;

--  The debugger engine for agents and people at the console (DBG-001,
--  docs/debugging.md 7): debug.* over CuBit.Debug, the engine the Debugger
--  app links. Observe only: a session is Debug_Observe, minted by procmgr
--  through the console's "debug-session" authority within the user
--  session's debug envelope (never secrets.svc, the trusted path, procmgr
--  or devmgr). Every result is a typed CCL value; every call that talks to
--  the target or procmgr takes an explicit deadline (milliseconds from the
--  call, never a default); every list is bounded by a named ceiling below
--  and says how far it got rather than cutting silently.
--
--    (debug.attach (Attach_Query "clock.svc" 2000))      Debug_Session
--    (debug.threads 1)                                   List<Debug_Thread>
--    (debug.frames (Frame_Query 1 1 2000))               List<Debug_Frame>
--    (debug.locals (Locals_Query 1 1 0))                 List<Debug_Binding>
--    (debug.value (Value_Query 1 "Plain_Work.Totals"))    Debug_Binding
--    (debug.children (Children_Query 1 7 0 16))          List<Debug_Binding>
--    (debug.graph (Graph_Query 1 7 4 32))                Debug_Graph
--    (debug.memory (Memory_Query 1 4198400 64))          Debug_Memory
--    (debug.detach 1)                                    Boolean
--
--  A Debug_Binding's ref names the value for children and graph; refs
--  belong to their session and its latest stop (frames start a new one).
package CCL.Interfaces.Debug with SPARK_Mode is

   --  What Publish adds to a catalog (CCL_Host_Environment sums these).
   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 14;
   PUBLISHED_TYPES : constant := 20;
   use Standard.Interfaces;

   --  The types, as CCL source; the digest and keys are SHA-256 of it (then
   --  "#" and the type's name); tests/ccl-console checks them.
   TYPE_SOURCE : constant String :=
     "(type Debug_Wait (enum Unknown Running Ready In_Call Receiving Completion_Wait Futex Sleeping Not_Started Other)) " &
     "(type Debug_Session (record (session Integer) (program String) (process Integer) (build_id String) (mode String))) " &
     "(type Attach_Query (record (program String) (deadline Integer))) " &
     "(type Debug_Thread (record (thread Integer) (name String) (wait Debug_Wait) (cpu Integer) (peer Integer) " &
     "(deadline Integer) (pc Integer))) " &
     "(type Frame_Query (record (session Integer) (thread Integer) (deadline Integer))) " &
     "(type Debug_Frame (record (frame Integer) (subprogram String) (file String) (line Integer) (pc Integer) " &
     "(debug_info Boolean))) " &
     "(type Locals_Query (record (session Integer) (thread Integer) (frame Integer))) " &
     "(type Debug_Binding (record (ref Integer) (name String) (type_name String) (value String) (address Integer) " &
     "(children Integer))) " &
     "(type Value_Query (record (session Integer) (name String))) " &
     "(type Children_Query (record (session Integer) (ref Integer) (first_child Integer) (child_count Integer))) " &
     "(type Graph_Query (record (session Integer) (ref Integer) (depth Integer) (nodes Integer))) " &
     "(type Debug_Edge (record (from_node Integer) (to_node Integer) (label String))) " &
     "(type Debug_Graph (record (nodes (List Debug_Binding)) (edges (List Debug_Edge)) (truncated Boolean))) " &
     "(type Memory_Query (record (session Integer) (address Integer) (length Integer))) " &
     "(type Debug_Memory (record (address Integer) (bytes String) (got Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#F8D3_56DC_99B6_D268#,
      16#95C3_9D5B_0BE6_5AEF#,
      16#3B7D_B592_ED61_5DAD#,
      16#A59A_FF29_F76B_90BC#];
   SESSION_KEY : constant CCL.Objects.Schema_Key :=
     [16#4EC0_664F_C48C_92D5#,
      16#5E31_15D9_C135_80E9#,
      16#0C3C_D08E_67B0_CAFA#,
      16#8E09_2679_6705_C425#];
   ATTACH_QUERY_KEY : constant CCL.Objects.Schema_Key :=
     [16#D164_21FA_E32F_A8F8#,
      16#818D_372C_34F1_E812#,
      16#5B43_8BC8_F2B7_D2C8#,
      16#47CB_2864_8840_4CAB#];
   THREADS_KEY : constant CCL.Objects.Schema_Key :=
     [16#A4DC_1602_CABD_C60E#,
      16#C3BF_2B38_660C_01E0#,
      16#8BD5_2686_18BB_1941#,
      16#0375_A65D_F39A_4773#];
   FRAME_QUERY_KEY : constant CCL.Objects.Schema_Key :=
     [16#3E65_FB6F_78D3_21A1#,
      16#60B7_5D46_AD7A_A48C#,
      16#9939_5204_7993_ACA9#,
      16#B780_480A_9CEE_F71B#];
   FRAMES_KEY : constant CCL.Objects.Schema_Key :=
     [16#F7F2_20A3_82AC_D1B9#,
      16#E99E_6B89_E79C_2848#,
      16#1E5C_AE5E_133A_D641#,
      16#4B83_83AA_19A6_71D9#];
   LOCALS_QUERY_KEY : constant CCL.Objects.Schema_Key :=
     [16#1827_8CBE_34A0_4878#,
      16#A7EA_142A_12A3_E72A#,
      16#575F_4A35_10EC_6F58#,
      16#ACF8_51B5_FB5E_7892#];
   BINDING_KEY : constant CCL.Objects.Schema_Key :=
     [16#984B_A95C_9B25_0C28#,
      16#4A12_7195_994F_0575#,
      16#4FB5_8EE4_B278_44BB#,
      16#11ED_8686_D5AF_2E4C#];
   BINDINGS_KEY : constant CCL.Objects.Schema_Key :=
     [16#8C59_3F8D_EECB_4A4B#,
      16#27D6_5B6C_EF95_32BE#,
      16#5ECD_5A3D_AC2C_E1B7#,
      16#DE92_209E_3FB3_ED7F#];
   VALUE_QUERY_KEY : constant CCL.Objects.Schema_Key :=
     [16#1EEC_74AE_63BC_70B0#,
      16#C284_0385_3EAF_261A#,
      16#997A_2D00_7094_11A7#,
      16#916B_5C65_2E97_046E#];
   CHILDREN_QUERY_KEY : constant CCL.Objects.Schema_Key :=
     [16#6D1A_270A_13E7_99BA#,
      16#99E8_F104_FE75_A679#,
      16#B068_67C8_5DE7_DCA6#,
      16#23BA_A551_4D55_F86C#];
   GRAPH_QUERY_KEY : constant CCL.Objects.Schema_Key :=
     [16#8FB4_8D5B_0F98_46F7#,
      16#E923_3F14_13E8_754B#,
      16#1A47_58D2_DC75_EC28#,
      16#A705_5C45_366D_3BC5#];
   GRAPH_KEY : constant CCL.Objects.Schema_Key :=
     [16#506B_2C7C_26C8_2A11#,
      16#9A6D_3EBC_AA92_739F#,
      16#E8F3_D002_A8BF_6E01#,
      16#69F2_A688_8442_DFC0#];
   MEMORY_QUERY_KEY : constant CCL.Objects.Schema_Key :=
     [16#2BD7_FC39_243A_A362#,
      16#C83E_BF61_04E5_F0C1#,
      16#023F_2B04_02FF_B360#,
      16#C497_CD5D_CDDA_6A56#];
   MEMORY_KEY : constant CCL.Objects.Schema_Key :=
     [16#3ED2_88EC_C6F9_3AC8#,
      16#B421_BD36_9155_6D4E#,
      16#A2DB_72D5_87AC_1156#,
      16#DA13_27D4_4800_B412#];

   --  Debug_Wait's members, in declaration order (Variant_Cell is 1-based).
   type Wait_Member is
     (Unknown, Running, Ready, In_Call, Receiving, Completion_Wait, Futex, Sleeping,
      Not_Started, Other);
   SESSION_FIELDS : constant := 5;
   THREAD_FIELDS : constant := 7;
   FRAME_FIELDS : constant := 6;
   BINDING_FIELDS : constant := 6;
   EDGE_FIELDS : constant := 3;
   GRAPH_FIELDS : constant := 3;
   MEMORY_FIELDS : constant := 3;

   --  Named ceilings of one result (docs/debugging.md 7.3), sized so a
   --  result's cells fit one CCL value (CCL.Objects.Maximum_Cells).
   MAXIMUM_THREADS : constant := 32;
   MAXIMUM_FRAMES : constant := 48;
   MAXIMUM_BINDINGS : constant := 48;
   MAXIMUM_GRAPH_NODES : constant := 32;
   MAXIMUM_GRAPH_EDGES : constant := 48;
   --  Bytes one debug.memory reads, shown as hex (two characters a byte).
   MAXIMUM_MEMORY : constant := 1024;
   MAXIMUM_DEADLINE_MS : constant := 60_000;
   MAXIMUM_TEXT : constant := 256;

   type Operation is (Attach, Threads, Frames, Locals, Value, Children, Graph, Memory, Detach);
   function Name (Item : Operation) return String is
     (case Item is
         when Attach => "attach", when Threads => "threads", when Frames => "frames",
         when Locals => "locals", when Value => "value", when Children => "children",
         when Graph => "graph", when Memory => "memory", when Detach => "detach");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Debug);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Session, Attach_Query, Threads, Frame_Query, Frames, Locals_Query, Binding, Bindings,
      Value_Query, Children_Query, Graph_Query, Graph, Memory_Query, Memory : CCL.Objects.Binding;
   end record;
   procedure Define_Types (Bound : out Contracts; Accepted : out Boolean);
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Debug;
