------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Mesa's GPU timeline (anv_cubit_gpu_timeline, GPU-001 step 3,
--  docs/gpu-async-submission.md): a Vulkan timeline value the CPU knows is
--  reached, plus the points the session queue will signal later, each one a
--  (context, value) on the GPU's own timeline. Like a DRM syncobj timeline.
--
--  @description
--  Pure logic, proved (SPARK level 2); the C sync type (anv_cubit_sync.c)
--  holds a Timeline in its vk_sync and changes it only through these
--  procedures, under its lock. No IPC, no clock: resolving a timeline takes
--  the contexts' completed values as an argument (the queue's status lines).
--
--  Points are kept in increasing Vulkan value order, all above the reached
--  value. A point resolves when its context's completed value reaches its
--  GPU value; the timeline then takes the highest resolved point's value
--  (Vulkan's "the value is the largest signalled"), and every point up to
--  it is dropped. A point is never reported reached before its GPU value is.
--
--  Bound: at most one point per job the queue holds. A submit adds its
--  point after it reaps completion records and after the timeline resolved
--  against completed values that cover every reaped record, so every point
--  left belongs to a job still owed a record: at most GQ.Slots.
--
--  The wait set: the GPU waits one descriptor carries. A wait on the job's
--  own context is dropped (the ring executes a context's jobs in order, so
--  waiting for it would only idle the GPU for a turn); waits on another
--  context keep the highest value per context. A descriptor holds two.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.GPU_Queues;

package Native_GPU_Timeline with Pure, SPARK_Mode is

   package GQ renames CuBit.GPU_Queues;

   --  A Vulkan timeline value (the vk_sync's own).
   type Sync_Value is new Unsigned_64;
   --  A context's value on the GPU's timeline (a descriptor's signal value).
   subtype GPU_Value is GQ.Timeline_Value;
   use type GPU_Value;

   Max_Points : constant := GQ.Slots;
   subtype Point_Count is Natural range 0 .. Max_Points;
   subtype Point_Index is Positive range 1 .. Max_Points;

   type Point is record
      Value   : Sync_Value := 0;
      GPU     : GPU_Value := GQ.No_Wait;
      Context : GQ.Context_Index := 0;
   end record;
   Point_Bytes : constant := 24;
   for Point use record
      Value   at 0 range 0 .. 63;
      GPU     at 8 range 0 .. 63;
      Context at 16 range 0 .. 7;
   end record;
   for Point'Size use Point_Bytes * 8;

   type Point_Array is array (Point_Index) of Point;

   --  The C sync type stores this as Timeline_Bytes opaque bytes
   --  (native_gpu_timeline.h); only these procedures change it.
   type Timeline is record
      Value  : Sync_Value := 0;
      Count  : Point_Count := 0;
      Points : Point_Array := [others => (others => <>)];
   end record with Convention => C;
   Header_Bytes : constant := 16;
   Timeline_Bytes : constant := Header_Bytes + Max_Points * Point_Bytes;
   for Timeline use record
      Value  at 0 range 0 .. 63;
      Count  at 8 range 0 .. 31;
      Points at Header_Bytes range 0 .. Max_Points * Point_Bytes * 8 - 1;
   end record;
   for Timeline'Size use Timeline_Bytes * 8;
   for Timeline'Alignment use 8;

   --  Each context's completed value (the queue's status lines).
   Completed_Bytes : constant := GQ.Contexts_Per_Session * 8;
   type Completed_Values is array (GQ.Context_Index) of GPU_Value
     with Convention => C, Size => Completed_Bytes * 8;

   --  Points above the reached value, in strictly increasing order.
   function Ordered (T : Timeline) return Boolean is
     ((for all I in 1 .. T.Count => T.Points (I).Value > T.Value) and then
      (for all I in 1 .. T.Count =>
         (for all J in I + 1 .. T.Count => T.Points (I).Value < T.Points (J).Value)));

   --  The highest value signalled or pending.
   function Last_Value (T : Timeline) return Sync_Value is
     (if T.Count = 0 then T.Value else T.Points (T.Count).Value);

   function Point_Reached (P : Point; Completed : Completed_Values) return Boolean is
     (P.GPU <= Completed (P.Context));

   procedure Initialize (T : out Timeline; Initial : Sync_Value)
     with Post => Ordered (T) and then T.Value = Initial and then T.Count = 0;

   --  The CPU signals V (vkSignalSemaphore, a CPU-side queue signal).
   --  Refused below the reached value; drops points V covers.
   procedure Signal (T : in out Timeline; V : Sync_Value; OK : out Boolean)
     with Pre  => Ordered (T),
          Post => Ordered (T) and then OK = (V >= T'Old.Value) and then
                  (if OK then T.Value = V and then
                     (for all I in 1 .. T.Count =>
                        (for some J in 1 .. T'Old.Count => T.Points (I) = T'Old.Points (J)))
                   else T = T'Old);

   type Add_Result is (Added, Not_Increasing, Full, Bad_Context) with Convention => C;
   for Add_Result use (Added => 0, Not_Increasing => 1, Full => 2, Bad_Context => 3);

   --  The queue will signal V when Context reaches GPU. V must exceed every
   --  value signalled or pending (Vulkan's rule for a signal operation).
   procedure Add_Point
     (T : in out Timeline; V : Sync_Value; Context : GQ.Context_Index; GPU : GPU_Value;
      Result : out Add_Result)
     with Pre  => Ordered (T),
          Post => Ordered (T) and then Result /= Bad_Context and then
                  (Result = Not_Increasing) = (V <= Last_Value (T'Old)) and then
                  (Result = Added) = (V > Last_Value (T'Old) and then T'Old.Count < Max_Points) and then
                  (if Result = Added then
                     T.Value = T'Old.Value and then T.Count = T'Old.Count + 1 and then
                     T.Points (T.Count) = (Value => V, GPU => GPU, Context => Context) and then
                     (for all I in 1 .. T'Old.Count => T.Points (I) = T'Old.Points (I))
                   else T = T'Old);

   --  Take every point whose context has completed it: the timeline's value
   --  becomes the highest such point's, and the points up to it go.
   procedure Resolve (T : in out Timeline; Completed : Completed_Values)
     with Pre  => Ordered (T),
          Post => Ordered (T) and then T.Value >= T'Old.Value and then
                  T.Count <= T'Old.Count and then
                  --  Never ahead of the GPU: the value is the old one or a reached point's.
                  (T.Value = T'Old.Value or else
                     (for some I in 1 .. T'Old.Count =>
                        Point_Reached (T'Old.Points (I), Completed) and then
                        T'Old.Points (I).Value = T.Value)) and then
                  --  Every reached point is covered.
                  (for all I in 1 .. T'Old.Count =>
                     (if Point_Reached (T'Old.Points (I), Completed) then
                        T'Old.Points (I).Value <= T.Value)) and then
                  --  What is left is the old points' unreached tail.
                  (for all I in 1 .. T.Count =>
                     T.Points (I) = T'Old.Points (I + (T'Old.Count - T.Count)) and then
                     not Point_Reached (T.Points (I), Completed));

   type Wait_Kind is (Reached, On_GPU, Unsubmitted) with Convention => C;
   for Wait_Kind use (Reached => 0, On_GPU => 1, Unsubmitted => 2);

   --  How to wait for W: reached already; on the GPU (P, the first point at
   --  or above W); or nothing is submitted for it yet.
   procedure Find (T : Timeline; W : Sync_Value; Kind : out Wait_Kind; P : out Point)
     with Pre  => Ordered (T),
          Post => (Kind = Reached) = (W <= T.Value) and then
                  (Kind = Unsubmitted) = (W > Last_Value (T)) and then
                  (if Kind = On_GPU then
                     P.Value >= W and then
                     (for some I in 1 .. T.Count =>
                        T.Points (I) = P and then (I = 1 or else T.Points (I - 1).Value < W)));

   --  A binary semaphore's payload moves (Mesa's vk_sync_move): Target takes
   --  Source's state, and Source becomes unsignalled, its next point above
   --  everything it had. Source_Next must be signalled or pending.
   procedure Move
     (Target, Source : in out Timeline; Source_Next : Sync_Value;
      Target_Next, New_Source_Next : out Sync_Value; OK : out Boolean)
     with Pre  => Ordered (Target) and then Ordered (Source),
          Post => Ordered (Target) and then Ordered (Source) and then
                  OK = (Source_Next <= Last_Value (Source'Old) and then
                        Last_Value (Source'Old) < Sync_Value'Last) and then
                  (if OK then
                     Target = Source'Old and then Target_Next = Source_Next and then
                     Source.Count = 0 and then Source.Value = Source'Old.Value and then
                     New_Source_Next = Last_Value (Source'Old) + 1 and then
                     New_Source_Next > Source.Value
                   else Target = Target'Old and then Source = Source'Old and then
                        Target_Next = 0 and then New_Source_Next = 0);

   ------------------------------------------------------------------------
   --  The waits one descriptor carries
   ------------------------------------------------------------------------
   --  The highest value waited for per context (GQ.No_Wait: none).
   type Wait_Set is array (GQ.Context_Index) of GPU_Value
     with Convention => C, Size => Completed_Bytes * 8;

   procedure Clear (S : out Wait_Set)
     with Post => (for all C in GQ.Context_Index => S (C) = GQ.No_Wait);

   --  A wait for P by a job on Job's context.
   procedure Merge (S : in out Wait_Set; Job : GQ.Context_Index; P : Point)
     with Post => (for all C in GQ.Context_Index =>
                     S (C) = (if C /= Job and then C = P.Context and then P.GPU > S'Old (C)
                              then P.GPU else S'Old (C)));

   type Wait is record
      Context : GQ.Context_Index := 0;
      Target  : GPU_Value := GQ.No_Wait;
   end record;

   --  The set's waits in context order. Fits False: more than a descriptor holds.
   procedure Select_Waits (S : Wait_Set; First, Second : out Wait; Fits : out Boolean)
     with Post =>
       (if Fits then
          --  Every wait in the set is one of the two, at its value.
          (for all C in GQ.Context_Index =>
             (if S (C) /= GQ.No_Wait then
                (First.Context = C and then First.Target = S (C)) or else
                (Second.Context = C and then Second.Target = S (C)))) and then
          --  Neither invents one.
          (First.Target = GQ.No_Wait or else S (First.Context) = First.Target) and then
          (Second.Target = GQ.No_Wait or else S (Second.Context) = Second.Target));

   ------------------------------------------------------------------------
   --  The C interface (native_gpu_timeline.h). C holds Timeline and
   --  Wait_Set as opaque storage it never writes itself.
   ------------------------------------------------------------------------
   procedure C_Initialize (T : out Timeline; Initial : Sync_Value)
     with Export, Convention => C, External_Name => "native_gpu_timeline_initialize",
          Post => Ordered (T);
   --  Returns through OK (1: signalled, 0: below the reached value).
   procedure C_Signal (T : in out Timeline; V : Sync_Value; OK : out Unsigned_32)
     with Export, Convention => C, External_Name => "native_gpu_timeline_signal",
          Pre => Ordered (T), Post => Ordered (T);
   procedure C_Add
     (T : in out Timeline; V : Sync_Value; Context : Unsigned_32; GPU : GPU_Value;
      Result : out Add_Result)
     with Export, Convention => C, External_Name => "native_gpu_timeline_add",
          Pre => Ordered (T), Post => Ordered (T);
   procedure C_Resolve (T : in out Timeline; Completed : Completed_Values)
     with Export, Convention => C, External_Name => "native_gpu_timeline_resolve",
          Pre => Ordered (T), Post => Ordered (T);
   procedure C_Value (T : Timeline; Value, Last : out Sync_Value)
     with Export, Convention => C, External_Name => "native_gpu_timeline_value",
          Pre => Ordered (T);
   procedure C_Find
     (T : Timeline; W : Sync_Value; Kind : out Wait_Kind; Context : out Unsigned_32;
      GPU : out GPU_Value)
     with Export, Convention => C, External_Name => "native_gpu_timeline_find",
          Pre => Ordered (T);
   procedure C_Move
     (Target, Source : in out Timeline; Source_Next : Sync_Value;
      Target_Next, New_Source_Next : out Sync_Value; OK : out Unsigned_32)
     with Export, Convention => C, External_Name => "native_gpu_timeline_move",
          Pre => Ordered (Target) and then Ordered (Source),
          Post => Ordered (Target) and then Ordered (Source);
   procedure C_Clear (S : out Wait_Set)
     with Export, Convention => C, External_Name => "native_gpu_wait_set_clear";
   --  OK 0: Job or Context is not a context index (nothing changes).
   procedure C_Merge
     (S : in out Wait_Set; Job, Context : Unsigned_32; GPU : GPU_Value; OK : out Unsigned_32)
     with Export, Convention => C, External_Name => "native_gpu_wait_set_merge";
   procedure C_Select
     (S : Wait_Set; First_Context : out Unsigned_32; First_Target : out GPU_Value;
      Second_Context : out Unsigned_32; Second_Target : out GPU_Value; Fits : out Unsigned_32)
     with Export, Convention => C, External_Name => "native_gpu_wait_set_select";
   --  Timeline_Bytes, for the C side's check of its storage.
   function C_Timeline_Bytes return Unsigned_32 is (Timeline_Bytes)
     with Export, Convention => C, External_Name => "native_gpu_timeline_bytes";

end Native_GPU_Timeline;
