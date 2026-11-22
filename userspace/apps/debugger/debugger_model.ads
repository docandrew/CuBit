------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Debugger (DBG-001): the GUI's snapshot of one debug session
------------------------------------------------------------------------------
--  What the views draw, filled by Debugger_Source from the engine
--  (CuBit.Debug, the contract in docs/debugging.md section 7). Every
--  ceiling is named; a read that would pass one is cut and says so
--  (Truncated), never wraps. Nothing here is invented: an empty snapshot
--  draws as empty panes.
with Interfaces; use Interfaces;

package Debugger_Model with SPARK_Mode is

   ---------------------------------------------------------------------------
   --  Text: a short string with its length, no heap.
   TEXT_CAPACITY : constant := 120;
   subtype Text_Length is Natural range 0 .. TEXT_CAPACITY;
   type Text is record
      Data   : String (1 .. TEXT_CAPACITY) := [others => ' '];
      Length : Text_Length := 0;
   end record;
   function Image (T : Text) return String is (T.Data (1 .. T.Length));
   --  S, cut to the capacity (callers never see more than fits).
   function To_Text (S : String) return Text;
   function "=" (L : Text; R : String) return Boolean is (Image (L) = R);

   --  A graph field's name or value: shorter, since a graph holds many.
   SHORT_CAPACITY : constant := 48;
   subtype Short_Length is Natural range 0 .. SHORT_CAPACITY;
   type Short_Text is record
      Data   : String (1 .. SHORT_CAPACITY) := [others => ' '];
      Length : Short_Length := 0;
   end record;
   function Image (T : Short_Text) return String is (T.Data (1 .. T.Length));
   function To_Short (S : String) return Short_Text;

   ---------------------------------------------------------------------------
   --  Ceilings (named, no magic numbers).
   MAX_THREADS        : constant := 32;
   MAX_FRAMES         : constant := 64;
   MAX_LOCALS         : constant := 1_024;
   MAX_DEPTH          : constant := 12;
   MAX_GRAPH_NODES    : constant := 512;
   MAX_NODE_FIELDS    : constant := 12;
   MAX_GRAPH_EDGES    : constant := 1_024;
   MEMORY_WINDOW      : constant := 512;
   MAX_SOURCE_LINES   : constant := 8_192;
   SOURCE_POOL_BYTES  : constant := 524_288;

   subtype Thread_Count is Natural range 0 .. MAX_THREADS;
   subtype Thread_Index is Positive range 1 .. MAX_THREADS;
   subtype Frame_Count is Natural range 0 .. MAX_FRAMES;
   subtype Frame_Index is Positive range 1 .. MAX_FRAMES;
   subtype Local_Count is Natural range 0 .. MAX_LOCALS;
   subtype Local_Index is Positive range 1 .. MAX_LOCALS;
   subtype Tree_Depth is Natural range 0 .. MAX_DEPTH;
   subtype Node_Count is Natural range 0 .. MAX_GRAPH_NODES;
   subtype Node_Index is Positive range 1 .. MAX_GRAPH_NODES;
   subtype Field_Count is Natural range 0 .. MAX_NODE_FIELDS;
   subtype Field_Index is Positive range 1 .. MAX_NODE_FIELDS;
   subtype Edge_Count is Natural range 0 .. MAX_GRAPH_EDGES;
   subtype Edge_Index is Positive range 1 .. MAX_GRAPH_EDGES;
   subtype Memory_Length is Natural range 0 .. MEMORY_WINDOW;
   subtype Memory_Offset is Natural range 0 .. MEMORY_WINDOW - 1;
   subtype Line_Count is Natural range 0 .. MAX_SOURCE_LINES;
   subtype Line_Number is Positive range 1 .. MAX_SOURCE_LINES;
   subtype Pool_Length is Natural range 0 .. SOURCE_POOL_BYTES;
   subtype Pool_Index is Positive range 1 .. SOURCE_POOL_BYTES;

   subtype Target_Address is Unsigned_64;
   NULL_ADDRESS : constant Target_Address := 0;
   --  The engine's opaque value reference (Value_Ref); 0 is none.
   subtype Value_Ref is Unsigned_64;
   NO_REF : constant Value_Ref := 0;
   --  A graph node's stable identity: (address, type), hashed by the engine.
   subtype Node_Id is Unsigned_64;
   NO_NODE : constant Node_Id := 0;
   --  Monotonic microseconds (CuBit.Monotonic on CuBit).
   subtype Microseconds is Unsigned_64;
   NEVER : constant Microseconds := 0;

   ---------------------------------------------------------------------------
   type Target_State is (Not_Attached, Running, Stopped_Cooperatively, Exited);
   --  How each value was obtained (docs/debugging.md 8.1): Live is read while
   --  the target ran; Stopped while it was parked at a checkpoint.
   type Read_Mode is (Stopped, Live);

   type Thread_State is (Thread_Running, Thread_Parked, Thread_Blocked, Thread_Exited, Thread_Unknown);
   type Thread_Info is record
      Id    : Unsigned_64 := 0;
      Name  : Text;
      State : Thread_State := Thread_Unknown;
      PC    : Target_Address := NULL_ADDRESS;
      Wait  : Text;   --  wait reason, when the engine knows one
   end record;
   type Thread_Array is array (Thread_Index) of Thread_Info;

   --  Attach to process...: what runs, to pick a target from (procmgr's
   --  process-observer listing), at most MAX_PROCESSES of them.
   MAX_PROCESSES : constant := 64;
   subtype Process_Count is Natural range 0 .. MAX_PROCESSES;
   subtype Process_Index is Positive range 1 .. MAX_PROCESSES;
   type Process_Entry is record
      Name     : Text;
      Identity : Unsigned_64 := 0;
   end record;
   type Process_Array is array (Process_Index) of Process_Entry;

   type Frame_Info is record
      Func   : Text;
      File   : Text;
      Line   : Natural := 0;
      Column : Natural := 0;
      PC     : Target_Address := NULL_ADDRESS;
      CFA    : Target_Address := NULL_ADDRESS;
   end record;
   type Frame_Array is array (Frame_Index) of Frame_Info;

   --  The engine's Value_Node kinds (ENGINE-API.md), plus Discriminant for a
   --  variant record's discriminant component.
   type Value_Kind is
     (Scalar, Enum, Record_Value, Variant_Record, Array_Value, Pointer,
      Rust_Enum, Slice, Discriminant, Unknown);

   type Local_Entry is record
      Ref         : Value_Ref := NO_REF;
      Name        : Text;
      Type_Name   : Text;
      Value       : Text;
      --  An Ada range, "2 .. 5", or array bounds, "(1 .. 8)"; empty if none.
      Bounds      : Text;
      Out_Of_Range : Boolean := False;
      Kind        : Value_Kind := Unknown;
      Address     : Target_Address := NULL_ADDRESS;
      Size        : Unsigned_64 := 0;
      Child_Count : Natural := 0;
      Target      : Value_Ref := NO_REF;
      Depth       : Tree_Depth := 0;
      Expanded    : Boolean := False;
      Loaded      : Boolean := False;
      Changed_At  : Microseconds := NEVER;
   end record;
   type Local_Array is array (Local_Index) of Local_Entry;

   type Graph_Field is record
      Name       : Short_Text;
      Value      : Short_Text;
      Is_Pointer : Boolean := False;
      Pointee    : Value_Ref := NO_REF;     --  what to expand on click
      Target     : Node_Id := NO_NODE;      --  drawn edge, once expanded
      Dangling   : Boolean := False;
      Changed_At : Microseconds := NEVER;
   end record;
   type Field_Array is array (Field_Index) of Graph_Field;

   type Graph_Node is record
      Id          : Node_Id := NO_NODE;
      Title       : Text;     --  type name
      Label       : Text;     --  root variable name, or empty
      Address     : Target_Address := NULL_ADDRESS;
      Kind        : Value_Kind := Record_Value;
      Fields      : Field_Array;
      Field_Total : Field_Count := 0;
      More_Fields : Natural := 0;    --  fields past MAX_NODE_FIELDS
      Is_Root     : Boolean := False;
      Appeared_At : Microseconds := NEVER;
   end record;
   type Node_Array is array (Node_Index) of Graph_Node;

   type Graph_Edge is record
      From       : Node_Index := 1;
      From_Field : Field_Index := 1;
      To         : Node_Index := 1;
      Back       : Boolean := False;   --  closes a cycle
   end record;
   type Edge_Array is array (Edge_Index) of Graph_Edge;

   type Byte_Array is array (Memory_Offset) of Unsigned_8;
   type Line_Span is record
      First : Pool_Index := 1;
      Last  : Natural := 0;            --  First - 1 when empty
   end record;
   type Line_Array is array (Line_Number) of Line_Span;

   type Snapshot is record
      --  Session.
      State         : Target_State := Not_Attached;
      Mode          : Read_Mode := Stopped;
      Target_Name   : Text;
      Target_Pid    : Unsigned_64 := 0;
      Checkpoint    : Text;
      Engine_Note   : Text;        --  the engine's last message, verbatim
      Generation    : Unsigned_64 := 0;  --  reads so far
      Read_At       : Microseconds := NEVER;
      Read_Cost_Us  : Microseconds := 0;
      Can_Step      : Boolean := False;
      Can_Resume    : Boolean := False;
      --  Attach to process...: the list last read.
      Processes     : Process_Array;
      Process_Total : Process_Count := 0;
      --  Attached through the kernel (Debug_Observe; no stub in the target).
      Through_Kernel : Boolean := False;
      --  Threads and frames.
      Threads       : Thread_Array;
      Thread_Total  : Thread_Count := 0;
      Thread_Sel    : Thread_Index := 1;
      Frames        : Frame_Array;
      Frame_Total   : Frame_Count := 0;
      Frame_Sel     : Frame_Index := 1;
      --  Locals of the selected frame, in tree pre-order.
      Locals        : Local_Array;
      Local_Total   : Local_Count := 0;
      Locals_Cut    : Boolean := False;
      --  Data-structure graph.
      Nodes         : Node_Array;
      Node_Total    : Node_Count := 0;
      Edges         : Edge_Array;
      Edge_Total    : Edge_Count := 0;
      Graph_Cut     : Boolean := False;
      --  Memory window.
      Mem_Base      : Target_Address := NULL_ADDRESS;
      Mem_Bytes     : Byte_Array := [others => 0];
      Mem_Length    : Memory_Length := 0;
      Mem_Prev      : Byte_Array := [others => 0];
      Mem_Changed_At : Microseconds := NEVER;
      Mem_Note      : Text;
      --  Source of the selected frame's file.
      Source_File   : Text;
      Source_Pool   : String (Pool_Index) := [others => ' '];
      Pool_Used     : Pool_Length := 0;
      Lines         : Line_Array;
      Line_Total    : Line_Count := 0;
      Source_Note   : Text;
   end record;

   ---------------------------------------------------------------------------
   --  Building a snapshot (the provider's side).
   procedure Clear_Source (S : in out Snapshot);
   procedure Add_Source_Line (S : in out Snapshot; Line : String; Added : out Boolean);
   function Source_Line (S : Snapshot; N : Line_Number) return String
     with Pre => N <= S.Line_Total;

   procedure Add_Local (S : in out Snapshot; E : Local_Entry; Added : out Boolean);
   --  Insert Children right after the local at Parent (its pre-order
   --  subtree), at Parent's depth + 1, and mark it Loaded and Expanded.
   type Local_Batch is array (Positive range <>) of Local_Entry;
   procedure Insert_Children (S : in out Snapshot; Parent : Local_Index; Children : Local_Batch);
   --  The last index of Parent's subtree (Parent itself when it has none).
   function Subtree_Last (S : Snapshot; Parent : Local_Index) return Local_Index
     with Pre => Parent <= S.Local_Total;

   procedure Add_Node (S : in out Snapshot; N : Graph_Node; Index : out Natural);
   function Find_Node (S : Snapshot; Id : Node_Id) return Natural;
   procedure Add_Edge (S : in out Snapshot; E : Graph_Edge);
   --  Mark back edges: an edge whose target reaches its source again.
   procedure Classify_Edges (S : in out Snapshot);

   ---------------------------------------------------------------------------
   --  Changes since the previous read (docs/debugging.md 8.3): a value whose
   --  text differs from the same value (same name path, or same node id and
   --  field) in Before is stamped Now. The engine's own changed flags are
   --  stamped by the provider too; this catches what it cannot see.
   procedure Mark_Changes (Before : Snapshot; After : in out Snapshot; Now : Microseconds);
   --  Carry expansion state across a re-read: locals Before had expanded
   --  are reported, so the provider can load them again.
   function Was_Expanded (Before : Snapshot; Name_Path : String) return Boolean;
   function Name_Path (S : Snapshot; I : Local_Index) return String
     with Pre => I <= S.Local_Total;

   --  A changed mark lasts this long (a few frames' worth, then solid off).
   CHANGE_MARK_US : constant Microseconds := 1_200_000;
   function Recently (Stamp, Now : Microseconds) return Boolean is
     (Stamp /= NEVER and then Now >= Stamp and then Now - Stamp < CHANGE_MARK_US);

   --  The selected frame's own package is implied, as a "use" clause would
   --  make it: "Demo_World.Percent" shows as "Percent" while a Demo_World
   --  subprogram is selected. Only the display changes.
   function Package_Prefix (S : Snapshot) return String;
   function Without_Package (S : Snapshot; Text : String) return String;
   --  A value is marked while its change is recent (a few frames) and,
   --  after a stop, until the next read: what moved since the last stop.
   function Marked (Stamp : Microseconds; S : Snapshot; Now : Microseconds) return Boolean is
     (Recently (Stamp, Now) or else
      (Stamp /= NEVER and then S.Mode = Stopped and then Stamp >= S.Read_At));

   function Kind_Has_Children (K : Value_Kind) return Boolean is
     (K in Record_Value | Variant_Record | Array_Value | Pointer | Rust_Enum | Slice);
end Debugger_Model;
