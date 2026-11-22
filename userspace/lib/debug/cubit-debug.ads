------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The debugger engine (DBG-001): one engine with three faces, the library
--  the Debugger app links, the debug.* CCL calls, and later a remote grant.
--
--  @description
--  The preview implements the cooperative path only (docs/debugging.md,
--  "cooperative attach (preview)"): the target links CuBit.Debug_Stub,
--  registers the debug-target service, and shares its own pages READ-ONLY
--  with the debugger that calls it (CuBit.Debug_Protocol). Its threads
--  park at CuBit.Debug_Stub.Checkpoint when the debugger asks: a
--  cooperative stop, not a kernel one. Every value comes from the target's
--  memory through those grants, typed by the target's DWARF (its build-id
--  debug file); nothing is invented.
--
--  Results are bounded by the named ceilings below; a list that would pass
--  one says so (More), nothing is silently dropped. Calls that talk to the
--  target take an explicit deadline (absolute monotonic milliseconds,
--  CuBit.Messages.Deadline_After); reads through the grants do not block.
--
--  A Graph result is large (Graph_Node_Ceiling nodes with their fields):
--  allocate it on the heap (new Graph_Result), not on a task stack.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Debug_Target;

package CuBit.Debug is

   ---------------------------------------------------------------------------
   --  Ceilings (named, visible to callers)
   ---------------------------------------------------------------------------
   Session_Ceiling        : constant := 4;
   Thread_Ceiling         : constant := 16;
   Frame_Ceiling          : constant := 64;
   Ref_List_Ceiling       : constant := 128;
   Children_Page_Ceiling  : constant := 64;
   Memory_Read_Ceiling    : constant := 4096;
   Source_Page_Ceiling    : constant := 64;
   Graph_Node_Ceiling     : constant := 128;
   Graph_Edge_Ceiling     : constant := 256;
   Graph_Field_Ceiling    : constant := 12;
   Graph_Depth_Ceiling    : constant := 16;

   Name_Capacity          : constant := 96;
   Display_Capacity       : constant := 160;
   Path_Capacity          : constant := 256;
   Source_Line_Capacity   : constant := 200;
   Field_Name_Capacity    : constant := 48;
   Field_Display_Capacity : constant := 64;
   Build_Id_Bytes         : constant := 20;

   ---------------------------------------------------------------------------
   --  Bounded texts
   ---------------------------------------------------------------------------
   subtype Name_Length is Natural range 0 .. Name_Capacity;
   type Name_Text is record
      Bytes  : String (1 .. Name_Capacity) := [others => ' '];
      Length : Name_Length := 0;
   end record;
   subtype Display_Length is Natural range 0 .. Display_Capacity;
   type Display_Text is record
      Bytes  : String (1 .. Display_Capacity) := [others => ' '];
      Length : Display_Length := 0;
   end record;
   subtype Path_Length is Natural range 0 .. Path_Capacity;
   type Path_Text is record
      Bytes  : String (1 .. Path_Capacity) := [others => ' '];
      Length : Path_Length := 0;
   end record;
   subtype Source_Line_Length is Natural range 0 .. Source_Line_Capacity;
   type Source_Line_Text is record
      Bytes  : String (1 .. Source_Line_Capacity) := [others => ' '];
      Length : Source_Line_Length := 0;
   end record;
   subtype Field_Name_Length is Natural range 0 .. Field_Name_Capacity;
   type Field_Name_Text is record
      Bytes  : String (1 .. Field_Name_Capacity) := [others => ' '];
      Length : Field_Name_Length := 0;
   end record;
   subtype Field_Display_Length is Natural range 0 .. Field_Display_Capacity;
   type Field_Display_Text is record
      Bytes  : String (1 .. Field_Display_Capacity) := [others => ' '];
      Length : Field_Display_Length := 0;
   end record;

   function Image (T : Name_Text) return String is (T.Bytes (1 .. T.Length));
   function Image (T : Display_Text) return String is (T.Bytes (1 .. T.Length));
   function Image (T : Path_Text) return String is (T.Bytes (1 .. T.Length));
   function Image (T : Source_Line_Text) return String is (T.Bytes (1 .. T.Length));
   function Image (T : Field_Name_Text) return String is (T.Bytes (1 .. T.Length));
   function Image (T : Field_Display_Text) return String is (T.Bytes (1 .. T.Length));

   --  Text truncated to the capacity, ending in "~" when cut.
   function To_Name (S : String) return Name_Text;
   function To_Display (S : String) return Display_Text;
   function To_Path (S : String) return Path_Text;

   ---------------------------------------------------------------------------
   --  Basic types
   ---------------------------------------------------------------------------
   type Target_Address is mod 2 ** 64;
   subtype Deadline_Ms is Unsigned_64;
   Forever : constant Deadline_Ms := Deadline_Ms'Last;

   --  Rights (docs/debugging.md 2.1). Observe only: through the kernel,
   --  Debug_Observe; through the cooperative stub, Pause is the stub's
   --  park, which the target opted into by linking it.
   type Debug_Rights is (Observe);

   type Outcome is
     (Ok,
      More,            --  a ceiling was reached; ask again from the cursor
      Not_Attached,    --  no such session
      Refused,         --  the target or the kernel refused (grant, policy)
      No_Target,       --  no debug-target service answered
      Timed_Out,       --  the deadline passed
      Not_Stopped,     --  frames and locals need the target parked
      Stale,           --  the frame or reference is from an earlier stop
      No_Debug_Info,   --  no debug file for the target's build-id
      Unreadable,      --  the memory is not shared with the debugger
      Not_Found,       --  no such thread, frame, unit, file or line
      Malformed,       --  the debug information or a reply is malformed
      Exited);         --  the target has ended

   type Target_State is (Running, Stopped_Cooperatively, Exited, Detached);

   type Session is private;
   No_Session : constant Session;

   ---------------------------------------------------------------------------
   --  Session
   ---------------------------------------------------------------------------
   type Program_Info is record
      Program     : Name_Text;      --  the target's name, as its stub says
      Process     : Unsigned_64 := 0;  --  its identity word
      Build_Id    : String (1 .. 2 * Build_Id_Bytes) := [others => '0'];
      Debug_File  : Path_Text;      --  where its DWARF was read
      Attach_Mode : Name_Text;      --  "cooperative attach (preview)"
      Checkpoints : Unsigned_64 := 0;  --  checkpoints passed so far
   end record;

   procedure Attach
     (Target     : CuBit.Debug_Target.Process_Handle;
      Rights     : Debug_Rights;
      Until_Time : Deadline_Ms;
      S          : out Session;
      R          : out Outcome);
   procedure Detach (S : in out Session);
   function State (S : Session) return Target_State;
   --  Attached through the kernel (a Debug_Observe session; the target
   --  needs no stub): threads and wait reasons, registers, and memory
   --  through read-only views, unwound from the registers the kernel saved.
   --  Pause and Resume answer Refused there (the kernel has no stop yet);
   --  Frames and Locals read a thread as it is now, each read a new epoch.
   function Through_Kernel (S : Session) return Boolean;
   procedure Describe (S : Session; Info : out Program_Info; R : out Outcome);

   --  Ask every checkpointing thread to park at its next checkpoint, and
   --  wait (until Until_Time) for all of them. R = Timed_Out when some
   --  did not park in time (the others stay parked; Resume releases them).
   procedure Pause (S : Session; Until_Time : Deadline_Ms; R : out Outcome);
   procedure Resume (S : Session; Until_Time : Deadline_Ms; R : out Outcome);

   ---------------------------------------------------------------------------
   --  Threads and frames
   ---------------------------------------------------------------------------
   type Thread_Id is new Unsigned_32;
   type Thread_State is (Running, Parked, Ended);
   --  What a thread waits on, as the kernel says (attached through the
   --  kernel); Not_Known through the cooperative stub.
   subtype Thread_Wait is CuBit.Debug_Target.Wait_Code;
   type Thread_Info is record
      Id    : Thread_Id := 0;
      Name  : Name_Text;
      State : Thread_State := Running;
      PC    : Target_Address := 0;   --  at its last checkpoint, or sampled
      Stack_Low, Stack_High : Target_Address := 0;
      Wait  : Thread_Wait := CuBit.Debug_Target.Not_Known;
      --  In_Call: the server's identity and when the call began (ms since
      --  boot); a wait's deadline (Unsigned_64'Last: none).
      Peer     : Unsigned_64 := 0;
      Since    : Unsigned_64 := 0;
      Deadline : Unsigned_64 := Unsigned_64'Last;
      CPU      : Unsigned_32 := 0;
   end record;
   subtype Thread_Count is Natural range 0 .. Thread_Ceiling;
   subtype Thread_Index is Positive range 1 .. Thread_Ceiling;
   type Thread_Array is array (Thread_Index) of Thread_Info;
   type Thread_List is record
      Items : Thread_Array;
      Count : Thread_Count := 0;
   end record;
   procedure Threads (S : Session; List : out Thread_List; R : out Outcome);

   --  Each stop has a new epoch; frames and references from an earlier
   --  stop are Stale.
   type Stop_Epoch is new Unsigned_64;
   subtype Frame_Count is Natural range 0 .. Frame_Ceiling;
   subtype Frame_Index is Natural range 0 .. Frame_Ceiling - 1;
   subtype Line_Number is Natural;
   subtype Column_Number is Natural;
   type Frame is record
      Thread   : Thread_Id := 0;
      Index    : Frame_Index := 0;        --  0 is the innermost
      Epoch    : Stop_Epoch := 0;
      Function_Name : Name_Text;           --  as in the source (Debug_Demo.Insert)
      File     : Path_Text;                --  the source file's name
      Line     : Line_Number := 0;
      Column   : Column_Number := 0;
      PC       : Target_Address := 0;
      CFA      : Target_Address := 0;
      In_Stub  : Boolean := False;         --  CuBit.Debug_Stub's own frame
      Has_Debug_Info : Boolean := False;   --  False: a runtime frame without DWARF
   end record;
   type Frame_Array is array (Frame_Index) of Frame;
   type Frame_List is record
      Items : Frame_Array;
      Count : Frame_Count := 0;
   end record;
   procedure Frames
     (S : Session; Thread : Thread_Id; List : out Frame_List; R : out Outcome);

   ---------------------------------------------------------------------------
   --  Values
   ---------------------------------------------------------------------------
   --  A DWARF type's identity in the debug file (its .debug_info offset).
   type Type_Key is new Unsigned_64;
   No_Type : constant Type_Key := 0;

   type Location_Kind is
     (In_Memory,        --  Address names the bytes
      Register_Value,   --  the value itself is Bits (a register, or a constant)
      Optimized_Out,    --  the compiler kept no location here
      Unavailable);     --  not computable in the preview (named in Display)

   type Value_Ref is record
      Name     : Name_Text;
      Location : Location_Kind := Unavailable;
      Address  : Target_Address := 0;
      Bits     : Unsigned_64 := 0;
      Of_Type  : Type_Key := No_Type;
      --  Bounds of an unconstrained array reached through a fat pointer.
      Has_Bounds : Boolean := False;
      Low, High  : Integer_64 := 0;
      Epoch    : Stop_Epoch := 0;    --  0: a global, valid across stops
   end record;
   Null_Ref : constant Value_Ref;

   subtype Ref_Count is Natural range 0 .. Ref_List_Ceiling;
   subtype Ref_Index is Positive range 1 .. Ref_List_Ceiling;
   type Ref_Array is array (Ref_Index) of Value_Ref;
   type Ref_List is record
      Items : Ref_Array;
      Count : Ref_Count := 0;
   end record;

   type Value_Kind is
     (Scalar, Enum, Record_Value, Variant_Record, Array_Value, Pointer,
      Rust_Enum, Slice, Unknown);

   type Value_Node is record
      Self        : Value_Ref;
      Name        : Name_Text;
      Type_Name   : Name_Text;
      Kind        : Value_Kind := Unknown;
      Display     : Display_Text;
      Address     : Target_Address := 0;
      Size        : Unsigned_64 := 0;          --  bytes
      Child_Count : Natural := 0;
      Has_Target  : Boolean := False;          --  a non-null pointer
      Target      : Value_Ref;                 --  what it points at
      Changed     : Boolean := False;          --  since this session last read it
      --  Ada ranges: the subtype's bounds, and whether the value is outside
      --  them (only possible in -gnatp code after a bug).
      Has_Range   : Boolean := False;
      Range_Low, Range_High : Integer_64 := 0;
      Out_Of_Range : Boolean := False;
      --  Variant records: the discriminant's display.
      Variant     : Name_Text;
   end record;

   procedure Locals
     (S : Session; F : Frame; List : out Ref_List; R : out Outcome);
   --  Unit: a library-level package name as GNAT names it ("debug_demo"),
   --  or "" for every package of the program's own units.
   procedure Globals
     (S : Session; Unit : String; List : out Ref_List; R : out Outcome);
   procedure Value
     (S : Session; Ref : Value_Ref; Node : out Value_Node; R : out Outcome);

   subtype Child_Count is Natural range 0 .. Children_Page_Ceiling;
   subtype Child_Index is Positive range 1 .. Children_Page_Ceiling;
   type Node_Array is array (Child_Index) of Value_Node;
   type Node_List is record
      Items : Node_Array;
      Count : Child_Count := 0;
   end record;
   --  Children First .. First + Count - 1 (0-based) of Ref. R = More when
   --  the value has more children after the page.
   procedure Children
     (S : Session; Ref : Value_Ref; First : Natural; Count : Child_Count;
      List : out Node_List; R : out Outcome);

   ---------------------------------------------------------------------------
   --  Memory and source
   ---------------------------------------------------------------------------
   subtype Memory_Length is Natural range 0 .. Memory_Read_Ceiling;
   type Byte_Array is array (Positive range <>) of Unsigned_8;
   --  Bytes (1 .. Got): Got < Length when the rest is not shared.
   procedure Memory
     (S : Session; Address : Target_Address; Length : Memory_Length;
      Bytes : out Byte_Array; Got : out Memory_Length; R : out Outcome)
   with Pre => Bytes'First = 1 and then Bytes'Length >= Length;

   subtype Source_Count is Natural range 0 .. Source_Page_Ceiling;
   subtype Source_Index is Positive range 1 .. Source_Page_Ceiling;
   type Source_Array is array (Source_Index) of Source_Line_Text;
   type Source_Page is record
      First : Line_Number := 1;        --  the line number of Lines (1)
      Lines : Source_Array;
      Count : Source_Count := 0;
      Total : Line_Number := 0;        --  lines in the whole file
   end record;
   --  File: as a Frame names it.
   procedure Source
     (S : Session; File : String; First_Line : Line_Number;
      Page : out Source_Page; R : out Outcome);

   ---------------------------------------------------------------------------
   --  Data-structure graph (docs/debugging.md 9)
   ---------------------------------------------------------------------------
   --  Stable across reads: the object's address and its type.
   type Node_Id is record
      Address : Target_Address := 0;
      Of_Type : Type_Key := No_Type;
   end record;

   type Graph_Field is record
      Name     : Field_Name_Text;
      Display  : Field_Display_Text;
      Kind     : Value_Kind := Unknown;
      Changed  : Boolean := False;
      Is_Edge  : Boolean := False;   --  a pointer field (see the edges)
      Is_Null  : Boolean := False;   --  a null pointer (drawn as ground)
   end record;
   subtype Field_Count is Natural range 0 .. Graph_Field_Ceiling;
   subtype Field_Index is Positive range 1 .. Graph_Field_Ceiling;
   type Field_Array is array (Field_Index) of Graph_Field;

   type Graph_Node is record
      Id      : Node_Id;
      Value   : Value_Node;     --  the object itself (name, type, kind ...)
      Fields  : Field_Array;
      Field_Total : Natural := 0;    --  fields it has (Fields holds the first)
      Fields_Shown : Field_Count := 0;
      Depth   : Natural := 0;        --  from the nearest root
      Is_Root : Boolean := False;
      Incoming : Natural := 0;       --  > 1: shared
   end record;
   type Edge_Kind is (Pointer_Edge, Back_Edge, Shared_Edge);
   type Graph_Edge is record
      From, To : Positive := 1;      --  node indexes in Nodes
      From_Field : Natural := 0;     --  field index in From, 0: the node itself
      Label   : Field_Name_Text;
      Kind    : Edge_Kind := Pointer_Edge;
   end record;
   subtype Node_Count is Natural range 0 .. Graph_Node_Ceiling;
   subtype Node_Index is Positive range 1 .. Graph_Node_Ceiling;
   subtype Edge_Count is Natural range 0 .. Graph_Edge_Ceiling;
   subtype Edge_Index is Positive range 1 .. Graph_Edge_Ceiling;
   type Graph_Node_Array is array (Node_Index) of Graph_Node;
   type Graph_Edge_Array is array (Edge_Index) of Graph_Edge;
   type Graph_Result is record
      Nodes : Graph_Node_Array;
      Node_Total : Node_Count := 0;
      Edges : Graph_Edge_Array;
      Edge_Total : Edge_Count := 0;
      Truncated : Boolean := False;  --  Max_Nodes or a ceiling was reached
   end record;
   subtype Graph_Depth is Natural range 0 .. Graph_Depth_Ceiling;

   --  Follow pointers breadth-first from Roots, at most Depth levels and
   --  Max_Nodes objects. R = More when it stopped at a bound.
   procedure Graph
     (S : Session; Roots : Ref_List; Depth : Graph_Depth; Max_Nodes : Node_Count;
      Result : in out Graph_Result; R : out Outcome);

private
   subtype Session_Slot is Natural range 0 .. Session_Ceiling;
   type Session is record
      Slot : Session_Slot := 0;
      Generation : Unsigned_32 := 0;
   end record;
   No_Session : constant Session := (Slot => 0, Generation => 0);
   Null_Ref : constant Value_Ref := (others => <>);
end CuBit.Debug;
