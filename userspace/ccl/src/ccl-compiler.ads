with CCL.Language;
with CCL.Catalog;
with CCL.Debug_Maps;
with CCL.VM;

package CCL.Compiler with
   SPARK_Mode => On
is
   type Compilation_Status is
     (Compilation_Succeeded,
      Analysis_Failed,
      Ownership_Check_Failed,
      Unsupported_Form,
      Malformed_Typed_Tree,
      Too_Many_Locals,
      Too_Many_Imports,
      Too_Many_Matches,
      Stack_Limit,
      Debug_Map_Full,
      Program_Full);

   --  A compilation whose program has the rooms its discriminants give
   --  (CCL.VM.Sized_Program) and whose debug map Last_Entry + 1 entries.
   --  Program_Full and Debug_Map_Full say these rooms were too small;
   --  Compilation_Of grows them until the program fits.
   type Sized_Compilation
     (Last_Instruction : CCL.VM.Instruction_Index;
      Last_Constant    : CCL.VM.Constant_Index;
      Text_Room        : CCL.VM.Constant_Text_Room;
      Last_Entry       : CCL.Debug_Maps.Entry_Index) is
   record
      Status          : Compilation_Status := Analysis_Failed;
      Diagnostic_Node : CCL.Language.Node_Reference := CCL.Language.NO_NODE;
      Source_Position : CCL.Language.Source_Position := 0;
      Program         : CCL.VM.Sized_Program (Last_Instruction, Last_Constant, Text_Room);
      Linkage         : CCL.Catalog.Linkage_Table;
      Debug           : CCL.Debug_Maps.Debug_Map (Last_Entry);
   end record;
   subtype Compilation_Result is Sized_Compilation
     (Last_Instruction => CCL.VM.DEFAULT_LAST_INSTRUCTION,
      Last_Constant    => CCL.VM.DEFAULT_LAST_CONSTANT,
      Text_Room        => CCL.VM.DEFAULT_CONSTANT_BYTES,
      Last_Entry       => CCL.Debug_Maps.DEFAULT_ENTRIES - 1);

   procedure Compile
     (Analysis : CCL.Language.Sized_Analysis;
      Result   : out Sized_Compilation);

   --  A compilation's rooms (its discriminants), how they start for an
   --  analysis (the defaults; the debug map from its node count), and how
   --  they grow: Program_Full doubles the program's, Debug_Map_Full the
   --  map's, each within its ceiling.
   type Program_Rooms is record
      Last_Instruction : CCL.VM.Instruction_Index := CCL.VM.DEFAULT_LAST_INSTRUCTION;
      Last_Constant    : CCL.VM.Constant_Index := CCL.VM.DEFAULT_LAST_CONSTANT;
      Text_Room        : CCL.VM.Constant_Text_Room := CCL.VM.DEFAULT_CONSTANT_BYTES;
      Last_Entry       : CCL.Debug_Maps.Entry_Index := CCL.Debug_Maps.DEFAULT_ENTRIES - 1;
   end record;
   function First_Rooms (Analysis : CCL.Language.Sized_Analysis) return Program_Rooms;
   --  Whether Status says a room was too small and that room can still grow.
   function Outgrown (Rooms : Program_Rooms; Status : Compilation_Status) return Boolean;
   function Grown (Rooms : Program_Rooms; Status : Compilation_Status) return Program_Rooms
     with Pre => Outgrown (Rooms, Status);
   --  Each growth doubles a room, so the ceilings stop growth within this
   --  many attempts (each ceiling is at most 2 ** 24 times its default).
   GROWTH_STEPS : constant := 2 * 24;

   --  Analysis compiled into rooms sized by it: from the defaults (the
   --  debug map from its node count), doubled until the program fits.
   --  Program_Full and Debug_Map_Full then mean a named ceiling
   --  (CCL.VM.INSTRUCTION_CEILING, CONSTANT_CEILING, CONSTANT_BYTE_CEILING,
   --  CCL.Debug_Maps.ENTRY_CEILING), never a fixed buffer.
   function Compilation_Of (Analysis : CCL.Language.Sized_Analysis) return Sized_Compilation;
end CCL.Compiler;
