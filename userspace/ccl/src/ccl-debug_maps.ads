with CCL.Language;
with CCL.Ownership;
with CCL.VM;

package CCL.Debug_Maps with
   SPARK_Mode => On
is
   --  One entry per compiled node at most: a named ceiling (the syntax
   --  tree's), and the default map's room; a map is sized by its program's
   --  tree (CCL.Compiler.Compilation_Of).
   ENTRY_CEILING   : constant := CCL.Language.NODE_CEILING;
   DEFAULT_ENTRIES : constant := CCL.Language.DEFAULT_NODE_ROOM;

   subtype Entry_Index is Natural range 0 .. ENTRY_CEILING - 1;
   subtype Entry_Count is Natural range 0 .. ENTRY_CEILING;

   type Debug_Entry is record
      First_PC : CCL.VM.Program_Length := 0;
      End_PC   : CCL.VM.Program_Length := 0;
      Node     : CCL.Language.Node_Reference := CCL.Language.NO_NODE;
      Source_First : CCL.Language.Source_Position := 0;
      Source_End   : CCL.Language.Source_Position := 0;
   end record;

   --  The room after Last + 1 doubles, within the ceiling.
   function Grown_Entries (Last : Entry_Index) return Entry_Index is
     (if Last >= Entry_Index'Last / 2 then Entry_Index'Last else 2 * Last + 1);

   --  Room for Last_Entry + 1 entries.
   type Debug_Map (Last_Entry : Entry_Index) is private;
   type Add_Result is (Added, Map_Full);

   procedure Initialize (Item : out Debug_Map);

   procedure Add
     (Item   : in out Debug_Map;
      New_Entry : Debug_Entry;
      Result : out Add_Result);

   procedure Set_Local_Name
     (Item       : in out Debug_Map;
      Local      : CCL.Ownership.Binding_Id;
      Identifier : CCL.Language.Name);

   function Has_Local_Name
     (Item  : Debug_Map;
      Local : CCL.Ownership.Binding_Id) return Boolean;

   function Local_Name
     (Item  : Debug_Map;
      Local : CCL.Ownership.Binding_Id) return CCL.Language.Name;

   function Length (Item : Debug_Map) return Entry_Count;

   function Element
     (Item  : Debug_Map;
      Index : Entry_Index) return Debug_Entry;

   type Validation_Error is
     (Debug_Map_Valid,
      Empty_PC_Range,
      PC_Outside_Program,
      Invalid_Source_Range,
      Invalid_Node_Reference);

   procedure Validate
     (Item           : Debug_Map;
      Program_Length : CCL.VM.Program_Length;
      Error          : out Validation_Error);

   procedure Find_Innermost
     (Item  : Debug_Map;
      PC    : CCL.VM.Instruction_Index;
      Match : out Debug_Entry;
      Found : out Boolean);

private
   type Entry_Array is array (Entry_Index range <>) of Debug_Entry;
   type Local_Name_Array is
     array (CCL.Ownership.Binding_Id) of CCL.Language.Name;
   type Local_Name_Presence is
     array (CCL.Ownership.Binding_Id) of Boolean;

   type Debug_Map (Last_Entry : Entry_Index) is record
      Count   : Entry_Count := 0;
      Entries : Entry_Array (0 .. Last_Entry) := [others => (others => <>)];
      Local_Names : Local_Name_Array := [others => (others => <>)];
      Named_Locals : Local_Name_Presence := [others => False];
   end record;

   function Length (Item : Debug_Map) return Entry_Count is (Item.Count);

   function Element
     (Item  : Debug_Map;
      Index : Entry_Index) return Debug_Entry is
     (if Index <= Item.Last_Entry then Item.Entries (Index) else (others => <>));

   function Has_Local_Name
     (Item  : Debug_Map;
      Local : CCL.Ownership.Binding_Id) return Boolean is
     (Item.Named_Locals (Local));

   function Local_Name
     (Item  : Debug_Map;
      Local : CCL.Ownership.Binding_Id) return CCL.Language.Name is
     (Item.Local_Names (Local));
end CCL.Debug_Maps;
