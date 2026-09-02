with CCL.Language;
with CCL.VM;

package CCL.Debug_Maps with
   SPARK_Mode => On
is
   MAX_ENTRIES : constant := CCL.Language.MAX_AST_NODES;

   subtype Entry_Index is Natural range 0 .. MAX_ENTRIES - 1;
   subtype Entry_Count is Natural range 0 .. MAX_ENTRIES;

   type Debug_Entry is record
      First_PC : CCL.VM.Program_Length := 0;
      End_PC   : CCL.VM.Program_Length := 0;
      Node     : CCL.Language.Node_Reference := CCL.Language.NO_NODE;
      Source_First : CCL.Language.Source_Position := 0;
      Source_End   : CCL.Language.Source_Position := 0;
   end record;

   type Debug_Map is private;
   type Add_Result is (Added, Map_Full);

   procedure Initialize (Item : out Debug_Map);

   procedure Add
     (Item   : in out Debug_Map;
      New_Entry : Debug_Entry;
      Result : out Add_Result);

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
   type Entry_Array is array (Entry_Index) of Debug_Entry;

   type Debug_Map is record
      Count   : Entry_Count := 0;
      Entries : Entry_Array := [others => (others => <>)];
   end record;

   function Length (Item : Debug_Map) return Entry_Count is (Item.Count);

   function Element
     (Item  : Debug_Map;
      Index : Entry_Index) return Debug_Entry is (Item.Entries (Index));
end CCL.Debug_Maps;
