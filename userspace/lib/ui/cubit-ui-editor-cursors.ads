with CuBit.UI.Editor.Documents;

package CuBit.UI.Editor.Cursors with SPARK_Mode is
   subtype Cursor_Position is
     CuBit.UI.Editor.Documents.Document_Position;
   MAX_CURSORS : constant := 32;

   subtype Cursor_Index is Positive range 1 .. MAX_CURSORS;
   subtype Cursor_Count is Positive range 1 .. MAX_CURSORS;

   type Cursor_State is record
      Position : Cursor_Position := 1;
      Anchor   : Cursor_Position := 1;
      Preferred_Column :
        CuBit.UI.Editor.Documents.Display_Column := 1;
   end record;

   type Cursor_Set is private;
   type Toggle_Result is
     (Cursor_Added, Cursor_Removed, Primary_Moved, Cursor_Limit_Reached);

   procedure Initialize
     (Cursors : out Cursor_Set; Position : Cursor_Position := 1);
   function Length (Cursors : Cursor_Set) return Cursor_Count;
   function Primary_Index (Cursors : Cursor_Set) return Cursor_Index;
   function Element
     (Cursors : Cursor_Set; Index : Cursor_Index) return Cursor_State
   with Pre => Index <= Length (Cursors);

   procedure Set_Element
     (Cursors : in out Cursor_Set; Index : Cursor_Index;
      Value : Cursor_State)
   with Pre => Index <= Length (Cursors);

   procedure Toggle_At
     (Cursors : in out Cursor_Set; Position : Cursor_Position;
      Result : out Toggle_Result);

   procedure Coalesce (Cursors : in out Cursor_Set);

private
   type Cursor_Array is array (Cursor_Index) of Cursor_State;
   type Cursor_Set is record
      Items   : Cursor_Array := [others => (others => 1)];
      Last    : Cursor_Count := 1;
      Primary : Cursor_Index := 1;
   end record
   with Type_Invariant => Cursor_Set.Primary <= Cursor_Set.Last;
end CuBit.UI.Editor.Cursors;
