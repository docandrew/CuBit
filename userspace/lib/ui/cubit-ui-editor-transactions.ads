with CuBit.UI.Editor.Cursors;
with CuBit.UI.Editor.Documents;

package CuBit.UI.Editor.Transactions with SPARK_Mode is
   MAX_EDITS : constant := CuBit.UI.Editor.Cursors.MAX_CURSORS;
   subtype Edit_Index is Positive range 1 .. MAX_EDITS;
   subtype Edit_Count is Positive range 1 .. MAX_EDITS;

   type Edit_Range is record
      First : CuBit.UI.Editor.Documents.Document_Position := 1;
      Last  : CuBit.UI.Editor.Documents.Document_Position := 1;
   end record
   with Dynamic_Predicate => Edit_Range.First <= Edit_Range.Last;

   type Edit_Plan is private;

   function Cursors_Within_Document
     (Cursors : CuBit.UI.Editor.Cursors.Cursor_Set;
      Document_Length : CuBit.UI.Editor.Documents.Line_Character_Count)
      return Boolean;

   function Is_Valid_For
     (Plan : Edit_Plan;
      Document_Length : CuBit.UI.Editor.Documents.Line_Character_Count)
      return Boolean;

   procedure Build
     (Cursors : CuBit.UI.Editor.Cursors.Cursor_Set;
      Document_Length : CuBit.UI.Editor.Documents.Line_Character_Count;
      Plan : out Edit_Plan)
   with Pre =>
     Document_Length <= CuBit.UI.Editor.Documents.MAX_DOCUMENT_CAPACITY and then
     Cursors_Within_Document (Cursors, Document_Length),
     Post => Is_Valid_For (Plan, Document_Length);

   function Length (Plan : Edit_Plan) return Edit_Count;
   function Element (Plan : Edit_Plan; Index : Edit_Index) return Edit_Range
   with Pre => Index <= Length (Plan);
   function Removed_Characters (Plan : Edit_Plan) return Natural;

   function Final_Length_Fits
     (Plan : Edit_Plan; Document_Length, Inserted_Length : Natural;
      Capacity : CuBit.UI.Editor.Documents.Document_Capacity) return Boolean
   with Pre =>
     Document_Length <= Capacity and then
     Removed_Characters (Plan) <= Document_Length;

   procedure Replace_All
     (Value : in out CuBit.UI.Editor.Documents.Document;
      Cursors : in out CuBit.UI.Editor.Cursors.Cursor_Set;
      Text : String; Result : out CuBit.UI.Editor.Documents.Edit_Result)
   with Pre =>
     CuBit.UI.Editor.Documents.Length (Value) <=
       CuBit.UI.Editor.Documents.MAX_DOCUMENT_CAPACITY and then
     Cursors_Within_Document
       (Cursors,
        CuBit.UI.Editor.Documents.Line_Character_Count
          (CuBit.UI.Editor.Documents.Length (Value)));

private
   type Edit_Array is array (Edit_Index) of Edit_Range;
   type Edit_Plan is record
      Items : Edit_Array := [others => (First => 1, Last => 1)];
      Last : Edit_Count := 1;
      Removed : Natural := 0;
   end record;
end CuBit.UI.Editor.Transactions;
