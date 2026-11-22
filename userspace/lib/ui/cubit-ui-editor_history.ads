with CuBit.UI.Editor.Cursors;
with CuBit.UI.Editor.Documents;

generic
   Capacity : CuBit.UI.Editor.Documents.Document_Capacity;
   Depth    : Positive;
package CuBit.UI.Editor_History with SPARK_Mode is
   subtype Fixed_Document is
     CuBit.UI.Editor.Documents.Document (Capacity);

   type Operation_Kind is
     (No_Operation, Insert_Characters, Delete_Backward, Delete_Forward,
      Other_Edit);

   type History is private;

   procedure Initialize (Value : in out History);

   procedure Save_Before_Edit
     (Value : in out History;
      Document : Fixed_Document;
      Cursors : CuBit.UI.Editor.Cursors.Cursor_Set;
      Operation : Operation_Kind := Other_Edit);

   procedure Break_Sequence (Value : in out History);

   function Can_Undo (Value : History) return Boolean;
   function Can_Redo (Value : History) return Boolean;

   procedure Undo
     (Value : in out History;
      Document : in out Fixed_Document;
      Cursors : in out CuBit.UI.Editor.Cursors.Cursor_Set);

   procedure Redo
     (Value : in out History;
      Document : in out Fixed_Document;
      Cursors : in out CuBit.UI.Editor.Cursors.Cursor_Set);

private
   subtype Entry_Index is Positive range 1 .. Depth;
   subtype Entry_Count is Natural range 0 .. Depth;

   type Snapshot is record
      Document : Fixed_Document;
      Cursors  : CuBit.UI.Editor.Cursors.Cursor_Set;
   end record;
   type Snapshot_Array is array (Entry_Index) of Snapshot;

   type History is record
      Undo_Entries : Snapshot_Array;
      Redo_Entries : Snapshot_Array;
      Undo_Last : Entry_Count := 0;
      Redo_Last : Entry_Count := 0;
      Last_Operation : Operation_Kind := No_Operation;
      Sequence_Active : Boolean := False;
   end record;
end CuBit.UI.Editor_History;
