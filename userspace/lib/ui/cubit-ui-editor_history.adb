package body CuBit.UI.Editor_History with SPARK_Mode is

   procedure Initialize (Value : in out History) is
   begin
      Value.Undo_Last := 0;
      Value.Redo_Last := 0;
      Value.Last_Operation := No_Operation;
      Value.Sequence_Active := False;
   end Initialize;

   procedure Push
     (Entries : in out Snapshot_Array;
      Last : in out Entry_Count;
      Item : Snapshot)
   is
   begin
      if Last = Depth then
         for Index in 1 .. Depth - 1 loop
            Entries (Index) := Entries (Index + 1);
         end loop;
      else
         Last := Last + 1;
      end if;
      Entries (Last) := Item;
   end Push;

   procedure Save_Before_Edit
     (Value : in out History;
      Document : Fixed_Document;
      Cursors : CuBit.UI.Editor.Cursors.Cursor_Set;
      Operation : Operation_Kind := Other_Edit)
   is
      Coalescing : constant Boolean :=
        Value.Sequence_Active and then
        Operation /= No_Operation and then
        Operation /= Other_Edit and then
        Operation = Value.Last_Operation;
   begin
      if not Coalescing then
         Push
           (Value.Undo_Entries, Value.Undo_Last,
            (Document => Document, Cursors => Cursors));
      end if;
      Value.Redo_Last := 0;
      Value.Last_Operation := Operation;
      Value.Sequence_Active := Operation /= No_Operation and then
        Operation /= Other_Edit;
   end Save_Before_Edit;

   procedure Break_Sequence (Value : in out History) is
   begin
      Value.Last_Operation := No_Operation;
      Value.Sequence_Active := False;
   end Break_Sequence;

   function Can_Undo (Value : History) return Boolean is
     (Value.Undo_Last > 0);

   function Can_Redo (Value : History) return Boolean is
     (Value.Redo_Last > 0);

   procedure Undo
     (Value : in out History;
      Document : in out Fixed_Document;
      Cursors : in out CuBit.UI.Editor.Cursors.Cursor_Set)
   is
   begin
      if Value.Undo_Last = 0 then
         return;
      end if;
      Push
        (Value.Redo_Entries, Value.Redo_Last,
         (Document => Document, Cursors => Cursors));
      Document := Value.Undo_Entries (Value.Undo_Last).Document;
      Cursors := Value.Undo_Entries (Value.Undo_Last).Cursors;
      Value.Undo_Last := Value.Undo_Last - 1;
      Break_Sequence (Value);
   end Undo;

   procedure Redo
     (Value : in out History;
      Document : in out Fixed_Document;
      Cursors : in out CuBit.UI.Editor.Cursors.Cursor_Set)
   is
   begin
      if Value.Redo_Last = 0 then
         return;
      end if;
      Push
        (Value.Undo_Entries, Value.Undo_Last,
         (Document => Document, Cursors => Cursors));
      Document := Value.Redo_Entries (Value.Redo_Last).Document;
      Cursors := Value.Redo_Entries (Value.Redo_Last).Cursors;
      Value.Redo_Last := Value.Redo_Last - 1;
      Break_Sequence (Value);
   end Redo;

end CuBit.UI.Editor_History;
