package body CuBit.UI.Editor.Transactions with SPARK_Mode is
   package Cursor_Store renames CuBit.UI.Editor.Cursors;
   package Documents renames CuBit.UI.Editor.Documents;

   function Cursors_Within_Document
     (Cursors : CuBit.UI.Editor.Cursors.Cursor_Set;
      Document_Length : Documents.Line_Character_Count) return Boolean
   is
      Valid : Boolean := True;
   begin
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Cursors) loop
         pragma Loop_Invariant (Valid =
           (for all Checked in 1 .. Index - 1 =>
              CuBit.UI.Editor.Cursors.Element (Cursors, Checked).Position <=
                Document_Length + 1 and then
              CuBit.UI.Editor.Cursors.Element (Cursors, Checked).Anchor <=
                Document_Length + 1));
         if CuBit.UI.Editor.Cursors.Element (Cursors, Index).Position >
              Document_Length + 1 or else
           CuBit.UI.Editor.Cursors.Element (Cursors, Index).Anchor >
              Document_Length + 1
         then
            Valid := False;
         end if;
      end loop;
      return Valid;
   end Cursors_Within_Document;

   function Before (Left, Right : Edit_Range) return Boolean is
     (Left.First < Right.First or else
      (Left.First = Right.First and then Left.Last < Right.Last));

   function Overlaps_Or_Duplicates (Left, Right : Edit_Range) return Boolean is
     (Right.First < Left.Last or else
      (Left.First = Left.Last and then Right.First = Right.Last and then
       Left.First = Right.First));

   procedure Build
     (Cursors : CuBit.UI.Editor.Cursors.Cursor_Set;
      Document_Length : Documents.Line_Character_Count;
      Plan : out Edit_Plan)
   is
      Candidate : Edit_Range;
      Prior : Edit_Range;
      Count : constant Cursor_Store.Cursor_Count := Cursor_Store.Length (Cursors);
      Output_Last : Edit_Count := 1;
   begin
      Plan := (others => <>);
      for Index in 1 .. Count loop
         declare
            Cursor : constant Cursor_Store.Cursor_State :=
              Cursor_Store.Element (Cursors, Index);
         begin
            Plan.Items (Index) :=
              (First => Documents.Document_Position'Min
                 (Cursor.Position, Cursor.Anchor),
               Last => Documents.Document_Position'Max
                 (Cursor.Position, Cursor.Anchor));
         end;
      end loop;
      Plan.Last := Count;

      for Index in 2 .. Plan.Last loop
         Candidate := Plan.Items (Index);
         declare
            Position : Edit_Index := Index;
         begin
            while Position > 1 and then
              Before (Candidate, Plan.Items (Position - 1))
            loop
               pragma Loop_Invariant (Position <= Index);
               Plan.Items (Position) := Plan.Items (Position - 1);
               Position := Position - 1;
            end loop;
            Plan.Items (Position) := Candidate;
         end;
      end loop;

      for Index in 2 .. Plan.Last loop
         pragma Loop_Invariant (Output_Last < Index);
         pragma Loop_Invariant
           (for all Prior_Index in 2 .. Output_Last =>
              Plan.Items (Prior_Index - 1).Last <=
                Plan.Items (Prior_Index).First);
         Candidate := Plan.Items (Index);
         Prior := Plan.Items (Output_Last);
         if Overlaps_Or_Duplicates (Prior, Candidate) then
            Plan.Items (Output_Last).Last :=
              Documents.Document_Position'Max (Prior.Last, Candidate.Last);
         else
            Output_Last := Output_Last + 1;
            Plan.Items (Output_Last) := Candidate;
         end if;
      end loop;
      Plan.Last := Output_Last;
      Plan.Removed := 0;
      for Index in 1 .. Plan.Last loop
         pragma Loop_Invariant
           (Plan.Removed <= Plan.Items (Index).First - 1);
         Plan.Removed := Plan.Removed +
           (Plan.Items (Index).Last - Plan.Items (Index).First);
      end loop;
   end Build;

   function Length (Plan : Edit_Plan) return Edit_Count is (Plan.Last);
   function Element (Plan : Edit_Plan; Index : Edit_Index) return Edit_Range is
     (Plan.Items (Index));
   function Removed_Characters (Plan : Edit_Plan) return Natural is
     (Plan.Removed);

   function Final_Length_Fits
     (Plan : Edit_Plan; Document_Length, Inserted_Length : Natural;
      Capacity : Documents.Document_Capacity) return Boolean
   is
      Base_Length : constant Natural :=
        Document_Length - Plan.Removed;
   begin
      return Inserted_Length <= Capacity and then
        Inserted_Length * Plan.Last <= Capacity - Base_Length;
   end Final_Length_Fits;

end CuBit.UI.Editor.Transactions;
