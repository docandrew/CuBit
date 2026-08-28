with CuBit.UI.Editor.Buffers;

package body CuBit.UI.Editor.Transactions with SPARK_Mode is
   package Cursor_Store renames CuBit.UI.Editor.Cursors;
   package Documents renames CuBit.UI.Editor.Documents;
   package Buffers renames CuBit.UI.Editor.Buffers;
   use type Buffers.Append_Result;
   use type Documents.Edit_Result;
   use type Cursor_Store.Toggle_Result;

   function Cursors_Within_Document
     (Cursors : CuBit.UI.Editor.Cursors.Cursor_Set;
      Document_Length : Documents.Line_Character_Count) return Boolean is
     (for all Index in 1 .. Cursor_Store.Length (Cursors) =>
        Cursor_Store.Element (Cursors, Index).Position <= Document_Length + 1
        and then
        Cursor_Store.Element (Cursors, Index).Anchor <= Document_Length + 1);

   function Before (Left, Right : Edit_Range) return Boolean is
     (Left.First < Right.First or else
      (Left.First = Right.First and then Left.Last < Right.Last));

   function Overlaps_Or_Duplicates (Left, Right : Edit_Range) return Boolean is
     (Right.First < Left.Last or else
      (Left.First = Left.Last and then Right.First = Right.Last and then
       Left.First = Right.First));

   function Is_Valid_For
     (Plan : Edit_Plan; Document_Length : Documents.Line_Character_Count)
      return Boolean is
     (Plan.Removed <= Document_Length and then
      (for all Index in 1 .. Plan.Last =>
         Plan.Items (Index).Last <= Document_Length + 1 and then
         (if Index > 1 then
             Plan.Items (Index - 1).Last <= Plan.Items (Index).First)));

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
         pragma Loop_Invariant
           (for all Checked in 1 .. Index - 1 =>
              Plan.Items (Checked).Last <= Document_Length + 1);
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
         pragma Loop_Invariant
           (for all Checked in 1 .. Plan.Last =>
              Plan.Items (Checked).Last <= Document_Length + 1);
         Candidate := Plan.Items (Index);
         declare
            Position : Edit_Index := Index;
         begin
            while Position > 1 and then
              Before (Candidate, Plan.Items (Position - 1))
            loop
               pragma Loop_Invariant (Position <= Index);
               pragma Loop_Invariant
                 (Candidate.Last <= Document_Length + 1);
               pragma Loop_Invariant
                 (for all Checked in 1 .. Plan.Last =>
                    Plan.Items (Checked).Last <= Document_Length + 1);
               Plan.Items (Position) := Plan.Items (Position - 1);
               Position := Position - 1;
            end loop;
            Plan.Items (Position) := Candidate;
         end;
      end loop;

      for Index in 2 .. Plan.Last loop
         pragma Loop_Invariant (Output_Last < Index);
         pragma Loop_Invariant
           (for all Checked in 1 .. Output_Last =>
              Plan.Items (Checked).Last <= Document_Length + 1);
         pragma Loop_Invariant
           (for all Checked in Index .. Plan.Last =>
              Plan.Items (Checked).Last <= Document_Length + 1);
         pragma Loop_Invariant
           (for all Prior_Index in 2 .. Output_Last =>
              Plan.Items (Prior_Index - 1).Last <=
                Plan.Items (Prior_Index).First);
         Candidate := Plan.Items (Index);
         Prior := Plan.Items (Output_Last);
         pragma Assert (Candidate.Last <= Document_Length + 1);
         pragma Assert (Prior.Last <= Document_Length + 1);
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
         pragma Loop_Invariant (Plan.Removed <= Document_Length);
         Plan.Removed := Plan.Removed +
           (Plan.Items (Index).Last - Plan.Items (Index).First);
         pragma Assert (Plan.Removed <= Plan.Items (Index).Last - 1);
         pragma Assert
           (Plan.Items (Index).Last <= Document_Length + 1);
         pragma Assert (Plan.Removed <= Document_Length);
      end loop;
      pragma Assert (Plan.Removed <= Document_Length);
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

   procedure Replace_All
     (Value : in out Documents.Document;
      Cursors : in out Cursor_Store.Cursor_Set;
      Text : String; Result : out Documents.Edit_Result)
   is
      Document_Length : constant Documents.Line_Character_Count :=
        Documents.Line_Character_Count (Documents.Length (Value));
      Capacity : constant Documents.Document_Capacity :=
        Documents.Capacity_Of (Value);
      Source : constant String := Documents.Content (Value);
      Plan : Edit_Plan;
   begin
      Build (Cursors, Document_Length, Plan);
      if not Final_Length_Fits
        (Plan, Document_Length, Text'Length, Capacity)
      then
         Result := Documents.Capacity_Exceeded;
         return;
      end if;

      declare
         Buffer : Buffers.Candidate_Buffer (Capacity);
         Candidate : Documents.Document (Capacity);
         Append_Status : Buffers.Append_Result;
         Candidate_Status : Documents.Edit_Result;
         Input_Position : Documents.Document_Position := 1;
         Positions : array (Edit_Index) of Documents.Document_Position :=
           [others => 1];
         New_Cursors : Cursor_Store.Cursor_Set;
         Toggle_Status : Cursor_Store.Toggle_Result;
         Previous_Position : Documents.Document_Position;
      begin
         Buffers.Initialize (Buffer);
         for Edit in 1 .. Plan.Last loop
            pragma Loop_Invariant (Input_Position <= Document_Length + 1);
            if Input_Position < Plan.Items (Edit).First then
               Buffers.Append
                 (Buffer,
                  Source (Input_Position .. Plan.Items (Edit).First - 1),
                  Append_Status);
               if Append_Status /= Buffers.Appended then
                  Result := Documents.Capacity_Exceeded;
                  return;
               end if;
            end if;
            Buffers.Append (Buffer, Text, Append_Status);
            if Append_Status /= Buffers.Appended then
               Result := Documents.Capacity_Exceeded;
               return;
            end if;
            Positions (Edit) := Buffers.Length (Buffer) + 1;
            Input_Position := Plan.Items (Edit).Last;
         end loop;
         pragma Assert (Input_Position <= Document_Length + 1);
         if Input_Position <= Document_Length then
            Buffers.Append
              (Buffer, Source (Input_Position .. Document_Length),
               Append_Status);
            if Append_Status /= Buffers.Appended then
               Result := Documents.Capacity_Exceeded;
               return;
            end if;
         end if;

         Documents.Initialize
           (Candidate, Buffers.Content (Buffer), Candidate_Status);
         if Candidate_Status /= Documents.Applied then
            Result := Documents.Capacity_Exceeded;
            return;
         end if;

         Cursor_Store.Initialize (New_Cursors, Positions (1));
         Previous_Position := Positions (1);
         for Edit in 2 .. Plan.Last loop
            if Positions (Edit) /= Previous_Position then
               Cursor_Store.Toggle_At
                 (New_Cursors, Positions (Edit), Toggle_Status);
               if Toggle_Status = Cursor_Store.Cursor_Limit_Reached then
                  Result := Documents.Capacity_Exceeded;
                  return;
               end if;
               Previous_Position := Positions (Edit);
            end if;
         end loop;

         Value := Candidate;
         Cursors := New_Cursors;
         Result := Documents.Applied;
      end;
   end Replace_All;

end CuBit.UI.Editor.Transactions;
