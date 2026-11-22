with Text_Limits; use Text_Limits;

--  Selections (docs/text-editor.md, "Selections"): any number up to the
--  set's capacity, each an anchor and a head (byte offsets; equal for a
--  plain cursor), one of them primary. After Normalize the set is sorted
--  by start and disjoint: selections that overlap, and cursors that meet,
--  merge. Every editing command acts on all of them.
--
--  Proved: no runtime errors, the primary index stays in range and every
--  offset stays within the document. Tested (tests/text-editor) against a
--  reference: that Normalize sorts and merges.
package Text_Cursors with SPARK_Mode is
   MAXIMUM_SELECTIONS : constant := 2 ** 20;
   subtype Selection_Count is Natural range 0 .. MAXIMUM_SELECTIONS;
   subtype Selection_Index is Selection_Count range 1 .. MAXIMUM_SELECTIONS;

   type Selection is record
      Anchor, Head : Byte_Offset := 0;
      --  The column vertical moves aim for; kept across short lines.
      Goal : Column_Count := 0;
      Has_Goal : Boolean := False;
   end record;

   function First_Of (S : Selection) return Byte_Offset is (Byte_Offset'Min (S.Anchor, S.Head));
   function Last_Of (S : Selection) return Byte_Offset is (Byte_Offset'Max (S.Anchor, S.Head));
   function Is_Empty (S : Selection) return Boolean is (S.Anchor = S.Head);
   function Caret (At_Offset : Byte_Offset) return Selection is
     (Anchor => At_Offset, Head => At_Offset, Goal => 0, Has_Goal => False);

   type Set (Capacity : Selection_Index) is private;

   function Count (S : Set) return Selection_Count;
   function Primary (S : Set) return Selection_Index
     with Post => Primary'Result <= Count (S) or else Count (S) = 0;
   function Element (S : Set; Index : Selection_Index) return Selection
     with Pre => Index <= Count (S);
   --  Every offset is at most Limit.
   function Within (S : Set; Limit : Byte_Offset) return Boolean;

   --  One cursor at At_Offset.
   procedure Initialize (S : in out Set; At_Offset : Byte_Offset := 0)
     with Post => Count (S) = 1 and then Primary (S) = 1;
   procedure Replace (S : in out Set; Index : Selection_Index; Value : Selection)
     with Pre => Index <= Count (S),
          Post => Count (S) = Count (S'Old) and then Primary (S) = Primary (S'Old);
   --  Add a selection (made primary when Make_Primary); False when the set
   --  is full. Call Normalize after adding.
   procedure Add (S : in out Set; Value : Selection; Make_Primary : Boolean; Added : out Boolean)
     with Pre => Count (S) >= 1,
          Post => Count (S) >= 1 and then (if Added then Count (S) = Count (S'Old) + 1 else Count (S) = Count (S'Old));
   procedure Set_Primary (S : in out Set; Index : Selection_Index)
     with Pre => Index <= Count (S);
   --  Only the primary selection remains.
   procedure Keep_Primary (S : in out Set)
     with Pre => Count (S) >= 1, Post => Count (S) = 1 and then Primary (S) = 1;
   --  Remove the primary selection (when there are others); the one before
   --  it becomes primary.
   procedure Remove_Primary (S : in out Set)
     with Pre => Count (S) >= 1, Post => Count (S) >= 1;

   --  Sort by start and merge overlapping selections and meeting cursors.
   --  The primary follows its selection.
   procedure Normalize (S : in out Set)
     with Pre => Count (S) >= 1, Post => Count (S) >= 1 and then Count (S) <= Count (S'Old);
   function Is_Normal (S : Set) return Boolean;

   --  Move offsets after an edit elsewhere: Length bytes inserted at
   --  Offset (offsets after it move; at it, they stay), or removed from it.
   procedure Adjust_Insert (S : in out Set; Offset : Byte_Offset; Length : Byte_Count)
     with Pre => Within (S, MAXIMUM_BYTES - Length);
   procedure Adjust_Delete (S : in out Set; Offset : Byte_Offset; Length : Byte_Count)
     with Pre => Offset <= MAXIMUM_BYTES - Length;
   --  Clamp every offset to Limit.
   procedure Clamp (S : in out Set; Limit : Byte_Offset)
     with Post => Within (S, Limit) and then Count (S) = Count (S'Old);
private
   type Selection_Array is array (Selection_Index range <>) of Selection;
   type Set (Capacity : Selection_Index) is record
      Items : Selection_Array (1 .. Capacity) := [others => (others => <>)];
      Used : Selection_Count := 1;
      Main : Selection_Index := 1;
   end record
     with Type_Invariant => Set.Used <= Set.Capacity and then Set.Main <= Natural'Max (Set.Used, 1);

   function Count (S : Set) return Selection_Count is (S.Used);
   function Primary (S : Set) return Selection_Index is (S.Main);
   function Element (S : Set; Index : Selection_Index) return Selection is (S.Items (Index));
   function Within (S : Set; Limit : Byte_Offset) return Boolean is
     (for all K in 1 .. S.Used => S.Items (K).Anchor <= Limit and then S.Items (K).Head <= Limit);
end Text_Cursors;
