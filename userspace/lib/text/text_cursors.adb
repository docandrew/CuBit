package body Text_Cursors with SPARK_Mode is
   procedure Initialize (S : in out Set; At_Offset : Byte_Offset := 0) is
   begin
      --  Only items 1 .. Used are ever read.
      S.Items (1) := Caret (At_Offset);
      S.Used := 1;
      S.Main := 1;
   end Initialize;

   procedure Replace (S : in out Set; Index : Selection_Index; Value : Selection) is
   begin
      S.Items (Index) := Value;
   end Replace;

   procedure Add (S : in out Set; Value : Selection; Make_Primary : Boolean; Added : out Boolean) is
   begin
      Added := S.Used < S.Capacity;
      if Added then
         S.Used := S.Used + 1;
         S.Items (S.Used) := Value;
         if Make_Primary then
            S.Main := S.Used;
         end if;
      end if;
   end Add;

   procedure Set_Primary (S : in out Set; Index : Selection_Index) is
   begin
      S.Main := Index;
   end Set_Primary;

   procedure Keep_Primary (S : in out Set) is
   begin
      S.Items (1) := S.Items (S.Main);
      S.Used := 1;
      S.Main := 1;
   end Keep_Primary;

   procedure Remove_Primary (S : in out Set) is
   begin
      if S.Used <= 1 then
         return;
      end if;
      if S.Main < S.Used then
         S.Items (S.Main .. S.Used - 1) := S.Items (S.Main + 1 .. S.Used);
      end if;
      S.Used := S.Used - 1;
      S.Main := (if S.Main > 1 then S.Main - 1 else S.Used);
   end Remove_Primary;

   function Before (A, B : Selection) return Boolean is
     (First_Of (A) < First_Of (B) or else (First_Of (A) = First_Of (B) and then Last_Of (A) < Last_Of (B)));

   function Is_Normal (S : Set) return Boolean is
     (for all K in 2 .. S.Used =>
        First_Of (S.Items (K - 1)) <= First_Of (S.Items (K))
        and then Last_Of (S.Items (K - 1)) <= First_Of (S.Items (K))
        and then not (Last_Of (S.Items (K - 1)) = First_Of (S.Items (K))
                      and then (Is_Empty (S.Items (K - 1)) or else Is_Empty (S.Items (K)))));

   --  Heap sort of Items (1 .. Used) by Before; Main follows its element.
   procedure Sort (S : in out Set)
     with Pre => S.Used >= 1 and then S.Used <= S.Capacity and then S.Main <= S.Used,
          Post => S.Used = S.Used'Old and then S.Main <= S.Used
   is
      Count : constant Selection_Index := S.Used;

      procedure Swap (A, B : Selection_Index)
        with Pre => A <= Count and then B <= Count and then S.Used = Count and then S.Main <= Count,
             Post => S.Used = Count and then S.Main <= Count
      is
         Held : constant Selection := S.Items (A);
      begin
         S.Items (A) := S.Items (B);
         S.Items (B) := Held;
         if S.Main = A then
            S.Main := B;
         elsif S.Main = B then
            S.Main := A;
         end if;
      end Swap;

      procedure Sift (Root_At : Selection_Index; Last : Selection_Index)
        with Pre => Last <= Count and then Root_At <= Last and then S.Used = Count and then S.Main <= Count,
             Post => S.Used = Count and then S.Main <= Count
      is
         Root : Selection_Index := Root_At;
      begin
         while Root <= Last / 2 loop
            pragma Loop_Invariant (Root <= Last and then S.Used = Count and then S.Main <= Count);
            pragma Loop_Variant (Increases => Root);
            declare
               Child : Selection_Index := 2 * Root;
            begin
               if Child < Last and then Before (S.Items (Child), S.Items (Child + 1)) then
                  Child := Child + 1;
               end if;
               exit when not Before (S.Items (Root), S.Items (Child));
               Swap (Root, Child);
               Root := Child;
            end;
         end loop;
      end Sift;
   begin
      if Count = 1 or else (for all K in 2 .. Count => not Before (S.Items (K), S.Items (K - 1))) then
         return;
      end if;
      for Root in reverse 1 .. Count / 2 loop
         pragma Loop_Invariant (S.Used = Count and then S.Main <= Count);
         Sift (Root, Count);
      end loop;
      for Last in reverse 2 .. Count loop
         pragma Loop_Invariant (S.Used = Count and then S.Main <= Count);
         Swap (1, Last);
         Sift (1, Last - 1);
      end loop;
   end Sort;

   procedure Normalize (S : in out Set) is
      Kept : Selection_Index := 1;
   begin
      Sort (S);
      for K in 2 .. S.Used loop
         pragma Loop_Invariant (Kept < K and then S.Main <= S.Used and then S.Used = S.Used'Loop_Entry);
         declare
            Prior : constant Selection := S.Items (Kept);
            Item : constant Selection := S.Items (K);
            Overlap : constant Boolean :=
              Last_Of (Prior) > First_Of (Item)
              or else (Last_Of (Prior) = First_Of (Item) and then (Is_Empty (Prior) or else Is_Empty (Item)));
         begin
            if Overlap then
               --  Merge into Kept, keeping the direction of the later one.
               declare
                  Low : constant Byte_Offset := First_Of (Prior);
                  High : constant Byte_Offset := Byte_Offset'Max (Last_Of (Prior), Last_Of (Item));
               begin
                  S.Items (Kept) :=
                    (if Item.Head >= Item.Anchor then (Anchor => Low, Head => High, Goal => Item.Goal,
                                                       Has_Goal => Item.Has_Goal)
                     else (Anchor => High, Head => Low, Goal => Item.Goal, Has_Goal => Item.Has_Goal));
               end;
               if S.Main = K then
                  S.Main := Kept;
               end if;
            else
               Kept := Kept + 1;
               S.Items (Kept) := Item;
               if S.Main = K then
                  S.Main := Kept;
               end if;
            end if;
            if S.Main > K then
               --  Not yet visited: it moves down with the others later.
               null;
            end if;
         end;
      end loop;
      --  The primary was at an index after Kept only if it was merged
      --  away; then the last kept one is primary.
      if S.Main > Kept then
         S.Main := Kept;
      end if;
      S.Used := Kept;
   end Normalize;

   procedure Adjust_Insert (S : in out Set; Offset : Byte_Offset; Length : Byte_Count) is
   begin
      for K in 1 .. S.Used loop
         if S.Items (K).Anchor > Offset and then S.Items (K).Anchor <= MAXIMUM_BYTES - Length then
            S.Items (K).Anchor := S.Items (K).Anchor + Length;
         end if;
         if S.Items (K).Head > Offset and then S.Items (K).Head <= MAXIMUM_BYTES - Length then
            S.Items (K).Head := S.Items (K).Head + Length;
         end if;
      end loop;
   end Adjust_Insert;

   procedure Adjust_Delete (S : in out Set; Offset : Byte_Offset; Length : Byte_Count) is
      function Moved (P : Byte_Offset) return Byte_Offset is
        (if P <= Offset then P elsif P <= Offset + Length then Offset else P - Length);
   begin
      for K in 1 .. S.Used loop
         S.Items (K).Anchor := Moved (S.Items (K).Anchor);
         S.Items (K).Head := Moved (S.Items (K).Head);
      end loop;
   end Adjust_Delete;

   procedure Clamp (S : in out Set; Limit : Byte_Offset) is
   begin
      for K in 1 .. S.Used loop
         S.Items (K).Anchor := Byte_Offset'Min (S.Items (K).Anchor, Limit);
         S.Items (K).Head := Byte_Offset'Min (S.Items (K).Head, Limit);
         pragma Loop_Invariant
           (for all J in 1 .. K => S.Items (J).Anchor <= Limit and then S.Items (J).Head <= Limit);
      end loop;
   end Clamp;
end Text_Cursors;
