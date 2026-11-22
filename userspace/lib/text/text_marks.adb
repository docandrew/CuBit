package body Text_Marks with SPARK_Mode is
   function Element (S : Set; Index : Mark_Index) return Mark is (S.Items (Index));

   procedure Initialize (S : in out Set) is
   begin
      S.Used := 0;
   end Initialize;

   procedure Clear (S : in out Set) is
   begin
      S.Used := 0;
   end Clear;

   procedure Add (S : in out Set; Item : Mark; Added : out Boolean) is
      Place : Mark_Index := 1;
   begin
      Added := S.Used < S.Capacity;
      if not Added then
         return;
      end if;
      while Place <= S.Used and then S.Items (Place).First <= Item.First loop
         pragma Loop_Invariant (Place <= S.Used);
         pragma Loop_Variant (Increases => Place);
         Place := Place + 1;
      end loop;
      if Place <= S.Used then
         S.Items (Place + 1 .. S.Used + 1) := S.Items (Place .. S.Used);
      end if;
      S.Items (Place) := Item;
      S.Used := S.Used + 1;
   end Add;

   procedure Remove (S : in out Set; Id : Mark_Id) is
      Kept : Mark_Count := 0;
   begin
      for K in 1 .. S.Used loop
         pragma Loop_Invariant (Kept < K and then (for all J in 1 .. Kept => S.Items (J).First <= S.Items (J).Last)
                                and then (for all J in K .. S.Used => S.Items (J).First <= S.Items (J).Last));
         if S.Items (K).Id /= Id then
            Kept := Kept + 1;
            S.Items (Kept) := S.Items (K);
         end if;
      end loop;
      S.Used := Kept;
   end Remove;

   function Find (S : Set; Id : Mark_Id) return Mark_Count is
   begin
      for K in 1 .. S.Used loop
         if S.Items (K).Id = Id then
            return K;
         end if;
      end loop;
      return 0;
   end Find;

   function Blocks_Edit (S : Set; First, Last : Byte_Offset) return Boolean is
   begin
      for K in 1 .. S.Used loop
         declare
            M : Mark renames S.Items (K);
         begin
            exit when M.First > Last;
            if Protects (M) then
               if First = Last then
                  if M.First < First and then First < M.Last then
                     return True;
                  end if;
               elsif First < M.Last and then M.First < Last then
                  return True;
               end if;
            end if;
         end;
      end loop;
      return False;
   end Blocks_Edit;

   procedure Adjust_Insert (S : in out Set; Offset : Byte_Offset; Length : Byte_Count) is
   begin
      for K in 1 .. S.Used loop
         pragma Loop_Invariant (for all J in 1 .. S.Used => S.Items (J).First <= S.Items (J).Last);
         declare
            M : Mark renames S.Items (K);
         begin
            if M.Last <= MAXIMUM_BYTES - Length then
               if Offset <= M.First and then M.First < M.Last then
                  --  Before the region (or at its start): it moves.
                  M.First := M.First + Length;
                  M.Last := M.Last + Length;
               elsif Offset < M.First then
                  M.First := M.First + Length;
                  M.Last := M.Last + Length;
               elsif Offset < M.Last then
                  M.Last := M.Last + Length;
               end if;
            end if;
         end;
      end loop;
   end Adjust_Insert;

   procedure Adjust_Delete (S : in out Set; Offset : Byte_Offset; Length : Byte_Count) is
      function Moved (P : Byte_Offset) return Byte_Offset is
        (if P <= Offset then P elsif P <= Offset + Length then Offset else P - Length);
   begin
      for K in 1 .. S.Used loop
         pragma Loop_Invariant (for all J in 1 .. S.Used => S.Items (J).First <= S.Items (J).Last);
         S.Items (K) := (S.Items (K) with delta First => Moved (S.Items (K).First), Last => Moved (S.Items (K).Last));
      end loop;
   end Adjust_Delete;

   procedure Set_Extent (S : in out Set; Index : Mark_Index; First, Last : Byte_Offset) is
   begin
      S.Items (Index).First := First;
      S.Items (Index).Last := Last;
   end Set_Extent;
end Text_Marks;
