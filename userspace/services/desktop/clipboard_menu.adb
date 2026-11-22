package body Clipboard_Menu with SPARK_Mode is

   --  Scroll so Selected is visible.
   procedure Follow (M : in out Menu_State)
     with Pre => (if M.Count = 0 then M.Selected = 0
                  else M.Selected in 1 .. M.Count) and then
                 (for all R in Row_Index =>
                    (if R <= M.Count then M.Slots (R) /= 0)),
          Post => Valid (M) and then M.Count = M.Count'Old and then
                  M.Selected = M.Selected'Old and then M.Slots = M.Slots'Old
   is
   begin
      if M.Selected = 0 then
         M.First := 1;
      elsif M.Selected < M.First then
         M.First := M.Selected;
      elsif M.Selected >= M.First + Visible_Rows then
         M.First := M.Selected - Visible_Rows + 1;
      end if;
      if M.First > Row_Index'Max (1, M.Count) then
         M.First := Row_Index'Max (1, M.Count);
      end if;
      if M.Selected /= 0 and then M.Selected >= M.First + Visible_Rows then
         M.First := M.Selected - Visible_Rows + 1;
      end if;
   end Follow;

   procedure Reload (M : in out Menu_State; Slots : Row_Slots; Count : Row_Count)
   is
      Keep : constant Clipboard_Store.Slot_Count :=
        (if M.Selected in 1 .. M.Count then M.Slots (M.Selected) else 0);
      Previous : constant Row_Count := M.Selected;
   begin
      M.Slots := Slots;
      M.Count := Count;
      M.Selected := (if Count = 0 then 0
                     elsif Previous = 0 then 1
                     else Row_Count'Min (Previous, Count));
      if Keep /= 0 then
         for R in Row_Index loop
            pragma Loop_Invariant
              (if M.Count = 0 then M.Selected = 0
               else M.Selected in 1 .. M.Count);
            if R <= Count and then Slots (R) = Keep then
               M.Selected := R;
            end if;
         end loop;
      end if;
      Follow (M);
   end Reload;

   procedure Press
     (M : in out Menu_State; K : Key; Digit : Digit_Value;
      Result : out Action; Slot : out Clipboard_Store.Slot_Count) is
   begin
      Result := Nothing;
      Slot := 0;
      case K is
         when Escape =>
            Result := Close;
         when Up =>
            if M.Selected > 1 then
               M.Selected := M.Selected - 1;
            end if;
         when Down =>
            if M.Selected /= 0 and then M.Selected < M.Count then
               M.Selected := M.Selected + 1;
            end if;
         when Home =>
            if M.Count > 0 then
               M.Selected := 1;
            end if;
         when Last =>
            M.Selected := M.Count;
         when Number =>
            if Digit <= M.Count then
               M.Selected := Digit;
               Result := Paste;
            end if;
         when Enter =>
            if M.Selected /= 0 then
               Result := Paste;
            end if;
         when Pin =>
            if M.Selected /= 0 then
               Result := Toggle_Pin;
            end if;
         when Delete =>
            if M.Selected /= 0 then
               Result := Remove;
            end if;
         when Clear =>
            Result := Remove_All;
      end case;
      Follow (M);
      if Result in Paste | Toggle_Pin | Remove then
         Slot := M.Slots (M.Selected);
      end if;
   end Press;

   procedure Select_Visible
     (M : in out Menu_State; Offset : Natural; Hit : out Boolean) is
   begin
      Hit := False;
      if Offset < Visible_Rows and then M.Count > 0 and then
        Natural (M.First) + Offset <= Natural (M.Count)
      then
         M.Selected := M.First + Row_Count (Offset);
         Hit := True;
      end if;
      Follow (M);
   end Select_Visible;
end Clipboard_Menu;
