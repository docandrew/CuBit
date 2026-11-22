with CCL.Interfaces.Desktop_Launch;

package body Desktop_Launch_Menus is
   package DL renames CCL.Interfaces.Desktop_Launch;
   use type Desktop_Launch.Category;

   function Has (M : Desktop_Launch.Menu; Group : Category) return Boolean is
     ((for some K in 1 .. M.Count => M.Entries (K).Group = Group));

   function Rows (M : Desktop_Launch.Menu) return Row_Count is
      Result : Row_Count := 0;
   begin
      for Group in Category loop
         if Has (M, Group) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Rows;

   function Category_Of (M : Desktop_Launch.Menu; Row : Row_Count) return Category is
      Seen : Row_Count := 0;
   begin
      for Group in Category loop
         if Has (M, Group) then
            Seen := Seen + 1;
            if Seen = Row then
               return Group;
            end if;
         end if;
      end loop;
      return Category'First;
   end Category_Of;

   function Items (M : Desktop_Launch.Menu; Row : Row_Count) return Item_Count is
      Result : Item_Count := 0;
   begin
      if Row not in 1 .. Rows (M) then
         return 0;
      end if;
      for K in 1 .. M.Count loop
         if M.Entries (K).Group = Category_Of (M, Row) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Items;

   function Entry_Of (M : Desktop_Launch.Menu; Row : Row_Count; Index : Positive) return Item_Count is
      Seen : Natural := 0;
   begin
      if Row not in 1 .. Rows (M) then
         return 0;
      end if;
      for K in 1 .. M.Count loop
         if M.Entries (K).Group = Category_Of (M, Row) then
            Seen := Seen + 1;
            if Seen = Index then
               return K;
            end if;
         end if;
      end loop;
      return 0;
   end Entry_Of;

   procedure Locate (M : Desktop_Launch.Menu; Entry_Index : Positive; Row : out Row_Count; Index : out Item_Count) is
   begin
      Row := 0;
      Index := 0;
      for R in 1 .. Rows (M) loop
         for I in 1 .. Items (M, R) loop
            if Entry_Of (M, R, I) = Entry_Index then
               Row := R;
               Index := I;
               return;
            end if;
         end loop;
      end loop;
   end Locate;

   function Category_Name (Group : Category) return String is
     (case Group is
         when DL.System => "System", when DL.Development => "Development", when DL.Web => "Web",
         when DL.Games => "Games", when DL.Media => "Media", when DL.Tools => "Tools",
         when DL.Reference => "Reference");

   procedure Reset (S : out Menu_State) is
   begin
      S := (others => <>);
   end Reset;

   procedure Open_Row (S : in out Menu_State; M : Desktop_Launch.Menu; Row : Row_Count) is
   begin
      if Row in 1 .. Rows (M) then
         S.Open := Row;
         S.Row := Row;
         S.Item := 0;
      end if;
   end Open_Row;

   --  The rows the keyboard selects, in order: the categories, then the
   --  enabled power rows.
   function Selectable (M : Desktop_Launch.Menu; Row : Row_Count) return Boolean is
     (Row in 1 .. Rows (M) or else Row = Shut_Down_Row (M)
      or else (Restart_Enabled and then Row = Restart_Row (M)));

   procedure Press (S : in out Menu_State; M : Desktop_Launch.Menu; Pressed : Key; Result : out Choice;
                    Entry_Index : out Item_Count) is
      Last : constant Row_Count := Shut_Down_Row (M);
   begin
      Result := Nothing;
      Entry_Index := 0;
      if S.In_Submenu and then S.Open in 1 .. Rows (M) then
         declare
            Count : constant Item_Count := Items (M, S.Open);
         begin
            case Pressed is
               when Up => S.Item := (if S.Item <= 1 then Count else S.Item - 1);
               when Down => S.Item := (if S.Item >= Count then 1 else S.Item + 1);
               when Left | Escape =>
                  S.In_Submenu := False;
                  S.Open := 0;
                  S.Item := 0;
               when Right => null;
               when Enter =>
                  if S.Item in 1 .. Count then
                     Result := Launch;
                     Entry_Index := Entry_Of (M, S.Open, S.Item);
                  end if;
            end case;
         end;
         return;
      end if;
      case Pressed is
         when Up =>
            loop
               S.Row := (if S.Row <= 1 then Last else S.Row - 1);
               exit when Selectable (M, S.Row);
            end loop;
            S.Open := 0;
         when Down =>
            loop
               S.Row := (if S.Row >= Last then 1 else S.Row + 1);
               exit when Selectable (M, S.Row);
            end loop;
            S.Open := 0;
         when Right | Enter =>
            if S.Row in 1 .. Rows (M) then
               Open_Row (S, M, S.Row);
               S.In_Submenu := True;
               S.Item := 1;
            elsif Pressed = Enter and then S.Row = Shut_Down_Row (M) then
               Result := Shut_Down;
            elsif Pressed = Enter and then Restart_Enabled and then S.Row = Restart_Row (M) then
               Result := Restart;
            end if;
         when Left => S.Open := 0;
         when Escape => Result := Close;
      end case;
   end Press;

   procedure Hover_Row (S : in out Menu_State; M : Desktop_Launch.Menu; Row : Row_Count) is
   begin
      S.Hover := Row;
      if Row in 1 .. Rows (M) then
         S.Row := Row;
         if S.Open /= Row then
            Open_Row (S, M, Row);
            S.In_Submenu := False;
         end if;
      elsif Row in Restart_Row (M) | Shut_Down_Row (M) then
         if Selectable (M, Row) then
            S.Row := Row;
         end if;
         S.Open := 0;
         S.In_Submenu := False;
      end if;
   end Hover_Row;

   procedure Hover_Item (S : in out Menu_State; Index : Item_Count) is
   begin
      if S.Open > 0 and then Index > 0 then
         S.Item := Index;
         S.In_Submenu := True;
         S.Hover := 0;
      end if;
   end Hover_Item;

   procedure Click_Row (S : in out Menu_State; M : Desktop_Launch.Menu; Row : Row_Count; Result : out Choice) is
   begin
      Result := Nothing;
      if Row = Shut_Down_Row (M) then
         Result := Shut_Down;
      elsif Row = Restart_Row (M) then
         Result := (if Restart_Enabled then Restart else Nothing);
      elsif Row in 1 .. Rows (M) then
         Open_Row (S, M, Row);
         S.In_Submenu := True;
         S.Item := 1;
      end if;
   end Click_Row;
end Desktop_Launch_Menus;
