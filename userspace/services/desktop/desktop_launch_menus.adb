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
         when DL.Games => "Games", when DL.Media => "Media", when DL.Tools => "Tools");

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

   procedure Press (S : in out Menu_State; M : Desktop_Launch.Menu; Pressed : Key; Result : out Choice;
                    Entry_Index : out Item_Count) is
      --  Keys move among the categories; Power is for the pointer, as
      --  before (it takes no keyboard selection).
      Top : constant Row_Count := Natural'Max (1, Rows (M));
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
            S.Row := (if S.Row <= 1 then Top else S.Row - 1);
            S.Open := 0;
         when Down =>
            S.Row := (if S.Row >= Top then 1 else S.Row + 1);
            S.Open := 0;
         when Right | Enter =>
            if S.Row in 1 .. Rows (M) then
               Open_Row (S, M, S.Row);
               S.In_Submenu := True;
               S.Item := 1;
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
      elsif Row = Power_Row (M) then
         S.Row := Row;
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
      if Row = Power_Row (M) then
         Result := Power;
      elsif Row in 1 .. Rows (M) then
         Open_Row (S, M, Row);
         S.In_Submenu := True;
         S.Item := 1;
      end if;
   end Click_Row;
end Desktop_Launch_Menus;
