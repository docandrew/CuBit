package body CuBit.UI.Popup_Menus is
   package L renames Client_Popup_Layout;
   use type Controls.Pointer_Action;

   ICON_COLUMN : constant := 26;
   ARROW_COLUMN : constant := 18;
   ACCELERATOR_GAP : constant := 24;
   FRAME : constant := 3;
   TEXT_INSET : constant := 6;
   MINIMUM_WIDTH : constant := 140;

   procedure Clear (M : out Model) is
   begin
      M := (others => <>);
   end Clear;

   procedure Append (M : in out Model; Menu : Menu_Index; Row : Item) is
   begin
      if Menu <= M.Count and then M.Menus (Menu).Count < MAXIMUM_ITEMS then
         M.Menus (Menu).Count := M.Menus (Menu).Count + 1;
         M.Menus (Menu).Rows (M.Menus (Menu).Count) := Row;
      end if;
   end Append;

   procedure Add
     (M : in out Model; Menu : Menu_Index; Caption : String; Choice : Command;
      Accelerator : String := ""; Enabled : Boolean := True; Checked : Boolean := False;
      Has_Icon : Boolean := False; Picture : CuBit.UI.Icons.Icon := CuBit.UI.Icons.File)
   is
      Row : Item;
   begin
      Row.Caption_Length := Natural'Min (Caption'Length, MAXIMUM_CAPTION);
      Row.Caption (1 .. Row.Caption_Length) := Caption (Caption'First .. Caption'First + Row.Caption_Length - 1);
      Row.Accelerator_Length := Natural'Min (Accelerator'Length, MAXIMUM_ACCELERATOR);
      Row.Accelerator (1 .. Row.Accelerator_Length) :=
        Accelerator (Accelerator'First .. Accelerator'First + Row.Accelerator_Length - 1);
      Row.Choice := Choice;
      Row.Enabled := Enabled and then Choice /= NO_COMMAND;
      Row.Checked := Checked;
      Row.Has_Icon := Has_Icon;
      Row.Picture := Picture;
      Append (M, Menu, Row);
   end Add;

   procedure Add_Separator (M : in out Model; Menu : Menu_Index) is
   begin
      Append (M, Menu, (Separator => True, Enabled => False, others => <>));
   end Add_Separator;

   procedure Add_Submenu (M : in out Model; Menu : Menu_Index; Caption : String; Child : out Menu_Count) is
      Row : Item;
   begin
      Child := 0;
      if M.Count = MAXIMUM_MENUS or else Menu > M.Count then
         return;
      end if;
      M.Count := M.Count + 1;
      M.Menus (M.Count) := (others => <>);
      Child := M.Count;
      Row.Caption_Length := Natural'Min (Caption'Length, MAXIMUM_CAPTION);
      Row.Caption (1 .. Row.Caption_Length) := Caption (Caption'First .. Caption'First + Row.Caption_Length - 1);
      Row.Submenu := Child;
      Append (M, Menu, Row);
   end Add_Submenu;

   function Items (M : Model; Menu : Menu_Index) return Item_Count is
     (if Menu <= M.Count then M.Menus (Menu).Count else 0);

   function Height_Of (Row : Item) return Natural is (if Row.Separator then SEPARATOR_HEIGHT else ROW_HEIGHT);

   function Size (M : Model; Menu : Menu_Index) return L.Box is
      Width : Natural := MINIMUM_WIDTH;
      Height : Natural := 2 * FRAME;
   begin
      for K in 1 .. Items (M, Menu) loop
         declare
            Row : Item renames M.Menus (Menu).Rows (K);
         begin
            Height := Height + Height_Of (Row);
            Width := Natural'Max
              (Width, ICON_COLUMN + UI_Text_Width (Row.Caption (1 .. Row.Caption_Length))
                 + (if Row.Accelerator_Length > 0
                    then ACCELERATOR_GAP + UI_Text_Width (Row.Accelerator (1 .. Row.Accelerator_Length)) else 0)
                 + ARROW_COLUMN + 2 * FRAME);
         end;
      end loop;
      return (0, 0, Natural'Min (Width, L.MAXIMUM_COORDINATE), Natural'Min (Height, L.MAXIMUM_COORDINATE));
   end Size;

   function To_Box (R : Rect) return L.Box is
     (Natural'Min (R.x, L.MAXIMUM_COORDINATE), Natural'Min (R.y, L.MAXIMUM_COORDINATE),
      Natural'Min (R.w, L.MAXIMUM_COORDINATE), Natural'Min (R.h, L.MAXIMUM_COORDINATE));
   function To_Rect (B : L.Box) return Rect is (B.X, B.Y, B.W, B.H);

   function Is_Open (S : Popup_State) return Boolean is (S.Open_Levels > 0);
   function Depth (S : Popup_State) return Menu_Count is (S.Open_Levels);
   function Selected (S : Popup_State) return Item_Count is
     (if S.Open_Levels = 0 then 0 else S.Levels (S.Open_Levels).Selected);

   procedure Open (S : in out Popup_State; M : Model; X, Y : Natural; Area : Rect) is
      Want : constant L.Box := Size (M, ROOT);
   begin
      S.Window := To_Box (Area);
      S.Open_Levels := 1;
      S.Levels (1) :=
        (Menu => ROOT,
         Area => L.Place (Natural'Min (X, L.MAXIMUM_COORDINATE), Natural'Min (Y, L.MAXIMUM_COORDINATE),
                          Want.W, Want.H, S.Window),
         Selected => 0);
      S.Pressed_Inside := False;
   end Open;

   procedure Close (S : in out Popup_State) is
   begin
      S.Open_Levels := 0;
      S.Pressed_Inside := False;
   end Close;

   function Covered (S : Popup_State) return Rect is
      Result : Rect := (others => 0);
   begin
      for K in 1 .. S.Open_Levels loop
         Result := (if Is_Empty (Result) then To_Rect (S.Levels (K).Area)
                    else Union_Rect (Result, To_Rect (S.Levels (K).Area)));
      end loop;
      return Result;
   end Covered;

   --  The top of Row in its menu's box.
   function Row_Top (M : Model; At_Level : Level; Row : Item_Index) return Natural is
      Y : Natural := At_Level.Area.Y + FRAME;
   begin
      for K in 1 .. Natural'Min (Row, Items (M, At_Level.Menu) + 1) - 1 loop
         Y := Y + Height_Of (M.Menus (At_Level.Menu).Rows (K));
      end loop;
      return Y;
   end Row_Top;

   --  The row of At_Level under Y, 0 for none.
   function Row_At (M : Model; At_Level : Level; Y : Natural) return Item_Count is
      Top : Natural := At_Level.Area.Y + FRAME;
   begin
      for K in 1 .. Items (M, At_Level.Menu) loop
         declare
            H : constant Natural := Height_Of (M.Menus (At_Level.Menu).Rows (K));
         begin
            if Y >= Top and then Y < Top + H then
               return K;
            end if;
            Top := Top + H;
         end;
      end loop;
      return 0;
   end Row_At;

   function Selectable (M : Model; Menu : Menu_Index) return L.Selectable_Rows is
      Result : L.Selectable_Rows := [others => False];
   begin
      for K in 1 .. Items (M, Menu) loop
         Result (K) := not M.Menus (Menu).Rows (K).Separator
           and then (M.Menus (Menu).Rows (K).Enabled or else M.Menus (Menu).Rows (K).Submenu > 0);
      end loop;
      return Result;
   end Selectable;

   --  Open the submenu of the deepest level's selected row.
   procedure Open_Submenu (S : in out Popup_State; M : Model) is
      Here : constant Level := S.Levels (S.Open_Levels);
   begin
      if Here.Selected = 0 or else S.Open_Levels = MAXIMUM_MENUS then
         return;
      end if;
      declare
         Child : constant Menu_Count := M.Menus (Here.Menu).Rows (Here.Selected).Submenu;
      begin
         if Child = 0 or else Child > M.Count then
            return;
         end if;
         declare
            Want : constant L.Box := Size (M, Child);
         begin
            S.Open_Levels := S.Open_Levels + 1;
            S.Levels (S.Open_Levels) :=
              (Menu => Child,
               Area => L.Place_Beside (Here.Area, Natural'Min (Row_Top (M, Here, Here.Selected), L.MAXIMUM_COORDINATE),
                                       Want.W, Want.H, S.Window),
               Selected => 0);
         end;
      end;
   end Open_Submenu;

   procedure Draw
     (C : Canvas; Map : in out Controls.Control_Map; S : in out Popup_State; M : Model; Colors : Theme;
      Base : Controls.Control_ID)
   is
   begin
      for K in 1 .. S.Open_Levels loop
         declare
            Here : constant Level := S.Levels (K);
            Box : constant Rect := To_Rect (Here.Area);
            Clipped : constant Canvas := With_Clip (C, Box);
            Y : Natural := Box.y + FRAME;
         begin
            Controls.Add_Surface (Map, Base + K - 1, Box);
            Fill_Rect (C, Box, Colors.panel);
            Stroke_Rect (C, Box, Colors.highlight, Colors.darkShadow);
            if Box.w > 2 and then Box.h > 2 then
               Stroke_Rect (C, Content_Rect (Box, 1, 1), Colors.panel, Colors.shadow);
            end if;
            for R in 1 .. Items (M, Here.Menu) loop
               declare
                  Row : Item renames M.Menus (Here.Menu).Rows (R);
                  Line : constant Rect :=
                    (Box.x + FRAME, Y, (if Box.w > 2 * FRAME then Box.w - 2 * FRAME else 0), Height_Of (Row));
                  Chosen : constant Boolean := R = Here.Selected;
                  Ink : constant Color :=
                    (if not Row.Enabled and then Row.Submenu = 0 then Colors.muted
                     elsif Chosen then Colors.selectionText else Colors.text);
                  Back : constant Color := (if Chosen then Colors.selection else Colors.panel);
                  Text_Y : constant Natural := Center_Text_Y (Line);
               begin
                  if Row.Separator then
                     Fill_Rect (Clipped, (Line.x + TEXT_INSET, Line.y + SEPARATOR_HEIGHT / 2, Line.w - 2 * TEXT_INSET, 1),
                                Colors.shadow);
                     Fill_Rect (Clipped, (Line.x + TEXT_INSET, Line.y + SEPARATOR_HEIGHT / 2 + 1,
                                          Line.w - 2 * TEXT_INSET, 1), Colors.highlight);
                  else
                     Fill_Rect (Clipped, Line, Back);
                     if Row.Has_Icon then
                        CuBit.UI.Icons.Draw
                          (Clipped, Line.x + (ICON_COLUMN - CuBit.UI.Icons.ICON_SIZE) / 2,
                           Line.y + (ROW_HEIGHT - CuBit.UI.Icons.ICON_SIZE) / 2, Row.Picture, Row.Enabled);
                     elsif Row.Checked then
                        Draw_UI_Text_Transparent (Clipped, Line.x + TEXT_INSET, Text_Y, "*", Ink);
                     end if;
                     Draw_UI_Text_Transparent
                       (Clipped, Line.x + ICON_COLUMN, Text_Y, Row.Caption (1 .. Row.Caption_Length), Ink);
                     if Row.Accelerator_Length > 0 then
                        declare
                           Hint : constant String := Row.Accelerator (1 .. Row.Accelerator_Length);
                        begin
                           Draw_UI_Text_Transparent
                             (Clipped, Line.x + Line.w - ARROW_COLUMN - UI_Text_Width (Hint), Text_Y, Hint,
                              (if Chosen then Colors.selectionText else Colors.muted));
                        end;
                     end if;
                     if Row.Submenu > 0 then
                        Draw_UI_Text_Transparent (Clipped, Line.x + Line.w - ARROW_COLUMN + 4, Text_Y, ">", Ink);
                     end if;
                  end if;
                  Y := Y + Height_Of (Row);
               end;
            end loop;
         end;
      end loop;
   end Draw;

   procedure Handle_Key (S : in out Popup_State; M : Model; Pressed : Key; Chosen : out Command;
                         Handled : out Boolean) is
   begin
      Chosen := NO_COMMAND;
      Handled := S.Open_Levels > 0;
      if not Handled then
         return;
      end if;
      declare
         Here : Level renames S.Levels (S.Open_Levels);
         Rows : constant L.Selectable_Rows := Selectable (M, Here.Menu);
         Count : constant Item_Count := Items (M, Here.Menu);
      begin
         case Pressed is
            when Down => Here.Selected := L.Next (Rows, Count, Natural'Min (Here.Selected, Count), True);
            when Up => Here.Selected := L.Next (Rows, Count, Natural'Min (Here.Selected, Count), False);
            when Home => Here.Selected := L.Next (Rows, Count, 0, True);
            when End_Key => Here.Selected := L.Next (Rows, Count, 0, False);
            when Right => Open_Submenu (S, M);
               if S.Open_Levels > 0 then
                  declare
                     Inner : Level renames S.Levels (S.Open_Levels);
                  begin
                     if Inner.Selected = 0 then
                        Inner.Selected := L.Next (Selectable (M, Inner.Menu), Items (M, Inner.Menu), 0, True);
                     end if;
                  end;
               end if;
            when Left =>
               if S.Open_Levels > 1 then
                  S.Open_Levels := S.Open_Levels - 1;
               end if;
            when Escape =>
               if S.Open_Levels > 1 then
                  S.Open_Levels := S.Open_Levels - 1;
               else
                  Close (S);
               end if;
            when Enter =>
               if Here.Selected in 1 .. Count then
                  declare
                     Row : Item renames M.Menus (Here.Menu).Rows (Here.Selected);
                  begin
                     if Row.Submenu > 0 then
                        Open_Submenu (S, M);
                     elsif Row.Enabled then
                        Chosen := Row.Choice;
                        Close (S);
                     end if;
                  end;
               end if;
         end case;
      end;
   end Handle_Key;

   procedure Handle_Pointer
     (S : in out Popup_State; M : Model; Action : Controls.Pointer_Action; X, Y : Natural;
      Chosen : out Command; Handled : out Boolean)
   is
      Hit_Level : Menu_Count := 0;
   begin
      Chosen := NO_COMMAND;
      Handled := S.Open_Levels > 0;
      if not Handled then
         return;
      end if;
      --  The deepest open menu under the pointer.
      for K in reverse 1 .. S.Open_Levels loop
         if Point_In_Rect (X, Y, To_Rect (S.Levels (K).Area)) then
            Hit_Level := K;
            exit;
         end if;
      end loop;
      if Hit_Level = 0 then
         if Action = Controls.Pointer_Press then
            --  Outside: close, and consume the press (no click-through).
            Close (S);
         elsif Action = Controls.Pointer_Release and then not S.Pressed_Inside then
            --  The release of the press that opened the menu (right-click
            --  hold): leave it open.
            null;
         end if;
         return;
      end if;
      declare
         Row : constant Item_Count := Row_At (M, S.Levels (Hit_Level), Y);
         Menu : constant Menu_Index := S.Levels (Hit_Level).Menu;
      begin
         --  Hovering a parent level closes deeper ones not under the row.
         if Action = Controls.Pointer_Move or else Action = Controls.Pointer_Press then
            if Hit_Level < S.Open_Levels and then Row /= S.Levels (Hit_Level).Selected then
               S.Open_Levels := Hit_Level;
            end if;
            if Row > 0 and then Selectable (M, Menu) (Row) then
               S.Levels (Hit_Level).Selected := Row;
               if Hit_Level = S.Open_Levels and then M.Menus (Menu).Rows (Row).Submenu > 0 then
                  Open_Submenu (S, M);
               end if;
            end if;
            if Action = Controls.Pointer_Press then
               S.Pressed_Inside := True;
            end if;
         elsif Action = Controls.Pointer_Release then
            if Row > 0 and then M.Menus (Menu).Rows (Row).Enabled and then M.Menus (Menu).Rows (Row).Submenu = 0
            then
               Chosen := M.Menus (Menu).Rows (Row).Choice;
               Close (S);
            end if;
            S.Pressed_Inside := False;
         end if;
      end;
   end Handle_Pointer;
end CuBit.UI.Popup_Menus;
