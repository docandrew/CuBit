with Client_Popup_Layout;

package body CuBit.UI.Tooltips is
   package TP renames Client_Tooltip_Policy;
   package PL renames Client_Popup_Layout;

   --  Below and right of the pointer, clear of the cursor.
   POINTER_GAP : constant := 18;
   PADDING : constant := 6;
   HINT_GAP : constant := 12;

   procedure Clear (Table : in out Tip_Table) is
   begin
      Table.Count := 0;
   end Clear;

   procedure Add (Table : in out Tip_Table; Item : Tip_Entry) is
   begin
      if Table.Count < MAXIMUM_TIPS then
         Table.Count := Table.Count + 1;
         Table.Entries (Table.Count) := Item;
      end if;
   end Add;

   function Make (Kind : Tip_Kind; Text, Hint : String) return Tip_Entry is
      Item : Tip_Entry;
   begin
      Item.Kind := Kind;
      Item.Text_Length := Natural'Min (Text'Length, MAXIMUM_TEXT);
      Item.Text (1 .. Item.Text_Length) := Text (Text'First .. Text'First + Item.Text_Length - 1);
      Item.Hint_Length := Natural'Min (Hint'Length, MAXIMUM_HINT);
      Item.Hint (1 .. Item.Hint_Length) := Hint (Hint'First .. Hint'First + Item.Hint_Length - 1);
      return Item;
   end Make;

   procedure Set (Table : in out Tip_Table; Id : Controls.Control_ID; Text : String; Hint : String := "") is
      Item : Tip_Entry := Make (Control_Tip, Text, Hint);
   begin
      Item.Id := Id;
      Add (Table, Item);
   end Set;

   procedure Set_Region (Table : in out Tip_Table; Area : Rect; Text : String; Hint : String := "") is
      Item : Tip_Entry := Make (Region_Tip, Text, Hint);
   begin
      Item.Area := Area;
      Add (Table, Item);
   end Set_Region;

   function Visible (Tip : Tooltip) return Boolean is (TP.Showing (Tip.Policy));
   function Area_Of (Tip : Tooltip) return Rect is (if Visible (Tip) then Tip.Box else (others => 0));
   function Next_Deadline (Tip : Tooltip) return Unsigned_64 is (TP.Next_Deadline (Tip.Policy));

   --  Targets: a control's ID, or REGION_TARGET + the region's place among
   --  the regions (set in the same order every frame).
   REGION_TARGET : constant := 1_000_000;

   --  The target under (X, Y): regions first, then the control hit.
   function Find (Table : Tip_Table; Map : Controls.Control_Map; X, Y : Natural) return Natural is
      Hit : constant Controls.Control_ID := Controls.Hit (Map, X, Y);
      Regions : Natural := 0;
   begin
      for K in 1 .. Table.Count loop
         if Table.Entries (K).Kind = Region_Tip then
            Regions := Regions + 1;
            if Point_In_Rect (X, Y, Table.Entries (K).Area) then
               return REGION_TARGET + Regions;
            end if;
         end if;
      end loop;
      if Hit /= Controls.NO_CONTROL and then Hit < REGION_TARGET then
         for K in 1 .. Table.Count loop
            if Table.Entries (K).Kind = Control_Tip and then Table.Entries (K).Id = Hit then
               return Hit;
            end if;
         end loop;
      end if;
      return 0;
   end Find;

   --  The entry a target names, 0 for none.
   function Entry_Of (Table : Tip_Table; On : Natural) return Natural is
      Regions : Natural := 0;
   begin
      for K in 1 .. Table.Count loop
         if Table.Entries (K).Kind = Region_Tip then
            Regions := Regions + 1;
            if On = REGION_TARGET + Regions then
               return K;
            end if;
         elsif On < REGION_TARGET and then Table.Entries (K).Id = On then
            return K;
         end if;
      end loop;
      return 0;
   end Entry_Of;

   --  Where the tip for the current target goes within Area (empty when
   --  hidden or the target has no entry).
   function Layout (Tip : Tooltip; Table : Tip_Table; Area : Rect) return Rect is
      K : constant Natural := Entry_Of (Table, Tip.Policy.On);
   begin
      if not Visible (Tip) or else K = 0 or else Is_Empty (Area) then
         return (others => 0);
      end if;
      declare
         Item : Tip_Entry renames Table.Entries (K);
         Width : constant Natural :=
           2 * PADDING + UI_Text_Width (Item.Text (1 .. Item.Text_Length))
           + (if Item.Hint_Length > 0 then HINT_GAP + UI_Text_Width (Item.Hint (1 .. Item.Hint_Length)) else 0);
         Height : constant Natural := 2 * PADDING + UI_Text_Height;
         Placed : constant PL.Box :=
           PL.Place (Natural'Min (Tip.Policy.X + POINTER_GAP, PL.MAXIMUM_COORDINATE),
                     Natural'Min (Tip.Policy.Y + POINTER_GAP, PL.MAXIMUM_COORDINATE),
                     Natural'Min (Width, PL.MAXIMUM_COORDINATE), Natural'Min (Height, PL.MAXIMUM_COORDINATE),
                     (Natural'Min (Area.x, PL.MAXIMUM_COORDINATE), Natural'Min (Area.y, PL.MAXIMUM_COORDINATE),
                      Natural'Min (Area.w, PL.MAXIMUM_COORDINATE), Natural'Min (Area.h, PL.MAXIMUM_COORDINATE)));
      begin
         return (Placed.X, Placed.Y, Placed.W, Placed.H);
      end;
   end Layout;

   --  After a change: the old box and the new one repaint.
   procedure Relayout (Tip : in out Tooltip; Table : Tip_Table; Before : Rect; Damage : out Rect) is
   begin
      Tip.Box := Layout (Tip, Table, Tip.Area);
      Damage := (if Is_Empty (Before) then Tip.Box
                 elsif Is_Empty (Tip.Box) then Before
                 else Union_Rect (Before, Tip.Box));
   end Relayout;

   procedure Pointer_At
     (Tip : in out Tooltip; Table : Tip_Table; Map : Controls.Control_Map; X, Y : Natural; Now_Ms : Unsigned_64;
      Damage : out Rect)
   is
      Was_Shown : constant Boolean := Visible (Tip);
      Was_On : constant Natural := Tip.Policy.On;
      Before : constant Rect := Area_Of (Tip);
   begin
      TP.Pointer_At (Tip.Policy, Find (Table, Map, X, Y), X, Y, Now_Ms);
      Damage := (others => 0);
      if Was_Shown and then (not Visible (Tip) or else Tip.Policy.On /= Was_On) then
         --  Hidden, or slid to the next target.
         Relayout (Tip, Table, Before, Damage);
      end if;
   end Pointer_At;

   procedure Dismiss (Tip : in out Tooltip; Damage : out Rect) is
   begin
      Damage := Area_Of (Tip);
      TP.Dismiss (Tip.Policy);
      Tip.Box := (others => 0);
   end Dismiss;

   procedure Tick (Tip : in out Tooltip; Table : Tip_Table; Now_Ms : Unsigned_64; Damage : out Rect) is
      Changed : Boolean;
   begin
      TP.Tick (Tip.Policy, Now_Ms, Changed);
      Damage := (others => 0);
      if Changed then
         Relayout (Tip, Table, (others => 0), Damage);
      end if;
   end Tick;

   function Text (Tip : Tooltip; Table : Tip_Table) return String is
      K : constant Natural := Entry_Of (Table, Tip.Policy.On);
   begin
      return (if Visible (Tip) and then K > 0 then Table.Entries (K).Text (1 .. Table.Entries (K).Text_Length) else "");
   end Text;

   procedure Draw (C : Canvas; Tip : in out Tooltip; Table : Tip_Table; Area : Rect; Colors : Theme) is
      K : constant Natural := Entry_Of (Table, Tip.Policy.On);
   begin
      Tip.Area := Area;
      Tip.Box := Layout (Tip, Table, Area);
      if Is_Empty (Tip.Box) then
         return;
      end if;
      declare
         Item : Tip_Entry renames Table.Entries (K);
         Box : constant Rect := Tip.Box;
         Inner : constant Canvas := With_Clip (C, Box);
      begin
         Fill_Rect (C, Box, Colors.panel);
         Stroke_Rect (C, Box, Colors.edge, Colors.shadow);
         Draw_UI_Text_Transparent (Inner, Box.x + PADDING, Box.y + PADDING, Item.Text (1 .. Item.Text_Length),
                                   Colors.text);
         if Item.Hint_Length > 0 then
            Draw_UI_Text_Transparent
              (Inner, Box.x + Box.w - PADDING - UI_Text_Width (Item.Hint (1 .. Item.Hint_Length)), Box.y + PADDING,
               Item.Hint (1 .. Item.Hint_Length), Colors.muted);
         end if;
      end;
   end Draw;
end CuBit.UI.Tooltips;
