with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.State;
with CuBit.UI.Trees; use CuBit.UI.Trees;
with CuBit.UI.Combo_Boxes;
procedure Tree_Preview is
   package IO renames Ada.Streams.Stream_IO;
   Width : constant := 840;
   Height : constant := 420;
   Pixels : aliased array (0 .. Width * Height - 1) of Color := [others => 0];
   C : constant Canvas := (addr => Pixels'Address, width => Width, height => Height,
     pitch => Width * 4, others => <>);
   F : IO.File_Type;
   RGB : Stream_Element_Array (1 .. Width * Height * 3);
   At_Byte : Stream_Element_Offset := 1;
   UI : CuBit.UI.State.UI_State;
   Map : CuBit.UI.Controls.Control_Map;
   Model : CuBit.UI.Combo_Boxes.Model;
   Combo : CuBit.UI.Combo_Boxes.Combo_State;
   Choice : aliased constant String := "All locations";
   Selected : Natural := 4;
   Content : Rect;
   Result : Widget_Result;
   procedure Item (I, Depth : Natural; Caption : String; Icon : Tree_Item_Icon;
                   Children : Boolean := False; Expanded : Boolean := False;
                   Last : Boolean := False; Branches : Unsigned_64 := 0;
                   T : Theme) is
      R : constant Rect := (Content.x, Content.y + (I - 1) * TREE_ROW_HEIGHT,
                            Content.w - 18, TREE_ROW_HEIGHT);
   begin
      Tree_Item (With_Clip (C, Content), UI, Map, I, R, R, T, Caption, I, Selected,
        depth => Depth, expanded => Expanded, hasChildren => Children, icon => Icon,
        lastSibling => Last, ancestorBranches => Branches,
        result => Result, retainedInput => True);
   end Item;
begin
   Model.Count := 1; Model.Choices (1) := (Choice'Unchecked_Access, True);
   CuBit.UI.Combo_Boxes.Set_Selection (Combo, Model, 1);
   for Dark in Boolean loop
      declare
         X : constant Natural := (if Dark then 420 else 0);
         T : constant Theme := (if Dark then CuBit_Alloy_Dark else CuBit_Alloy);
      begin
         Fill_Rect (C, (X, 0, 420, Height), T.panel);
         Draw_UI_Text (C, X + 20, 14, (if Dark then "Tree view / dark" else "Tree view / light"), T.text, T.panel);
         CuBit.UI.Controls.Clear (Map); CuBit.UI.State.Begin_Frame (UI);
         CuBit.UI.Combo_Boxes.Draw (C, Map, Combo, Model, 800,
           (X + 20, 40, 380, CuBit.UI.Combo_Boxes.Default_Height), T);
         View_Frame (C, (X + 20, 80, 380, 314), T, False, Content);
         Item (1, 0, "Computer", Computer_Icon, True, True, T => T);
         Item (2, 1, "Home", Folder_Icon, True, True, T => T);
         Item (3, 2, "Documents", Folder_Icon, True, True, Branches => 1, T => T);
         Item (4, 3, "Design notes", No_Icon, Branches => 3, T => T);
         Item (5, 3, "Reports", Folder_Icon, True, False, True, 3, T);
         Item (6, 2, "Downloads", Folder_Icon, True, False, Branches => 1, T => T);
         Item (7, 2, "Bookmarks", Folder_Icon, True, False, True, 1, T);
         Item (8, 1, "Devices", Bus_Icon, True, True, True, T => T);
         Item (9, 2, "NVMe storage", Storage_Icon, T => T);
         Item (10, 2, "Network", Network_Icon, T => T);
         Item (11, 2, "Display", Display_Icon, Last => True, T => T);
         Draw_Vertical_Scrollbar (C, (Content.x + Content.w - 16, Content.y, 16, Content.h),
           T, 0, 19, 0, False, False, 11);
      end;
   end loop;
   for P of Pixels loop
      RGB (At_Byte) := Stream_Element (Shift_Right (P, 16) and 255);
      RGB (At_Byte + 1) := Stream_Element (Shift_Right (P, 8) and 255);
      RGB (At_Byte + 2) := Stream_Element (P and 255);
      At_Byte := At_Byte + 3;
   end loop;
   IO.Create (F, IO.Out_File, "/tmp/cubit-tree-preview.ppm");
   String'Write (IO.Stream (F), "P6" & ASCII.LF & "840 420" & ASCII.LF & "255" & ASCII.LF);
   IO.Write (F, RGB); IO.Close (F);
end Tree_Preview;
