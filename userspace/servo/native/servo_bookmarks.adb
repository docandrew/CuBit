with System; with Interfaces; use Interfaces;
with CuBit.UI.Trees; with CuBit.UI.Widgets;
package body Servo_Bookmarks is
   use CuBit.UI; use CuBit.UI.Input;
   package Ed renames CuBit.UI.Editor;
   package CB renames CuBit.UI.Combo_Boxes;
   package CT renames CuBit.UI.Controls;
   use type CT.Pointer_Action;
   Data : Model.Store := [others => (others => <>)];
   Loaded, Load_Error : Boolean := False;
   Revision : Natural := 0;
   function Read_File (Buffer : System.Address; Capacity : Unsigned_32) return Integer_32
     with Import, Convention => C, External_Name => "cubit_bookmarks_load";
   function Write_File (Buffer : System.Address; Length : Unsigned_32) return Unsigned_32
     with Import, Convention => C, External_Name => "cubit_bookmarks_save";
   function Read_Icon (URL : System.Address; Length : Unsigned_32; Pixels : System.Address) return Unsigned_32
     with Import, Convention => C, External_Name => "cubit_bookmark_icon";
   procedure Fetch_Icon (URL : String; Pixels : in out Model.Icon_Pixels) is
      Copy : aliased constant String := URL;
      Candidate : aliased Model.Icon_Pixels := [others => 0];
   begin
      if Read_Icon (Copy'Address, Copy'Length, Candidate'Address) = 1 then Pixels := Candidate; end if;
   end Fetch_Icon;
   procedure Paint_Icon (C : Canvas; X, Y : Natural; Pixels : Model.Icon_Pixels) is
      Bitmap : ARGB_Bitmap (0 .. 15, 0 .. 15);
   begin
      for YP in 0 .. 15 loop for XP in 0 .. 15 loop Bitmap (YP, XP) := Color (Pixels (1 + YP * 16 + XP)); end loop; end loop;
      Draw_Bitmap (C, X, Y, Bitmap);
   end Paint_Icon;
   procedure Message (S : in out Dialog; Text : String) is
   begin
      S.Message_Last := Natural'Min (Text'Length, S.Message'Length);
      S.Message (1 .. S.Message_Last) := Text (Text'First .. Text'First + S.Message_Last - 1);
   end Message;
   procedure Load is
      Buffer : aliased String (1 .. Model.Max_Encoded);
      Count : Integer_32;
      OK : Boolean;
   begin
      if Loaded then return; end if;
      Loaded := True;
      Count := Read_File (Buffer'Address, Buffer'Length);
      if Count < 0 or else Count > Buffer'Length then Load_Error := True;
      elsif Count > 0 then
         Model.Decode (Buffer (1 .. Natural (Count)), Data, OK); Load_Error := not OK;
      end if;
   end Load;
   -- Serialized main-thread persistence; avoid returning a large unconstrained
   -- String through the browser's deliberately bounded Ada secondary stack.
   Save_Buffer : aliased String (1 .. Model.Max_Encoded);
   function Save (Candidate : Model.Store) return Boolean is
      Last : Natural;
   begin
      if Load_Error then return False; end if;
      Model.Encode (Candidate, Save_Buffer, Last);
      if Write_File (Save_Buffer'Address, Unsigned_32 (Last)) = 0 then return False; end if;
      Data := Candidate; Revision := Revision + 1; return True;
   end Save;
   type Caption_Array is array (1 .. Model.Capacity) of aliased String (1 .. 256);
   Captions : Caption_Array;
   Root_Name : aliased constant String := "Bookmarks";
   procedure Parents (S : in out Dialog; Definition : out CB.Model) is
   begin
      Definition := (others => <>); Definition.Count := 1;
      Definition.Choices (1) := (Root_Name'Access, True); S.Parent_IDs := [others => 0];
      for I in Data'Range loop
         if Data (I).Used and then Data (I).Folder and then I /= S.Editing and then Definition.Count < CB.Max_Choices then
            Definition.Count := Definition.Count + 1;
            Captions (Definition.Count) := Data (I).Name;
            -- Package-lifetime fixed buffers, borrowed only during synchronous UI calls.
            Definition.Choices (Definition.Count) := (Captions (Definition.Count)'Unrestricted_Access, True);
            S.Parent_IDs (Definition.Count) := I;
         end if;
      end loop;
   end Parents;
   procedure Select_Item (S : in out Dialog; Item : Model.ID) is
      OK : Boolean; M : CB.Model; Choice : Natural := 1;
   begin
      S.Icon := (if Item = 0 then [others => 0] else Data (Item).Icon);
      S.Editing := Item; S.Folder := Item /= 0 and then Data (Item).Folder;
      Ed.Initialize (S.Name, (if Item = 0 then "" else Model.Title (Data, Item)), OK);
      Ed.Initialize (S.URL, Model.Address (Data, Item), OK);
      Parents (S, M);
      if Item /= 0 then
         for I in 1 .. M.Count loop if S.Parent_IDs (I) = Data (Item).Parent then Choice := I; end if; end loop;
      end if;
      CB.Set_Selection (S.Parent, M, Choice); S.Confirm_Delete := False;
      S.Revision := Revision; S.Message_Last := 0;
   end Select_Item;
   procedure New_Item (S : in out Dialog; Folder : Boolean) is
      Parent : Model.ID := (if S.Editing /= 0 and then Data (S.Editing).Folder then S.Editing else 0);
      M : CB.Model; OK : Boolean;
   begin
      Select_Item (S, 0); S.Folder := Folder; S.Focus := 1;
      Ed.Initialize (S.Name, (if Folder then "New folder" else Ed.Content (S.Page_Title)), OK);
      Ed.Initialize (S.URL, (if Folder then "" else Ed.Content (S.Page_URL)), OK);
      if not Folder then Fetch_Icon (Ed.Content (S.Page_URL), S.Icon); end if;
      Ed.Select_All (S.Name); Parents (S, M);
      for I in 1 .. M.Count loop
         if S.Parent_IDs (I) = Parent then CB.Set_Selection (S.Parent, M, I); end if;
      end loop;
   end New_Item;
   procedure Open (S : in out Dialog; URL, Title : String; Add_Page : Boolean) is
      OK : Boolean;
   begin
      Load; S := (others => <>); S.Opened := True;
      Ed.Initialize (S.Page_URL, URL, OK);
      Ed.Initialize (S.Page_Title, (if Title'Length = 0 then URL else Title), OK);
      Select_Item (S, Model.Find_URL (Data, URL));
      if Add_Page and then S.Editing = 0 then New_Item (S, False); end if;
      if not Add_Page then S.Focus := 4; end if;
      if Load_Error then Message (S, "Could not read bookmarks. Existing file preserved; saving disabled."); end if;
   end Open;
   function Is_Open (S : Dialog) return Boolean is (S.Opened);
   function Location (S : Dialog) return String is (Model.Address (Data, S.Editing));
   procedure Action (S : in out Dialog; Target : Natural; Result : out Outcome) is
      Candidate : Model.Store := Data; Item : Model.ID := S.Editing;
      OK : Boolean;
   begin
      Result := Repaint;
      case Target is
         when 1 =>
            if S.Revision /= Revision then Message (S, "Bookmarks changed in another window. Select the item again."); return; end if;
            Model.Update (Candidate, Item, S.Parent_IDs (Positive'Max (1, CB.Selection (S.Parent))),
              Ed.Content (S.Name), Ed.Content (S.URL), S.Folder, OK);
            if OK and then not S.Folder then Fetch_Icon (Ed.Content (S.URL), Candidate (Item).Icon); end if;
            if not OK then Message (S, "Use a name, an HTTP(S) address, and a valid folder. Limit: 64 items.");
            elsif not Save (Candidate) then Message (S, "Could not save bookmarks. Your changes remain here; try again.");
            else Select_Item (S, Item); Message (S, "Bookmark saved."); end if;
         when 2 => New_Item (S, False);
         when 3 => New_Item (S, True);
         when 4 =>
            if S.Revision /= Revision then Message (S, "Bookmarks changed in another window. Select the item again."); return; end if;
            Model.Delete (Candidate, Item, OK);
            if not OK then Message (S, "Select a bookmark or an empty folder to delete.");
            elsif not S.Confirm_Delete then S.Confirm_Delete := True; Message (S, "Click Delete again to confirm removal.");
            elsif not Save (Candidate) then Message (S, "Could not delete bookmark. Saved data is unchanged.");
            else Select_Item (S, 0); Message (S, "Item deleted."); end if;
         when 5 =>
            if Item /= 0 and then Data (Item).Used and then not Data (Item).Folder then
               S.Opened := False; Result := Navigate;
            end if;
         when 6 => S.Opened := False; Result := Closed;
         when 20 => S.Focus := 1;
         when 21 => if not S.Folder then S.Focus := 2; end if;
         when 600 .. 665 => S.Focus := 3;
         when 801 .. 800 + Model.Capacity =>
            Item := Target - 800;
            if Data (Item).Used then
               Select_Item (S, Item); S.Focus := 4;
               if Data (Item).Folder then S.Expanded (Item) := not S.Expanded (Item); end if;
            end if;
         when others => null;
      end case;
   end Action;
   procedure Handle (S : in out Dialog; Input : Input_Event; Result : out Outcome) is
      Target, X, Y : Natural; Changed, Handled : Boolean; M : CB.Model;
      P : CT.Pointer_Action; K : CB.Key;
      Ctrl : constant Boolean := (Input.payload1 and 2) /= 0;
      Shift : constant Boolean := (Input.payload1 and 1) /= 0;
      procedure Edit (E : in out Ed.Edit_State) is
      begin
         if Input.kind = INPUT_TEXT and then Input.payload0 in 32 .. 126 then
            Ed.Insert (E, String'(1 => Character'Val (Input.payload0)), Changed);
         elsif Input.kind = INPUT_KEY_DOWN then
            case Input.payload0 is
               when 16#0E# => Ed.Backspace (E, Changed);
               when 16#53# => Ed.Delete_Forward (E, Changed);
               when 16#4B# => Ed.Move (E, (if Ctrl then Ed.Move_Word_Left else Ed.Move_Left), Shift);
               when 16#4D# => Ed.Move (E, (if Ctrl then Ed.Move_Word_Right else Ed.Move_Right), Shift);
               when 16#47# => Ed.Move (E, Ed.Move_Start, Shift);
               when 16#4F# => Ed.Move (E, Ed.Move_End, Shift);
               when 16#1E# => if Ctrl then Ed.Select_All (E); end if;
               when others => null;
            end case;
         end if;
      end Edit;
   begin
      Result := Repaint; Parents (S, M);
      if Input.kind = INPUT_RESYNC then
         S.Captured := 0; CB.Dismiss (S.Parent); CT.Clear (S.Map); S.Confirm_Delete := False;
      elsif Input.kind = INPUT_KEY_DOWN then
         if Input.payload0 = 16#01# then
            if CB.Is_Open (S.Parent) then CB.Dismiss (S.Parent);
            else S.Opened := False; Result := Closed; end if;
         elsif Input.payload0 = 16#0F# then
            CB.Dismiss (S.Parent);
            S.Focus := (if Shift then (if S.Focus = 1 then 9 else S.Focus - 1)
                        else (if S.Focus = 9 then 1 else S.Focus + 1));
         elsif S.Focus = 3 then
            case Input.payload0 is
               when 16#48# => K := CB.Up;
               when 16#50# => K := (if (Input.payload1 and 4) /= 0 then CB.Toggle else CB.Down);
               when 16#3E# => K := CB.Toggle;
               when 16#1C# | 16#39# => K := (if CB.Is_Open (S.Parent) then CB.Commit else CB.Toggle);
               when others => return;
            end case;
            CB.Handle_Key (S.Parent, M, K, Changed, Handled);
         elsif Input.payload0 = 16#1C# then
            Action (S, (if S.Focus <= 2 then 1 elsif S.Focus = 4 then 5 else S.Focus - 3), Result);
         elsif S.Focus = 4 and then Input.payload0 in 16#48# | 16#50# then
            for I in 1 .. S.Row_Count loop
               if S.Row_IDs (I) = S.Editing then
                  Target := (if Input.payload0 = 16#48# then Natural'Max (1, I - 1) else Natural'Min (S.Row_Count, I + 1));
                  Select_Item (S, S.Row_IDs (Target));
                  if Target <= S.Scroll then S.Scroll := Target - 1;
                  elsif Target > S.Scroll + S.Visible then S.Scroll := Target - S.Visible; end if;
                  return;
               end if;
            end loop;
            if S.Row_Count > 0 then Select_Item (S, S.Row_IDs (1)); end if;
         elsif S.Focus = 1 then Edit (S.Name);
         elsif S.Focus = 2 and then not S.Folder then Edit (S.URL); end if;
      elsif Input.kind = INPUT_TEXT then
         if S.Focus = 1 then Edit (S.Name);
         elsif S.Focus = 2 and then not S.Folder then Edit (S.URL); end if;
      elsif Input.kind = INPUT_POINTER_WHEEL then
         if CB.Is_Open (S.Parent) then CB.Handle_Wheel (S.Parent, M, Pointer_Wheel_Delta (Input), Handled);
         elsif Pointer_Wheel_Delta (Input) > 0 then S.Scroll := (if S.Scroll > 0 then S.Scroll - 1 else 0);
         elsif S.Row_Count > S.Visible then S.Scroll := Natural'Min (S.Scroll + 1, S.Row_Count - S.Visible); end if;
      elsif Input.kind in INPUT_POINTER_MOVE | INPUT_POINTER_DOWN | INPUT_POINTER_UP then
         X := Pointer_X (Input); Y := Pointer_Y (Input); Target := CT.Hit (S.Map, X, Y);
         P := (if Input.kind = INPUT_POINTER_DOWN then CT.Pointer_Press elsif Input.kind = INPUT_POINTER_UP then CT.Pointer_Release else CT.Pointer_Move);
         if P = CT.Pointer_Press and then CB.Is_Open (S.Parent) and then not CB.Is_Combo_Control (600, Target) then
            CB.Handle_Pointer (S.Parent, M, S.Map, 600, Target, P, Changed, Handled); return;
         end if;
         CuBit.UI.State.Set_Pointer (S.UI, X, Y, Input.kind = INPUT_POINTER_DOWN or else (Input.kind = INPUT_POINTER_MOVE and then (Input.payload1 and 1) /= 0));
         if P = CT.Pointer_Press then S.Captured := Target; end if;
         CT.Dispatch_Pointer (S.Map, S.Captured, P, X, Y, Changed, Handled);
         CB.Handle_Pointer (S.Parent, M, S.Map, 600, Target, P, Changed, Handled);
         if CB.Is_Combo_Control (600, Target) then S.Focus := 3;
         elsif P = CT.Pointer_Release and then Target = S.Captured and then CT.Take_Activated (S.Map, Target) then Action (S, Target, Result); end if;
         if P = CT.Pointer_Release then S.Captured := 0; end if;
      else Result := Unchanged; end if;
   end Handle;
   procedure Draw (C : Canvas; S : in out Dialog; Colors : Theme) is
      B : constant Rect := ((C.width - Natural'Min (720, C.width)) / 2, (C.height - Natural'Min (438, C.height)) / 2,
                            Natural'Min (720, C.width), Natural'Min (438, C.height));
      Content, Row, R : Rect; M : CB.Model; Widget : Widget_Result; Selected : Natural := S.Editing;
      Dummy : Boolean;
      procedure Button (ID : Positive; X, Y, W : Natural; Caption : String) is
      begin
         CuBit.UI.Widgets.Button (C, S.UI, S.Map, ID, (B.x + X, B.y + Y, W, 26), B,
           Colors, Caption, Widget, retainedInput => True);
      end Button;
      procedure Rows (Parent : Model.ID; Depth : Natural) is
      begin
         for I in Data'Range loop
            if Data (I).Used and then Data (I).Parent = Parent then
               S.Row_Count := S.Row_Count + 1; S.Row_IDs (S.Row_Count) := I; S.Depths (S.Row_Count) := Depth;
               if Data (I).Folder and then S.Expanded (I) then Rows (I, Depth + 1); end if;
            end if;
         end loop;
      end Rows;
      procedure Field (ID, Y : Natural; Label : String; E : Ed.Edit_State; Focus : Boolean) is
         R : constant Rect := (B.x + 304, B.y + Y + 22, B.w - 320, 28);
      begin
         CuBit.UI.Widgets.Label (C, (R.x, B.y + Y, R.w, 20), Colors, Label);
         CT.Add_Button (S.Map, ID, R, B);
         Draw_Text_Edit_Field (C, R, Colors, Ed.Content (E), Ed.Cursor (E) - 1,
           Ed.Selection_First (E) - 1, Ed.Selection_Last (E) - 1, Focus, False);
      end Field;
   begin
      CT.Clear (S.Map); CuBit.UI.State.Begin_Frame (S.UI);
      Fill_Rect (C, B, Colors.face); Stroke_Rect (C, B, Colors.shadow, Colors.highlight);
      if B.w < 540 or B.h < 360 then
         CuBit.UI.Widgets.Label (C, B, Colors, "Enlarge window to edit bookmarks; Escape closes."); return;
      end if;
      CuBit.UI.Widgets.Label (C, (B.x + 16, B.y + 10, B.w - 32, 26), Colors, "Bookmarks");
      if (for some P of S.Icon => P /= 0) then Paint_Icon (C, B.x + B.w - 36, B.y + 14, S.Icon); end if;
      Button (2, 16, 44, 130, "New bookmark"); Button (3, 154, 44, 130, "New folder");
      CuBit.UI.Trees.View_Frame (C, (B.x + 16, B.y + 82, 268, B.h - 170), Colors, S.Focus = 4, Content);
      S.Visible := Positive'Max (1, Content.h / CuBit.UI.Trees.TREE_ROW_HEIGHT);
      S.Row_Count := 0; Rows (0, 0);
      S.Scroll := Natural'Min (S.Scroll, (if S.Row_Count > S.Visible then S.Row_Count - S.Visible else 0));
      if S.Row_Count > S.Visible then
         CuBit.UI.Widgets.Vertical_Scrollbar (C, S.UI, S.Map, 700,
           (Content.x + Content.w - 16, Content.y, 16, Content.h), B, Colors,
           0, S.Row_Count - 1, S.Scroll, Widget, pageSize => S.Visible, retainedInput => True);
      end if;
      for I in 1 .. S.Row_Count loop
         if I > S.Scroll and I <= S.Scroll + S.Visible then
            Row := (Content.x, Content.y + (I - S.Scroll - 1) * CuBit.UI.Trees.TREE_ROW_HEIGHT, Content.w - 18, CuBit.UI.Trees.TREE_ROW_HEIGHT);
            declare Item : constant Model.ID := S.Row_IDs (I); begin
               CuBit.UI.Trees.Tree_Item (With_Clip (C, Content), S.UI, S.Map, 800 + Item, Row, B, Colors,
                 Model.Title (Data, Item), Item, Selected, depth => S.Depths (I), expanded => S.Expanded (Item),
                 hasChildren => Data (Item).Folder, icon => (if Data (Item).Folder then CuBit.UI.Trees.Folder_Icon else CuBit.UI.Trees.Device_Icon),
                 focused => S.Focus = 4, result => Widget, retainedInput => True);
               if (for some P of Data (Item).Icon => P /= 0) then
                  Fill_Rect (With_Clip (C, Row),
                    (Row.x + S.Depths (I) * CuBit.UI.Trees.TREE_INDENT + 4, Row.y + 4, 16, 16),
                    (if Item = S.Editing then (if S.Focus = 4 then Colors.selection else Colors.panel)
                     elsif Widget.hot then Colors.highlight else Colors.field));
                  Paint_Icon (With_Clip (C, Row), Row.x + S.Depths (I) * CuBit.UI.Trees.TREE_INDENT + 4, Row.y + 4, Data (Item).Icon);
               end if;
            end;
         end if;
      end loop;
      if S.Row_Count = 0 then CuBit.UI.Widgets.Label (C, Content, Colors, "No bookmarks yet"); end if;
      Field (20, 82, "Name", S.Name, S.Focus = 1);
      Field (21, 146, (if S.Folder then "Folder (no address)" else "Address"), S.URL, S.Focus = 2 and not S.Folder);
      CuBit.UI.Widgets.Label (C, (B.x + 304, B.y + 210, B.w - 320, 20), Colors, "Folder");
      Button (1, 304, 276, 88, "Save"); Button (4, 400, 276, 88, "Delete");
      Button (5, 496, 276, 88, "Open"); Button (6, B.w - 104, B.h - 44, 88, "Done");
      if S.Focus >= 5 then
         R := CT.Bounds (S.Map, S.Focus - 3); Stroke_Rect (C, Inflate_Rect (R, 2), Colors.accent, Colors.accent);
      end if;
      CuBit.UI.Widgets.Label (C, (B.x + 16, B.y + B.h - 76, B.w - 32, 24), Colors, S.Message (1 .. S.Message_Last));
      Parents (S, M);
      CB.Draw (C, S.Map, S.Parent, M, 600, (B.x + 304, B.y + 234, B.w - 320, CB.Default_Height), Colors, Focused => S.Focus = 3);
      CuBit.UI.State.Finish_Frame (S.UI);
   end Draw;
end Servo_Bookmarks;
