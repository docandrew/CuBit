pragma Ada_2022;
with Ada.Unchecked_Deallocation;

package body Dash_Edit is
   use Dash_Model;

   --  Every panel ever shown, by its identity (from 1).
   type Panel_Array is array (Positive range <>) of Panel;
   type Panel_Array_Access is access Panel_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Panel_Array, Panel_Array_Access);
   Library : Panel_Array_Access;
   Known : Natural := 0;

   --  A layout: which panels, in which order, how large.
   type Id_List is array (Panel_Index) of Natural;
   type Size_List is array (Panel_Index) of Natural;
   type Layout is record
      Used : Panel_Count := 0;
      Ids : Id_List := [others => 0];
      Widths, Heights : Size_List := [others => 1];
   end record;
   type Layout_Array is array (Positive range <>) of Layout;
   type Layout_Array_Access is access Layout_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Layout_Array, Layout_Array_Access);
   History : Layout_Array_Access;
   --  Layouts 1 .. Undo_Top undo; Undo_Top + 1 .. Redo_Top redo.
   Undo_Top, Redo_Top : Natural := 0;
   --  A group is open (a drag), and its first edit is in the history.
   Group_Open, Group_Pushed : Boolean := False;
   Initial_History : constant := 16;
   Initial_Library : constant := 16;

   function Layout_Of (D : Dashboard) return Layout is
      L : Layout;
   begin
      L.Used := D.Panels_Used;
      for P in 1 .. D.Panels_Used loop
         L.Ids (P) := D.Panels (P).Id;
         L.Widths (P) := D.Panels (P).Width;
         L.Heights (P) := D.Panels (P).Height;
      end loop;
      return L;
   end Layout_Of;

   procedure Remember (P : Panel);
   procedure Remember (P : Panel) is
   begin
      if Library = null then
         Library := new Panel_Array (1 .. Initial_Library);
      end if;
      if P.Id > Library'Length then
         declare
            Grown : constant Panel_Array_Access := new Panel_Array (1 .. Natural'Max (P.Id, 2 * Library'Length));
         begin
            Grown (1 .. Library'Length) := Library.all;
            Free (Library);
            Library := Grown;
         end;
      end if;
      if P.Id >= 1 then
         Library (P.Id) := P;
         Known := Natural'Max (Known, P.Id);
      end if;
   end Remember;

   procedure Restore (D : in out Dashboard; L : Layout);
   procedure Restore (D : in out Dashboard; L : Layout) is
   begin
      D.Panels_Used := L.Used;
      for P in 1 .. L.Used loop
         D.Panels (P) := Library (L.Ids (P));
         D.Panels (P).Width := L.Widths (P);
         D.Panels (P).Height := L.Heights (P);
      end loop;
   end Restore;

   procedure Push (L : Layout; At_Index : Positive);
   procedure Push (L : Layout; At_Index : Positive) is
   begin
      if History = null then
         History := new Layout_Array (1 .. Initial_History);
      end if;
      if At_Index > History'Length then
         if History'Length >= Maximum_History then
            --  Full: the oldest edit is let go.
            History (1 .. History'Length - 1) := History (2 .. History'Length);
            History (History'Length) := L;
            return;
         end if;
         declare
            Grown : constant Layout_Array_Access :=
              new Layout_Array (1 .. Natural'Min (2 * History'Length, Maximum_History));
         begin
            Grown (1 .. History'Length) := History.all;
            Free (History);
            History := Grown;
         end;
      end if;
      History (At_Index) := L;
   end Push;

   procedure Start (D : Dashboard) is
   begin
      Known := 0;
      for P in 1 .. D.Panels_Used loop
         Remember (D.Panels (P));
      end loop;
      Undo_Top := 0;
      Redo_Top := 0;
   end Start;

   procedure Apply (D : in out Dashboard; Op : Operation; P : Positive; Done : out Boolean; Selected : out Natural) is
      Before : constant Layout := Layout_Of (D);
   begin
      Done := False;
      Selected := P;
      if P > D.Panels_Used then
         return;
      end if;
      case Op is
         when Move_Earlier =>
            if P > 1 then
               declare
                  Held : constant Panel := D.Panels (P - 1);
               begin
                  D.Panels (P - 1) := D.Panels (P);
                  D.Panels (P) := Held;
               end;
               Selected := P - 1;
               Done := True;
            end if;
         when Move_Later =>
            if P < D.Panels_Used then
               declare
                  Held : constant Panel := D.Panels (P + 1);
               begin
                  D.Panels (P + 1) := D.Panels (P);
                  D.Panels (P) := Held;
               end;
               Selected := P + 1;
               Done := True;
            end if;
         when Wider =>
            if D.Panels (P).Width < Grid_Columns then
               D.Panels (P).Width := D.Panels (P).Width + 1;
               Done := True;
            end if;
         when Narrower =>
            if D.Panels (P).Width > 1 then
               D.Panels (P).Width := D.Panels (P).Width - 1;
               Done := True;
            end if;
         when Taller =>
            if D.Panels (P).Height < Maximum_Grid_Height then
               D.Panels (P).Height := D.Panels (P).Height + 1;
               Done := True;
            end if;
         when Shorter =>
            if D.Panels (P).Height > 1 then
               D.Panels (P).Height := D.Panels (P).Height - 1;
               Done := True;
            end if;
         when Remove =>
            if D.Panels_Used > 1 then
               Remember (D.Panels (P));
               D.Panels (P .. D.Panels_Used - 1) := D.Panels (P + 1 .. D.Panels_Used);
               D.Panels_Used := D.Panels_Used - 1;
               Selected := Natural'Min (P, D.Panels_Used);
               Done := True;
            end if;
         when Duplicate =>
            if D.Panels_Used < Maximum_Panels then
               declare
                  Copy : Panel := D.Panels (P);
               begin
                  Copy.Id := Known + 1;
                  Remember (Copy);
                  D.Panels (P + 2 .. D.Panels_Used + 1) := D.Panels (P + 1 .. D.Panels_Used);
                  D.Panels (P + 1) := Copy;
                  D.Panels_Used := D.Panels_Used + 1;
                  Selected := P + 1;
                  Done := True;
               end;
            end if;
      end case;
      if Done then
         --  Remember every panel's present state (a resize changed one).
         for K in 1 .. D.Panels_Used loop
            Remember (D.Panels (K));
         end loop;
         if not (Group_Open and then Group_Pushed) then
            Push (Before, Undo_Top + 1);
            Undo_Top := Natural'Min (Undo_Top + 1, History'Length);
         end if;
         Group_Pushed := Group_Open;
         Redo_Top := Undo_Top;
      end if;
   end Apply;

   procedure Undo (D : in out Dashboard; Done : out Boolean) is
      Now : constant Layout := Layout_Of (D);
   begin
      Done := Undo_Top > 0;
      if not Done then
         return;
      end if;
      declare
         Back : constant Layout := History (Undo_Top);
      begin
         --  What undo leaves becomes the redo of this step.
         History (Undo_Top) := Now;
         Undo_Top := Undo_Top - 1;
         Restore (D, Back);
      end;
   end Undo;

   procedure Redo (D : in out Dashboard; Done : out Boolean) is
      Now : constant Layout := Layout_Of (D);
   begin
      Done := Undo_Top < Redo_Top;
      if not Done then
         return;
      end if;
      Undo_Top := Undo_Top + 1;
      declare
         Forward : constant Layout := History (Undo_Top);
      begin
         History (Undo_Top) := Now;
         Restore (D, Forward);
      end;
   end Redo;

   procedure Begin_Group is
   begin
      Group_Open := True;
      Group_Pushed := False;
   end Begin_Group;

   procedure End_Group is
   begin
      Group_Open := False;
      Group_Pushed := False;
   end End_Group;

   function Can_Undo return Boolean is (Undo_Top > 0);
   function Can_Redo return Boolean is (Undo_Top < Redo_Top);
end Dash_Edit;
