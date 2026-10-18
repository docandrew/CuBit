package body Text_Modal with SPARK_Mode is
   procedure Add (Actions : in out Action_Array; Count : in out Action_Count; Item : Command; Repeat : Positive := 1)
   is
   begin
      if Count < MAXIMUM_ACTIONS then
         Count := Count + 1;
         Actions (Count) := (Item => Item, Repeat => Repeat, Line => 0);
      end if;
   end Add;

   procedure Feed_Key
     (S : in out State; Pressed : Chord; Actions : out Action_Array; Count : out Action_Count; Handled : out Boolean)
   is
      Plain : constant Boolean := not Pressed.Control and then not Pressed.Alt;
   begin
      Actions := [others => (others => <>)];
      Count := 0;
      Handled := False;
      if Pressed.Item = Escape and then Plain then
         if S.Current = Insert then
            Add (Actions, Count, Move_Left);
         elsif S.Current in Visual | Visual_Line then
            Add (Actions, Count, Single_Selection);
            Add (Actions, Count, Move_Left);
         end if;
         S := (Current => Normal, Count => 0, Pending => No_Operator, Pending_G => False);
         Handled := True;
      elsif S.Current /= Insert and then Pressed.Item = Key_R and then Pressed.Control then
         Add (Actions, Count, Redo, Positive'Max (1, S.Count));
         S.Count := 0;
         Handled := True;
      elsif S.Current /= Insert and then Plain and then Pressed.Item in Enter | Backspace then
         --  Movement keys in Normal and Visual modes.
         Add (Actions, Count, (if Pressed.Item = Enter then (if S.Current = Normal then Move_Down else Select_Down)
                               else (if S.Current = Normal then Move_Left else Select_Left)));
         Handled := True;
      end if;
   end Feed_Key;

   procedure Feed_Text
     (S : in out State; C : Character; Actions : out Action_Array; Count : out Action_Count; Insert_It : out Boolean)
   is
      N : constant Positive := Positive'Max (1, S.Count);
      Visual_Mode : constant Boolean := S.Current in Visual | Visual_Line;

      --  A motion, as a move (Normal) or an extension (Visual).
      procedure Motion (Move, Extend : Command) is
      begin
         Add (Actions, Count, (if Visual_Mode then Extend else Move), N);
      end Motion;

      procedure Finish is
      begin
         S.Count := 0;
         S.Pending := No_Operator;
         S.Pending_G := False;
      end Finish;

      --  An operator with a motion (dw, c$, y...).
      procedure Apply_Operator (Extend : Command) is
         Op : constant Operator := S.Pending;
      begin
         Add (Actions, Count, Extend, N);
         case Op is
            when Delete_Operator => Add (Actions, Count, Cut);
            when Change_Operator =>
               Add (Actions, Count, Cut);
               S.Current := Insert;
            when Yank_Operator =>
               Add (Actions, Count, Copy);
               Add (Actions, Count, Single_Selection);
            when No_Operator => null;
         end case;
         Finish;
      end Apply_Operator;
   begin
      Actions := [others => (others => <>)];
      Count := 0;
      Insert_It := False;
      if S.Current = Insert then
         Insert_It := True;
         return;
      end if;
      --  Counts: 1-9, then 0 too.
      if C in '1' .. '9' or else (C = '0' and then S.Count > 0) then
         if S.Count <= (MAXIMUM_COUNT - 9) / 10 then
            S.Count := S.Count * 10 + (Character'Pos (C) - Character'Pos ('0'));
         end if;
         return;
      end if;
      if S.Pending_G then
         S.Pending_G := False;
         if C = 'g' then
            if S.Count > 0 then
               Count := 1;
               Actions (1) := (Item => Go_To_Line, Repeat => 1, Line => S.Count);
            else
               Motion (Move_Document_Start, Select_Document_Start);
               Actions (Count).Repeat := 1;
            end if;
         end if;
         Finish;
         return;
      end if;
      if S.Pending /= No_Operator then
         case C is
            when 'w' | 'e' => Apply_Operator (Select_Word_Right);
            when 'b' => Apply_Operator (Select_Word_Left);
            when '$' => Apply_Operator (Select_Line_End);
            when '0' | '^' => Apply_Operator (Select_Line_Home);
            when 'h' => Apply_Operator (Select_Left);
            when 'l' => Apply_Operator (Select_Right);
            when 'j' | 'k' | 'd' | 'c' | 'y' =>
               --  Whole lines: dd, cc, yy (and dj, dk).
               if S.Pending = Delete_Operator then
                  if C in 'j' | 'k' then
                     Add (Actions, Count, (if C = 'j' then Select_Down else Select_Up), N);
                  elsif N > 1 then
                     Add (Actions, Count, Select_Down, N - 1);
                  end if;
                  Add (Actions, Count, Select_Line);
                  Add (Actions, Count, Cut);
               elsif S.Pending = Yank_Operator then
                  Add (Actions, Count, Select_Line);
                  Add (Actions, Count, Copy);
                  Add (Actions, Count, Single_Selection);
               else
                  Add (Actions, Count, Move_Line_Home);
                  Add (Actions, Count, Select_Line_End);
                  Add (Actions, Count, Cut);
                  S.Current := Insert;
               end if;
               Finish;
            when others => Finish;
         end case;
         return;
      end if;
      case C is
         when 'h' => Motion (Move_Left, Select_Left);
         when 'l' | ' ' => Motion (Move_Right, Select_Right);
         when 'j' => Motion (Move_Down, Select_Down);
         when 'k' => Motion (Move_Up, Select_Up);
         when 'w' | 'e' => Motion (Move_Word_Right, Select_Word_Right);
         when 'b' => Motion (Move_Word_Left, Select_Word_Left);
         when '0' | '^' => Motion (Move_Line_Home, Select_Line_Home);
         when '$' => Motion (Move_Line_End, Select_Line_End);
         when 'G' =>
            if S.Count > 0 then
               Count := 1;
               Actions (1) := (Item => Go_To_Line, Repeat => 1, Line => S.Count);
            else
               Motion (Move_Document_End, Select_Document_End);
               Actions (Count).Repeat := 1;
            end if;
         when 'g' =>
            S.Pending_G := True;
            return;
         when 'i' => S.Current := Insert;
         when 'a' =>
            Add (Actions, Count, Move_Right);
            S.Current := Insert;
         when 'I' =>
            Add (Actions, Count, Move_Line_Home);
            S.Current := Insert;
         when 'A' =>
            Add (Actions, Count, Move_Line_End);
            S.Current := Insert;
         when 'o' =>
            Add (Actions, Count, Move_Line_End);
            Add (Actions, Count, New_Line);
            S.Current := Insert;
         when 'O' =>
            Add (Actions, Count, Move_Line_Home);
            Add (Actions, Count, New_Line);
            Add (Actions, Count, Move_Up);
            S.Current := Insert;
         when 'x' =>
            if Visual_Mode then
               Add (Actions, Count, Cut);
               S.Current := Normal;
            else
               Add (Actions, Count, Delete_Forward, N);
            end if;
         when 'd' =>
            if Visual_Mode then
               if S.Current = Visual_Line then
                  Add (Actions, Count, Select_Line);
               end if;
               Add (Actions, Count, Cut);
               S.Current := Normal;
            else
               S.Pending := Delete_Operator;
               return;
            end if;
         when 'c' =>
            if Visual_Mode then
               Add (Actions, Count, Cut);
               S.Current := Insert;
            else
               S.Pending := Change_Operator;
               return;
            end if;
         when 'y' =>
            if Visual_Mode then
               if S.Current = Visual_Line then
                  Add (Actions, Count, Select_Line);
               end if;
               Add (Actions, Count, Copy);
               Add (Actions, Count, Single_Selection);
               S.Current := Normal;
            else
               S.Pending := Yank_Operator;
               return;
            end if;
         when 'D' =>
            Add (Actions, Count, Select_Line_End);
            Add (Actions, Count, Cut);
         when 'C' =>
            Add (Actions, Count, Select_Line_End);
            Add (Actions, Count, Cut);
            S.Current := Insert;
         when 'p' | 'P' => Add (Actions, Count, Paste, N);
         when 'u' =>
            Add (Actions, Count, Undo, N);
            Add (Actions, Count, Collapse_Selection);
         when 'v' =>
            S.Current := (if S.Current = Visual then Normal else Visual);
         when 'V' =>
            if S.Current = Visual_Line then
               S.Current := Normal;
            else
               S.Current := Visual_Line;
               Add (Actions, Count, Select_Line);
            end if;
         when '/' => Add (Actions, Count, Find);
         when 'n' => Add (Actions, Count, Find_Next, N);
         when 'N' => Add (Actions, Count, Find_Previous, N);
         when '*' =>
            Add (Actions, Count, Select_Word_Or_Next);
            Add (Actions, Count, Find_Next);
         when ':' => Add (Actions, Count, Command_Palette);
         when '>' => Add (Actions, Count, Indent, N);
         when '<' => Add (Actions, Count, Outdent, N);
         when others => null;
      end case;
      Finish;
   end Feed_Text;
end Text_Modal;
