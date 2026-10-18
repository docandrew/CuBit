with Text_Commands; use Text_Commands;

--  The optional modal keymap (docs/text-editor.md, "Modal layer"): Vim's
--  Normal, Insert and Visual modes, the commonest subset. It is a state
--  machine from keys to commands of the one command table, so modal and
--  non-modal users share the same commands, undo history and CCL
--  operations. Insert mode passes keys to the default keymap.
--
--  Proved: no runtime errors; at most MAXIMUM_ACTIONS actions per key.
package Text_Modal with SPARK_Mode is
   type Mode is (Normal, Insert, Visual, Visual_Line);
   type Operator is (No_Operator, Delete_Operator, Change_Operator, Yank_Operator);

   MAXIMUM_COUNT : constant := 99_999;
   subtype Repeat_Count is Natural range 0 .. MAXIMUM_COUNT;

   type State is record
      Current : Mode := Normal;
      Count : Repeat_Count := 0;
      Pending : Operator := No_Operator;
      Pending_G : Boolean := False;
   end record;

   MAXIMUM_ACTIONS : constant := 8;
   subtype Action_Count is Natural range 0 .. MAXIMUM_ACTIONS;
   subtype Action_Index is Action_Count range 1 .. MAXIMUM_ACTIONS;
   type Action is record
      Item : Command := No_Command;
      Repeat : Positive := 1;
      Line : Natural := 0;   --  Go_To_Line's argument (G with a count)
   end record;
   type Action_Array is array (Action_Index) of Action;

   --  A key press. Handled False: in Insert mode, the default keymap's.
   procedure Feed_Key
     (S : in out State; Pressed : Chord; Actions : out Action_Array; Count : out Action_Count; Handled : out Boolean);
   --  A typed character. Insert_It: in Insert mode, type it.
   procedure Feed_Text
     (S : in out State; C : Character; Actions : out Action_Array; Count : out Action_Count; Insert_It : out Boolean);

   function Mode_Name (S : State) return String is
     (case S.Current is
         when Normal => "NORMAL", when Insert => "INSERT", when Visual => "VISUAL", when Visual_Line => "VISUAL LINE");
end Text_Modal;
