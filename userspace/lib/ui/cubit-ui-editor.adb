package body CuBit.UI.Editor with SPARK_Mode is
   function Is_Word_Character (Value : Character) return Boolean is
     ((Value >= 'a' and then Value <= 'z') or else
      (Value >= 'A' and then Value <= 'Z') or else
      (Value >= '0' and then Value <= '9') or else Value = '_');

   function Is_Whitespace (Value : Character) return Boolean is
     (Value = ' ' or else Value = ASCII.HT or else
      Value = ASCII.LF or else Value = ASCII.CR);

   function Content (State : Edit_State) return String is
     (State.Data (1 .. State.Last));

   function Length (State : Edit_State) return Text_Length is (State.Last);
   function Cursor (State : Edit_State) return Text_Position is (State.Point);

   function Selection_First (State : Edit_State) return Text_Position is
     (Text_Position'Min (State.Point, State.Anchor));

   function Selection_Last (State : Edit_State) return Text_Position is
     (Text_Position'Max (State.Point, State.Anchor));

   procedure Initialize
     (State : out Edit_State; Text : String; Accepted : out Boolean)
   is
   begin
      State := (others => <>);
      Accepted := Text'Length <= MAX_TEXT_LENGTH;
      if Accepted and then Text'Length > 0 then
         State.Data (1 .. Text'Length) := Text;
         State.Last := Text'Length;
         State.Point := Text'Length + 1;
         State.Anchor := State.Point;
      end if;
   end Initialize;

   procedure Delete_Selection
     (State : in out Edit_State; Changed : out Boolean)
   is
      First : constant Text_Position := Selection_First (State);
      Last  : constant Text_Position := Selection_Last (State);
      Count : constant Natural := Last - First;
   begin
      Changed := Count > 0;
      if Changed then
         if Last <= State.Last then
            State.Data (First .. State.Last - Count) :=
              State.Data (Last .. State.Last);
         end if;
         State.Last := State.Last - Count;
         State.Point := First;
         State.Anchor := First;
      end if;
   end Delete_Selection;

   procedure Insert
     (State : in out Edit_State; Text : String; Changed : out Boolean)
   is
      Removed : Boolean;
      Room : Natural;
      Count : Natural;
   begin
      Delete_Selection (State, Removed);
      Room := MAX_TEXT_LENGTH - State.Last;
      Count := Natural'Min (Room, Text'Length);
      Changed := Removed or else Count > 0;
      if Count > 0 then
         if State.Point <= State.Last then
            State.Data (State.Point + Count .. State.Last + Count) :=
              State.Data (State.Point .. State.Last);
         end if;
         for Offset in 0 .. Count - 1 loop
            pragma Loop_Invariant (Offset < Text'Length);
            State.Data (State.Point + Offset) := Text (Text'First + Offset);
         end loop;
         State.Last := State.Last + Count;
         State.Point := State.Point + Count;
         State.Anchor := State.Point;
      end if;
   end Insert;

   procedure Move
     (State : in out Edit_State; Direction : Movement;
      Extend_Selection : Boolean := False)
   is
      Limit : constant Text_Position := State.Last + 1;
      subtype Current_Position is
        Text_Position range 1 .. Limit;
      Target : Current_Position := State.Point;
   begin
      case Direction is
         when Move_Left =>
            if Target > 1 then Target := Target - 1; end if;
         when Move_Right =>
            if Target <= State.Last then Target := Target + 1; end if;
         when Move_Start =>
            Target := 1;
         when Move_End =>
            Target := State.Last + 1;
         when Move_Word_Left =>
            if Target > 1 and then
              Is_Whitespace (State.Data (Target - 1))
            then
               while Target > 1 and then
                 Is_Whitespace (State.Data (Target - 1))
               loop
                  pragma Loop_Invariant (Target <= State.Last + 1);
                  Target := Target - 1;
               end loop;
            end if;
            if Target > 1 and then
              Is_Word_Character (State.Data (Target - 1))
            then
               while Target > 1 and then
                 Is_Word_Character (State.Data (Target - 1))
               loop
                  pragma Loop_Invariant (Target <= State.Last + 1);
                  Target := Target - 1;
               end loop;
            elsif Target > 1 then
               Target := Target - 1;
            end if;
         when Move_Word_Right =>
            if Target <= State.Last and then
              Is_Word_Character (State.Data (Target))
            then
               while Target <= State.Last and then
                 Is_Word_Character (State.Data (Target))
               loop
                  pragma Loop_Invariant (Target <= State.Last + 1);
                  Target := Target + 1;
               end loop;
               while Target <= State.Last and then
                 Is_Whitespace (State.Data (Target))
               loop
                  pragma Loop_Invariant (Target <= State.Last + 1);
                  Target := Target + 1;
               end loop;
            elsif Target <= State.Last and then
              Is_Whitespace (State.Data (Target))
            then
               while Target <= State.Last and then
                 Is_Whitespace (State.Data (Target))
               loop
                  pragma Loop_Invariant (Target <= State.Last + 1);
                  Target := Target + 1;
               end loop;
            elsif Target <= State.Last then
               Target := Target + 1;
            end if;
      end case;
      State.Point := Target;
      if not Extend_Selection then State.Anchor := Target; end if;
   end Move;

   procedure Place_Cursor
     (State : in out Edit_State; Position : Text_Position;
      Extend_Selection : Boolean := False)
   is
      Target : constant Text_Position :=
        Text_Position'Min (Position, State.Last + 1);
   begin
      State.Point := Target;
      if not Extend_Selection then State.Anchor := Target; end if;
   end Place_Cursor;

   procedure Select_Word_At
     (State : in out Edit_State; Position : Text_Position)
   is
      Index : Text_Position;
      First : Text_Position;
      Last  : Text_Position;
      Word  : Boolean;
   begin
      if State.Last = 0 then
         State.Point := 1;
         State.Anchor := 1;
         return;
      end if;
      Index := Text_Position'Min (Position, State.Last);
      First := Index;
      Last := Index;
      Word := Is_Word_Character (State.Data (Index));
      while First > 1 and then
        Is_Word_Character (State.Data (First - 1)) = Word
      loop
         pragma Loop_Invariant (First <= State.Last);
         First := First - 1;
      end loop;
      while Last < State.Last and then
        Is_Word_Character (State.Data (Last + 1)) = Word
      loop
         pragma Loop_Invariant (Last <= State.Last);
         Last := Last + 1;
      end loop;
      State.Anchor := First;
      State.Point := Last + 1;
   end Select_Word_At;

   procedure Backspace (State : in out Edit_State; Changed : out Boolean) is
   begin
      Delete_Selection (State, Changed);
      if not Changed and then State.Point > 1 then
         State.Anchor := State.Point - 1;
         Delete_Selection (State, Changed);
      end if;
   end Backspace;

   procedure Delete_Forward
     (State : in out Edit_State; Changed : out Boolean)
   is
   begin
      Delete_Selection (State, Changed);
      if not Changed and then State.Point <= State.Last then
         State.Anchor := State.Point + 1;
         Delete_Selection (State, Changed);
      end if;
   end Delete_Forward;

   procedure Select_All (State : in out Edit_State) is
   begin
      State.Anchor := 1;
      State.Point := State.Last + 1;
   end Select_All;
end CuBit.UI.Editor;
