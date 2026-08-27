package CuBit.UI.Editor with SPARK_Mode is
   MAX_TEXT_LENGTH : constant := 1_024;

   subtype Text_Length is Natural range 0 .. MAX_TEXT_LENGTH;
   subtype Text_Position is Positive range 1 .. MAX_TEXT_LENGTH + 1;

   type Edit_State is private;
   type Movement is (Move_Left, Move_Right, Move_Word_Left, Move_Word_Right,
                     Move_Start, Move_End);

   procedure Initialize
     (State : out Edit_State; Text : String; Accepted : out Boolean);
   function Content (State : Edit_State) return String;
   function Length (State : Edit_State) return Text_Length;
   function Cursor (State : Edit_State) return Text_Position;
   function Selection_First (State : Edit_State) return Text_Position;
   function Selection_Last (State : Edit_State) return Text_Position;

   procedure Insert
     (State : in out Edit_State; Text : String; Changed : out Boolean);
   procedure Move
     (State : in out Edit_State; Direction : Movement;
      Extend_Selection : Boolean := False);
   procedure Place_Cursor
     (State : in out Edit_State; Position : Text_Position;
      Extend_Selection : Boolean := False);
   procedure Select_Word_At
     (State : in out Edit_State; Position : Text_Position);
   procedure Backspace (State : in out Edit_State; Changed : out Boolean);
   procedure Delete_Forward
     (State : in out Edit_State; Changed : out Boolean);
   procedure Select_All (State : in out Edit_State);

private
   type Edit_State is record
      Data   : String (1 .. MAX_TEXT_LENGTH) := [others => ' '];
      Last   : Text_Length := 0;
      Point  : Text_Position := 1;
      Anchor : Text_Position := 1;
   end record
   with Type_Invariant =>
     Edit_State.Point <= Edit_State.Last + 1 and then
     Edit_State.Anchor <= Edit_State.Last + 1;
end CuBit.UI.Editor;
