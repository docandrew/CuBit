package CuBit.UI.Editor.Documents with SPARK_Mode is
   MAX_DOCUMENT_CAPACITY : constant := 1_048_576;
   subtype Document_Capacity is
     Positive range 1 .. MAX_DOCUMENT_CAPACITY;
   subtype Document_Position is
     Positive range 1 .. MAX_DOCUMENT_CAPACITY + 1;
   subtype Line_Character_Count is Natural range 0 .. MAX_DOCUMENT_CAPACITY;
   subtype Display_Column is Positive range 1 .. MAX_DOCUMENT_CAPACITY + 1;

   type Document (Capacity : Document_Capacity) is private;
   type Edit_Result is (Applied, Capacity_Exceeded);

   procedure Initialize
     (Value : out Document; Text : String; Result : out Edit_Result);
   function Length (Value : Document) return Natural;
   function Content (Value : Document) return String;

   procedure Insert
     (Value : in out Document; Position : Document_Position;
      Text : String; Result : out Edit_Result)
   with Pre => Position <= Length (Value) + 1;

   procedure Delete
     (Value : in out Document; Position : Document_Position; Count : Natural)
   with Pre =>
     Position <= Length (Value) + 1 and then
     Count <= Length (Value) - Position + 1;

   function Line_Count (Value : Document) return Positive;
   function Line_Length
     (Value : Document; Line : Positive) return Line_Character_Count
   with Pre => Line <= Line_Count (Value);

   procedure Position_To_Line_Column
     (Value : Document; Position : Document_Position;
      Line, Column : out Positive)
   with Pre => Position <= Length (Value) + 1;

   function Line_Column_To_Position
     (Value : Document; Line, Column : Positive) return Document_Position
   with Pre =>
     Line <= Line_Count (Value) and then
     Column <= Line_Character_Count'Succ (Line_Length (Value, Line));

   type Vertical_Direction is (Up, Down);
   procedure Move_Vertically
     (Value : Document; Position : Document_Position;
      Preferred_Column : Display_Column; Direction : Vertical_Direction;
      Result : out Document_Position)
   with Pre => Position <= Length (Value) + 1;

private
   type Document (Capacity : Document_Capacity) is record
      Data : String (1 .. Capacity) := [others => ' '];
      Last : Natural := 0;
   end record
   with Type_Invariant => Document.Last <= Document.Capacity;
end CuBit.UI.Editor.Documents;
