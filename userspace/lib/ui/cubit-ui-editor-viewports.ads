package CuBit.UI.Editor.Viewports with SPARK_Mode is
   subtype Line_Number is Positive;

   type Viewport is private;

   procedure Initialize
     (Value : out Viewport; Visible_Lines : Positive);
   function First_Line (Value : Viewport) return Line_Number;
   function Line_Capacity (Value : Viewport) return Positive;
   function Last_Visible_Line
     (Value : Viewport; Document_Lines : Positive) return Line_Number;

   procedure Ensure_Visible
     (Value : in out Viewport; Line, Document_Lines : Line_Number)
   with Pre => Line <= Document_Lines;

   procedure Scroll_Lines
     (Value : in out Viewport; Amount : Integer;
      Document_Lines : Positive);

private
   type Viewport is record
      First : Line_Number := 1;
      Visible : Positive := 1;
   end record;
end CuBit.UI.Editor.Viewports;
