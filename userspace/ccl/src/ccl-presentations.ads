with CCL.Image_Store;
with CCL.Language;
with CCL.Literal_Tables;
with CCL.Types.Shapes;

--  How a result is shown, decided once for every front end: the native
--  console, the Workbench and the Observatory (over Control_Wire) all
--  render this description, so they present a value the same way.
--  - Failure: the diagnostic (CCL.Sessions.Result_Value_Image).
--  - Text: the value as text (its type from Result_Type_Image).
--  - Table: a record, or a list of them; the cells of its literal under
--    its row shape.
--  - Picture: an Image (interfaces/image.schema), by its stored pixels.
--  - Gallery: a list of Images, side by side.
package CCL.Presentations is
   type Form is (Failure, Text, Table, Picture, Gallery);
   --  A gallery shows at most the rows a table does.
   Maximum_Gallery : constant := CCL.Literal_Tables.Maximum_Rows;
   subtype Gallery_Count is Natural range 0 .. Maximum_Gallery;
   type Picture_Entry is record
      Image : CCL.Image_Store.Image_Id := CCL.Image_Store.No_Image;
      Width, Height : Natural := 0;
   end record;
   type Picture_Array is array (1 .. Maximum_Gallery) of Picture_Entry;
   type Presentation is record
      Kind : Form := Failure;
      Shape : CCL.Types.Shapes.Row_Shape;
      Cells : CCL.Literal_Tables.Table;
      Image : CCL.Image_Store.Image_Id := CCL.Image_Store.No_Image;
      Image_Width, Image_Height : Natural := 0;
      --  Gallery: its pictures, in order; Cells.Total counts them all.
      Pictures : Picture_Array := [others => (others => <>)];
      Picture_Count : Gallery_Count := 0;
   end record;
   procedure Describe
     (Outcome : CCL.Language.Interpretation_Result; Result : out Presentation);
   --  A cell's text in the result's literal.
   function Cell_Text
     (Outcome : CCL.Language.Interpretation_Result; Item : Presentation;
      Row : CCL.Literal_Tables.Row_Index; Column : CCL.Types.Component_Index) return String;
end CCL.Presentations;
