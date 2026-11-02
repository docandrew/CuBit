package body CuBit.Appearance with SPARK_Mode is
   function To_Image_Name (Text : String) return Image_Name is
      Result : Image_Name := Default_Image;
   begin
      Result.Length := Text'Length;
      Result.Text (1 .. Text'Length) := Text;
      return Result;
   end To_Image_Name;
end CuBit.Appearance;
