package body CCL_Application is
   Text : String (1 .. Maximum_Name) := "ccl" & [4 .. Maximum_Name => ' '];
   Length : Natural range 0 .. Maximum_Name := 3;

   procedure Set_Name (Name : String) is
   begin
      Length := Natural'Min (Name'Length, Maximum_Name);
      Text (1 .. Length) := Name (Name'First .. Name'First + Length - 1);
   end Set_Name;

   function Name return String is (Text (1 .. Length));
end CCL_Application;
