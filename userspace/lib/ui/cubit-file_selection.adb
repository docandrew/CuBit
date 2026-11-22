package body CuBit.File_Selection with SPARK_Mode is
   function Valid_Leaf (Name : String) return Boolean is
     (Name'Length in 1 .. Maximum_Name_Length and then
      Name (Name'First) in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' and then
      Name (Name'Last) /= ' ' and then
      (for all C of Name => C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' |
         '_' | '-' | '.' | ' '));

   procedure Set (Name : out File_Name; Text : String; Accepted : out Boolean) is
   begin
      Name := (others => <>);
      Accepted := Valid_Leaf (Text);
      if Accepted then
         Name.Length := Text'Length;
         Name.Text (1 .. Name.Length) := Text;
      end if;
   end Set;

   procedure Append
     (Files : in out File_List; Name : String; Accepted : out Boolean)
   is
      Item : File_Name;
   begin
      Set (Item, Name, Accepted);
      if Accepted then
         if Files.Count = Maximum_Files then
            Accepted := False;
         else
            Files.Count := Files.Count + 1;
            Files.Names (Files.Count) := Item;
         end if;
      end if;
   end Append;
end CuBit.File_Selection;
