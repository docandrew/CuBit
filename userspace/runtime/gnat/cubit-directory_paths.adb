package body CuBit.Directory_Paths with SPARK_Mode => On is
   function Value (Item : Path) return String is
     (Item.Data (1 .. Item.Length));

   procedure Set_Root
     (Name : String; Item : out Path; Success : out Boolean)
   is
   begin
      Item := (others => <>);
      Success := Name'Length <= Maximum_Bytes;
      if Success then
         Item.Length := Name'Length;
         Item.Data (1 .. Item.Length) := Name;
      end if;
   end Set_Root;

   procedure Append_Child
     (Parent : Path; Name : String; Result : out Path;
      Success : out Boolean)
   is
      Separator : Natural range 0 .. 1 := 0;
   begin
      Result := Parent;
      Success := False;
      if not Valid_Child_Name (Name) then
         return;
      end if;
      if Parent.Length > 0 and then Parent.Data (Parent.Length) /= '/' then
         Separator := 1;
      end if;
      if Name'Length > Maximum_Bytes - Parent.Length - Separator then
         return;
      end if;
      if Separator = 1 then
         Result.Length := Result.Length + 1;
         Result.Data (Result.Length) := '/';
      end if;
      Result.Data (Result.Length + 1 .. Result.Length + Name'Length) := Name;
      Result.Length := Result.Length + Name'Length;
      Success := True;
   end Append_Child;
end CuBit.Directory_Paths;
