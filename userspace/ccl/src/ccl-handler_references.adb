package body CCL.Handler_References with SPARK_Mode is
   procedure Create (Source, Name : String; Item : out Reference; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := Source'Length in 1 .. Maximum_Source and Name'Length in 1 .. Maximum_Name;
      if Success then
         Item.Source_Length := Source'Length;
         Item.Name_Length := Name'Length;
         Item.Text (1 .. Source'Length) := Source;
         Item.Entry_Name (1 .. Name'Length) := Name;
      end if;
   end Create;
end CCL.Handler_References;
