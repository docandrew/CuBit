package body IPv6_Link_Proof with SPARK_Mode is
   procedure Send (Frame : IPv6_Header.Bytes) is
   begin
      if Frames < Natural'Last then
         Frames := Frames + 1;
      end if;
   end Send;
   procedure Log (Text : String) is
      pragma Unreferenced (Text);
   begin
      if Lines < Natural'Last then
         Lines := Lines + 1;
      end if;
   end Log;
end IPv6_Link_Proof;
