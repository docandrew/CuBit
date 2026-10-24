package body IPv6_Link_Proof with SPARK_Mode is
   procedure Send (Link_Owner : Owner; Frame : IPv6_Header.Bytes) is
      pragma Unreferenced (Link_Owner, Frame);
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
