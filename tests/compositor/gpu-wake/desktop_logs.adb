package body Desktop_Logs is
   procedure Write (Text : String) is
   begin
      Lines := Lines + 1;
      Last_Line := Ada.Strings.Unbounded.To_Unbounded_String (Text);
   end Write;
end Desktop_Logs;
