package body TextIO is
   procedure Print (Value : String) is
   begin
      Ada.Strings.Unbounded.Append (Captured, Value);
   end Print;
   procedure Print (Value : Integer) is
   begin
      Print (Value'Image);
   end Print;
   procedure Println (Value : String) is
   begin
      Print (Value & ASCII.LF);
   end Println;
end TextIO;
