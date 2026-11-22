package body Log_Script is
   procedure Append (Text : String) is
   begin
      Lines := Lines & Text & ASCII.LF;
      Count := Count + 1;
   end Append;
end Log_Script;
