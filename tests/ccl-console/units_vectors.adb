--  Golden vectors for the browser's units.js: CCL.Units's own outputs.
with Ada.Text_IO; use Ada.Text_IO;
with CCL.Units;

procedure Units_Vectors is
   package U renames CCL.Units;
   type Sample is record
      Type_Name : access constant String;
      Raw : access constant String;
   end record;
   function S (Text : String) return access constant String is (new String'(Text));
   Samples : constant array (Positive range <>) of Sample :=
     [(S ("Bytes"), S ("0")), (S ("Bytes"), S ("1023")), (S ("Bytes"), S ("1024")),
      (S ("Bytes"), S ("1536")), (S ("Bytes"), S ("1048575")), (S ("Bytes"), S ("1073741824")),
      (S ("Bytes"), S ("9223372036854775807")), (S ("Bytes"), S ("-4")), (S ("Bytes"), S ("12x")),
      (S ("Timestamp"), S ("0")), (S ("Timestamp"), S ("1")), (S ("Timestamp"), S ("951782400000")),
      (S ("Timestamp"), S ("1790964695000")), (S ("Timestamp"), S ("4102444800000")),
      (S ("UNIX_File_Permissions"), S ("0")), (S ("UNIX_File_Permissions"), S ("16877")),
      (S ("UNIX_File_Permissions"), S ("33188")), (S ("UNIX_File_Permissions"), S ("41471")),
      (S ("UNIX_File_Permissions"), S ("511")),
      (S ("Milliseconds"), S ("0")), (S ("Milliseconds"), S ("999")), (S ("Milliseconds"), S ("1450")),
      (S ("Milliseconds"), S ("59999")), (S ("Milliseconds"), S ("125000")),
      (S ("Integer"), S ("1536"))];
begin
   Put_Line ("[");
   for I in Samples'Range loop
      Put_Line ("  {""type"": """ & Samples (I).Type_Name.all & """, ""raw"": """ & Samples (I).Raw.all &
                """, ""shown"": """ & U.Humanize (U.Unit_Of (Samples (I).Type_Name.all), Samples (I).Raw.all) &
                """}" & (if I = Samples'Last then "" else ","));
   end loop;
   Put_Line ("]");
end Units_Vectors;
