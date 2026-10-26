--  parameters-check (tests/program-parameters, headless test
--  program-parameters): reads its typed parameters and logs exactly what
--  it decoded. A launch without a valid value never gets here: procmgr
--  refuses it, and the generated Startup body stops the program first.
with CuBit.Messages;
with Parameters;
with Startup;
pragma Unreferenced (Startup);

procedure Main is
   use Parameters;
   P : constant Check_Parameters_Value := Value;
   function Image (N : Long_Long_Integer) return String is
      Text : constant String := Long_Long_Integer'Image (N);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;
begin
   CuBit.Messages.debugPrint
     ("parameters-check: decoded count=" & Image (Long_Long_Integer (P.count)) & " deadline=" &
      (case P.deadline.Choice is
          when Within => "Within " & Image (Long_Long_Integer (P.deadline.Within_Payload)),
          when Forever => "Forever") &
      " verbose=" & (if P.verbose then "true" else "false") & ASCII.LF);
end Main;
