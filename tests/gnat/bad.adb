--  An Ada compile error (tests/gnat): gnat1's diagnostic must come back
--  through the gcc driver, which exits with 1.
procedure Bad is
   Count : Integer := "not a number";
begin
   null;
end Bad;
