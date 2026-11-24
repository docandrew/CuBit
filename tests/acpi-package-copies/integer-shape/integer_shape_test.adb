with Ada.Text_IO;
with AML_Objects.Integer_Shape_Probe;
procedure Integer_Shape_Test is
begin
   if AML_Objects.Integer_Shape_Probe.Malformed_Integer_Accepted then
      raise Program_Error with "strengthened model accepted malformed integer";
   end if;
   Ada.Text_IO.Put_Line ("STRENGTHENED MODEL REJECTS INTEGER SIZE=1: PASS");
end Integer_Shape_Test;
