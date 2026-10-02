with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
procedure Integer_Oracle_Tests is
   use type Integer_Value;
   package Number_IO is new Ada.Text_IO.Modular_IO (Integer_Value);
   package Natural_IO is new Ada.Text_IO.Integer_IO (Natural);
   File : Ada.Text_IO.File_Type;
   Width, Op : Natural;
   Left, Right, Expected : Integer_Value;
   Result : Execution_Result;
   Checks : Natural := 0;
begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "../tests/aml-core/build/integer-oracle.txt");
   while not Ada.Text_IO.End_Of_File (File) loop
      Natural_IO.Get (File, Width);
      Natural_IO.Get (File, Op);
      Number_IO.Get (File, Left);
      Number_IO.Get (File, Right);
      Number_IO.Get (File, Expected);
      Result := Run ((if Op in 16#80# .. 16#82# then [16#A4#,Byte (Op),16#68#,0]
                      elsif Op = 16#78# then [16#A4#,16#78#,16#68#,16#69#,0,0]
                      else [16#A4#,Byte (Op),16#68#,16#69#,0]),
        (if Width = 32 then Bits_32 else Bits_64),
        [0 => Left, 1 => Right, others => 0], 2, 4);
      Checks := Checks + 1;
      if Result.Status /= Returned or else Result.Value /= Expected then
         raise Program_Error with "oracle mismatch" & Checks'Image;
      end if;
   end loop;
   Ada.Text_IO.Close (File);
   if Checks /= 7812 then raise Program_Error with "incomplete oracle"; end if;
   for W in Integer_Width loop
      for Scan_Op in Byte range 16#81# .. 16#82# loop
         Result := Run ([16#A4#, Scan_Op], W, [others => 0], 0, 4);
         if Result.Status /= Truncated then raise Program_Error with "missing scan operand"; end if;
         Checks := Checks + 1;
         Result := Run ([16#A4#, Scan_Op, 0], W, [others => 0], 0, 4);
         if Result.Status /= Truncated then raise Program_Error with "missing scan target"; end if;
         Checks := Checks + 1;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("AML-INTEGER-ORACLE: PASS" & Checks'Image);
end Integer_Oracle_Tests;
