with Ada.Text_IO;
with Interfaces;
with AML_Decode;
with AML_Fatal;
procedure Fatal_Framing_Tests is
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;
   use type AML_Fatal.Framing_Status;
   use type AML_Fatal.Request;
   use type Interfaces.Unsigned_32;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with "Fatal framing check failed"; end if;
   end Check;
   Data : constant AML_Decode.Bytes (11 .. 18) :=
     [16#5B#, 16#32#, 16#A5#, 16#78#, 16#56#, 16#34#, 16#F2#, 16#00#];
   Parsed : constant AML_Fatal.Framing_Result := AML_Fatal.Read_Fixed (Data);
begin
   for Length in Natural range 0 .. AML_Fatal.Fixed_Bytes - 1 loop
      Check (AML_Fatal.Read_Fixed (Data (Data'First .. Data'First + Length - 1)).Status = AML_Fatal.Truncated);
   end loop;
   Check (Parsed.Status = AML_Fatal.Accepted);
   if Parsed.Status /= AML_Fatal.Accepted then return; end if;
   Check (Parsed.Item.Kind = 16#A5#);
   Check (Parsed.Item.Code = 16#F2345678#);
   Check (AML_Fatal.Read_Fixed (Data (11 .. 17)).Status = AML_Fatal.Accepted);
   -- Header acceptance alone does not assert the following TermArg exists.
   Check (AML_Fatal.Read_Fixed ([16#5B#, 16#33#]).Status = AML_Fatal.Invalid_Opcode);
   Check (AML_Fatal.Read_Fixed ([16#00#, 16#32#]).Status = AML_Fatal.Invalid_Opcode);
   for Width in AML_Decode.Integer_Width loop
      declare
         Request : constant AML_Fatal.Request :=
           (Literal => Parsed.Item, Evaluated_Argument => 16#FEDCBA98_76543210#, Width => Width);
         Result : constant AML_Fatal.Observation := (Present => True, Item => Request);
      begin
         Check (AML_Fatal.ACPICA_Host_Argument (Request) = 16#76543210#);
         Check (Result.Present);
         if Result.Present then
            Check (Result.Item = Request);
            Check (Result.Item.Evaluated_Argument = 16#FEDCBA98_76543210#);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Fatal framing checks" & Checks'Image);
end Fatal_Framing_Tests;
