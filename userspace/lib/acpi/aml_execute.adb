pragma Ada_2022;
package body AML_Execute with SPARK_Mode is
   use AML_Decode;
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;
   function Run
     (Code : Bytes; Width : Integer_Width;
      Args : Arguments; Argument_Count : Natural; Budget : Natural)
      return Execution_Result
   is
      subtype Failure_Status is Execution_Status range No_Return .. Budget_Exceeded;
      function Failure (Status : Failure_Status; Charged : Natural)
         return Execution_Result is ((Status => Status, Charged => Charged));
      type Local_Array is array (Natural range 0 .. 7) of Integer_Value;
      Locals : Local_Array := [others => 0];
      Ready : array (Natural range 0 .. 7) of Boolean := [others => False];
      Offset : Natural := 0;
      Charged : Natural := 0;
      Op : Byte;
      Value : Integer_Value;
      State : Execution_Status;
      function Normalize (V : Integer_Value) return Integer_Value is
        (if Width = Bits_32 then V and 16#FFFF_FFFF# else V);
      procedure Operand (V : out Integer_Value; S : out Execution_Status)
        with Pre => Offset <= Code'Length and then Charged <= Budget,
             Post => Offset <= Code'Length and then Charged <= Budget
               and then Charged >= Charged'Old and then Offset >= Offset'Old
      is
         B : Byte;
         Literal : Integer_Result;
      begin
         V := 0;
         S := Returned;
         if Charged = Budget then S := Budget_Exceeded; return; end if;
         Charged := Charged + 1;
         if Offset = Code'Length then S := Truncated; return; end if;
         B := Code (Code'First + Offset);
         if B in 16#60# .. 16#67# then
            Offset := Offset + 1;
            if not Ready (Natural (B - 16#60#)) then
               S := Uninitialized; return;
            end if;
            V := Locals (Natural (B - 16#60#));
         elsif B in 16#68# .. 16#6E# then
            Offset := Offset + 1;
            if Natural (B - 16#68#) >= Argument_Count then
               S := Missing_Argument; return;
            end if;
            V := Normalize (Args (Natural (B - 16#68#)));
         else
            Literal := Read_Integer (Code (Code'First + Offset .. Code'Last), Width);
            if Literal.Kind /= Accepted then
               S := (if Literal.Kind = AML_Decode.Truncated then
                       AML_Execute.Truncated else AML_Execute.Unsupported);
               return;
            end if;
            V := Literal.Value;
            Offset := Offset + Literal.Consumed;
         end if;
      end Operand;
   begin
      while Offset < Code'Length loop
         pragma Loop_Invariant (Offset <= Code'Length and then Charged <= Budget);
         pragma Loop_Variant (Decreases => Budget - Charged);
         if Charged = Budget then return Failure (Budget_Exceeded, Charged); end if;
         Charged := Charged + 1;
         Op := Code (Code'First + Offset);
         Offset := Offset + 1;
         case Op is
            when 16#A3# => null; --  Noop
            when 16#A4# | 16#70# => --  Return / Store
               Operand (Value, State);
               if State /= Returned then return Failure (State, Charged); end if;
               if Op = 16#A4# then
                  return (Status => Returned, Charged => Charged, Value => Value);
               end if;
               if Offset = Code'Length then return Failure (Truncated, Charged); end if;
               Op := Code (Code'First + Offset);
               if Op not in 16#60# .. 16#67# then
                  return Failure (Unsupported, Charged);
               end if;
               Offset := Offset + 1;
               Locals (Natural (Op - 16#60#)) := Value;
               Ready (Natural (Op - 16#60#)) := True;
            when others => return Failure (Unsupported, Charged);
         end case;
      end loop;
      return Failure (No_Return, Charged);
   end Run;
end AML_Execute;
