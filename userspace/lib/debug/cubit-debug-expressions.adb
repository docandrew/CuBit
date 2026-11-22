------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Debug.Expressions with SPARK_Mode is
   package Cfi renames CuBit.Debug.Cfi;
   use type B.Offset;

   OP_Addr        : constant := 16#03#;
   OP_Deref       : constant := 16#06#;
   OP_Const1u     : constant := 16#08#;
   OP_Const1s     : constant := 16#09#;
   OP_Const2u     : constant := 16#0A#;
   OP_Const2s     : constant := 16#0B#;
   OP_Const4u     : constant := 16#0C#;
   OP_Const4s     : constant := 16#0D#;
   OP_Const8u     : constant := 16#0E#;
   OP_Const8s     : constant := 16#0F#;
   OP_Constu      : constant := 16#10#;
   OP_Consts      : constant := 16#11#;
   OP_Dup         : constant := 16#12#;
   OP_Drop        : constant := 16#13#;
   OP_Over        : constant := 16#14#;
   OP_Pick        : constant := 16#15#;
   OP_Swap        : constant := 16#16#;
   OP_Rot         : constant := 16#17#;
   OP_Abs         : constant := 16#19#;
   OP_And         : constant := 16#1A#;
   OP_Div         : constant := 16#1B#;
   OP_Minus       : constant := 16#1C#;
   OP_Mod         : constant := 16#1D#;
   OP_Mul         : constant := 16#1E#;
   OP_Neg         : constant := 16#1F#;
   OP_Not         : constant := 16#20#;
   OP_Or          : constant := 16#21#;
   OP_Plus        : constant := 16#22#;
   OP_Plus_Uconst : constant := 16#23#;
   OP_Shl         : constant := 16#24#;
   OP_Shr         : constant := 16#25#;
   OP_Shra        : constant := 16#26#;
   OP_Xor         : constant := 16#27#;
   OP_Bra         : constant := 16#28#;
   OP_Eq          : constant := 16#29#;
   OP_Ge          : constant := 16#2A#;
   OP_Gt          : constant := 16#2B#;
   OP_Le          : constant := 16#2C#;
   OP_Lt          : constant := 16#2D#;
   OP_Ne          : constant := 16#2E#;
   OP_Skip        : constant := 16#2F#;
   OP_Lit0        : constant := 16#30#;
   OP_Lit31       : constant := 16#4F#;
   OP_Reg0        : constant := 16#50#;
   OP_Reg31       : constant := 16#6F#;
   OP_Breg0       : constant := 16#70#;
   OP_Breg31      : constant := 16#8F#;
   OP_Regx        : constant := 16#90#;
   OP_Fbreg       : constant := 16#91#;
   OP_Bregx       : constant := 16#92#;
   OP_Piece       : constant := 16#93#;
   OP_Deref_Size  : constant := 16#94#;
   OP_Nop         : constant := 16#96#;
   OP_Push_Object_Address : constant := 16#97#;
   OP_Call_Frame_CFA : constant := 16#9C#;
   OP_Implicit_Value : constant := 16#9E#;
   OP_Stack_Value : constant := 16#9F#;
   OP_Entry_Value : constant := 16#A3#;
   OP_GNU_Entry_Value : constant := 16#F3#;
   Shift_Ceiling  : constant := 64;
   Word_Bytes     : constant := 8;

   procedure Evaluate
     (Data : B.Byte_Buffer; Expr_At, Expr_Size : B.Offset; Ctx : Context;
      Read : not null access procedure
        (Address : Unsigned_64; Size : Positive; Value : out Unsigned_64; Ok : out Boolean);
      As_Value : Boolean;
      R : out Result;
      Entry_Value : access procedure
        (Register : CuBit.Debug.Cfi.Register; Value : out Unsigned_64; Ok : out Boolean) := null)
   is
      subtype Depth is Natural range 0 .. Stack_Ceiling;
      Stack : array (1 .. Stack_Ceiling) of Unsigned_64 := [others => 0];
      Top : Depth := 0;
      C : B.Cursor := B.Make (Expr_At, Expr_At + Expr_Size);
      Op, V, W : Unsigned_64;
      S : Integer_64;
      Ok : Boolean := True;
      Is_Value : Boolean := False;
      In_Register : Boolean := False;
      Reg : Cfi.Register := 0;

      procedure Push (X : Unsigned_64) is
      begin
         if Top = Stack_Ceiling then
            Ok := False;
         else
            Top := Top + 1;
            Stack (Top) := X;
         end if;
      end Push;
      procedure Pop (X : out Unsigned_64) is
      begin
         if Top = 0 then
            Ok := False;
            X := 0;
         else
            X := Stack (Top);
            Top := Top - 1;
         end if;
      end Pop;
      procedure Reg_Value (N : Unsigned_64; Value : out Unsigned_64; Known : out Boolean) is
      begin
         Value := 0;
         Known := N < Cfi.Register_Columns and then Ctx.Regs.Known (Cfi.Register (N));
         if Known then
            Value := Ctx.Regs.Value (Cfi.Register (N));
         end if;
      end Reg_Value;
      procedure Fail (Kind : Result_Kind) is
      begin
         R := (Kind => Kind, others => <>);
      end Fail;
      function Signed (X : Unsigned_64) return Integer_64 renames B.To_Signed;
   begin
      R := (others => <>);
      if Expr_Size = 0 then
         Fail (Optimized_Out);
         return;
      end if;
      for Fuel in 1 .. Fuel_Ceiling loop
         exit when not Ok or else not C.Ok or else C.At_Byte >= C.Limit;
         B.U8 (Data, C, Op);
         if In_Register and then Op /= OP_Piece then
            Fail (Unsupported);   --  a register location must end the piece
            return;
         end if;
         case Op is
            when OP_Addr =>
               B.Sized (Data, C, Ctx.Address_Size, V);
               Push (V);
            when OP_Deref =>
               Pop (V);
               Read (V, Word_Bytes, W, Ok);
               Push (W);
            when OP_Deref_Size =>
               B.U8 (Data, C, W);
               Pop (V);
               if W not in 1 .. Word_Bytes then
                  Ok := False;
               else
                  Read (V, Positive (W), V, Ok);
                  Push (V);
               end if;
            when OP_Const1u => B.U8 (Data, C, V); Push (V);
            when OP_Const1s => B.U8 (Data, C, V); Push (B.To_Unsigned (B.Sign_Extend (V, 8)));
            when OP_Const2u => B.U16 (Data, C, V); Push (V);
            when OP_Const2s => B.U16 (Data, C, V); Push (B.To_Unsigned (B.Sign_Extend (V, 16)));
            when OP_Const4u => B.U32 (Data, C, V); Push (V);
            when OP_Const4s => B.U32 (Data, C, V); Push (B.To_Unsigned (B.Sign_Extend (V, 32)));
            when OP_Const8u | OP_Const8s => B.U64 (Data, C, V); Push (V);
            when OP_Constu => B.ULEB (Data, C, V); Push (V);
            when OP_Consts => B.SLEB (Data, C, S); Push (B.To_Unsigned (S));
            when OP_Dup =>
               if Top = 0 then Ok := False; else Push (Stack (Top)); end if;
            when OP_Drop => Pop (V);
            when OP_Over =>
               if Top < 2 then Ok := False; else Push (Stack (Top - 1)); end if;
            when OP_Pick =>
               B.U8 (Data, C, V);
               if V >= Unsigned_64 (Top) then Ok := False;
               else Push (Stack (Top - Natural (V))); end if;
            when OP_Swap =>
               if Top < 2 then Ok := False;
               else
                  V := Stack (Top);
                  Stack (Top) := Stack (Top - 1);
                  Stack (Top - 1) := V;
               end if;
            when OP_Rot =>
               if Top < 3 then Ok := False;
               else
                  V := Stack (Top);
                  Stack (Top) := Stack (Top - 1);
                  Stack (Top - 1) := Stack (Top - 2);
                  Stack (Top - 2) := V;
               end if;
            when OP_Abs =>
               Pop (V);
               Push (if Signed (V) < 0 and then Signed (V) /= Integer_64'First
                     then B.To_Unsigned (-Signed (V)) else V);
            when OP_Neg =>
               Pop (V);
               Push (not V + 1);
            when OP_Not =>
               Pop (V);
               Push (not V);
            when OP_And | OP_Div | OP_Minus | OP_Mod | OP_Mul | OP_Or | OP_Plus
               | OP_Shl | OP_Shr | OP_Shra | OP_Xor
               | OP_Eq | OP_Ge | OP_Gt | OP_Le | OP_Lt | OP_Ne =>
               Pop (W);   --  top
               Pop (V);   --  second
               if Ok then
                  case Op is
                     when OP_And => Push (V and W);
                     when OP_Or => Push (V or W);
                     when OP_Xor => Push (V xor W);
                     when OP_Plus => Push (V + W);
                     when OP_Minus => Push (V - W);
                     when OP_Mul => Push (V * W);
                     when OP_Div =>
                        declare
                           SV : constant Integer_64 := Signed (V);
                           SW : constant Integer_64 := Signed (W);
                        begin
                           if SW = 0 or else (SV = Integer_64'First and then SW = -1) then
                              Ok := False;
                           else
                              Push (B.To_Unsigned (SV / SW));
                           end if;
                        end;
                     when OP_Mod =>
                        if W = 0 then Ok := False; else Push (V mod W); end if;
                     when OP_Shl => Push (if W >= Shift_Ceiling then 0 else Shift_Left (V, Natural (W)));
                     when OP_Shr => Push (if W >= Shift_Ceiling then 0 else Shift_Right (V, Natural (W)));
                     when OP_Shra =>
                        Push (if W >= Shift_Ceiling then (if Signed (V) < 0 then not 0 else 0)
                              else Shift_Right_Arithmetic (V, Natural (W)));
                     when OP_Eq => Push (if V = W then 1 else 0);
                     when OP_Ne => Push (if V /= W then 1 else 0);
                     when OP_Ge => Push (if Signed (V) >= Signed (W) then 1 else 0);
                     when OP_Gt => Push (if Signed (V) > Signed (W) then 1 else 0);
                     when OP_Le => Push (if Signed (V) <= Signed (W) then 1 else 0);
                     when OP_Lt => Push (if Signed (V) < Signed (W) then 1 else 0);
                     when others => Ok := False;
                  end case;
               end if;
            when OP_Plus_Uconst =>
               B.ULEB (Data, C, W);
               Pop (V);
               Push (V + W);
            when OP_Skip | OP_Bra =>
               B.U16 (Data, C, V);
               S := B.Sign_Extend (V, 16);
               declare
                  Taken : Boolean := True;
               begin
                  if Op = OP_Bra then
                     Pop (W);
                     Taken := W /= 0;
                  end if;
                  if Taken then
                     declare
                        --  Address arithmetic wraps, as DWARF's does.
                        Target : constant Unsigned_64 := C.At_Byte + B.To_Unsigned (S);
                     begin
                        if Target < Expr_At or else Target - Expr_At > Expr_Size then
                           Ok := False;
                        else
                           C.At_Byte := Target;
                        end if;
                     end;
                  end if;
               end;
            when OP_Lit0 .. OP_Lit31 =>
               Push (Op - OP_Lit0);
            when OP_Reg0 .. OP_Reg31 | OP_Regx =>
               if Op = OP_Regx then
                  B.ULEB (Data, C, V);
               else
                  V := Op - OP_Reg0;
               end if;
               if V >= Cfi.Register_Columns then
                  Fail (Unsupported);   --  a vector or other register
                  return;
               end if;
               In_Register := True;
               Reg := Cfi.Register (V);
            when OP_Breg0 .. OP_Breg31 | OP_Bregx =>
               if Op = OP_Bregx then
                  B.ULEB (Data, C, V);
               else
                  V := Op - OP_Breg0;
               end if;
               B.SLEB (Data, C, S);
               declare
                  Known : Boolean;
               begin
                  Reg_Value (V, W, Known);
                  if not Known then
                     Fail (Failed);
                     return;
                  end if;
               end;
               Push (W + B.To_Unsigned (S));
            when OP_Fbreg =>
               B.SLEB (Data, C, S);
               if not Ctx.Has_Frame_Base then
                  Fail (Failed);
                  return;
               end if;
               Push (Ctx.Frame_Base + B.To_Unsigned (S));
            when OP_Call_Frame_CFA =>
               if not Ctx.Has_CFA then
                  Fail (Failed);
                  return;
               end if;
               Push (Ctx.CFA);
            when OP_Push_Object_Address =>
               if not Ctx.Has_Object then
                  Fail (Failed);
                  return;
               end if;
               Push (Ctx.Object_Address);
            when OP_Nop => null;
            when OP_Stack_Value =>
               Is_Value := True;
               exit;
            when OP_Implicit_Value =>
               B.ULEB (Data, C, V);
               if V > Word_Bytes then
                  Fail (Unsupported);
                  return;
               end if;
               B.Sized (Data, C, Natural (V), W);
               R := (Kind => Implicit_Value, Value => W, Has_Value => C.Ok, others => <>);
               return;
            when OP_Entry_Value | OP_GNU_Entry_Value =>
               --  The value a register had on entry, when the sub-expression is
               --  just that register: the call site in the caller can give it.
               B.ULEB (Data, C, V);
               B.U8 (Data, C, W);
               if V = 1 and then W in OP_Reg0 .. OP_Reg31
                 and then W - OP_Reg0 < Cfi.Register_Columns
               then
                  declare
                     Entry_Ok : Boolean := False;
                  begin
                     if Entry_Value /= null then
                        Entry_Value (Cfi.Register (W - OP_Reg0), V, Entry_Ok);
                     end if;
                     if Entry_Ok then
                        Push (V);
                     else
                        R := (Kind => Expressions.Entry_Value, Register => Cfi.Register (W - OP_Reg0),
                              others => <>);
                        return;
                     end if;
                  end;
               else
                  R := (Kind => Unsupported, others => <>);
                  return;
               end if;
            when OP_Piece =>
               B.ULEB (Data, C, V);
               --  The first piece only: report how many bytes it covers.
               if In_Register then
                  R := (Kind => Register_Location, Register => Reg, Piece_Bytes => V, others => <>);
                  R.Has_Value := Ctx.Regs.Known (Reg);
                  R.Value := Ctx.Regs.Value (Reg);
               elsif Top > 0 then
                  R := (Kind => Memory_Location, Address => Stack (Top), Piece_Bytes => V, others => <>);
               else
                  R := (Kind => Optimized_Out, Piece_Bytes => V, others => <>);
               end if;
               if not C.Ok then
                  Fail (Failed);
               end if;
               return;
            when others =>
               Fail (Unsupported);
               return;
         end case;
      end loop;
      if not Ok or else not C.Ok then
         Fail (Failed);
         return;
      end if;
      if In_Register then
         R := (Kind => Register_Location, Register => Reg,
               Has_Value => Ctx.Regs.Known (Reg), Value => Ctx.Regs.Value (Reg), others => <>);
      elsif Top = 0 then
         Fail (Failed);
      elsif Is_Value or else As_Value then
         R := (Kind => Implicit_Value, Value => Stack (Top), Has_Value => True, others => <>);
      else
         R := (Kind => Memory_Location, Address => Stack (Top), others => <>);
      end if;
   end Evaluate;
end CuBit.Debug.Expressions;
