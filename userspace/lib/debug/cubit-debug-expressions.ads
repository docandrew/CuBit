------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The DWARF location-expression evaluator (docs/debugging.md 6.6): a small
--  stack machine over DWARF's operations, bounded by fuel and a stack
--  ceiling. Its only effect is reads through Read; a bad expression can only
--  give a wrong or refused location, never a memory error in the engine.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Debug.Bytes;
with CuBit.Debug.Cfi;

package CuBit.Debug.Expressions with SPARK_Mode is
   package B renames CuBit.Debug.Bytes;

   Stack_Ceiling : constant := 64;
   Fuel_Ceiling  : constant := 1_024;

   type Result_Kind is
     (Memory_Location,     --  the object is at Address
      Register_Location,   --  the object is in Register (its value: Value, Has_Value)
      Implicit_Value,      --  the object's value is Value (DW_OP_stack_value, implicit_value)
      Entry_Value,         --  the value Register had when the function was entered
      Optimized_Out,       --  an empty expression
      Unsupported,         --  an operation the preview does not evaluate
      Failed);             --  malformed, out of fuel, or a needed input unknown

   type Result is record
      Kind     : Result_Kind := Failed;
      Address  : Unsigned_64 := 0;
      Register : CuBit.Debug.Cfi.Register := 0;
      Value    : Unsigned_64 := 0;
      Has_Value : Boolean := False;
      Piece_Bytes : Unsigned_64 := 0;   --  0: the whole object
   end record;

   type Context is record
      Regs           : CuBit.Debug.Cfi.Register_Set;
      Frame_Base     : Unsigned_64 := 0;
      Has_Frame_Base : Boolean := False;
      CFA            : Unsigned_64 := 0;
      Has_CFA        : Boolean := False;
      Object_Address : Unsigned_64 := 0;
      Has_Object     : Boolean := False;
      Address_Size   : Positive := 8;
   end record;

   --  Evaluate Data (Expr_At .. Expr_At + Expr_Size - 1).
   --  As_Value: the expression computes a value, not a location (a bound, a
   --  byte size): the result is Implicit_Value with the top of the stack.
   procedure Evaluate
     (Data : B.Byte_Buffer; Expr_At, Expr_Size : B.Offset; Ctx : Context;
      Read : not null access procedure
        (Address : Unsigned_64; Size : Positive; Value : out Unsigned_64; Ok : out Boolean);
      As_Value : Boolean;
      R : out Result;
      --  The value a register had on entry to the frame's function (its
      --  caller's call-site parameter), or Ok False; null: not resolved.
      Entry_Value : access procedure
        (Register : CuBit.Debug.Cfi.Register; Value : out Unsigned_64; Ok : out Boolean) := null);
end CuBit.Debug.Expressions;
