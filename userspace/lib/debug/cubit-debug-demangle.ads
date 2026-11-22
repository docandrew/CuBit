------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Rust symbol names for display (DBG-001): the v0 scheme (_R...) and the
--  legacy one (_ZN...E with a hash), demangled to Rust paths.
--
--  @description
--  Symbols come from an untrusted debug file: the decoder is bounded by an
--  output ceiling, a nesting ceiling and a step budget, and a malformed
--  symbol gives Valid False, never an error. Crate disambiguators and
--  legacy hashes are dropped; generic arguments are kept.
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.Debug.Demangle with SPARK_Mode is

   Output_Ceiling  : constant := 1_024;
   Nesting_Ceiling : constant := 64;
   Step_Ceiling    : constant := 16_384;

   subtype Output_Length is Natural range 0 .. Output_Ceiling;
   type Output is record
      Text   : String (1 .. Output_Ceiling) := [others => ' '];
      Length : Output_Length := 0;
      Valid  : Boolean := False;
      Cut    : Boolean := False;   --  longer than Output_Ceiling: shortened
   end record;

   function Is_V0 (Symbol : String) return Boolean is
     (Symbol'Length > 2 and then Symbol (Symbol'First .. Symbol'First + 1) = "_R");
   function Is_Legacy (Symbol : String) return Boolean is
     (Symbol'Length > 4 and then Symbol (Symbol'First .. Symbol'First + 2) = "_ZN"
      and then Symbol (Symbol'Last) = 'E');

   --  The symbol as a Rust path (Valid False: not a Rust symbol this
   --  decoder reads; Result then holds nothing).
   procedure Rust (Symbol : String; Result : out Output)
   with Pre => Symbol'Last < Positive'Last;

end CuBit.Debug.Demangle;
