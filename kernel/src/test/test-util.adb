-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Embedded Test Suite for CuBit
-------------------------------------------------------------------------------
-- with Ada.Assertions; use Ada.Assertions;
with Interfaces;

with Util;

package body Test.Util is

    procedure test_setBit_Unsigned_8 is
        u8 : Unsigned_8 := 0;
    begin
        Util.setBit(u8, 0);
        assert (u8 = 1);

        Util.setBit(u8, 7);
        assert (u8 = 129);
    end test_setBit_Unsigned_8;

    procedure runTests is
    begin
        test_setBit_Unsigned_8;
        test_setBit_Unsigned_16;
        test_setBit_Unsigned_32;
        test_setBit_Unsigned_64;
    end runTests;

end Test.Util;