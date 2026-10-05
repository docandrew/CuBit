------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where each string of a launch block starts (CuBit.Libc_Start): argv,
--  then the environment, then the working directory.
--
--  @description
--  Proved (tests/libc-ada): every start lies in the block's string bytes
--  (never in the program description after them),
--  and no more strings are found than declared. That a well-formed block
--  yields exactly as many as it declares is tested, not proved (as for
--  CuBit.Launch_Arguments.Locate): the caller treats fewer as malformed.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Launch_Arguments; use CuBit.Launch_Arguments;

package CuBit.Libc_Start_Layout with Pure, SPARK_Mode is

   type Starts is array (1 .. Maximum_Strings) of Block_Index;

   --  First (K) is where string K starts (an index into Item).
   procedure Locate_Strings
     (Item : Block; First : out Starts; Count : out String_Count)
   with Pre => Item'First = 1 and then Well_Formed (Item),
        Post => Count <= Strings_Declared (Item)
                and then (for all K in 1 .. Count =>
                            First (K) in Header_Bytes + 1 .. Strings_Last (Item));

end CuBit.Libc_Start_Layout;
