------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Runtime binding numbers: one 16#1_0000#-wide block per interface a host
--  grants, so no two interfaces can share a number (CCL.Catalog.Install
--  also refuses a number another operation holds). wifi.scan once took
--  Install's block and native config-values took Log's; both silently
--  routed one interface's calls to the other.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CCL.Binding_Blocks with Pure, SPARK_Mode is

   type Block is
     (Host_Clock, Config, Log, Images, Console, Files, Processes, Programs,
      Lexicon, Calc, Install, Wifi, Config_Values);
   --  The representation is the block's number; the compiler refuses two
   --  blocks with one number.
   for Block use
     (Host_Clock => 1, Config => 4, Log => 5, Images => 6, Console => 7,
      Files => 8, Processes => 9, Programs => 10, Lexicon => 11, Calc => 12,
      Install => 13, Wifi => 14, Config_Values => 15);

   BLOCK_WIDTH : constant := 16#1_0000#;

   --  The block's lowest number (Programs numbers from here).
   function Base (Item : Block) return Unsigned_32 is
     (Unsigned_32 (Block'Enum_Rep (Item)) * BLOCK_WIDTH);

   --  The first operation's number in an interface numbered from 1.
   function First (Item : Block) return Unsigned_32 is (Base (Item) + 1);
end CCL.Binding_Blocks;
