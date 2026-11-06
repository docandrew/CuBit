------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What wifi.svc writes into a watcher's lent page (CuBit.Wifi_Control
--  Watch, UI-027's Wi-Fi item): the connection's state, the network it is
--  on, which scanned networks have a remembered passphrase, and the last
--  scan, in the driver's own scan-block layout (CuBit.Wifi_Scan).
--
--  @description
--  The page, in 32-bit little-endian words from its start:
--    0          the report's generation (never 0 once written)
--    1          state (CuBit.Wifi_Control.State, bits 0..7), the last
--               join's failure (its position, 8..15), Target_Valid (bit 16)
--    2          remembered: bit I - 1 set when scan entry I's SSID has a
--               stored passphrase
--    3 .. 15    the network joined or being joined (a Wifi_Scan entry;
--               all zero unless Target_Valid)
--    16 ..      the last scan, a whole Wifi_Scan block (its generation
--               word is the driver's scan generation)
--  No passphrase or key ever travels in it. Decode is total and accepts
--  exactly what Encode writes.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Wifi_Control;
with CuBit.Wifi_Scan;

package CuBit.Wifi_Report with Pure, SPARK_Mode is
   package C renames CuBit.Wifi_Control;
   package WS renames CuBit.Wifi_Scan;
   use type WS.BSS_Entry;

   type Remembered_Set is array (WS.Entry_Index) of Boolean;
   Nothing_Remembered : constant Remembered_Set := [others => False];

   type Report is record
      Generation   : Unsigned_32 := 0;
      State        : C.State := C.No_Radio;
      Failure      : C.Failure := C.None;
      Target_Valid : Boolean := False;
      Target       : WS.BSS_Entry := WS.No_Entry;
      Remembered   : Remembered_Set := Nothing_Remembered;
      Scan_Generation : Unsigned_32 := 0;
      Scan         : WS.Scan_Result;
   end record;

   function Normal (R : Report) return Boolean is
     (WS.Normal (R.Scan)
      and then (if R.Target_Valid then WS.Normal (R.Target) else R.Target = WS.No_Entry)
      and then (for all I in WS.Entry_Index =>
                  (if I > R.Scan.Summary.Count then not R.Remembered (I))));

   Generation_Word  : constant := 0;
   State_Word       : constant := 1;
   Remembered_Word  : constant := 2;
   Target_First     : constant := 3;
   Scan_First       : constant := Target_First + WS.Entry_Words;
   Report_Words     : constant := Scan_First + WS.Block_Words;
   Word_Bytes       : constant := WS.Word_Bytes;
   Report_Bytes     : constant := Report_Words * Word_Bytes;
   subtype Word_Index is Natural range 0 .. Report_Words - 1;
   type Words is array (Word_Index) of Unsigned_32;

   Failure_Shift : constant := 8;
   Target_Flag   : constant Unsigned_32 := 2 ** 16;
   Byte_Mask     : constant Unsigned_32 := 16#FF#;

   pragma Compile_Time_Error
     (WS.Maximum_Entries > Unsigned_32'Size, "one remembered bit per scan entry");
   pragma Compile_Time_Error (Report_Bytes > C.Page_Bytes, "a report fits the lent page");

   function Encode (R : Report) return Words
   with Pre => Normal (R),
        Post => Encode'Result (Generation_Word) = R.Generation;

   type Decoded_Report (Valid : Boolean := False) is record
      case Valid is
         when True  => Item : Report;
         when False => null;
      end case;
   end record;

   function Decode (W : Words) return Decoded_Report
   with Post => (if Decode'Result.Valid then
                   Normal (Decode'Result.Item) and then Encode (Decode'Result.Item) = W);
end CuBit.Wifi_Report;
