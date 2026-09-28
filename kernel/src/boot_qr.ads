pragma Ada_2022;
--  Fixed QR Code Model 2, version 4-L encoder.  This is deliberately a
--  small, bounded byte-mode profile for bootstrap diagnostics, not a general
--  document/image QR library.
package Boot_QR is
   Version : constant := 4;
   Dimension : constant := 17 + 4 * Version; -- 33 modules
   Quiet_Zone : constant := 4;
   Maximum_Byte_Length : constant := 78;
   subtype Module_Index is Natural range 0 .. Dimension - 1;
   type Matrix is array (Module_Index, Module_Index) of Boolean;
   --  The bootstrap panel serializes calls with its renderer lock. Keeping the
   --  function-pattern scratch matrix static avoids consuming an AP stack.
   procedure Encode (Text : String; Result : out Matrix)
     with Pre => Text'Length <= Maximum_Byte_Length;
end Boot_QR;
