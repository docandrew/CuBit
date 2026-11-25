with CuBit.Failures;
with CCL.Interfaces.Graphics;

--  devmgr's renderer report for graphics.* (CCL.Interfaces.Graphics), one
--  body per platform: native/ reads it through the program's
--  graphics-observer endpoint (CuBit.Graphics_Report: the summary, then
--  every page of every entry against one generation); the Linux preview's
--  host/ has none (Available is False there, so the interface is not
--  published), a stand-in, not CuBit.
package CCL_Graphics is
   function Available return Boolean;
   procedure Read
     (Entries : out CCL.Interfaces.Graphics.Entry_Array; Count : out Natural;
      Success : out Boolean; Why : out CuBit.Failures.Failure)
     with Pre => Entries'First = 1 and then Entries'Length = CCL.Interfaces.Graphics.MAXIMUM_LISTED;
end CCL_Graphics;
