--  The Linux preview has no devmgr (a stand-in, not CuBit): graphics.* is
--  not published here.
package body CCL_Graphics is
   function Available return Boolean is (False);

   procedure Read
     (Entries : out CCL.Interfaces.Graphics.Entry_Array; Count : out Natural;
      Success : out Boolean; Why : out CuBit.Failures.Failure) is
   begin
      Entries := [others => (others => <>)];
      Count := 0;
      Success := False;
      Why := CuBit.Failures.Failed (CuBit.Failures.Unavailable, "graphics: the Linux preview has no devmgr");
   end Read;
end CCL_Graphics;
