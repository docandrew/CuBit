--  Linux-hosted stand-in for the launch block the kernel maps: the test
--  places the trailer bytes after the launch strings (ring table, if any,
--  then the parameters section, then the description) here, and the test
--  body of CuBit.Launch_Parameters reads them.
with CuBit.Parameter_Sections;

package Launch_Double is
   Maximum_Trailer : constant := 2 * CuBit.Parameter_Sections.Maximum_Section_Bytes;
   Trailer : CuBit.Parameter_Sections.Bytes (1 .. Maximum_Trailer) := [others => 0];
   Trailer_Length : Natural range 0 .. Maximum_Trailer := 0;
   --  False: as for a process without a launch block.
   Present : Boolean := False;
   --  Raised by Refuse, carrying its reason.
   Refused : exception;
end Launch_Double;
