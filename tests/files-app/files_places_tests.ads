--  Hosted tests of the place selection (CuBit.Filesystem_Places, shared by
--  the filesystem service and the mock), the drawer's captions
--  (Files_Places) and the status words (Files_Status): every kind.
package Files_Places_Tests is
   procedure Run;
   function Checks return Natural;
   function Failures return Natural;
end Files_Places_Tests;
