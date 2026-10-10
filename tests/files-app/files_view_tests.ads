--  Hosted tests of the Files view and the mock filesystem service through
--  the real queue protocol: keys and clicks in, state and frames out.
package Files_View_Tests is
   procedure Run;
   function Checks return Natural;
   function Failures return Natural;
end Files_View_Tests;
