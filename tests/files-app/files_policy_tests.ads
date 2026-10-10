--  Hosted tests of Files' policy units against reference implementations:
--  what the proofs leave to testing (sortedness, permutation, filter
--  equivalence) and the page decoder on good and hostile bytes.
package Files_Policy_Tests is
   procedure Run;
   function Checks return Natural;
   function Failures return Natural;
end Files_Policy_Tests;
