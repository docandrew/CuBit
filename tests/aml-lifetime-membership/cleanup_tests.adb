pragma Ada_2022;
with Ada.Text_IO;
with AML_Namespace;
with AML_Namespace.Lifetime_Test_Fixture;
with AML_Delays;
procedure Cleanup_Tests is
   package N is new AML_Namespace (Capacity => 16, Perform_Delay => AML_Delays.Unavailable_Provider);
   package F is new N.Lifetime_Test_Fixture;
   Checks : constant Natural := F.Run;
begin
   Ada.Text_IO.Put_Line ("Lifetime cleanup checks" & Checks'Image);
end Cleanup_Tests;
