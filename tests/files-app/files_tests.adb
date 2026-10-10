with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Environment_Variables;
with CuBit.Appearance;
with Files_Host_Support;
with Files_Policy_Tests;
with Files_View_Tests;

--  Hosted tests of Files (docs/files-app.md): the policy units against
--  reference implementations, then the view and the mock service through
--  the real queue protocol, with frames saved under build/.
procedure Files_Tests is
begin
   --  FILES_THEME=dark: the same tests (and screenshots) in the toolkit's
   --  dark scheme.
   Files_Host_Support.Install_Themes
     (if Ada.Environment_Variables.Exists ("FILES_THEME")
        and then Ada.Environment_Variables.Value ("FILES_THEME") = "dark"
      then CuBit.Appearance.Alloy_Dark else CuBit.Appearance.Alloy_Light);
   Files_Policy_Tests.Run;
   Files_View_Tests.Run;
   if Files_Policy_Tests.Failures + Files_View_Tests.Failures > 0 then
      Ada.Text_IO.Put_Line
        ("FAIL:" & Natural'Image (Files_Policy_Tests.Failures + Files_View_Tests.Failures) & " of" &
         Natural'Image (Files_Policy_Tests.Checks + Files_View_Tests.Checks) & " checks");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Ada.Text_IO.Put_Line
        ("PASS:" & Natural'Image (Files_Policy_Tests.Checks + Files_View_Tests.Checks) & " checks");
   end if;
end Files_Tests;
