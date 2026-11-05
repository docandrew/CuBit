pragma Ada_2022;
with ACPI_Native_Startup_Timing; with Timing_Test_Control;
procedure Native_Timing_Tests is
   package N renames ACPI_Native_Startup_Timing;
   package T renames Timing_Test_Control;
   Root,Module,Call : N.Token;
   Before : Natural;
begin
   T.Complete := False; T.Found_Module := False;
   N.Start; N.Enter (N.P.Startup,Root);
   T.Now := 10; N.Enter (N.P.Module_Execution,Module);
   T.Now := 20; N.Enter (N.P.Scoped_Read,Call); T.Now := 50; N.Leave (Call);
   T.Now := 60; N.Enter (N.P.AML_Sleep,Call); T.Now := 70; N.Leave (Call);
   T.Now := 90; N.Leave (Module); T.Now := 100; N.Leave (Root);
   N.Report;
   if not T.Complete or not T.Found_Module or not T.Found_Zero_Allocation or T.Lines /= 8 then raise Program_Error with "native report"; end if;
   Before := T.Lines; N.Report;
   N.Enter (N.P.Other_RPC,Call); N.Leave (Call);
   if T.Lines /= Before then raise Program_Error with "report repeated"; end if;
end Native_Timing_Tests;
