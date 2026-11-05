pragma Ada_2022;
with Ada.Command_Line; with ACPI_Native_Startup_Timing; with AML_Work_Kinds; with Timing_Test_Control;
procedure Work_Timing_Tests is
   package N renames ACPI_Native_Startup_Timing; package K renames AML_Work_Kinds;
   package T renames Timing_Test_Control;
   Mode : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   Root,Module,RPC : N.Token;
   Before : Natural;
begin
   if Mode > 3 then raise Program_Error; end if;
   T.Now := 0; T.Complete := False; N.Start; N.Enter (N.P.Startup,Root);
   T.Now := 10; N.Enter (N.P.Module_Execution,Module);
   if Mode = 1 then
      for I in 1 .. 9 loop N.Observe_Work (K.Namespace_Lookup,True); end loop;
      for I in 1 .. 9 loop N.Observe_Work (K.Namespace_Lookup,False); end loop;
   else
      T.Now := 20; N.Observe_Work (K.Namespace_Lookup,True);
      T.Now := 25; N.Observe_Work (K.Allocation_Preparation,True);
      T.Now := 50; T.Known := Mode /= 3;
      N.Observe_Work ((if Mode = 2 then K.Value_Clone else K.Allocation_Preparation),False);
      T.Known := True;
      if Mode /= 2 then
         T.Now := 55; N.Enter (N.P.Scoped_Read,RPC); T.Now := 65; N.Leave (RPC);
         T.Now := 80; N.Observe_Work (K.Namespace_Lookup,False);
      end if;
   end if;
   if T.Lines /= 0 then raise Program_Error with "hot-path log"; end if;
   if Mode /= 2 then T.Now := 90; N.Leave (Module); T.Now := 100; N.Leave (Root); end if;
   N.Report;
   if Mode = 0 then
      if not T.Complete or not T.Found_Allocation or not T.Found_Lookup or not T.Found_Gap then
         raise Program_Error with "work accounting";
      end if;
   else
      if T.Complete or not T.Incomplete then raise Program_Error with "false completeness"; end if;
      if Mode = 1 and not T.Found_Drop then raise Program_Error with "drop report"; end if;
   end if;
   Before := T.Lines; N.Report;
   N.Observe_Work (K.Namespace_Lookup,True); N.Observe_Work (K.Namespace_Lookup,False);
   if T.Lines /= Before then raise Program_Error with "repeated report"; end if;
end Work_Timing_Tests;
