with Ada.Text_IO; with Interfaces; with Test_Control;
with Desktop_Renderer_Startup; with Compositor_Backend_Selection;
procedure Startup_Denial_Tests is
 package T renames Test_Control; package S renames Desktop_Renderer_Startup;
 use type Interfaces.Unsigned_64, T.Stage;
 E : Compositor_Backend_Selection.Readiness; D : S.Pipeline_Diagnostic;
 Cases : Natural := 0;
 procedure Run(Last : T.Stage; Expected : String:=""; Config : Boolean:=True;
               Width : Interfaces.Unsigned_64:=1024; Epoch : Interfaces.Unsigned_64:=1) is
 begin
  S.Initialize(Config,Width,768,Epoch,E,D);
  for Step in T.Stage loop
   if T.Stage'Pos(Step)<=T.Stage'Pos(Last) then
    pragma Assert(T.Calls(Step)=1,"missing or repeated setup stage");
   else pragma Assert(T.Calls(Step)=0,"setup continued after denial"); end if;
  end loop;
  if not Compositor_Backend_Selection.Ready(E) then S.Stop; end if;
  pragma Assert(T.Stops=(if Compositor_Backend_Selection.Ready(E) then 0 else 1));
  pragma Assert(T.Stage_Logs=(if Expected="" then 0 else 1),"missing or duplicate stage diagnostic");
  pragma Assert(T.Stage_Length=Expected'Length and then
    T.Last_Stage(1..T.Stage_Length)=Expected,"wrong failure stage");
  Cases:=Cases+1;
 end;
begin
 T.Reset; Run(T.Readback); pragma Assert(Compositor_Backend_Selection.Ready(E));
 for Fail in T.Stage loop
  T.Reset; T.Deny:=True; T.Failure:=Fail; Run(Fail, Expected =>
    (case Fail is when T.Device => "device", when T.Health => "health",
      when T.Targets => "targets", when T.Pipeline => "pipeline",
      when T.Upload => "upload", when T.Readback => "readback"));
  pragma Assert(not Compositor_Backend_Selection.Ready(E));
  if Fail=T.Upload then pragma Assert(not E.Readback); end if;
 end loop;
 T.Reset; Run(T.Health,Expected=>"configuration",Config=>False); pragma Assert(not E.Configuration);
 T.Reset; Run(T.Health,Expected=>"configuration",Epoch=>0); pragma Assert(not E.Configuration);
 T.Reset; Run(T.Health,Expected=>"configuration",Width=>0); pragma Assert(not E.Configuration);
 T.Reset; Run(T.Health,Expected=>"configuration",Width=>2**62+1024); pragma Assert(not E.Configuration);
 T.Reset; T.Empty_Slot:=True; Run(T.Device,Expected=>"admission"); pragma Assert(not E.Admitted);
 T.Reset; T.Inspect_OK:=False; Run(T.Device,Expected=>"admission"); pragma Assert(not E.Admitted);
 Ada.Text_IO.Put_Line("PASS startup external-denial cases=" & Cases'Image);
end Startup_Denial_Tests;
