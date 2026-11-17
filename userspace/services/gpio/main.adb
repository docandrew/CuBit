pragma Ada_2022;
with GPIO_Main_Entry;
with Startup;
pragma Elaborate_All (Startup);
pragma Unreferenced (Startup); -- Its body validates the startup table first.
procedure Main is
   procedure Run is new GPIO_Main_Entry ((Present => False));
begin
   Run;
end Main;
