with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with CCL.Configurations;
with CCL.Declarations;

--  Linux-only boundary. No file is read during CCL evaluation, and failed
--  declarations never emit a partial runtime configuration.
procedure Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use type Ada.Streams.Stream_IO.Count;
   Input : Ada.Streams.Stream_IO.File_Type;
   Bytes : Ada.Streams.Stream_Element_Array (1 .. CCL.Declarations.MAX_SOURCE);
   Last : Ada.Streams.Stream_Element_Offset;
   Source : String (1 .. CCL.Declarations.MAX_SOURCE);
   Result : CCL.Configurations.Compilation_Result;
   use CCL.Configurations;
begin
   if Argument_Count not in 1 .. 2 or else
     (Argument_Count = 2 and then Argument (2) not in "--dump-plan" | "--dump-startup" | "--dump-system") then
      Put_Line (Standard_Error, "usage: ccl-config PROFILE.ccl [--dump-plan|--dump-startup|--dump-system]");
      Set_Exit_Status (Failure);
      return;
   end if;
   Ada.Streams.Stream_IO.Open (Input, Ada.Streams.Stream_IO.In_File, Argument (1));
   if Ada.Streams.Stream_IO.Size (Input) > Bytes'Length then
      Ada.Streams.Stream_IO.Close (Input);
      Put_Line (Standard_Error, "profile exceeds 8192-byte source bound");
      Set_Exit_Status (Failure);
      return;
   end if;
   Ada.Streams.Stream_IO.Read (Input, Bytes, Last);
   Ada.Streams.Stream_IO.Close (Input);
   for I in 1 .. Natural (Last) loop
      Source (I) := Character'Val (Bytes (Ada.Streams.Stream_Element_Offset (I)));
   end loop;
   CCL.Configurations.Compile (Source (1 .. Natural (Last)), Result);
   if not Result.Success then
      Put_Line (Standard_Error, Argument (1) & ": character" & Result.Position'Image &
                ": " & Result.Diagnostic'Image & " / " & Result.Syntax_Diagnostic'Image);
      Set_Exit_Status (Failure);
      return;
   end if;
   if Argument_Count = 2 and then
     ((Argument (2) = "--dump-startup" and then Result.Plan.Kind /= Startup_Profile)
      or else (Argument (2) = "--dump-system" and then Result.Plan.Kind /= System_Profile))
   then
      Put_Line (Standard_Error, "configuration profile kind mismatch");
      Set_Exit_Status (Failure);
      return;
   end if;
   --  Optional human-readable plan dump also supports legacy fixture comparisons.
   --  Images carry CCL source; no native consumer parses this output.
   if Argument_Count = 2 then
      case Result.Plan.Kind is
         when System_Profile =>
            for Item of Result.Plan.Settings (1 .. Result.Plan.Setting_Count) loop
               Put_Line (Item.Key.Data (1 .. Item.Key.Length) & "=" &
                         Item.Value.Data (1 .. Item.Value.Length));
            end loop;
         when Startup_Profile =>
            for Item of Result.Plan.Launches (1 .. Result.Plan.Launch_Count) loop
               declare
                  function Image (N : Long_Long_Integer) return String is
                     Text : constant String := N'Image;
                  begin
                     return Text (Text'First + 1 .. Text'Last);
                  end Image;
                  Pri : constant String := Item.Priority'Image;
               begin
                  Put (Item.Executable.Data (1 .. Item.Executable.Length) &
                       " pri=" & Pri (Pri'First + 1 .. Pri'Last) &
                       (if Item.Approval = Approve_Declared
                        then " network=declared" else "") &
                       (if Item.Role = Config_Storage then " role=config-storage" else ""));
                  --  Startup-policy fields appear only when set, so existing
                  --  profiles dump exactly as before.
                  if Item.Mode = Per_Device then Put (" launch=per-device"); end if;
                  if Item.Approve_Device then Put (" device=approved"); end if;
                  for Index in 1 .. Item.Dependency_Total loop
                     Put ((if Index = 1 then " after=" else ",") &
                          Image (Long_Long_Integer (Item.Dependencies (Index))));
                  end loop;
                  if Item.Approve_Scheduling then
                     Put (" scheduling=" & Image (Long_Long_Integer (Item.Scheduling_Budget)) &
                          "/" & Image (Long_Long_Integer (Item.Scheduling_Period)));
                  end if;
                  if Item.Has_Ready_Deadline then
                     Put (" ready-ms=" & Image (Long_Long_Integer (Item.Ready_Deadline_Ms)));
                  end if;
                  New_Line;
               end;
            end loop;
      end case;
   end if;
exception
   when Ada.Streams.Stream_IO.Name_Error | Ada.Streams.Stream_IO.Use_Error =>
      Put_Line (Standard_Error, "cannot read configuration source");
      Set_Exit_Status (Failure);
end Main;
