with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with CCL.Configurations.Changes;
with CCL.Declarations;

--  Linux-hosted read-only boundary. No file writes or activation. Deliberately
--  show keys and actions, never configuration values that may contain secrets.
procedure Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use CCL.Configurations;
   use CCL.Configurations.Changes;

   procedure Load (Path : String; Result : out Compilation_Result) is
      use type Ada.Streams.Stream_IO.Count;
      Input : Ada.Streams.Stream_IO.File_Type;
      Bytes : Ada.Streams.Stream_Element_Array (1 .. CCL.Declarations.MAX_SOURCE);
      Last : Ada.Streams.Stream_Element_Offset;
      Source : String (1 .. CCL.Declarations.MAX_SOURCE);
   begin
      Result := (others => <>);
      Ada.Streams.Stream_IO.Open (Input, Ada.Streams.Stream_IO.In_File, Path);
      if Ada.Streams.Stream_IO.Size (Input) > Bytes'Length then
         Ada.Streams.Stream_IO.Close (Input);
         Put_Line (Standard_Error, Path & ": source exceeds configuration bound");
         return;
      end if;
      Ada.Streams.Stream_IO.Read (Input, Bytes, Last);
      Ada.Streams.Stream_IO.Close (Input);
      for I in 1 .. Natural (Last) loop
         Source (I) := Character'Val (Bytes (Ada.Streams.Stream_Element_Offset (I)));
      end loop;
      Compile (Source (1 .. Natural (Last)), Result);
      if not Result.Success then
         Put_Line (Standard_Error, Path & ": character" & Result.Position'Image &
                   ": " & Diagnostic_Name (Result.Diagnostic));
      end if;
   exception
      when Ada.Streams.Stream_IO.Name_Error | Ada.Streams.Stream_IO.Use_Error |
           Ada.Streams.Stream_IO.Device_Error | Ada.Streams.Stream_IO.End_Error =>
         if Ada.Streams.Stream_IO.Is_Open (Input) then
            Ada.Streams.Stream_IO.Close (Input);
         end if;
         Put_Line (Standard_Error, Path & ": cannot read configuration source");
   end Load;

   Before, After : Compilation_Result;
   Changes : Review;
   Changed : Boolean := False;
begin
   if Argument_Count /= 2 then
      Put_Line (Standard_Error, "usage: ccl-config-review BEFORE.ccl AFTER.ccl");
      Set_Exit_Status (Failure);
      return;
   end if;
   Load (Argument (1), Before);
   Load (Argument (2), After);
   Changes := Compare (Before, After);
   if not Changes.Accepted then
      Put_Line (Standard_Error, "review requires two valid system-config declarations");
      Set_Exit_Status (Failure);
      return;
   end if;
   for I in 1 .. After.Plan.Setting_Count loop
      if Changes.Candidate (I) /= Unchanged then
         Changed := True;
         Put_Line ((if Changes.Candidate (I) = Added then "add " else "replace ") &
                   After.Plan.Settings (I).Key.Data (1 .. After.Plan.Settings (I).Key.Length));
      end if;
   end loop;
   for I in 1 .. Before.Plan.Setting_Count loop
      if Changes.Removed (I) then
         Changed := True;
         Put_Line ("remove " & Before.Plan.Settings (I).Key.Data
                     (1 .. Before.Plan.Settings (I).Key.Length));
      end if;
   end loop;
   if not Changed then
      Put_Line ("no effective setting changes");
   end if;
end Main;
