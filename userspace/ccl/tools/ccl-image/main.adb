with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with CCL.Declarations;
with CCL.Images;

--  Linux-only boundary: emits a data plan, never shell commands.
procedure Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use CCL.Images;
   use type Ada.Streams.Stream_IO.Count;
   Catalog, Profile : Compilation_Result;
   Checked : Diagnostic_Code;
   Tab : constant String := [1 => ASCII.HT];

   procedure Read_Document (Path : String; Result : out Compilation_Result) is
      File : Ada.Streams.Stream_IO.File_Type;
      Bytes : Ada.Streams.Stream_Element_Array (1 .. CCL.Declarations.MAX_SOURCE);
      Last : Ada.Streams.Stream_Element_Offset;
      Source : String (1 .. CCL.Declarations.MAX_SOURCE);
   begin
      Result := (others => <>);
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Path);
      if Ada.Streams.Stream_IO.Size (File) > Bytes'Length then
         Ada.Streams.Stream_IO.Close (File);
         Put_Line (Standard_Error, Path & ": source exceeds 8192 bytes");
         return;
      end if;
      Ada.Streams.Stream_IO.Read (File, Bytes, Last);
      Ada.Streams.Stream_IO.Close (File);
      for I in 1 .. Natural (Last) loop
         Source (I) := Character'Val (Bytes (Ada.Streams.Stream_Element_Offset (I)));
      end loop;
      Compile (Source (1 .. Natural (Last)), Result);
      if not Result.Success then
         Put_Line (Standard_Error, Path & ": character" & Result.Position'Image &
                   ": " & Result.Diagnostic'Image & " / " & Result.Syntax_Diagnostic'Image);
      end if;
   end Read_Document;
begin
   if Argument_Count /= 2 then
      Put_Line (Standard_Error, "usage: ccl-image CATALOG.ccl IMAGE.ccl");
      Set_Exit_Status (Failure); return;
   end if;
   Read_Document (Argument (1), Catalog);
   Read_Document (Argument (2), Profile);
   if not Catalog.Success or else not Profile.Success then
      Set_Exit_Status (Failure); return;
   end if;
   Validate (Catalog.Plan, Profile.Plan, Checked);
   if Checked /= No_Error then
      Put_Line (Standard_Error, Argument (2) & ": " & Checked'Image);
      Set_Exit_Status (Failure); return;
   end if;
   --  Path alphabet excludes tabs/newlines, so this versioned interchange
   --  cannot inject fields. The host adapter treats every field as data.
   Put_Line ("CCL-IMAGE-1" & Tab & Profile.Plan.Layout'Image & Tab &
             Value (Profile.Plan.Catalog_Name) & Tab & Value (Profile.Plan.Provider));
   for Item of Profile.Plan.Placements (1 .. Profile.Plan.Placement_Count) loop
      for Input of Catalog.Plan.Artifacts (1 .. Catalog.Plan.Artifact_Count) loop
         if Input.Name = Item.Artifact_Name then
            Put_Line (Item.Region'Image & Tab & Item.Role'Image & Tab &
                      Value (Input.Name) & Tab & Input.Kind'Image & Tab &
                      Value (Input.Source) & Tab & Value (Item.Destination));
         end if;
      end loop;
   end loop;
exception
   when Ada.Streams.Stream_IO.Name_Error | Ada.Streams.Stream_IO.Use_Error =>
      Put_Line (Standard_Error, "cannot read image declaration");
      Set_Exit_Status (Failure);
end Main;
