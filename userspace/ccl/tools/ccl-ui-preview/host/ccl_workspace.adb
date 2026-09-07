with CCL_Workspace_Names;
--  Linux-only deterministic, process-local mock. Never opens host files.
package body CCL_Workspace is
   use CCL_Workspace_Names;
   use CuBit.File_Selection;
   Names : File_List;
   type Sources is array (Positive range 1 .. Maximum_Files) of Source_Buffer;
   type Lengths is array (Positive range 1 .. Maximum_Files) of Source_Length;
   Data : Sources := [others => [others => ' ']];
   Sizes : Lengths := [others => 0];
   Initialized : Boolean := False;

   function Supported return Boolean is (True);
   function Location return String is ("Linux mock workspace (memory only)");
   procedure Shutdown is null;

   function Valid_Text (Text : String) return Boolean is
     (Text'Length <= Maximum_Source_Bytes and then
      (for all C of Text => C in ' ' .. '~' | ASCII.LF | ASCII.CR | ASCII.HT));

   procedure Initialize is
      Accepted : Boolean;
      Hello : constant String := "# Mock workspace: no host files are accessed." & ASCII.LF & "(+ 20 22)";
      Arithmetic : constant String := "(let ((minutes (/ 3661 60))) minutes)";
   begin
      if Initialized then return; end if;
      Append (Names, "hello.ccl", Accepted);
      Data (1) (1 .. Hello'Length) := Hello;
      Sizes (1) := Hello'Length;
      Append (Names, "arithmetic.ccl", Accepted);
      Data (2) (1 .. Arithmetic'Length) := Arithmetic;
      Sizes (2) := Arithmetic'Length;
      Initialized := True;
   end Initialize;

   procedure List_Files (Files : out File_List; Result : out Storage_Result) is
   begin
      Initialize;
      Files := Names;
      Result := Succeeded;
   end List_Files;

   procedure Load
     (Name : String; Text : out Source_Buffer; Length : out Source_Length;
      Result : out Storage_Result)
   is
   begin
      Text := [others => ' '];
      Length := 0;
      Result := Invalid_Name;
      if not Valid_Source_Name (Name) then return; end if;
      Initialize;
      for Index in 1 .. Names.Count loop
         if Value (Names.Names (Index)) = Name then
            Text := Data (Index);
            Length := Sizes (Index);
            Result := Succeeded;
            return;
         end if;
      end loop;
      Result := Not_Found;
   end Load;

   procedure Save_New (Name, Text : String; Result : out Storage_Result) is
      Accepted : Boolean;
   begin
      Result := Invalid_Name;
      if not Valid_Source_Name (Name) then return; end if;
      Result := Invalid_Source;
      if not Valid_Text (Text) then return; end if;
      Initialize;
      for Item in 1 .. Names.Count loop
         if Value (Names.Names (Item)) = Name then
            Result := Conflict;
            return;
         end if;
      end loop;
      Append (Names, Name, Accepted);
      if not Accepted then
         Result := Limit_Reached;
         return;
      end if;
      Data (Names.Count) (1 .. Text'Length) := Text;
      Sizes (Names.Count) := Text'Length;
      Result := Succeeded;
   end Save_New;

   procedure Scan (Latest : out Revision) is
      Number : Revision;
      Pending : Boolean;
   begin
      Initialize;
      Latest := 0;
      for Index in 1 .. Names.Count loop
         Decode (Value (Names.Names (Index)), Number, Pending);
         if not Pending then Latest := Revision'Max (Latest, Number); end if;
      end loop;
   end Scan;

   procedure Suggest_Name (Name : out File_Name; Result : out Storage_Result) is
      Latest : Revision;
      Accepted : Boolean;
   begin
      Name := (others => <>);
      Scan (Latest);
      if Latest = Revision'Last then
         Result := Limit_Reached;
      else
         Set (Name, Filename (Latest + 1, False), Accepted);
         Result := (if Accepted then Succeeded else Invalid_Name);
      end if;
   end Suggest_Name;

end CCL_Workspace;
