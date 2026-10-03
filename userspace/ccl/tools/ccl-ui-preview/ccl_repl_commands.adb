with CuBit.File_Selection;
with CCL_Workspace;

procedure CCL_REPL_Commands
  (Item : in out CCL.Sessions.Session; Source : String;
   Fuel : CCL.Sessions.Fuel_Budget; Outcome : out CCL.Language.Interpretation_Result)
is
   use type CCL_Workspace.Storage_Result;
   First : Natural := Source'First;
   Last : Natural := Source'Last;
   function Command (Word : String) return Boolean is
     (Last - First + 1 >= Word'Length and then Source (First .. First + Word'Length - 1) = Word and then
      (Last - First + 1 = Word'Length or else Source (First + Word'Length) = ' '));
   --  The argument after the command word, with ".ccl" added if missing.
   function File_Argument (Word : String) return String is
      From : Natural := First + Word'Length;
   begin
      while From <= Last and then Source (From) = ' ' loop From := From + 1; end loop;
      if From > Last then return ""; end if;
      return (if Last - From + 1 > 4 and then Source (Last - 3 .. Last) = ".ccl"
              then Source (From .. Last) else Source (From .. Last) & ".ccl");
   end File_Argument;
   function Failure (Result : CCL_Workspace.Storage_Result) return String is
     (case Result is
        when CCL_Workspace.Conflict => "a file with that name exists; choose a new name",
        when CCL_Workspace.Not_Found => "no such file in the workspace",
        when CCL_Workspace.Invalid_Name => "invalid name (letters, digits, - and _, ending .ccl)",
        when CCL_Workspace.Unavailable => "no workspace in this session",
        when CCL_Workspace.Limit_Reached => "the workspace or file is full",
        when CCL_Workspace.Access_Denied => "the workspace refused access",
        when others => "workspace I/O failed");
begin
   while First <= Last and then Source (First) = ' ' loop First := First + 1; end loop;
   while Last >= First and then Source (Last) = ' ' loop Last := Last - 1; end loop;
   if Command (":files") then
      declare
         Files : CuBit.File_Selection.File_List;
         Result : CCL_Workspace.Storage_Result;
         Names : String (1 .. 900) := [others => ' '];
         Used : Natural := 0;
      begin
         CCL_Workspace.List_Files (Files, Result);
         if Result /= CCL_Workspace.Succeeded then
            CCL.Sessions.Note (Item, Source, Failure (Result), Outcome);
            return;
         end if;
         for I in 1 .. Files.Count loop
            declare
               Name : constant String := CuBit.File_Selection.Value (Files.Names (I));
            begin
               exit when Name'Length + 2 > Names'Length - Used;
               if Used > 0 then Names (Used + 1 .. Used + 2) := ", "; Used := Used + 2; end if;
               Names (Used + 1 .. Used + Name'Length) := Name;
               Used := Used + Name'Length;
            end;
         end loop;
         CCL.Sessions.Note
           (Item, Source, (if Used = 0 then "no .ccl files in " & CCL_Workspace.Location
                           else Names (1 .. Used)), Outcome);
      end;
   elsif Command (":save") then
      declare
         Name : constant String := File_Argument (":save");
         Result : CCL_Workspace.Storage_Result;
      begin
         if not CCL_Workspace.Valid_Source_Name (Name) then
            CCL.Sessions.Note (Item, Source, Failure (CCL_Workspace.Invalid_Name), Outcome);
         elsif CCL.Sessions.Kept_Definitions (Item) = 0 then
            CCL.Sessions.Note (Item, Source, "no definitions to save (values are not saved)", Outcome);
         else
            CCL_Workspace.Save_New (Name, CCL.Sessions.Definitions_Source (Item), Result);
            CCL.Sessions.Note
              (Item, Source,
               (if Result = CCL_Workspace.Succeeded then "saved" &
                  Natural'Image (CCL.Sessions.Kept_Definitions (Item)) & " definitions to " & Name
                else Failure (Result)), Outcome);
         end if;
      end;
   elsif Command (":load") then
      declare
         Name : constant String := File_Argument (":load");
         Text : CCL_Workspace.Source_Buffer;
         Length : CCL_Workspace.Source_Length;
         Result : CCL_Workspace.Storage_Result;
      begin
         if not CCL_Workspace.Valid_Source_Name (Name) then
            CCL.Sessions.Note (Item, Source, Failure (CCL_Workspace.Invalid_Name), Outcome);
            return;
         end if;
         CCL_Workspace.Load (Name, Text, Length, Result);
         if Result /= CCL_Workspace.Succeeded then
            CCL.Sessions.Note (Item, Source, Failure (Result), Outcome);
         else
            Submit (Item, Text (1 .. Length), Fuel, Outcome, Shown => Source);
         end if;
      end;
   else
      Submit (Item, Source, Fuel, Outcome);
   end if;
end CCL_REPL_Commands;
