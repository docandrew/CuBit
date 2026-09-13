with CCL.Language;

package body CCL.Images with SPARK_Mode => On is
   use CCL.Declarations;

   function Value (Item : Text) return String is (Item.Data (1 .. Item.Length));

   function Safe_Path (Path : String) return Boolean is
      First : Integer := Path'First;
   begin
      if Path'Length = 0 or else Path'Length > MAX_PATH then return False; end if;
      if Path (Path'First) = '/' or else Path (Path'Last) in '/' | '.' then
         return False;
      end if;
      for I in Path'Range loop
         if Path (I) not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' | '/' then
            return False;
         end if;
         if Path (I) = '/' then
            if I = First or else Path (I - 1) = '.' then return False; end if;
            First := I + 1;
         end if;
      end loop;
      return First <= Path'Last and then Path (First .. Path'Last) not in "." | "..";
   end Safe_Path;

   function Overlaps (Left, Right : Text) return Boolean is
      function Prefix (A, B : Text) return Boolean is
        (A.Length < B.Length and then A.Data (1 .. A.Length) = B.Data (1 .. A.Length)
         and then B.Data (A.Length + 1) = '/');
   begin
      return Left = Right or else Prefix (Left, Right) or else Prefix (Right, Left);
   end Overlaps;

   procedure Compile (Source : String; Result : out Compilation_Result) is
      Reader : Scanner;
      Name : Symbol;
      Evaluated : CCL.Language.Interpretation_Result;
      Have_Catalog, Have_Provider, Have_Layout : Boolean := False;
      type Role_Set is array (Role_Kind) of Boolean;
      Roles : Role_Set := [others => False];

      procedure Fail (Code : Diagnostic_Code) is
      begin
         if Result.Diagnostic = No_Error then
            Result.Diagnostic := Code;
            Result.Position := Position (Reader);
         end if;
      end Fail;
      function Stopped return Boolean is
        (Result.Diagnostic /= No_Error or else Failed (Reader));
      procedure Read_Text (Item : out Text; Identifier : Boolean := False) is
      begin
         Item := (others => <>);
         Evaluate (Reader, Evaluated);
         if Stopped then return; end if;
         if not Evaluated.Has_Text or else
           not Safe_Path (Evaluated.Result_Text.Data (1 .. Evaluated.Result_Text.Length))
         then Fail (Invalid_Path); return; end if;
         Item.Length := Evaluated.Result_Text.Length;
         Item.Data (1 .. Item.Length) := Evaluated.Result_Text.Data (1 .. Item.Length);
         if Identifier then
            for C of Value (Item) loop
               if C = '/' then Fail (Invalid_Value); end if;
            end loop;
         end if;
      end Read_Text;
      procedure Read_Region (Region : out Region_Kind) is
      begin
         Region := Bootstrap;
         Read_Symbol (Reader, Name);
         if Matches (Name, "bootstrap") then Region := Bootstrap;
         elsif Matches (Name, "optical") then Region := Optical;
         else Fail (Invalid_Value);
         end if;
      end Read_Region;
      procedure Add_Artifact is
         Item : Artifact;
      begin
         Read_Text (Item.Name, True);
         Read_Symbol (Reader, Name);
         if Matches (Name, "repository") then Item.Kind := Repository_File;
         elsif Matches (Name, "supplied-file") then Item.Kind := Supplied_File;
         elsif Matches (Name, "supplied-tree") then Item.Kind := Supplied_Tree;
         else Fail (Invalid_Value);
         end if;
         Read_Text (Item.Source, Item.Kind /= Repository_File);
         for Old of Result.Plan.Artifacts (1 .. Result.Plan.Artifact_Count) loop
            if Old.Name = Item.Name then Fail (Duplicate_Name); end if;
         end loop;
         if Result.Plan.Artifact_Count = MAX_ITEMS then Fail (Too_Many_Items); end if;
         if Stopped then return; end if;
         Result.Plan.Artifact_Count := Result.Plan.Artifact_Count + 1;
         Result.Plan.Artifacts (Result.Plan.Artifact_Count) := Item;
      end Add_Artifact;
      procedure Add_Requirement is
         Item : Requirement;
      begin
         Read_Text (Item.Provider, True);
         Read_Text (Item.Artifact_Name, True);
         Read_Text (Item.Destination);
         for Old of Result.Plan.Requirements (1 .. Result.Plan.Requirement_Count) loop
            if Old.Provider = Item.Provider and then Old.Artifact_Name = Item.Artifact_Name then
               Fail (Duplicate_Name);
            end if;
         end loop;
         if Result.Plan.Requirement_Count = MAX_ITEMS then Fail (Too_Many_Items); end if;
         if Stopped then return; end if;
         Result.Plan.Requirement_Count := Result.Plan.Requirement_Count + 1;
         Result.Plan.Requirements (Result.Plan.Requirement_Count) := Item;
      end Add_Requirement;
      procedure Add_Placement (Role : Role_Kind) is
         Item : Placement;
         procedure Destination (Path : String) is
         begin
            Item.Destination.Length := Path'Length;
            Item.Destination.Data (1 .. Path'Length) := Path;
         end Destination;
      begin
         Item.Role := Role;
         if Role = Content then
            Read_Region (Item.Region);
         elsif Role in Kernel | Boot_Menu then
            Item.Region := Optical;
         end if;
         Read_Text (Item.Artifact_Name, True);
         case Role is
            when Content => Read_Text (Item.Destination);
            when Startup => Destination ("init.ccl");
            when Settings => Destination ("system.ccl");
            when Kernel => Destination ("boot/cubit_kernel");
            when Boot_Menu => Destination ("boot/grub/grub.cfg");
         end case;
         if Role /= Content and then Roles (Role) then Fail (Duplicate_Field); end if;
         Roles (Role) := True;
         for Old of Result.Plan.Placements (1 .. Result.Plan.Placement_Count) loop
            if Old.Region = Item.Region and then Overlaps (Old.Destination, Item.Destination) then
               Fail (Duplicate_Destination);
            end if;
         end loop;
         if Result.Plan.Placement_Count = MAX_ITEMS then Fail (Too_Many_Items); end if;
         if Stopped then return; end if;
         Result.Plan.Placement_Count := Result.Plan.Placement_Count + 1;
         Result.Plan.Placements (Result.Plan.Placement_Count) := Item;
      end Add_Placement;
   begin
      Result := (others => <>);
      Start (Reader, Source);
      Open_Form (Reader);
      Read_Symbol (Reader, Name);
      if Matches (Name, "image-artifacts") then Result.Plan.Kind := Artifact_Catalog;
      elsif Matches (Name, "system-image") then Result.Plan.Kind := System_Image;
      else Fail (Unknown_Declaration);
      end if;
      Read_Symbol (Reader, Name);
      if not Stopped then
         declare
            Format : constant Format_Selection := Select_Format (Name.Data (1 .. Name.Length));
         begin
            if not Format.Supported then
               Fail (Unsupported_Version);
            else
               case Format.Version is
                  when V1 => null;
               end case;
            end if;
         end;
      end if;
      while not Stopped and then not At_End (Reader) and then not At_Close (Reader) loop
         Open_Form (Reader);
         Read_Symbol (Reader, Name);
         if Matches (Name, "catalog") then
            if Have_Catalog then Fail (Duplicate_Field); end if;
            Have_Catalog := True;
            Read_Text (Result.Plan.Catalog_Name, True);
         elsif Result.Plan.Kind = Artifact_Catalog then
            if Matches (Name, "artifact") then Add_Artifact;
            elsif Matches (Name, "bootstrap-requires") then Add_Requirement;
            else Fail (Unknown_Declaration);
            end if;
         elsif Matches (Name, "provider") then
            if Have_Provider then Fail (Duplicate_Field); end if;
            Have_Provider := True;
            Read_Text (Result.Plan.Provider, True);
         elsif Matches (Name, "layout") then
            if Have_Layout then Fail (Duplicate_Field); end if;
            Have_Layout := True;
            Read_Symbol (Reader, Name);
            if Matches (Name, "bootstrap-only") then Result.Plan.Layout := Bootstrap_Only;
            elsif Matches (Name, "optical") then Result.Plan.Layout := Optical_Image;
            else Fail (Invalid_Value);
            end if;
         elsif Matches (Name, "file") then Add_Placement (Content);
         elsif Matches (Name, "startup") then Add_Placement (Startup);
         elsif Matches (Name, "settings") then Add_Placement (Settings);
         elsif Matches (Name, "kernel") then Add_Placement (Kernel);
         elsif Matches (Name, "boot-menu") then Add_Placement (Boot_Menu);
         else Fail (Unknown_Declaration);
         end if;
         Close_Form (Reader);
      end loop;
      Close_Form (Reader);
      if not Stopped and then not At_End (Reader) then Fail (Invalid_Syntax); end if;
      if not Stopped and then
        (not Have_Catalog or else
         (Result.Plan.Kind = Artifact_Catalog and then Result.Plan.Artifact_Count = 0)
         or else (Result.Plan.Kind = System_Image and then
           (not Have_Provider or else not Have_Layout or else not Roles (Settings)
            or else (Result.Plan.Layout = Optical_Image and then
              (not Roles (Kernel) or else not Roles (Boot_Menu) or else not Roles (Startup))))))
      then Fail (Missing_Field);
      end if;
      if Failed (Reader) and then Result.Diagnostic = No_Error then
         Result.Diagnostic := Invalid_Syntax;
         Result.Syntax_Diagnostic := Diagnostic (Reader);
         Result.Position := Position (Reader);
      end if;
      Result.Success := not Stopped;
      if not Result.Success then Result.Plan := (others => <>); end if;
   end Compile;

   procedure Validate (Catalog, Profile : Document; Diagnostic : out Diagnostic_Code) is
      Found, Provider_Found : Boolean := False;
      Archive_Path : constant Text :=
        (Length => 15, Data => "boot/initrd.img" & [1 .. MAX_PATH - 15 => ' ']);
   begin
      Diagnostic := No_Error;
      if Catalog.Kind /= Artifact_Catalog or else Profile.Kind /= System_Image then
         Diagnostic := Invalid_Value; return;
      end if;
      if Catalog.Catalog_Name /= Profile.Catalog_Name then
         Diagnostic := Catalog_Mismatch; return;
      end if;
      --  Check all requirement references, including providers not selected.
      for Needed of Catalog.Requirements (1 .. Catalog.Requirement_Count) loop
         Found := False;
         for Item of Catalog.Artifacts (1 .. Catalog.Artifact_Count) loop
            Found := Found or Item.Name = Needed.Artifact_Name;
         end loop;
         if not Found then Diagnostic := Unknown_Artifact; return; end if;
         if Needed.Provider = Profile.Provider then
            Provider_Found := True;
            Found := False;
            for Item of Profile.Placements (1 .. Profile.Placement_Count) loop
               Found := Found or (Item.Region = Bootstrap and then
                 Item.Artifact_Name = Needed.Artifact_Name and then
                 Item.Destination = Needed.Destination);
            end loop;
            if not Found then Diagnostic := Missing_Bootstrap_Dependency; return; end if;
         end if;
      end loop;
      if not Provider_Found then Diagnostic := Unknown_Provider; return; end if;
      for Item of Profile.Placements (1 .. Profile.Placement_Count) loop
         Found := False;
         for Input of Catalog.Artifacts (1 .. Catalog.Artifact_Count) loop
            if Item.Artifact_Name = Input.Name then
               Found := True;
               if Input.Kind = Supplied_Tree and then
                 (Item.Role /= Content or else Item.Region /= Optical)
               then Diagnostic := Invalid_Placement; return; end if;
            end if;
         end loop;
         if not Found then Diagnostic := Unknown_Artifact; return; end if;
         if (Profile.Layout = Bootstrap_Only and then Item.Region /= Bootstrap)
           or else (Item.Region = Optical and then Overlaps (Item.Destination, Archive_Path))
           or else (Item.Role = Content and then Item.Region = Bootstrap and then
             Value (Item.Destination) in "init.ccl" | "system.ccl")
         then Diagnostic := Invalid_Placement; return; end if;
      end loop;
   end Validate;
end CCL.Images;
