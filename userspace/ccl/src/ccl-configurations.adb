with Interfaces; use Interfaces;
with CCL.Language;
with CCL.VM;

package body CCL.Configurations with SPARK_Mode => On is
   use CCL.Declarations;
   use type CCL.VM.Value_Kind;
   function Diagnostic_Name (Code : Diagnostic_Code) return String is
   begin
      case Code is
         when No_Error => return "NO_ERROR";
         when Invalid_Syntax => return "INVALID_SYNTAX";
         when Unsupported_Version => return "UNSUPPORTED_VERSION";
         when Unknown_Declaration => return "UNKNOWN_DECLARATION";
         when Invalid_Key => return "INVALID_KEY";
         when Invalid_Value => return "INVALID_VALUE";
         when Duplicate_Key => return "DUPLICATE_KEY";
         when Too_Many_Entries => return "TOO_MANY_ENTRIES";
         when Invalid_Executable => return "INVALID_EXECUTABLE";
         when Invalid_Priority => return "INVALID_PRIORITY";
         when Invalid_Approval => return "INVALID_APPROVAL";
         when Invalid_Role => return "INVALID_ROLE";
         when Duplicate_Field => return "DUPLICATE_FIELD";
         when Missing_Field => return "MISSING_FIELD";
         when Trailing_Input => return "TRAILING_INPUT";
      end case;
   end Diagnostic_Name;

   procedure Compile (Source : String; Result : out Compilation_Result) is
      Reader : Scanner;
      Name : Symbol;
      Kind : Profile_Kind := System_Profile;
      Value : CCL.Language.Interpretation_Result;
      Count : Natural range 0 .. 128 := 0;
      Have_Storage : Boolean := False;

      procedure Fail (Code : Diagnostic_Code) is
      begin
         if Result.Diagnostic = No_Error then
            Result.Diagnostic := Code;
            Result.Position := Position (Reader);
         end if;
      end Fail;

      function Stopped return Boolean is
        (Result.Diagnostic /= No_Error or else Failed (Reader));

      procedure Store_Value (Text : String) is
      begin
         Result.Plan.Settings (Count).Value.Length := Text'Length;
         Result.Plan.Settings (Count).Value.Data (1 .. Text'Length) := Text;
      end Store_Value;

      procedure Integer_Field (Low, High : Integer_64; Number : out Integer_64) is
      begin
         Number := Low;
         Evaluate (Reader, Value);
         if Stopped then return; end if;
         if not CCL.Language.Has_Scalar (Value)
           or else Value.Result_Value.Kind /= CCL.VM.Integer_Value
           or else Value.Result_Value.Integer not in Low .. High
         then Fail (Invalid_Value);
         else Number := Value.Result_Value.Integer;
         end if;
      end Integer_Field;

      procedure Read_Key (Key : out Key_Text; Executable : Boolean := False) is
      begin
         Key := (others => <>);
         Evaluate (Reader, Value);
         if Stopped then return; end if;
         if not Value.Has_Text or else Value.Result_Text.Length not in
           1 .. (if Executable then 64 else 128)
         then
            Fail ((if Executable then Invalid_Executable else Invalid_Key)); return;
         end if;
         for C of Value.Result_Text.Data (1 .. Value.Result_Text.Length) loop
            if C not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_' then
               Fail ((if Executable then Invalid_Executable else Invalid_Key));
            end if;
         end loop;
         Key.Length := Value.Result_Text.Length;
         Key.Data (1 .. Key.Length) := Value.Result_Text.Data (1 .. Key.Length);
         if Executable and then (Key.Data (1) = '.' or else Key.Data (Key.Length) = '.') then
            Fail (Invalid_Executable);
         end if;
      end Read_Key;

      procedure Setting is
         Key : Key_Text;
      begin
         Read_Key (Key);
         for Old of Result.Plan.Settings (1 .. Count) loop
            if Key = Old.Key then Fail (Duplicate_Key); end if;
         end loop;
         if Count = MAX_SETTINGS then Fail (Too_Many_Entries); end if;
         if Stopped then return; end if;
         Count := Count + 1;
         Result.Plan.Settings (Count).Key := Key;
         Result.Plan.Setting_Count := Count;
         Evaluate (Reader, Value);
         if Stopped then return; end if;
         if Value.Has_Text then
            --  Nonempty printable values fit devmgr's one-page seed buffer.
            --  CR/LF/NUL cannot inject a second config entry.
            if Value.Result_Text.Length = 0 then Fail (Invalid_Value); end if;
            for C of Value.Result_Text.Data (1 .. Value.Result_Text.Length) loop
               if C not in ' ' .. '~' then Fail (Invalid_Value); end if;
            end loop;
            Store_Value (Value.Result_Text.Data (1 .. Value.Result_Text.Length));
         elsif CCL.Language.Has_Scalar (Value) then
            case Value.Result_Value.Kind is
               when CCL.VM.Integer_Value =>
                  declare
                     Text : constant String := Value.Result_Value.Integer'Image;
                  begin
                     Store_Value (Text ((if Text (Text'First) = ' ' then Text'First + 1
                                  else Text'First) .. Text'Last));
                  end;
               when CCL.VM.Boolean_Value =>
                  Store_Value ((if Value.Result_Value.Boolean then "true" else "false"));
               when CCL.VM.Variant_Value | CCL.VM.Object_Value | CCL.VM.Resource_Value =>
                  Fail (Invalid_Value);
            end case;
         else Fail (Invalid_Value);
         end if;
      end Setting;

      procedure Launch is
         Executable : Key_Text;
         Have_Priority, Have_Network, Have_Role : Boolean := False;
         Priority : Integer_64 := 5;
         Approval : Network_Approval := Deny;
         Role : Startup_Role := Application;
      begin
         if Count = 16 then Fail (Too_Many_Entries); return; end if;
         Read_Key (Executable, True);
         while not Stopped and then not At_Close (Reader) and then not At_End (Reader) loop
            Open_Form (Reader);
            Read_Symbol (Reader, Name);
            if Matches (Name, "priority") then
               if Have_Priority then Fail (Duplicate_Field); end if;
               Integer_Field (1, 10, Priority);
               if Result.Diagnostic = Invalid_Value then Result.Diagnostic := Invalid_Priority; end if;
               Have_Priority := True;
            elsif Matches (Name, "network") then
               if Have_Network then Fail (Duplicate_Field); end if;
               Read_Symbol (Reader, Name);
               if Matches (Name, "deny") then Approval := Deny;
               elsif Matches (Name, "approve-declared") then Approval := Approve_Declared;
               else Fail (Invalid_Approval);
               end if;
               Have_Network := True;
            elsif Matches (Name, "role") then
               if Have_Role then Fail (Duplicate_Field); end if;
               Read_Symbol (Reader, Name);
               if Matches (Name, "application") then Role := Application;
               elsif Matches (Name, "config-storage") then
                  if Have_Storage then Fail (Invalid_Role); end if;
                  Role := Config_Storage;
                  Have_Storage := True;
               else Fail (Invalid_Role);
               end if;
               Have_Role := True;
            else Fail (Unknown_Declaration);
            end if;
            Close_Form (Reader);
         end loop;
         if not Have_Priority then Fail (Missing_Field); end if;
         if Stopped then return; end if;
         Count := Count + 1;
         Result.Plan.Launch_Count := Count;
         Result.Plan.Launches (Count) :=
           (Executable => (Length => Executable.Length,
                           Data => Executable.Data (1 .. 64)),
            Priority => Startup_Priority (Priority), Approval => Approval, Role => Role);
      end Launch;
   begin
      Result := (others => <>);
      Start (Reader, Source);
      Open_Form (Reader);
      Read_Symbol (Reader, Name);
      if Matches (Name, "system-config") then Kind := System_Profile;
      elsif Matches (Name, "startup") then Kind := Startup_Profile;
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
      while not Stopped and then not At_Close (Reader) and then not At_End (Reader) loop
         Open_Form (Reader);
         Read_Symbol (Reader, Name);
         if Kind = System_Profile and then Matches (Name, "setting") then Setting;
         elsif Kind = Startup_Profile and then Matches (Name, "start") then Launch;
         else Fail (Unknown_Declaration);
         end if;
         Close_Form (Reader);
      end loop;
      Close_Form (Reader);
      if not Stopped and then not At_End (Reader) then Fail (Trailing_Input); end if;
      --  Require an explicit nonempty boot profile.
      if Count = 0 then Fail (Missing_Field); end if;
      if Failed (Reader) and then Result.Diagnostic = No_Error then
         Result.Diagnostic := Invalid_Syntax;
         Result.Syntax_Diagnostic := Diagnostic (Reader);
         Result.Position := Position (Reader);
      end if;
      Result.Plan.Kind := Kind;
      Result.Success := not Stopped;
      if not Result.Success then
         Result.Plan := (others => <>);
      end if;
   end Compile;
end CCL.Configurations;
