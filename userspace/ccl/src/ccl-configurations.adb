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
         when Invalid_Dependency => return "INVALID_DEPENDENCY";
         when Invalid_Launch_Mode => return "INVALID_LAUNCH_MODE";
         when Invalid_Scheduling => return "INVALID_SCHEDULING";
         when Invalid_Deadline => return "INVALID_DEADLINE";
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
               when CCL.VM.Variant_Value | CCL.VM.Object_Value | CCL.VM.Resource_Value |
                    CCL.VM.Text_Value | CCL.VM.Character_Value | CCL.VM.List_Value |
                    CCL.VM.Function_Value =>
                  Fail (Invalid_Value);
            end case;
         else Fail (Invalid_Value);
         end if;
      end Setting;

      --  (KEYWORD n) inside a field, as in (budget-us 1500).
      procedure Keyed_Integer
        (Keyword : String; Low, High : Integer_64; Code : Diagnostic_Code;
         Number : out Integer_64)
      is
      begin
         Number := Low;
         Open_Form (Reader);
         Read_Symbol (Reader, Name);
         if not Stopped and then not Matches (Name, Keyword) then Fail (Code); end if;
         Integer_Field (Low, High, Number);
         if Result.Diagnostic = Invalid_Value then Result.Diagnostic := Code; end if;
         Close_Form (Reader);
      end Keyed_Integer;

      procedure Launch is
         Executable : Key_Text;
         Have_Priority, Have_Network, Have_Role, Have_Mode, Have_Device,
           Have_Scheduling, Have_Deadline, Have_Render : Boolean := False;
         Priority : Integer_64 := 5;
         Approval : Network_Approval := Deny;
         Role : Startup_Role := Application;
         Item : Launch_Entry;
         Number : Integer_64;

         procedure Dependency is
            Target : Key_Text;
            Found : Launch_Number := 0;
         begin
            Read_Key (Target, True);
            if Stopped then return; end if;
            --  An earlier entry, named exactly once so the reference is
            --  unambiguous.
            for Index in 1 .. Count loop
               declare
                  Earlier : Executable_Text renames Result.Plan.Launches (Index).Executable;
               begin
                  if Earlier.Data (1 .. Earlier.Length) = Target.Data (1 .. Target.Length) then
                     if Found /= 0 then Fail (Invalid_Dependency); return; end if;
                     Found := Index;
                  end if;
               end;
            end loop;
            if Found = 0 then Fail (Invalid_Dependency); return; end if;
            for Existing of Item.Dependencies (1 .. Item.Dependency_Total) loop
               if Existing = Found then Fail (Invalid_Dependency); return; end if;
            end loop;
            if Item.Dependency_Total = MAX_DEPENDENCIES then
               Fail (Invalid_Dependency); return;
            end if;
            Item.Dependency_Total := Item.Dependency_Total + 1;
            Item.Dependencies (Item.Dependency_Total) := Found;
         end Dependency;
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
            elsif Matches (Name, "render") then
               if Have_Render then Fail (Duplicate_Field); end if;
               Read_Symbol (Reader, Name);
               if Matches (Name, "deny") then Item.Approve_Render := False;
               elsif Matches (Name, "approve-declared") then
                  Item.Approve_Render := True;
               else Fail (Invalid_Approval);
               end if;
               Have_Render := True;
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
            elsif Matches (Name, "launch") then
               if Have_Mode then Fail (Duplicate_Field); end if;
               Read_Symbol (Reader, Name);
               if Matches (Name, "at-startup") then Item.Mode := At_Startup;
               elsif Matches (Name, "per-device") then Item.Mode := Per_Device;
               else Fail (Invalid_Launch_Mode);
               end if;
               Have_Mode := True;
            elsif Matches (Name, "approve-device") then
               if Have_Device then Fail (Duplicate_Field); end if;
               Item.Approve_Device := True;
               Have_Device := True;
            elsif Matches (Name, "after") then
               --  (after "a.svc" "b.drv" ...)
               if Item.Dependency_Total > 0 then Fail (Duplicate_Field); end if;
               loop
                  Dependency;
                  exit when Stopped or else At_Close (Reader) or else At_End (Reader);
               end loop;
            elsif Matches (Name, "approve-scheduling") then
               --  (approve-scheduling realtime (budget-us b) (period-us p))
               if Have_Scheduling then Fail (Duplicate_Field); end if;
               Read_Symbol (Reader, Name);
               if not Stopped and then not Matches (Name, "realtime") then
                  Fail (Invalid_Scheduling);
               end if;
               Keyed_Integer ("budget-us", 1, CCL.Scheduling_Limits.MAX_MICROSECONDS,
                              Invalid_Scheduling, Number);
               if not Stopped then Item.Scheduling_Budget := Number; end if;
               Keyed_Integer ("period-us", 1, CCL.Scheduling_Limits.MAX_MICROSECONDS,
                              Invalid_Scheduling, Number);
               if not Stopped then Item.Scheduling_Period := Number; end if;
               if not Stopped and then not CCL.Scheduling_Limits.Admissible
                 (Item.Scheduling_Budget, Item.Scheduling_Period)
               then
                  Fail (Invalid_Scheduling);
               end if;
               Item.Approve_Scheduling := True;
               Have_Scheduling := True;
            elsif Matches (Name, "ready-deadline-ms") then
               if Have_Deadline then Fail (Duplicate_Field); end if;
               Integer_Field (1, MAX_READY_DEADLINE_MS, Number);
               if Result.Diagnostic = Invalid_Value then Result.Diagnostic := Invalid_Deadline; end if;
               if not Stopped then Item.Ready_Deadline_Ms := Ready_Deadline (Number); end if;
               Item.Has_Ready_Deadline := True;
               Have_Deadline := True;
            else Fail (Unknown_Declaration);
            end if;
            Close_Form (Reader);
         end loop;
         if not Have_Priority then Fail (Missing_Field); end if;
         --  A per-device driver exists to receive device resources, and only
         --  a per-device driver may be granted them.
         if Item.Mode = Per_Device xor Item.Approve_Device then Fail (Invalid_Launch_Mode); end if;
         if Item.Mode = Per_Device and then Role /= Application then Fail (Invalid_Role); end if;
         if Stopped then return; end if;
         Count := Count + 1;
         Result.Plan.Launch_Count := Count;
         Item.Executable := (Length => Executable.Length, Data => Executable.Data (1 .. 64));
         Item.Priority := Startup_Priority (Priority);
         Item.Approval := Approval;
         Item.Role := Role;
         Result.Plan.Launches (Count) := Item;
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
