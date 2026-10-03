with CCL.VM;
with CCL.Declarations;
with CCL.Scheduling_Limits;
with CCL.Resource_Sections;
with CuBit.Network_Authority;
with CuBit.TLS_Scopes;

with CCL.Manifests.Encoding;

--  The v1 keyword reader, kept until every manifest is typed
--  (docs/ccl-typed-manifests.md). Its catalog reader stays after that.
package body CCL.Manifests.Keywords with SPARK_Mode => On is
   use Interfaces;
   use Model;
   use all type CCL.Resource_Sections.Match_Kind;
   use all type CCL.Resource_Sections.Platform_Device;
   use all type CCL.Resource_Sections.Interrupt_Mode;
   use type CCL.Language.Interpretation_Status;
   use type CCL.VM.Value_Kind;


   procedure Run
     (Source, Catalog_Source : String; Catalog_Only : Boolean;
      Result : out Compilation_Result; Catalog_Out : out Model.Catalog_Model)
   is
      Text : String (1 .. MAX_DECLARATION_LENGTH) := [others => ' '];
      Last : Natural range 0 .. MAX_DECLARATION_LENGTH := 0;
      Cursor : Positive range 1 .. MAX_DECLARATION_LENGTH + 1 := 1;
      Failed : Boolean := False;
      Decl : Model.Declaration;
      Cat : Model.Catalog_Model;
      Identity : Metadata_Text renames Decl.Identity;
      Version : Metadata_Text renames Decl.Version;
      Have_Identity, Have_Version : Boolean := False;
      Requests : Request_Array renames Decl.Requests;
      Count : Request_Count renames Decl.Count;
      Services : Service_Array renames Cat.Services;
      Service_Count : Request_Count renames Cat.Service_Count;
      First_Slot : Slot_Number renames Cat.First_Slot;
      Last_Slot : Slot_Number renames Cat.Last_Slot;
      Have_Slots : Boolean := False;
      Fixed : Binding_Array renames Cat.Fixed;
      Fixed_Count : Natural renames Cat.Fixed_Count;
      Scopes : Scope_Array renames Decl.Scopes;
      Scope_Count : Scope_Count_Type renames Decl.Scope_Count;
      Streams : Stream_Array renames Decl.Streams;
      Stream_Order : Stream_Order_Array renames Decl.Stream_Order;
      Stream_Count : Stream_Count_Type renames Decl.Stream_Count;
      Explicit_No_Requests : Boolean renames Decl.Explicit_No_Requests;
      Match : Match_Kind renames Decl.Match;
      Match_Values : Match_Value_Array renames Decl.Match_Values;
      Value : CCL.Language.Interpretation_Result;

      procedure Fail (Code : Diagnostic_Code; At_Position : Natural) is
      begin
         if not Failed then
            Failed := True;
            Result.Diagnostic := Code;
            Result.Position := At_Position;
         end if;
      end Fail;

      function Space (C : Character) return Boolean is
        (C = ' ' or else C = ASCII.HT or else C = ASCII.LF or else C = ASCII.CR);

      procedure Skip is
      begin
         while Cursor <= Last loop
            if Space (Text (Cursor)) then
               Cursor := Cursor + 1;
            elsif Text (Cursor) = '#' then
               while Cursor <= Last and then Text (Cursor) /= ASCII.LF loop
                  Cursor := Cursor + 1;
               end loop;
            else
               exit;
            end if;
         end loop;
      end Skip;

      procedure Expect (C : Character) is
      begin
         Skip;
         if Cursor > Last then
            Fail (Unexpected_End, Cursor);
         elsif Text (Cursor) /= C then
            Fail (Expected_Form, Cursor);
         else
            Cursor := Cursor + 1;
         end if;
      end Expect;

      procedure Atom (Item : out Metadata_Text) is
         First : Positive;
      begin
         Item := (others => <>);
         Skip;
         First := Cursor;
         while Cursor <= Last and then not Space (Text (Cursor))
           and then Text (Cursor) /= '(' and then Text (Cursor) /= ')'
           and then Text (Cursor) /= '#'
         loop
            Cursor := Cursor + 1;
         end loop;
         if Cursor = First or else Cursor - First > Item.Data'Length then
            Fail (Expected_Form, First);
         else
            Item.Length := Cursor - First;
            Item.Data (1 .. Item.Length) := Text (First .. Cursor - 1);
         end if;
      end Atom;

      function Is_Text (Item : Metadata_Text; Expected : String) return Boolean is
        (Item.Data (1 .. Item.Length) = Expected);

      --  Delimit one ordinary CCL expression, preserving its source position.
      --  This scanner does not implement another evaluator or string decoder.
      procedure Expression is
         First : Positive;
         Depth : Natural range 0 .. CCL.Language.MAX_NESTING := 0;
         Quoted, Escaped, Comment : Boolean := False;
         C : Character;
      begin
         Skip;
         First := Cursor;
         while Cursor <= Last and then not Failed loop
            C := Text (Cursor);
            if Comment then
               if C = ASCII.LF then Comment := False; end if;
            elsif Quoted then
               if Escaped then Escaped := False;
               elsif C = '\' then Escaped := True;
               elsif C = '"' then Quoted := False;
               end if;
            elsif Depth = 0 and then
              (C = ')' or else Space (C) or else C = '#')
            then
               exit;
            elsif C = '#' then Comment := True;
            elsif C = '"' then Quoted := True;
            elsif C = '(' then
               if Depth = CCL.Language.MAX_NESTING then
                  Fail (Nesting_Too_Deep, Cursor);
               else Depth := Depth + 1;
               end if;
            elsif C = ')' then Depth := Depth - 1;
            end if;
            Cursor := Cursor + 1;
         end loop;
         if Failed then return; end if;
         CCL.Language.Interpret (Text (First .. Cursor - 1), 1_024, Value);
         if Value.Status /= CCL.Language.Succeeded then
            Fail (Invalid_Expression, First);
            Result.Expression_Diagnostic := Value.Diagnostic;
            if Value.Diagnostic_Position > 0 then
               Result.Position := First + Value.Diagnostic_Position - 1;
            end if;
         end if;
      end Expression;

      procedure Text_Field (Item : out Metadata_Text) is
      begin
         Item := (others => <>);
         Expression;
         if Failed then return; end if;
         if not Value.Has_Text then
            Fail (Expected_Text, Cursor);
         elsif not Valid_Metadata_Text (Value.Result_Text.Data (1 .. Value.Result_Text.Length)) then
            Fail (Invalid_Text, Cursor);
         else
            Item.Length := Value.Result_Text.Length;
            Item.Data (1 .. Item.Length) := Value.Result_Text.Data (1 .. Item.Length);
         end if;
      end Text_Field;

      procedure Read_Rights
        (Rights : out Rights_Kind; Kind : Request_Kind := Service_Request)
      is
         Name : Metadata_Text;
      begin
         Rights := Read_Only;
         Atom (Name);
         if Kind = Notification_Request then
            if Is_Text (Name, "publish") then Rights := Read_Only;
            elsif Is_Text (Name, "manage") then Rights := Write_Only;
            elsif Is_Text (Name, "publish-and-manage") then Rights := Read_Write;
            else Fail (Unknown_Rights, Cursor);
            end if;
         elsif Is_Text (Name, "read") then Rights := Read_Only;
         elsif Is_Text (Name, "write") then Rights := Write_Only;
         elsif Is_Text (Name, "read-write") then Rights := Read_Write;
         else Fail (Unknown_Rights, Cursor);
         end if;
      end Read_Rights;

      procedure Read_Binding_Name (Name : out Binding_Name) is
      begin
         Atom (Name);
         if Failed then return; end if;
         if not Valid_Binding_Name (Name.Data (1 .. Name.Length)) then
            Fail (Invalid_Binding_Name, Cursor);
         end if;
      end Read_Binding_Name;

      procedure Store_Request (Item : in out Request; Named : Boolean := False) is
      begin
         if not Named then Read_Binding_Name (Item.Name); end if;
         for Index in 1 .. Count loop
            if Requests (Index).Name = Item.Name then Fail (Duplicate_Binding, Cursor); end if;
         end loop;
         if Count = MAX_REQUESTS then Fail (Too_Many_Requests, Cursor);
         elsif not Failed then
            Count := Count + 1;
            Requests (Count) := Item;
         end if;
      end Store_Request;

      procedure Add_Request (Kind : Request_Kind) is
         Name : Metadata_Text;
         Item : Request;
         Offered : Rights_Kind := Read_Only;
         Found : Boolean := False;
      begin
         Item.Kind := Kind;
         if Kind = Render_Request then
            -- One admitted session per launch. Distinct binding names must
            -- not disguise duplicate requests that procmgr will reject.
            for Existing of Requests (1 .. Count) loop
               if Existing.Kind = Render_Request then
                  Fail (Duplicate_Field, Cursor);
               end if;
            end loop;
         end if;
         if Kind not in Framebuffer_Request | Render_Request then
            Atom (Name);
            for Binding of Services (1 .. Service_Count) loop
               if Name = Binding.Name and then Kind = Binding.Kind then
                  Item.Service := Binding.ID;
                  Offered := Binding.Rights;
                  Found := True;
               end if;
            end loop;
            if not Found then
               Fail ((if Kind = Notification_Request then Unknown_Notification
                      else Unknown_Service), Cursor);
            end if;
         end if;
         Read_Rights (Item.Rights, Kind);
         if Kind = Render_Request and then Item.Rights /= Read_Write then
            Fail (Unknown_Rights, Cursor);
         end if;
         if Kind not in Framebuffer_Request | Render_Request and then Offered /= Read_Write
           and then Item.Rights /= Offered
         then
            Fail (Rights_Not_Offered, Cursor);
         end if;
         Store_Request (Item);
      end Add_Request;

      procedure Network_Integer
        (Low, High : Integer_64; Number : out Integer_64)
      is
      begin
         Number := Low;
         Expression;
         if Failed then return; end if;
         if not CCL.Language.Has_Scalar (Value)
           or else Value.Result_Value.Kind /= CCL.VM.Integer_Value
           or else Value.Result_Value.Integer not in Low .. High
         then
            Fail (Invalid_Network_Scope, Cursor);
         else
            Number := Value.Result_Value.Integer;
         end if;
      end Network_Integer;

      procedure Add_Network is
         Item : Request;
         Name : Metadata_Text;
         Number : Integer_64;
         Octet : Natural range 0 .. 255 := 0;
         Digit_Count : Natural range 0 .. 3 := 0;
         Octets : Natural range 0 .. 4 := 0;
      begin
         Item.Kind := Network_Request;
         Item.Rights := Read_Write;
         Atom (Name);
         if Is_Text (Name, "tcp-connect") then
            Item.Network.Action := CuBit.Network_Authority.Connect_TCP;
         elsif Is_Text (Name, "tcp-listen") then
            Item.Network.Action := CuBit.Network_Authority.Listen_TCP;
         elsif Is_Text (Name, "udp-connect") then
            Item.Network.Action := CuBit.Network_Authority.Connect_UDP;
         else
            Fail (Invalid_Network_Scope, Cursor); return;
         end if;
         Expect ('(');
         Atom (Name);
         if not Is_Text (Name, "ipv4") then Fail (Invalid_Network_Scope, Cursor); end if;
         Expression;
         if Failed then return; end if;
         if not Value.Has_Text or else Value.Result_Text.Length not in 7 .. 15 then
            Fail (Invalid_Network_Scope, Cursor); return;
         end if;
         --  Canonical dotted decimal only: no shorthand, octal, or truncation.
         for C of Value.Result_Text.Data (1 .. Value.Result_Text.Length) loop
            if C = '.' then
               if Digit_Count = 0 or else Octets = 3 then
                  Fail (Invalid_Network_Scope, Cursor); return;
               end if;
               Item.Network.Network := Shift_Left (Item.Network.Network, 8) or Unsigned_32 (Octet);
               Octets := Octets + 1;
               Octet := 0;
               Digit_Count := 0;
            elsif C in '0' .. '9' then
               if Digit_Count = 3 or else (Digit_Count > 0 and then Octet = 0)
                 or else Octet * 10 + Character'Pos (C) - Character'Pos ('0') > 255
               then
                  Fail (Invalid_Network_Scope, Cursor); return;
               end if;
               Octet := Octet * 10 + Character'Pos (C) - Character'Pos ('0');
               Digit_Count := Digit_Count + 1;
            else
               Fail (Invalid_Network_Scope, Cursor); return;
            end if;
         end loop;
         if Octets /= 3 or else Digit_Count = 0 then
            Fail (Invalid_Network_Scope, Cursor); return;
         end if;
         Item.Network.Network := Shift_Left (Item.Network.Network, 8) or Unsigned_32 (Octet);
         Network_Integer (0, 32, Number);
         Item.Network.Prefix := Natural (Number);
         Expect (')');
         Expect ('(');
         Atom (Name);
         if not Is_Text (Name, "ports") then Fail (Invalid_Network_Scope, Cursor); end if;
         Network_Integer (1, 65_535, Number);
         Item.Network.First_Port := Unsigned_16 (Number);
         Network_Integer (1, 65_535, Number);
         Item.Network.Last_Port := Unsigned_16 (Number);
         Expect (')');
         Expect ('(');
         Atom (Name);
         if not Is_Text (Name, "dns") then Fail (Invalid_Network_Scope, Cursor); end if;
         Atom (Name);
         if Is_Text (Name, "allow") then Item.Network.Resolve_Names := True;
         elsif Is_Text (Name, "deny") then Item.Network.Resolve_Names := False;
         else Fail (Invalid_Network_Scope, Cursor);
         end if;
         Expect (')');
         Expect ('(');
         Atom (Name);
         if not Is_Text (Name, "connections") then Fail (Invalid_Network_Scope, Cursor); end if;
         Network_Integer
           (1, Integer_64 (CuBit.Network_Authority.Connection_Count'Last), Number);
         Item.Network.Connections := CuBit.Network_Authority.Connection_Count (Number);
         Expect (')');
         if not CuBit.Network_Authority.Valid (Item.Network) then
            Fail (Invalid_Network_Scope, Cursor);
         end if;
         Store_Request (Item);
      end Add_Network;

      procedure Integer_Field
        (Low, High : Integer_64; Code : Diagnostic_Code; Number : out Integer_64)
      is
      begin
         Number := Low;
         Expression;
         if Failed then return; end if;
         if not CCL.Language.Has_Scalar (Value)
           or else Value.Result_Value.Kind /= CCL.VM.Integer_Value
           or else Value.Result_Value.Integer not in Low .. High
         then
            Fail (Code, Cursor);
         else
            Number := Value.Result_Value.Integer;
         end if;
      end Integer_Field;

      --  (KEYWORD n): one named integer, as in (bar 0) or (bytes 4096).
      procedure Keyed_Integer
        (Keyword : String; Low, High : Integer_64; Code : Diagnostic_Code;
         Number : out Integer_64)
      is
         Name : Metadata_Text;
      begin
         Number := Low;
         Expect ('(');
         Atom (Name);
         if not Failed and then not Is_Text (Name, Keyword) then Fail (Code, Cursor); end if;
         Integer_Field (Low, High, Code, Number);
         Expect (')');
      end Keyed_Integer;

      procedure Add_Match (Kind : Match_Kind) is
         Name : Metadata_Text;
         Number : Integer_64;
      begin
         if Match /= No_Match then Fail (Duplicate_Device_Match, Cursor); return; end if;
         Match := Kind;
         case Kind is
            when PCI_Class_Match =>
               --  class, subclass, programming interface
               for Index in Match_Values'Range loop
                  Integer_Field (0, PCI_CODE_LAST, Invalid_Device_Match, Number);
                  Match_Values (Index) := Unsigned_16 (Number);
               end loop;
            when PCI_ID_Match =>
               for Index in 1 .. 2 loop
                  Integer_Field (0, PCI_ID_LAST, Invalid_Device_Match, Number);
                  Match_Values (Index) := Unsigned_16 (Number);
               end loop;
               --  0xFFFF is "no device" in PCI configuration space.
               if Match_Values (1) = PCI_ID_LAST then Fail (Invalid_Device_Match, Cursor); end if;
            when Platform_Match =>
               Atom (Name);
               if Is_Text (Name, "ps2-controller") then
                  Match_Values (1) := Unsigned_16 (Platform_Device'Enum_Rep (PS2_Controller));
               elsif Is_Text (Name, "ata-primary") then
                  Match_Values (1) := Unsigned_16 (Platform_Device'Enum_Rep (ATA_Primary));
               elsif Is_Text (Name, "cmos-rtc") then
                  Match_Values (1) := Unsigned_16 (Platform_Device'Enum_Rep (CMOS_RTC));
               else Fail (Invalid_Device_Match, Cursor);
               end if;
            when No_Match => Fail (Invalid_Device_Match, Cursor);
         end case;
      end Add_Match;

      --  (bar n) for a PCI device, (resource n) for a platform device.
      procedure Resource_Index (Item : in out Request) is
         Number : Integer_64;
      begin
         if Match = Platform_Match then
            Keyed_Integer ("resource", 0, MAX_PLATFORM_RESOURCES - 1,
                           Invalid_Device_Resource, Number);
         else
            Keyed_Integer ("bar", 0, PCI_BAR_COUNT - 1, Invalid_Device_Resource, Number);
         end if;
         Item.Index := Unsigned_32 (Number);
      end Resource_Index;

      procedure Add_Resource (Kind : Device_Request) is
         Item : Request;
         Name : Metadata_Text;
         Number : Integer_64;
      begin
         --  Resources are relative to the matched device, so the match
         --  must come first.
         if Match = No_Match then Fail (Missing_Device_Match, Cursor); return; end if;
         Item.Kind := Kind;
         Read_Binding_Name (Item.Name);
         case Kind is
            when Device_Memory_Request =>
               Resource_Index (Item);
               Keyed_Integer ("max-bytes", PAGE_BYTES, MAX_DEVICE_MEMORY_BYTES,
                              Invalid_Device_Resource, Number);
               if Number mod PAGE_BYTES /= 0 then Fail (Invalid_Device_Resource, Cursor); end if;
               Item.Amount := Unsigned_64 (Number);
               Read_Rights (Item.Rights);
               if Item.Rights = Write_Only then Fail (Invalid_Device_Resource, Cursor); end if;
            when IO_Port_Request =>
               Resource_Index (Item);
               Keyed_Integer ("count", 1, IO_PORT_SPACE, Invalid_Device_Resource, Number);
               Item.Amount := Unsigned_64 (Number);
               Item.Rights := Read_Write;
            when Interrupt_Request =>
               Item.Rights := Read_Only;
               if Match = Platform_Match then
                  Item.Extra := Unsigned_64 (Interrupt_Mode'Enum_Rep (Platform_Line));
                  Keyed_Integer ("resource", 0, MAX_PLATFORM_RESOURCES - 1,
                                 Invalid_Device_Resource, Number);
                  Item.Index := Unsigned_32 (Number);
                  Item.Amount := 1;
               else
                  Atom (Name);
                  if Is_Text (Name, "msix") then
                     Item.Extra := Unsigned_64 (Interrupt_Mode'Enum_Rep (MSI_X));
                  elsif Is_Text (Name, "msi") then
                     Item.Extra := Unsigned_64 (Interrupt_Mode'Enum_Rep (MSI));
                  elsif Is_Text (Name, "line") then
                     Item.Extra := Unsigned_64 (Interrupt_Mode'Enum_Rep (Line));
                  else Fail (Invalid_Device_Resource, Cursor); return;
                  end if;
                  if Is_Text (Name, "line") then
                     Item.Amount := 1;
                  else
                     Keyed_Integer ("vectors", 1, MAX_INTERRUPT_VECTORS,
                                    Invalid_Device_Resource, Number);
                     Item.Amount := Unsigned_64 (Number);
                  end if;
               end if;
            when DMA_Request =>
               Keyed_Integer ("bytes", PAGE_BYTES, MAX_DMA_BYTES,
                              Invalid_Device_Resource, Number);
               if Number mod PAGE_BYTES /= 0 then Fail (Invalid_Device_Resource, Cursor); end if;
               Item.Amount := Unsigned_64 (Number);
               Item.Rights := Read_Write;
         end case;
         if Failed then return; end if;
         Store_Request (Item, Named => True);
      end Add_Resource;

      --  (request-scheduling NAME realtime (budget-us b) (period-us p)).
      --  Rejects shapes the kernel's admission can never accept.
      procedure Add_Scheduling is
         Item : Request;
         Name : Metadata_Text;
         Budget, Period : Integer_64;
      begin
         Item.Kind := Scheduling_Request;
         Item.Rights := Read_Only;
         Read_Binding_Name (Item.Name);
         Atom (Name);
         if not Failed and then not Is_Text (Name, "realtime") then
            Fail (Invalid_Scheduling, Cursor);
         end if;
         Keyed_Integer ("budget-us", 1, CCL.Scheduling_Limits.MAX_MICROSECONDS,
                        Invalid_Scheduling, Budget);
         Keyed_Integer ("period-us", 1, CCL.Scheduling_Limits.MAX_MICROSECONDS,
                        Invalid_Scheduling, Period);
         if Failed then return; end if;
         if not CCL.Scheduling_Limits.Admissible (Budget, Period) then
            Fail (Invalid_Scheduling, Cursor);
            return;
         end if;
         Item.Amount := Unsigned_64 (Budget);
         Item.Extra := Unsigned_64 (Period);
         Store_Request (Item, Named => True);
      end Add_Scheduling;

      procedure Slot_Expression (Slot : out Slot_Number) is
      begin
         Slot := Slot_Number'First;
         Expression;
         if Failed then return; end if;
         if not CCL.Language.Has_Scalar (Value)
           or else Value.Result_Value.Kind /= CCL.VM.Integer_Value
           or else Value.Result_Value.Integer not in
             Integer_64 (Slot_Number'First) .. Integer_64 (Slot_Number'Last)
         then Fail (Invalid_Slot, Cursor);
         else Slot := Natural (Value.Result_Value.Integer);
         end if;
      end Slot_Expression;


      procedure Add_Scope (Domain : Access_Domain) is
         Item : Scope;
         Name : Metadata_Text;
         Right : Access_Right;
         Any_Right : Boolean := False;
      begin
         Item.Domain := Domain;
         Expect ('(');
         Atom (Name);
         if not Is_Text (Name, "rights") then Fail (Invalid_Access_Rights, Cursor); end if;
         loop
            Skip;
            exit when Failed or else Cursor > Last or else Text (Cursor) = ')';
            Atom (Name);
            if Is_Text (Name, "read") then Right := Read_File;
            elsif Is_Text (Name, "write") then Right := Write_File;
            elsif Is_Text (Name, "execute") then Right := Execute_File;
            elsif Is_Text (Name, "create") then Right := Create_File;
            else Fail (Invalid_Access_Rights, Cursor); return;
            end if;
            if Domain = Config_Domain and then Right in Execute_File | Create_File then
               Fail (Invalid_Access_Rights, Cursor);
            end if;
            if Item.Rights (Right) then Fail (Invalid_Access_Rights, Cursor); end if;
            Item.Rights (Right) := True;
            Any_Right := True;
         end loop;
         Expect (')');
         if not Any_Right then Fail (Invalid_Access_Rights, Cursor); end if;
         Skip;
         if Cursor + 2 <= Last and then Text (Cursor .. Cursor + 2) = "all" then
            --  Broad access is deliberately spelled out, never an empty string.
            Atom (Name);
            if not Is_Text (Name, "all") then Fail (Invalid_Path, Cursor); end if;
         else
            Expression;
            if Failed then return; end if;
            if not Value.Has_Text or else Value.Result_Text.Length not in 1 .. 64 then
               Fail (Invalid_Path, Cursor); return;
            end if;
            Item.Path.Length := Value.Result_Text.Length;
            Item.Path.Data (1 .. Item.Path.Length) := Value.Result_Text.Data (1 .. Item.Path.Length);
         end if;
         -- Preserve exact path bytes; never normalize an authority scope.
         if not Valid_Scope_Path (Item.Path.Data (1 .. Item.Path.Length)) and then Item.Path.Length > 0 then
            Fail (Invalid_Path, Cursor);
         end if;
         for Existing of Scopes (1 .. Scope_Count) loop
            if Existing.Domain = Item.Domain and then Existing.Path = Item.Path then
               Fail (Duplicate_Scope, Cursor);
            end if;
         end loop;
         if Scope_Count = MAX_SCOPES then Fail (Too_Many_Scopes, Cursor);
         elsif not Failed then Scope_Count := Scope_Count + 1; Scopes (Scope_Count) := Item;
         end if;
      end Add_Scope;

      --  (tls-scope "host:port") authorizes connections through tls.svc to
      --  matching names; the pattern is validated exactly as tls.svc will
      --  interpret it, and stored unchanged.
      procedure Add_TLS_Scope is
         Item : Scope;
         Parsed : CuBit.TLS_Scopes.Scope;
         Valid : Boolean;
      begin
         Item.Domain := Tls_Domain;
         Item.Rights (Read_File) := True;
         Expression;
         if Failed then return; end if;
         if not Value.Has_Text or else Value.Result_Text.Length not in 1 .. 64 then
            Fail (Invalid_Path, Cursor); return;
         end if;
         CuBit.TLS_Scopes.Parse
           (Value.Result_Text.Data (1 .. Value.Result_Text.Length), Parsed, Valid);
         if not Valid then
            Fail (Invalid_Path, Cursor); return;
         end if;
         Item.Path.Length := Value.Result_Text.Length;
         Item.Path.Data (1 .. Item.Path.Length) :=
           Value.Result_Text.Data (1 .. Item.Path.Length);
         for Existing of Scopes (1 .. Scope_Count) loop
            if Existing.Domain = Item.Domain and then Existing.Path = Item.Path then
               Fail (Duplicate_Scope, Cursor);
            end if;
         end loop;
         if Scope_Count = MAX_SCOPES then Fail (Too_Many_Scopes, Cursor);
         elsif not Failed then Scope_Count := Scope_Count + 1; Scopes (Scope_Count) := Item;
         end if;
      end Add_TLS_Scope;

      procedure Add_Stream is
         Name : Metadata_Text;
         Kind : Stream_Kind;
         Item : Stream_Declaration;
      begin
         Atom (Name);
         if Is_Text (Name, "stdout") then Kind := Standard_Output;
         elsif Is_Text (Name, "stderr") then Kind := Standard_Error;
         elsif Is_Text (Name, "log") then Kind := Log_Output;
         else Fail (Unknown_Stream, Cursor); return;
         end if;
         if Streams (Kind).Present then Fail (Duplicate_Stream, Cursor); return; end if;
         Atom (Name);
         if Is_Text (Name, "text") then Item.Format := Text_Lines;
         elsif Is_Text (Name, "raw-bytes") then Item.Format := Raw_Bytes;
         else Fail (Unknown_Stream, Cursor); return;
         end if;
         Expression;
         if Failed then return; end if;
         if not CCL.Language.Has_Scalar (Value)
           or else Value.Result_Value.Kind /= CCL.VM.Integer_Value
           or else Value.Result_Value.Integer not in 1 .. 256
         then Fail (Invalid_Stream_Pages, Cursor); return;
         end if;
         Item.Pages := Positive (Value.Result_Value.Integer);
         Item.Present := True;
         Streams (Kind) := Item;
         Stream_Count := Stream_Count + 1;
         Stream_Order (Stream_Count) := Kind;
      end Add_Stream;

      procedure Header (Kind : String) is
         Name : Metadata_Text;
      begin
         Expect ('(');
         Atom (Name);
         if not Is_Text (Name, Kind) then Fail (Unknown_Declaration, 1); end if;
         Atom (Name);
         if not Failed then
            declare
               Format : constant CCL.Declarations.Format_Selection :=
                 CCL.Declarations.Select_Format (Name.Data (1 .. Name.Length));
            begin
               if not Format.Supported then
                  Fail (Unsupported_Version, Cursor);
               else
                  case Format.Version is
                     when CCL.Declarations.V1 => null;
                  end case;
               end if;
            end;
         end if;
      end Header;

      procedure Catalog is
         Name : Metadata_Text;
         Item : Service_Binding;
      begin
         Header ("service-catalog");
         loop
            Skip;
            exit when Failed or else Cursor > Last or else Text (Cursor) = ')';
            Expect ('(');
            Atom (Name);
            if Is_Text (Name, "application-slots") then
               if Have_Slots then Fail (Duplicate_Field, Cursor); end if;
               Slot_Expression (First_Slot);
               Slot_Expression (Last_Slot);
               if First_Slot > Last_Slot then Fail (Invalid_Slot, Cursor); end if;
               Have_Slots := True;
               Expect (')');
            elsif Is_Text (Name, "fixed-binding") then
               declare
                  Binding : Named_Binding;
               begin
                  Read_Binding_Name (Binding.Name);
                  Slot_Expression (Binding.Slot);
                  Expect (')');
                  for Existing of Fixed (1 .. Fixed_Count) loop
                     if Existing.Name = Binding.Name then Fail (Duplicate_Binding, Cursor); end if;
                  end loop;
                  if Fixed_Count = MAX_BINDINGS then Fail (Too_Many_Requests, Cursor);
                  elsif not Failed then Fixed_Count := Fixed_Count + 1; Fixed (Fixed_Count) := Binding;
                  end if;
               end;
            else
               if Is_Text (Name, "service") then Item.Kind := Service_Request;
               elsif Is_Text (Name, "notification") then Item.Kind := Notification_Request;
               else Fail (Unknown_Declaration, Cursor);
               end if;
               Atom (Item.Name);
               Expression;
               if Failed then return; end if;
               if not CCL.Language.Has_Scalar (Value)
                 or else Value.Result_Value.Kind /= CCL.VM.Integer_Value
                 or else Value.Result_Value.Integer not in 1 .. Integer_64 (Unsigned_32'Last)
               then
                  Fail (Invalid_Service_ID, Cursor);
                  return;
               end if;
               Item.ID := Unsigned_32 (Value.Result_Value.Integer);
               if Item.Kind = Notification_Request and then Item.ID > 17 then
                  Fail (Invalid_Notification_ID, Cursor);
               end if;
               Read_Rights (Item.Rights, Item.Kind);
               Expect (')');
               for Binding of Services (1 .. Service_Count) loop
                  if Binding.Name = Item.Name or else
                    (Binding.Kind = Item.Kind and then Binding.ID = Item.ID)
                  then
                     Fail (Duplicate_Service, Cursor);
                  end if;
               end loop;
               if Service_Count = MAX_REQUESTS then Fail (Too_Many_Services, Cursor);
               elsif not Failed then
                  Service_Count := Service_Count + 1;
                  Services (Service_Count) := Item;
               end if;
            end if;
         end loop;
         Expect (')');
         Skip;
         if Cursor <= Last then Fail (Trailing_Input, Cursor); end if;
         if not Have_Slots then Fail (Missing_Field, Cursor); end if;
      end Catalog;


      Name : Metadata_Text;
   begin
      Result := (others => <>);
      Catalog_Out := (others => <>);
      Result.In_Catalog := True;
      if Catalog_Source'Length > MAX_DECLARATION_LENGTH then
         Fail (Source_Too_Long, 1);
         return;
      end if;
      Last := Catalog_Source'Length;
      Text (1 .. Last) := Catalog_Source;
      Catalog;
      if Failed then return; end if;
      Catalog_Out := Cat;
      if Catalog_Only then
         Result.Success := True;
         return;
      end if;
      Result.In_Catalog := False;
      Cursor := 1;
      if Source'Length > MAX_DECLARATION_LENGTH then
         Fail (Source_Too_Long, 1);
         return;
      end if;
      Last := Source'Length;
      Text (1 .. Last) := Source;
      Header ("executable-manifest");
      loop
         Skip;
         exit when Failed or else Cursor > Last or else Text (Cursor) = ')';
         Expect ('(');
         Atom (Name);
         if Is_Text (Name, "identity") then
            if Have_Identity then Fail (Duplicate_Field, Cursor); end if;
            Text_Field (Identity);
            Have_Identity := True;
         elsif Is_Text (Name, "version") then
            if Have_Version then Fail (Duplicate_Field, Cursor); end if;
            Text_Field (Version);
            Have_Version := True;
         elsif Is_Text (Name, "request-service") then Add_Request (Service_Request);
         elsif Is_Text (Name, "request-notification") then Add_Request (Notification_Request);
         elsif Is_Text (Name, "request-framebuffer") then Add_Request (Framebuffer_Request);
         elsif Is_Text (Name, "request-network") then Add_Network;
         elsif Is_Text (Name, "request-render") then Add_Request (Render_Request);
         elsif Is_Text (Name, "filesystem-scope") then Add_Scope (Filesystem_Domain);
         elsif Is_Text (Name, "config-scope") then Add_Scope (Config_Domain);
         elsif Is_Text (Name, "tls-scope") then Add_TLS_Scope;
         elsif Is_Text (Name, "stream") then Add_Stream;
         elsif Is_Text (Name, "match-pci-class") then Add_Match (PCI_Class_Match);
         elsif Is_Text (Name, "match-pci-id") then Add_Match (PCI_ID_Match);
         elsif Is_Text (Name, "platform-device") then Add_Match (Platform_Match);
         elsif Is_Text (Name, "device-memory") then Add_Resource (Device_Memory_Request);
         elsif Is_Text (Name, "io-ports") then Add_Resource (IO_Port_Request);
         elsif Is_Text (Name, "interrupt") then Add_Resource (Interrupt_Request);
         elsif Is_Text (Name, "dma") then Add_Resource (DMA_Request);
         elsif Is_Text (Name, "request-scheduling") then Add_Scheduling;
         elsif Is_Text (Name, "requests-none") then
            if Explicit_No_Requests then Fail (Duplicate_Field, Cursor); end if;
            Explicit_No_Requests := True;
         else Fail (Unknown_Declaration, Cursor);
         end if;
         Expect (')');
      end loop;
      Expect (')');
      Skip;
      if Cursor <= Last then Fail (Trailing_Input, Cursor); end if;
      if not Have_Identity or else not Have_Version then Fail (Missing_Field, Cursor); end if;
      if Failed then return; end if;
      Encoding.Encode (Decl, Cat, Cursor, Result);
   end Run;

   procedure Compile (Source, Catalog_Source : String; Result : out Compilation_Result) is
      Ignored : Model.Catalog_Model;
   begin
      Run (Source, Catalog_Source, False, Result, Ignored);
   end Compile;

   procedure Read_Catalog
     (Catalog_Source : String; Catalog : out Model.Catalog_Model; Result : out Compilation_Result) is
   begin
      Run ("", Catalog_Source, True, Result, Catalog);
   end Read_Catalog;
end CCL.Manifests.Keywords;
