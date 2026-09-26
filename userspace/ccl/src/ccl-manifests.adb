with CCL.VM;
with CCL.Declarations;
with CuBit.Network_Authority;
with CuBit.TLS_Scopes;

package body CCL.Manifests with SPARK_Mode => On is
   use Interfaces;
   use type CCL.Language.Interpretation_Status;
   use type CCL.VM.Value_Kind;

   MAX_REQUESTS : constant := MAX_BINDINGS;
   --  Current procmgr ABI: slot 0 is bootstrap, slot 63 is the reply slot.
   subtype Slot_Number is Natural range 1 .. 62;
   type Rights_Kind is (Read_Only, Write_Only, Read_Write);
   for Rights_Kind use (Read_Only => 1, Write_Only => 2, Read_Write => 3);
   type Request_Kind is
     (Framebuffer_Request, Service_Request, Notification_Request, Network_Request);
   for Request_Kind use
     (Framebuffer_Request => 1, Service_Request => 2, Notification_Request => 7,
      Network_Request => CuBit.Network_Authority.Manifest_Request);
   type Request is record
      Kind : Request_Kind := Service_Request;
      Service : Unsigned_32 := 0;
      Network : CuBit.Network_Authority.Scope;
      Rights : Rights_Kind := Read_Only;
      Slot : Slot_Number := Slot_Number'First;
      Name : Binding_Name;
   end record;
   type Request_Array is array (Positive range 1 .. MAX_REQUESTS) of Request;
   subtype Metadata_Text is Binding_Name;

   type Service_Binding is record
      Kind : Request_Kind := Service_Request;
      Name : Metadata_Text;
      ID : Unsigned_32 := 0;
      Rights : Rights_Kind := Read_Only;
   end record;
   type Service_Array is array (Positive range 1 .. MAX_REQUESTS) of Service_Binding;
   MAX_SCOPES : constant := 16;
   type Access_Right is (Read_File, Write_File, Execute_File, Create_File);
   for Access_Right use (Read_File => 1, Write_File => 2, Execute_File => 4, Create_File => 8);
   type Access_Rights is array (Access_Right) of Boolean;
   --  Tls_Domain entries carry a CuBit.TLS_Scopes pattern ("host:port")
   --  with the Read_File bit meaning "connect"; see Add_TLS_Scope.
   type Access_Domain is (Filesystem_Domain, Config_Domain, Tls_Domain);
   for Access_Domain use
     (Filesystem_Domain => 0, Config_Domain => 1, Tls_Domain => 2);
   type Scope is record
      Domain : Access_Domain := Filesystem_Domain;
      Path : Metadata_Text;
      Rights : Access_Rights := [others => False];
   end record;
   type Scope_Array is array (Positive range 1 .. MAX_SCOPES) of Scope;
   type Stream_Kind is (Standard_Output, Standard_Error, Log_Output);
   for Stream_Kind use (Standard_Output => 2, Standard_Error => 3, Log_Output => 4);
   type Stream_Type is (Raw_Bytes, Text_Lines);
   for Stream_Type use (Raw_Bytes => 0, Text_Lines => 1);
   type Stream_Declaration is record
      Present : Boolean := False;
      Pages : Positive range 1 .. 256 := 1;
      Format : Stream_Type := Text_Lines;
   end record;
   type Stream_Array is array (Stream_Kind) of Stream_Declaration;

   procedure Compile
     (Source, Catalog_Source : String; Result : out Compilation_Result)
   is
      Text : String (1 .. MAX_DECLARATION_LENGTH) := [others => ' '];
      Last : Natural range 0 .. MAX_DECLARATION_LENGTH := 0;
      Cursor : Positive range 1 .. MAX_DECLARATION_LENGTH + 1 := 1;
      Failed : Boolean := False;
      Identity, Version : Metadata_Text;
      Have_Identity, Have_Version : Boolean := False;
      Requests : Request_Array := [others => (others => <>)];
      Count : Natural range 0 .. MAX_REQUESTS := 0;
      Services : Service_Array := [others => (others => <>)];
      Service_Count : Natural range 0 .. MAX_REQUESTS := 0;
      First_Slot, Last_Slot : Slot_Number := Slot_Number'First;
      Have_Slots : Boolean := False;
      Fixed : Binding_Array := [others => (others => <>)];
      Fixed_Count : Natural range 0 .. MAX_BINDINGS := 0;
      Scopes : Scope_Array := [others => (others => <>)];
      Scope_Count : Natural range 0 .. MAX_SCOPES := 0;
      Streams : Stream_Array := [others => (others => <>)];
      Stream_Order : array (Positive range 1 .. 3) of Stream_Kind := [others => Standard_Output];
      Stream_Count : Natural range 0 .. 3 := 0;
      Explicit_No_Requests : Boolean := False;
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
         elsif Value.Result_Text.Length not in 1 .. Item.Data'Length then
            Fail (Invalid_Text, Cursor);
         else
            for C of Value.Result_Text.Data (1 .. Value.Result_Text.Length) loop
               if C not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_' then
                  Fail (Invalid_Text, Cursor);
               end if;
            end loop;
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
         -- Lowercase kebab names map injectively to Ada Slot_<name> identifiers.
         -- No trailing/consecutive hyphens, underscores, quotes, or case aliases.
         if Name.Length = 0 or else Name.Data (1) not in 'a' .. 'z' then
            Fail (Invalid_Binding_Name, Cursor);
            return;
         end if;
         for Index in 1 .. Name.Length loop
            declare
               C : constant Character := Name.Data (Index);
            begin
               if C = '-' then
                  if Index = Name.Length or else Name.Data (Index + 1) = '-' then
                     Fail (Invalid_Binding_Name, Cursor);
                  end if;
               elsif C not in 'a' .. 'z' | '0' .. '9' then
                  Fail (Invalid_Binding_Name, Cursor);
               end if;
            end;
         end loop;
      end Read_Binding_Name;

      procedure Store_Request (Item : in out Request) is
      begin
         Read_Binding_Name (Item.Name);
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
         if Kind /= Framebuffer_Request then
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
         if Kind /= Framebuffer_Request and then Offered /= Read_Write
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
         if not CuBit.Network_Authority.Valid (Item.Network) then
            Fail (Invalid_Network_Scope, Cursor);
         end if;
         Store_Request (Item);
      end Add_Network;

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

      procedure Allocate_Slots is
         Used : array (Slot_Number) of Boolean := [others => False];
         Reserved : array (Slot_Number) of Boolean := [others => False];
         Found : Boolean;
      begin
         for Item of Fixed (1 .. Fixed_Count) loop Reserved (Item.Slot) := True; end loop;
         for Index in 1 .. Count loop
            Found := False;
            for Item of Fixed (1 .. Fixed_Count) loop
               if Item.Name = Requests (Index).Name then
                  Requests (Index).Slot := Item.Slot;
                  Found := True;
               end if;
            end loop;
            if not Found then
               for Slot in First_Slot .. Last_Slot loop
                  if not Used (Slot) and then not Reserved (Slot) then
                     Requests (Index).Slot := Slot;
                     Found := True;
                     exit;
                  end if;
               end loop;
            end if;
            if not Found then Fail (Slots_Exhausted, Cursor); return; end if;
            if Used (Requests (Index).Slot) then Fail (Duplicate_Slot, Cursor); return; end if;
            Used (Requests (Index).Slot) := True;
         end loop;
      end Allocate_Slots;

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
         -- ASCII-only initial profile, no wildcards, NULs, or traversal components.
         for I in 1 .. Item.Path.Length loop
            declare
               C : constant Character := Item.Path.Data (I);
            begin
               if C not in ' ' .. '~' or else C in '*' | '?' | '\' then
                  Fail (Invalid_Path, Cursor);
               end if;
               if C = '.' and then (I = 1 or else Item.Path.Data (I - 1) = '/') then
                  if I = Item.Path.Length or else Item.Path.Data (I + 1) = '/' then
                     Fail (Invalid_Path, Cursor);
                  elsif Item.Path.Data (I + 1) = '.' and then
                    (I + 1 = Item.Path.Length or else Item.Path.Data (I + 2) = '/')
                  then Fail (Invalid_Path, Cursor);
                  end if;
               end if;
               if C = '/' and then I < Item.Path.Length and then Item.Path.Data (I + 1) = '/' then
                  Fail (Invalid_Path, Cursor);
               end if;
            end;
         end loop;
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

      procedure Append (Output : in out Section; N : Unsigned_64; Bytes : Positive) is
      begin
         for Index in 0 .. Bytes - 1 loop
            Output.Length := Output.Length + 1;
            Output.Data (Output.Length) := Unsigned_8 (Shift_Right (N, Index * 8) and 255);
         end loop;
      end Append;

      procedure Write_Text (Output : in out Section; S : String) is
      begin
         for C of S loop Append (Output, Character'Pos (C), 1); end loop;
      end Write_Text;

      procedure Pair (Key : String; Item : Metadata_Text) is
      begin
         Append (Result.Identity, Key'Length, 1);
         Append (Result.Identity, Unsigned_64 (Item.Length), 2);
         Write_Text (Result.Identity, Key);
         Write_Text (Result.Identity, Item.Data (1 .. Item.Length));
      end Pair;

      Name : Metadata_Text;
   begin
      Result := (others => <>);
      Result.In_Catalog := True;
      if Catalog_Source'Length > MAX_DECLARATION_LENGTH then
         Fail (Source_Too_Long, 1);
         return;
      end if;
      Last := Catalog_Source'Length;
      Text (1 .. Last) := Catalog_Source;
      Catalog;
      if Failed then return; end if;
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
         elsif Is_Text (Name, "filesystem-scope") then Add_Scope (Filesystem_Domain);
         elsif Is_Text (Name, "config-scope") then Add_Scope (Config_Domain);
         elsif Is_Text (Name, "tls-scope") then Add_TLS_Scope;
         elsif Is_Text (Name, "stream") then Add_Stream;
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
      if Explicit_No_Requests and then Count > 0 then Fail (Duplicate_Field, Cursor); end if;
      if Failed then return; end if;
      Allocate_Slots;
      if Failed then return; end if;

      --  Canonical little-endian wire ABI; no host layout/padding dependence.
      Append (Result.Identity, 16#4449_4243#, 4);
      Append (Result.Identity, 1, 2);
      Append (Result.Identity, 2, 2);
      Pair ("id", Identity);
      Pair ("version", Version);
      if Count > 0 or else Explicit_No_Requests then
      Append (Result.Capabilities, 16#4342_4954#, 4);
      Append (Result.Capabilities, 1, 2);
      Append (Result.Capabilities, Unsigned_64 (Count), 2);
      for Item of Requests (1 .. Count) loop
         Append (Result.Capabilities, Unsigned_64 (Request_Kind'Enum_Rep (Item.Kind)), 1);
         Append (Result.Capabilities, Unsigned_64 (Rights_Kind'Enum_Rep (Item.Rights)), 1);
         Append (Result.Capabilities, Unsigned_64 (Item.Slot), 2);
         if Item.Kind = Network_Request then
            Append (Result.Capabilities, Unsigned_64 (Item.Network.Network), 4);
            Append (Result.Capabilities, CuBit.Network_Authority.Descriptor (Item.Network), 8);
         else
            Append (Result.Capabilities, Unsigned_64 (Item.Service), 4);
            Append (Result.Capabilities, 0, 8);
         end if;
      end loop;
      end if;
      if Scope_Count > 0 then
         Append (Result.Access_Scopes, 16#4343_4143#, 4);
         Append (Result.Access_Scopes, 1, 2);
         Append (Result.Access_Scopes, Unsigned_64 (Scope_Count), 2);
         Append (Result.Access_Scopes, 0, 8);
         for Item of Scopes (1 .. Scope_Count) loop
            declare
               Mask : Unsigned_64 := 0;
            begin
               for Right in Access_Right loop
                  if Item.Rights (Right) then Mask := Mask or Unsigned_64 (Access_Right'Enum_Rep (Right)); end if;
               end loop;
               Append (Result.Access_Scopes, Mask, 1);
            end;
            Append (Result.Access_Scopes, Unsigned_64 (Item.Path.Length), 1);
            Append (Result.Access_Scopes, Unsigned_64 (Access_Domain'Enum_Rep (Item.Domain)), 1);
            Append (Result.Access_Scopes, 0, 5);
            Write_Text (Result.Access_Scopes, Item.Path.Data (1 .. Item.Path.Length));
            for I in Item.Path.Length + 1 .. 64 loop Append (Result.Access_Scopes, 0, 1); end loop;
            Append (Result.Access_Scopes, 0, 8);
         end loop;
      end if;
      if Stream_Count > 0 then
         Append (Result.Streams, 16#5453_4243#, 4);
         Append (Result.Streams, 1, 2);
         Append (Result.Streams, Unsigned_64 (Stream_Count), 2);
         for Kind of Stream_Order (1 .. Stream_Count) loop
            Append (Result.Streams, Unsigned_64 (Stream_Kind'Enum_Rep (Kind)), 2);
            Append (Result.Streams, Unsigned_64 (Streams (Kind).Pages), 2);
            Append (Result.Streams, Unsigned_64 (Stream_Type'Enum_Rep (Streams (Kind).Format)), 2);
            Append (Result.Streams, 0, 2);
         end loop;
      end if;
      Result.Binding_Count := Count;
      for Index in 1 .. Count loop
         Result.Bindings (Index) := (Name => Requests (Index).Name,
                                     Slot => Requests (Index).Slot);
      end loop;
      Result.Success := True;
   end Compile;
end CCL.Manifests;
