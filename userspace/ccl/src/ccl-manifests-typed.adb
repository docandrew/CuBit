with CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.Resource_Sections;
with CCL.Scheduling_Limits;
with CuBit.Network_Authority;
with CuBit.TLS_Scopes;
with CCL.Manifests.Model;
with CCL.Manifests.Encoding;
with CCL.Manifests.Keywords;
with CCL.Diagnostics;
with CCL.Types;
with CuBit.Failures;

package body CCL.Manifests.Typed with SPARK_Mode => On is
   use Standard.Interfaces;
   use Model;
   use all type CCL.Resource_Sections.Match_Kind;
   use all type CCL.Resource_Sections.Platform_Device;
   use all type CCL.Resource_Sections.Interrupt_Mode;
   use type CCL.Language.Interpretation_Status;
   package Views renames CCL.Objects.Views;

   --  Evaluation of a manifest is pure: no host operation is visible.
   type No_Host is null record;
   procedure Refuse
     (Context : in out No_Host; Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context, Binding, Argument);
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
   end Refuse;
   procedure Evaluate is new CCL.Language.Interpret_Object_With_Values (No_Host, Refuse);

   MANIFEST_FUEL : constant := 100_000;
   MAXIMUM_DISTANCE : constant := 2;

   --  Edit distance, for "did you mean": names are at most 64 characters.
   function Distance (Left, Right : String) return Natural is
      Row : array (0 .. Right'Length) of Natural;
      Diagonal, Above : Natural;
   begin
      if Left'Length > Binding_Name_Length'Last or else Right'Length > Binding_Name_Length'Last then
         return Natural'Last;
      end if;
      for J in Row'Range loop Row (J) := J; end loop;
      for I in 1 .. Left'Length loop
         Diagonal := Row (0);
         Row (0) := I;
         for J in 1 .. Right'Length loop
            Above := Row (J);
            Row (J) := Natural'Min (Natural'Min (Row (J) + 1, Row (J - 1) + 1),
                                    Diagonal + (if Left (Left'First + I - 1) = Right (Right'First + J - 1)
                                                then 0 else 1));
            Diagonal := Above;
         end loop;
      end loop;
      return Row (Right'Length);
   end Distance;

   --  The name at Position in Source (1-based), as source wrote it.
   function Name_At (Source : String; Position : Positive) return String is
      First : constant Natural := Source'First + Position - 1;
      Last : Natural;
      function Name_Character (C : Character) return Boolean is
        (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' | '?' | '+' | '*' | '/' | '%' | '<' | '>' | '=');
   begin
      if First not in Source'Range or else not Name_Character (Source (First)) then return ""; end if;
      Last := First;
      while Last < Source'Last and then Name_Character (Source (Last + 1)) loop Last := Last + 1; end loop;
      return Source (First .. Last);
   end Name_At;

   ROOT_TYPE_NAME : constant String := "Executable_Manifest";
   --  The manifest's value never leaves this process: an in-process key.
   LOCAL_KEY : constant CCL.Objects.Schema_Key :=
     [16#4D41_4E49_4645_5354#, 16#2D4C_4F43_414C_0001#, 0, 0];

   procedure Compile (Source, Catalog_Source, Schema_Source : String; Result : out Compilation_Result) is
      Program : constant String := Schema_Source & ASCII.LF & Source;
      Checked : CCL.Language.Analysis_Result;
      Manifest_Type : CCL.Types.Type_Reference;
      Decl : Declaration;
      Cat : Catalog_Model;
      Catalog : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Contract : CCL.Objects.Binding;
      Published, Captured : Boolean;
      Host : No_Host;
      Value : CCL.Language.Object_Interpretation_Result;
      Object : Views.Snapshot;
      Failed : Boolean := False;

      --  The entry being read, for messages: "requests entry 2".
      Where : String (1 .. 32) := [others => ' '];
      Where_Length : Natural range 0 .. 32 := 0;
      procedure Locate (Text : String) is
      begin
         Where_Length := Natural'Min (Text'Length, Where'Length);
         Where (1 .. Where_Length) := Text (Text'First .. Text'First + Where_Length - 1);
      end Locate;
      function Entry_Text (List : String; Index : Positive) return String is
        (List & " entry" & Positive'Image (Index));

      procedure Fail (Code : Diagnostic_Code; Detail : String := ""; Remedy : String := "") is
         Context : constant String := Where (1 .. Where_Length);
      begin
         if not Failed then
            Failed := True;
            Result.Success := False;
            Result.Diagnostic := Code;
            Result.Why := CuBit.Failures.Failed
              (CuBit.Failures.Invalid_Argument,
               (if Context'Length > 0 then Context & ": " else "") &
               (if Detail'Length > 0 then Detail else Diagnostic_Code'Image (Code)),
               Remedy);
         end if;
      end Fail;

      function Text (At_Cursor : Views.Cursor) return String is (Views.Text (Object, At_Cursor));
      function Number (At_Cursor : Views.Cursor) return Integer_64 is
        (CCL.Objects.Integer_Of (Views.Scalar (Object, At_Cursor)));
      function Flag (At_Cursor : Views.Cursor) return Boolean is (Views.Scalar (Object, At_Cursor).First = 1);
      --  An enum or variant's alternative, from 1, and its name.
      function Choice (At_Cursor : Views.Cursor) return Natural is (Views.Alternative (Object, At_Cursor));
      function Alt (At_Cursor : Views.Cursor) return String is
        (if Choice (At_Cursor) in 1 .. Views.Describe (Object, At_Cursor).Count
         then CCL.Types.Image (Views.Describe (Object, At_Cursor).Parts (Choice (At_Cursor)).Identifier)
         else "");
      --  A record's field by name: the schema (interfaces/executable-manifest.ccl), not
      --  this reader, decides the order.
      function Named (At_Cursor : Views.Cursor; Name : String) return Views.Cursor is
         Shape : constant CCL.Types.Description := Views.Describe (Object, At_Cursor);
      begin
         for Part in 1 .. Shape.Count loop
            if CCL.Types.Image (Shape.Parts (Part).Identifier) = Name then
               return Views.Field (Object, At_Cursor, Part);
            end if;
         end loop;
         return Views.No_Value;
      end Named;

      procedure Set_Text (Item : out Metadata_Text; Value : String) is
      begin
         Item := (others => <>);
         Item.Length := Value'Length;
         Item.Data (1 .. Value'Length) := Value;
      end Set_Text;

      procedure Metadata (At_Cursor : Views.Cursor; Item : out Metadata_Text) is
         Value : constant String := Text (At_Cursor);
      begin
         Item := (others => <>);
         if not Valid_Metadata_Text (Value) then
            Fail (Invalid_Text, """" & Value & """ is not 1 to 64 letters, digits, '.', '-' or '_'");
            return;
         end if;
         Set_Text (Item, Value);
      end Metadata;

      procedure Binding (At_Cursor : Views.Cursor; Item : in out Request) is
         Value : constant String := Text (At_Cursor);
      begin
         if not Valid_Binding_Name (Value) then
            Fail (Invalid_Binding_Name, "binding """ & Value & """ is not a lowercase kebab name",
                  "start with a letter; use a-z, 0-9 and single hyphens");
            return;
         end if;
         Set_Text (Item.Name, Value);
      end Binding;

      function Rights_Of (Name : String) return Rights_Kind is
        (if Name in "Read" | "Publish" then Read_Only
         elsif Name in "Write" | "Manage" then Write_Only else Read_Write);

      procedure Store (Item : Request) is
      begin
         if Failed then return; end if;
         for Index in 1 .. Decl.Count loop
            if Decl.Requests (Index).Name = Item.Name then
               Fail (Duplicate_Binding, "binding """ & Item.Name.Data (1 .. Item.Name.Length) & """ is used twice",
                     "give each request its own binding name");
               return;
            end if;
         end loop;
         if Decl.Count = MAX_REQUESTS then Fail (Too_Many_Requests); return; end if;
         Decl.Count := Decl.Count + 1;
         Decl.Requests (Decl.Count) := Item;
      end Store;

      --  The catalog's closest name of one kind to Name, if one is close.
      function Closest (Kind : Request_Kind; Name : String) return String is
         Best : Natural := MAXIMUM_DISTANCE + 1;
         Found : Metadata_Text;
      begin
         for Entry_Binding of Cat.Services (1 .. Cat.Service_Count) loop
            if Entry_Binding.Kind = Kind and then
              Distance (Name, Entry_Binding.Name.Data (1 .. Entry_Binding.Name.Length)) < Best
            then
               Best := Distance (Name, Entry_Binding.Name.Data (1 .. Entry_Binding.Name.Length));
               Found := Entry_Binding.Name;
            end if;
         end loop;
         return Found.Data (1 .. Found.Length);
      end Closest;

      --  The catalog's names of one kind, for a message (cut to fit).
      function Known (Kind : Request_Kind) return String is
         --  Room beside the remedy's own words ("use one of: ").
         Text : String (1 .. CuBit.Failures.MAXIMUM_TEXT - 16) := [others => ' '];
         Length : Natural := 0;
      begin
         for Entry_Binding of Cat.Services (1 .. Cat.Service_Count) loop
            if Entry_Binding.Kind = Kind then
               declare
                  Name : constant String :=
                    (if Length = 0 then "" else ", ") & Entry_Binding.Name.Data (1 .. Entry_Binding.Name.Length);
               begin
                  if Length + Name'Length + 5 > Text'Length then
                     Text (Length + 1 .. Length + 5) := ", ...";
                     Length := Length + 5;
                     exit;
                  end if;
                  Text (Length + 1 .. Length + Name'Length) := Name;
                  Length := Length + Name'Length;
               end;
            end if;
         end loop;
         return Text (1 .. Length);
      end Known;

      --  A catalog service or notification by name, with its offered rights.
      procedure Catalogued
        (Name : String; Kind : Request_Kind; Item : in out Request; Requested : Rights_Kind)
      is
         Found : Boolean := False;
         Offered : Rights_Kind := Read_Only;
      begin
         for Entry_Binding of Cat.Services (1 .. Cat.Service_Count) loop
            if Entry_Binding.Name.Data (1 .. Entry_Binding.Name.Length) = Name and then Entry_Binding.Kind = Kind then
               Item.Service := Entry_Binding.ID;
               Offered := Entry_Binding.Rights;
               Found := True;
            end if;
         end loop;
         if not Found then
            Fail ((if Kind = Notification_Request then Unknown_Notification else Unknown_Service),
                  "the catalog has no " & (if Kind = Notification_Request then "notification" else "service") &
                  " named """ & Name & """" &
                  (if Closest (Kind, Name)'Length > 0 then "; did you mean """ & Closest (Kind, Name) & """?" else ""),
                  (if Closest (Kind, Name)'Length > 0 then "write """ & Closest (Kind, Name) & """"
                   else "use one of: " & Known (Kind)));
         elsif Offered /= Read_Write and then Requested /= Offered then
            Fail (Rights_Not_Offered, """" & Name & """ is offered with other rights",
                  "request the rights the catalog offers for it");
         end if;
         Item.Rights := Requested;
      end Catalogued;

      --  Canonical dotted-decimal IPv4 only: no shorthand, octal or truncation.
      procedure IPv4 (Address : String; Network : out Unsigned_32) is
         Octet : Natural range 0 .. 255 := 0;
         Digits_Seen : Natural range 0 .. 3 := 0;
         Octets : Natural range 0 .. 4 := 0;
      begin
         Network := 0;
         if Address'Length not in 7 .. 15 then Fail (Invalid_Network_Scope); return; end if;
         for C of Address loop
            if C = '.' then
               if Digits_Seen = 0 or else Octets = 3 then Fail (Invalid_Network_Scope); return; end if;
               Network := Shift_Left (Network, 8) or Unsigned_32 (Octet);
               Octets := Octets + 1;
               Octet := 0;
               Digits_Seen := 0;
            elsif C in '0' .. '9' then
               if Digits_Seen = 3 or else (Digits_Seen > 0 and then Octet = 0)
                 or else Octet * 10 + Character'Pos (C) - Character'Pos ('0') > 255
               then
                  Fail (Invalid_Network_Scope); return;
               end if;
               Octet := Octet * 10 + Character'Pos (C) - Character'Pos ('0');
               Digits_Seen := Digits_Seen + 1;
            else
               Fail (Invalid_Network_Scope); return;
            end if;
         end loop;
         if Octets /= 3 or else Digits_Seen = 0 then Fail (Invalid_Network_Scope); return; end if;
         Network := Shift_Left (Network, 8) or Unsigned_32 (Octet);
      end IPv4;

      function In_Range (Value, Low, High : Integer_64) return Boolean is (Value in Low .. High);

      procedure Add_Request (At_Cursor : Views.Cursor) is
         Item : Request;
         Kind : constant String := Alt (At_Cursor);
         Body_Cursor : constant Views.Cursor := Views.Payload (Object, At_Cursor);
         Number_Value : Integer_64;
      begin
         if Kind = "Service" then
               Item.Kind := Service_Request;
               Catalogued (Text (Named (Body_Cursor, "service")), Service_Request, Item,
                           Rights_Of (Alt (Named (Body_Cursor, "access"))));
               Binding (Named (Body_Cursor, "binding"), Item);
         elsif Kind = "Notification" then
               Item.Kind := Notification_Request;
               Catalogued (Text (Named (Body_Cursor, "notification")), Notification_Request, Item,
                           Rights_Of (Alt (Named (Body_Cursor, "access"))));
               Binding (Named (Body_Cursor, "binding"), Item);
         elsif Kind = "Framebuffer" then
               Item.Kind := Framebuffer_Request;
               Item.Rights := Rights_Of (Alt (Named (Body_Cursor, "access")));
               Binding (Named (Body_Cursor, "binding"), Item);
         elsif Kind = "Render" then
               --  One admitted session per launch.
               for Existing of Decl.Requests (1 .. Decl.Count) loop
                  if Existing.Kind = Render_Request then Fail (Duplicate_Field); end if;
               end loop;
               Item.Kind := Render_Request;
               Item.Rights := Read_Write;
               Binding (Body_Cursor, Item);
         elsif Kind = "Network" then
               Item.Kind := Network_Request;
               Item.Rights := Read_Write;
               Item.Network.Action :=
                 (if Alt (Named (Body_Cursor, "action")) = "TCP_Connect" then CuBit.Network_Authority.Connect_TCP
                  elsif Alt (Named (Body_Cursor, "action")) = "TCP_Listen" then CuBit.Network_Authority.Listen_TCP
                  else CuBit.Network_Authority.Connect_UDP);
               IPv4 (Text (Named (Body_Cursor, "address")), Item.Network.Network);
               Number_Value := Number (Named (Body_Cursor, "prefix"));
               if not In_Range (Number_Value, 0, 32) then Fail (Invalid_Network_Scope); return; end if;
               Item.Network.Prefix := Natural (Number_Value);
               Number_Value := Number (Named (Body_Cursor, "first_port"));
               if not In_Range (Number_Value, 1, 65_535) then Fail (Invalid_Network_Scope); return; end if;
               Item.Network.First_Port := Unsigned_16 (Number_Value);
               Number_Value := Number (Named (Body_Cursor, "last_port"));
               if not In_Range (Number_Value, 1, 65_535) then Fail (Invalid_Network_Scope); return; end if;
               Item.Network.Last_Port := Unsigned_16 (Number_Value);
               Item.Network.Resolve_Names := Flag (Named (Body_Cursor, "resolve_names"));
               Number_Value := Number (Named (Body_Cursor, "connections"));
               if not In_Range (Number_Value, 1, Integer_64 (CuBit.Network_Authority.Connection_Count'Last)) then
                  Fail (Invalid_Network_Scope); return;
               end if;
               Item.Network.Connections := CuBit.Network_Authority.Connection_Count (Number_Value);
               if not CuBit.Network_Authority.Valid (Item.Network) then Fail (Invalid_Network_Scope); return; end if;
               Binding (Named (Body_Cursor, "binding"), Item);
         elsif Kind in "Device_Memory" | "IO_Ports" | "Interrupt" | "DMA" then
               --  Resources are relative to the matched device.
               if Decl.Match = No_Match then Fail (Missing_Device_Match); return; end if;
               Binding (Named (Body_Cursor, "binding"), Item);
               if Kind = "Device_Memory" then
                     Item.Kind := Device_Memory_Request;
                     Number_Value := Number (Named (Body_Cursor, "index"));
                     if not In_Range (Number_Value, 0, (if Decl.Match = Platform_Match
                                                        then MAX_PLATFORM_RESOURCES else PCI_BAR_COUNT) - 1)
                     then Fail (Invalid_Device_Resource); return; end if;
                     Item.Index := Unsigned_32 (Number_Value);
                     Number_Value := Number (Named (Body_Cursor, "max_bytes"));
                     if not In_Range (Number_Value, PAGE_BYTES, MAX_DEVICE_MEMORY_BYTES)
                       or else Number_Value mod PAGE_BYTES /= 0
                     then Fail (Invalid_Device_Resource); return; end if;
                     Item.Amount := Unsigned_64 (Number_Value);
                     Item.Rights := Rights_Of (Alt (Named (Body_Cursor, "access")));
                     if Item.Rights = Write_Only then Fail (Invalid_Device_Resource); return; end if;
               elsif Kind = "IO_Ports" then
                     Item.Kind := IO_Port_Request;
                     Number_Value := Number (Named (Body_Cursor, "index"));
                     if not In_Range (Number_Value, 0, (if Decl.Match = Platform_Match
                                                        then MAX_PLATFORM_RESOURCES else PCI_BAR_COUNT) - 1)
                     then Fail (Invalid_Device_Resource); return; end if;
                     Item.Index := Unsigned_32 (Number_Value);
                     Number_Value := Number (Named (Body_Cursor, "count"));
                     if not In_Range (Number_Value, 1, IO_PORT_SPACE) then Fail (Invalid_Device_Resource); return; end if;
                     Item.Amount := Unsigned_64 (Number_Value);
                     Item.Rights := Read_Write;
               elsif Kind = "Interrupt" then
                     Item.Kind := Interrupt_Request;
                     Item.Rights := Read_Only;
                     if Alt (Named (Body_Cursor, "mode")) = "Platform_Line" then
                           if Decl.Match /= Platform_Match then Fail (Invalid_Device_Resource); return; end if;
                           Item.Extra := Unsigned_64 (Interrupt_Mode'Enum_Rep (Platform_Line));
                           Number_Value := Number (Named (Body_Cursor, "index"));
                           if not In_Range (Number_Value, 0, MAX_PLATFORM_RESOURCES - 1) then
                              Fail (Invalid_Device_Resource); return;
                           end if;
                           Item.Index := Unsigned_32 (Number_Value);
                           Item.Amount := 1;
                     elsif Alt (Named (Body_Cursor, "mode")) = "Line" then
                           if Decl.Match = Platform_Match then Fail (Invalid_Device_Resource); return; end if;
                           Item.Extra := Unsigned_64 (Interrupt_Mode'Enum_Rep (Line));
                           Item.Amount := 1;
                     else
                           if Decl.Match = Platform_Match then Fail (Invalid_Device_Resource); return; end if;
                           Item.Extra := Unsigned_64 (Interrupt_Mode'Enum_Rep
                             ((if Alt (Named (Body_Cursor, "mode")) = "MSI_X" then MSI_X else MSI)));
                           Number_Value := Number (Named (Body_Cursor, "vectors"));
                           if not In_Range (Number_Value, 1, MAX_INTERRUPT_VECTORS) then
                              Fail (Invalid_Device_Resource); return;
                           end if;
                           Item.Amount := Unsigned_64 (Number_Value);
                     end if;
               else
                     Item.Kind := DMA_Request;
                     Number_Value := Number (Named (Body_Cursor, "bytes"));
                     if not In_Range (Number_Value, PAGE_BYTES, MAX_DMA_BYTES)
                       or else Number_Value mod PAGE_BYTES /= 0
                     then Fail (Invalid_Device_Resource); return; end if;
                     Item.Amount := Unsigned_64 (Number_Value);
                     Item.Rights := Read_Write;
               end if;
         elsif Kind = "Scheduling" then
               Item.Kind := Scheduling_Request;
               Item.Rights := Read_Only;
               Binding (Named (Body_Cursor, "binding"), Item);
               declare
                  Budget : constant Integer_64 := Number (Named (Body_Cursor, "budget_us"));
                  Period : constant Integer_64 := Number (Named (Body_Cursor, "period_us"));
               begin
                  if not In_Range (Budget, 1, CCL.Scheduling_Limits.MAX_MICROSECONDS)
                    or else not In_Range (Period, 1, CCL.Scheduling_Limits.MAX_MICROSECONDS)
                    or else not CCL.Scheduling_Limits.Admissible (Budget, Period)
                  then Fail (Invalid_Scheduling); return; end if;
                  Item.Amount := Unsigned_64 (Budget);
                  Item.Extra := Unsigned_64 (Period);
               end;
         else
            Fail (Invalid_Expression, "request kind """ & Kind & """ is not one this tool encodes");
            return;
         end if;
         Store (Item);
      end Add_Request;

      procedure Add_Scope (At_Cursor : Views.Cursor) is
         Item : Scope;
         Kind : constant String := Alt (At_Cursor);
         Body_Cursor : constant Views.Cursor := Views.Payload (Object, At_Cursor);
      begin
         if Kind = "TLS" then
            declare
               Pattern : constant String := Text (Body_Cursor);
               Parsed : CuBit.TLS_Scopes.Scope;
               Valid : Boolean;
            begin
               Item.Domain := Tls_Domain;
               Item.Rights (Read_File) := True;
               if Pattern'Length not in 1 .. MAX_NAME_TEXT then Fail (Invalid_Path); return; end if;
               CuBit.TLS_Scopes.Parse (Pattern, Parsed, Valid);
               if not Valid then Fail (Invalid_Path); return; end if;
               Set_Text (Item.Path, Pattern);
            end;
         else
            Item.Domain := (if Kind = "Filesystem" then Filesystem_Domain else Config_Domain);
            declare
               Rights : constant Views.Cursor := Named (Body_Cursor, "rights");
               Place : constant Views.Cursor := Named (Body_Cursor, "place");
               Right : Access_Right;
            begin
               if Views.Length (Object, Rights) = 0 then Fail (Invalid_Access_Rights); return; end if;
               for Index in 1 .. Views.Length (Object, Rights) loop
                  declare
                     Name : constant String := Alt (Views.Element (Object, Rights, Index));
                  begin
                     Right := (if Name = "Read" then Read_File elsif Name = "Write" then Write_File
                               elsif Name = "Execute" then Execute_File else Create_File);
                  end;
                  if (Item.Domain = Config_Domain and then Right in Execute_File | Create_File)
                    or else Item.Rights (Right)
                  then
                     Fail (Invalid_Access_Rights); return;
                  end if;
                  Item.Rights (Right) := True;
               end loop;
               if Alt (Place) = "Under" then
                  declare
                     Path : constant String := Text (Views.Payload (Object, Place));
                  begin
                     --  Broad access is the explicit Everything, never an empty path.
                     if not Valid_Scope_Path (Path) then Fail (Invalid_Path); return; end if;
                     Set_Text (Item.Path, Path);
                  end;
               end if;
            end;
         end if;
         for Existing of Decl.Scopes (1 .. Decl.Scope_Count) loop
            if Existing.Domain = Item.Domain and then Existing.Path = Item.Path then
               Fail (Duplicate_Scope); return;
            end if;
         end loop;
         if Decl.Scope_Count = MAX_SCOPES then Fail (Too_Many_Scopes); return; end if;
         Decl.Scope_Count := Decl.Scope_Count + 1;
         Decl.Scopes (Decl.Scope_Count) := Item;
      end Add_Scope;

      procedure Add_Stream (At_Cursor : Views.Cursor) is
         Kind_Name : constant String := Alt (Named (At_Cursor, "kind"));
         Kind : constant Stream_Kind :=
           (if Kind_Name = "Standard_Output" then Standard_Output
            elsif Kind_Name = "Standard_Error" then Standard_Error else Log_Output);
         Pages : constant Integer_64 := Number (Named (At_Cursor, "pages"));
      begin
         if Decl.Streams (Kind).Present then Fail (Duplicate_Stream); return; end if;
         if not In_Range (Pages, 1, 256) then Fail (Invalid_Stream_Pages); return; end if;
         Decl.Streams (Kind) :=
           (Present => True, Pages => Positive (Pages),
            Format => (if Alt (Named (At_Cursor, "format")) = "Text" then Text_Lines else Raw_Bytes));
         Decl.Stream_Count := Decl.Stream_Count + 1;
         Decl.Stream_Order (Decl.Stream_Count) := Kind;
      end Add_Stream;

      --  A program this executable may start: its exact name, as the
      --  launch table (CuBit.Launch_Authority) holds it.
      procedure Add_Launch (At_Cursor : Views.Cursor) is
         Program : constant String := Text (Named (At_Cursor, "program"));
         Name : Metadata_Text;
      begin
         if Views.Length (Object, Named (At_Cursor, "invoked_as")) > 0 then
            Fail (Invalid_Launch, "invoked_as aliases need the next launch table version",
                  "name the program as procmgr launches it, without aliases");
            return;
         end if;
         if Program'Length not in 1 .. MAX_NAME_TEXT or else
           (for some C of Program => C not in '!' .. '~')
         then
            Fail (Invalid_Launch, "program """ & Program & """ is not 1 to 64 printable characters without spaces");
            return;
         end if;
         Set_Text (Name, Program);
         for Existing of Decl.Launches (1 .. Decl.Launch_Count) loop
            if Existing = Name then Fail (Invalid_Launch, "program """ & Program & """ is listed twice"); return; end if;
         end loop;
         if Decl.Launch_Count = MAX_LAUNCHES then
            Fail (Invalid_Launch, "at most" & Natural'Image (MAX_LAUNCHES) & " programs may be listed");
            return;
         end if;
         Decl.Launch_Count := Decl.Launch_Count + 1;
         Decl.Launches (Decl.Launch_Count) := Name;
      end Add_Launch;

      procedure Add_Match (At_Cursor : Views.Cursor) is
         Kind : constant String := Alt (At_Cursor);
         Body_Cursor : Views.Cursor;
         Number_Value : Integer_64;
      begin
         if Kind = "No_Device" then return; end if;
         Body_Cursor := Views.Payload (Object, At_Cursor);
         if Kind = "PCI_Class" then
            Decl.Match := PCI_Class_Match;
            for Index in 1 .. MATCH_VALUES loop
               Number_Value := Number (Named (Body_Cursor, (case Index is when 1 => "class",
                                                                        when 2 => "subclass",
                                                                        when others => "interface")));
               if not In_Range (Number_Value, 0, PCI_CODE_LAST) then Fail (Invalid_Device_Match); return; end if;
               Decl.Match_Values (Index) := Unsigned_16 (Number_Value);
            end loop;
         elsif Kind = "PCI_ID" then
            Decl.Match := PCI_ID_Match;
            for Index in 1 .. 2 loop
               Number_Value := Number (Named (Body_Cursor, (if Index = 1 then "vendor" else "device")));
               if not In_Range (Number_Value, 0, PCI_ID_LAST) then Fail (Invalid_Device_Match); return; end if;
               Decl.Match_Values (Index) := Unsigned_16 (Number_Value);
            end loop;
            --  0xFFFF is "no device" in PCI configuration space.
            if Decl.Match_Values (1) = PCI_ID_LAST then Fail (Invalid_Device_Match); return; end if;
         elsif Kind = "Platform" then
            Decl.Match := Platform_Match;
            Decl.Match_Values (1) := Unsigned_16 (Platform_Device'Enum_Rep
              ((if Alt (Body_Cursor) = "PS2_Controller" then PS2_Controller
                elsif Alt (Body_Cursor) = "ATA_Primary" then ATA_Primary else CMOS_RTC)));
         else
            Fail (Invalid_Device_Match, "device match """ & Kind & """ is not one this tool encodes");
         end if;
      end Add_Match;
      --  For a misspelled member (File_Right.Reed): that type's members.
      function Members_Of (Qualified : String) return String is
         Types : constant CCL.Types.Registry := CCL.Language.Analysis_Types (Checked);
         Dot : Natural := 0;
      begin
         for I in Qualified'Range loop
            if Qualified (I) = '.' then Dot := I; end if;
         end loop;
         if Dot = 0 or else Dot - Qualified'First > CCL.Types.Maximum_Name_Length then return ""; end if;
         declare
            Owner : constant CCL.Types.Type_Reference :=
              CCL.Types.Find (Types, CCL.Types.Named (Qualified (Qualified'First .. Dot - 1)));
            Shape : constant CCL.Types.Description := CCL.Types.Describe (Types, Owner);
            Text : String (1 .. CuBit.Failures.MAXIMUM_TEXT) := [others => ' '];
            Length : Natural := 0;
         begin
            if CCL.Types."/=" (Shape.Form, CCL.Types.Sum) then return ""; end if;
            for Part in 1 .. Shape.Count loop
               declare
                  Name : constant String := (if Length = 0 then "" else ", ") &
                    Qualified (Qualified'First .. Dot) & CCL.Types.Image (Shape.Parts (Part).Identifier);
               begin
                  exit when Length + Name'Length > Text'Length;
                  Text (Length + 1 .. Length + Name'Length) := Name;
                  Length := Length + Name'Length;
               end;
            end loop;
            return (if Length = 0 then "" else "use one of: " & Text (1 .. Length));
         end;
      end Members_Of;
      --  A failure the CCL checker found: in the schema, or in the manifest
      --  (positions then count from the manifest's own first character).
      procedure Explain_Checker_Failure
        (Status : CCL.Language.Interpretation_Status; Diagnostic : CCL.Language.Diagnostic_Code;
         At_Position : Natural; Subject : CCL.Types.Name)
      is
         Offset : constant Natural := Schema_Source'Length + 1;
         In_Schema : constant Boolean := At_Position in 1 .. Offset;
         Position : constant Natural := (if At_Position > Offset then At_Position - Offset else 0);
         Line, Column : Positive := 1;
         Message : constant String :=
           (if Subject.Length > 0 then "field " & CCL.Types.Image (Subject) & ": "
            elsif Position > 0 and then Name_At (Source, Position)'Length > 0
            then """" & Name_At (Source, Position) & """: " else "") &
           (if CCL.Language."=" (Diagnostic, CCL.Language.No_Diagnostic) then CCL.Diagnostics.Message (Status)
            elsif CCL.Language."=" (Diagnostic, CCL.Language.Unknown_Name) then "no such name"
            else CCL.Diagnostics.Message (Diagnostic));
      begin
         for I in Source'First .. Source'First + Natural'Min (Position, Source'Length) - 2 loop
            if Source (I) = ASCII.LF then Line := Line + 1; Column := 1; else Column := Column + 1; end if;
         end loop;
         if In_Schema then
            Fail (Invalid_Expression, "the manifest schema (interfaces/executable-manifest.ccl) does not check at character" &
                  Natural'Image (At_Position) & ": " & Message);
         else
            Fail (Invalid_Expression,
                  (if Position > 0 then "line" & Positive'Image (Line) & ", column" & Positive'Image (Column) & ": "
                   else "the manifest is not an Executable_Manifest: ") & Message,
                  Members_Of (Name_At (Source, Position)));
         end if;
         Result.Expression_Diagnostic := Diagnostic;
         Result.Position := Position;
      end Explain_Checker_Failure;
   begin
      Result := (others => <>);
      Result.In_Catalog := True;
      Keywords.Read_Catalog (Catalog_Source, Cat, Result);
      if not Result.Success then return; end if;
      Result := (others => <>);
      if Schema_Source'Length = 0 then
         Fail (Invalid_Expression, "no manifest schema was given",
               "keep interfaces/executable-manifest.ccl beside the catalogs directory");
         return;
      end if;
      CCL.Catalog.Initialize (Catalog);
      CCL.Catalog.Initialize (Grants);
      --  The schema's declarations and definitions, then the manifest: one
      --  CCL program whose result must be an Executable_Manifest.
      CCL.Language.Analyze (Program, Catalog, Checked);
      if CCL.Language."/=" (CCL.Language.Analysis_Status_Of (Checked), CCL.Language.Analysis_Succeeded) then
         Explain_Checker_Failure
           ((if CCL.Language."=" (CCL.Language.Analysis_Status_Of (Checked), CCL.Language.Analysis_Type_Check_Failed)
             then CCL.Language.Type_Check_Failed else CCL.Language.Parse_Failed),
            CCL.Language.Analysis_Diagnostic (Checked), CCL.Language.Analysis_Diagnostic_Position (Checked),
            CCL.Language.Analysis_Diagnostic_Subject (Checked));
         return;
      end if;
      Manifest_Type := CCL.Types.Find (CCL.Language.Analysis_Types (Checked), CCL.Types.Named (ROOT_TYPE_NAME));
      CCL.Objects.Bind (CCL.Language.Analysis_Types (Checked), Manifest_Type, LOCAL_KEY, Contract, Published);
      if not Published then
         Fail (Invalid_Expression, "the manifest schema does not declare a persistable " & ROOT_TYPE_NAME);
         return;
      end if;
      Evaluate (Program, MANIFEST_FUEL, Catalog, Grants, Host, Contract, Value);
      if Value.Status /= CCL.Language.Succeeded or else not Value.Has_Value then
         Explain_Checker_Failure (Value.Status, Value.Diagnostic, Value.Diagnostic_Position, Value.Diagnostic_Subject);
         return;
      end if;
      Views.Capture (Object, Contract, Value.Value, Captured);
      if not Captured then Fail (Invalid_Expression); return; end if;
      declare
         Root : constant Views.Cursor := Views.Root (Object);
         Requests : constant Views.Cursor := Named (Root, "requests");
         Scopes : constant Views.Cursor := Named (Root, "scopes");
         Streams : constant Views.Cursor := Named (Root, "streams");
      begin
         Locate ("identity");
         Metadata (Named (Root, "identity"), Decl.Identity);
         Locate ("version");
         Metadata (Named (Root, "version"), Decl.Version);
         --  The device match first: resources are relative to it.
         Locate ("device");
         Add_Match (Named (Root, "device"));
         for Index in 1 .. Views.Length (Object, Requests) loop
            exit when Failed;
            Locate (Entry_Text ("requests", Index));
            Add_Request (Views.Element (Object, Requests, Index));
         end loop;
         for Index in 1 .. Views.Length (Object, Scopes) loop
            exit when Failed;
            Locate (Entry_Text ("scopes", Index));
            Add_Scope (Views.Element (Object, Scopes, Index));
         end loop;
         Locate ("streams");
         if Views.Length (Object, Streams) > MAX_STREAMS then Fail (Duplicate_Stream); end if;
         for Index in 1 .. Views.Length (Object, Streams) loop
            exit when Failed;
            Add_Stream (Views.Element (Object, Streams, Index));
         end loop;
         Decl.Explicit_No_Requests := Flag (Named (Root, "requests_none"));
         declare
            Launches : constant Views.Cursor := Named (Root, "may_launch");
         begin
            for Index in 1 .. Views.Length (Object, Launches) loop
               exit when Failed;
               Locate (Entry_Text ("may_launch", Index));
               Add_Launch (Views.Element (Object, Launches, Index));
            end loop;
         end;
      end;
      if Failed then return; end if;
      Encoding.Encode (Decl, Cat, 0, Result);
   end Compile;
end CCL.Manifests.Typed;
