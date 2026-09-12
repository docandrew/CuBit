with CCL.VM;

package body CCL.Manifests with SPARK_Mode => On is
   use Interfaces;
   use type CCL.Language.Interpretation_Status;
   use type CCL.VM.Value_Kind;

   MAX_REQUESTS : constant := MAX_BINDINGS;
   --  Current procmgr ABI: slot 0 is bootstrap, slot 63 is the reply slot.
   subtype Slot_Number is Natural range 1 .. 62;
   type Rights_Kind is (Read_Only, Write_Only, Read_Write);
   for Rights_Kind use (Read_Only => 1, Write_Only => 2, Read_Write => 3);
   type Request is record
      Service : Unsigned_32 := 0;
      Rights : Rights_Kind := Read_Only;
      Slot : Slot_Number := Slot_Number'First;
      Name : Binding_Name;
   end record;
   type Request_Array is array (Positive range 1 .. MAX_REQUESTS) of Request;
   subtype Metadata_Text is Binding_Name;

   type Service_Binding is record
      Name : Metadata_Text;
      ID : Unsigned_32 := 0;
      Rights : Rights_Kind := Read_Only;
   end record;
   type Service_Array is array (Positive range 1 .. MAX_REQUESTS) of Service_Binding;

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

      procedure Read_Rights (Rights : out Rights_Kind) is
         Name : Metadata_Text;
      begin
         Rights := Read_Only;
         Atom (Name);
         if Is_Text (Name, "read") then Rights := Read_Only;
         elsif Is_Text (Name, "write") then Rights := Write_Only;
         elsif Is_Text (Name, "read-write") then Rights := Read_Write;
         else Fail (Unknown_Rights, Cursor);
         end if;
      end Read_Rights;

      procedure Add_Request is
         Name : Metadata_Text;
         Item : Request;
         Offered : Rights_Kind := Read_Only;
         Found : Boolean := False;
      begin
         Atom (Name);
         for Binding of Services (1 .. Service_Count) loop
            if Name = Binding.Name then
               Item.Service := Binding.ID;
               Offered := Binding.Rights;
               Found := True;
            end if;
         end loop;
         if not Found then Fail (Unknown_Service, Cursor); end if;
         Read_Rights (Item.Rights);
         if Offered /= Read_Write and then Item.Rights /= Offered then
            Fail (Rights_Not_Offered, Cursor);
         end if;
         Atom (Item.Name);
         if Failed then return; end if;
         -- Lowercase kebab names map injectively to Ada Slot_<name> identifiers.
         -- No trailing/consecutive hyphens, underscores, quotes, or case aliases.
         if Item.Name.Length = 0 or else Item.Name.Data (1) not in 'a' .. 'z' then
            Fail (Invalid_Binding_Name, Cursor);
            return;
         end if;
         for Index in 1 .. Item.Name.Length loop
            declare
               C : constant Character := Item.Name.Data (Index);
            begin
               if C = '-' then
                  if Index = Item.Name.Length or else Item.Name.Data (Index + 1) = '-' then
                     Fail (Invalid_Binding_Name, Cursor);
                  end if;
               elsif C not in 'a' .. 'z' | '0' .. '9' then
                  Fail (Invalid_Binding_Name, Cursor);
               end if;
            end;
         end loop;
         for Index in 1 .. Count loop
            if Requests (Index).Name = Item.Name then Fail (Duplicate_Binding, Cursor); end if;
         end loop;
         if Count = MAX_REQUESTS then Fail (Too_Many_Requests, Cursor);
         elsif Count > Last_Slot - First_Slot then Fail (Slots_Exhausted, Cursor);
         elsif not Failed then
            Item.Slot := First_Slot + Count;
            Count := Count + 1;
            Requests (Count) := Item;
         end if;
      end Add_Request;

      procedure Slot_Expression (Slot : out Slot_Number) is
      begin
         Slot := Slot_Number'First;
         Expression;
         if Failed then return; end if;
         if not Value.Has_Value or else Value.Has_Text or else Value.Has_Character
           or else Value.Result_Value.Kind /= CCL.VM.Integer_Value
           or else Value.Result_Value.Integer not in
             Integer_64 (Slot_Number'First) .. Integer_64 (Slot_Number'Last)
         then Fail (Invalid_Slot, Cursor);
         else Slot := Natural (Value.Result_Value.Integer);
         end if;
      end Slot_Expression;

      procedure Header (Kind : String) is
         Name : Metadata_Text;
      begin
         Expect ('(');
         Atom (Name);
         if not Is_Text (Name, Kind) then Fail (Unknown_Declaration, 1); end if;
         Expression;
         if not Failed and then
           (not Value.Has_Value or else Value.Has_Text or else Value.Has_Character
            or else Value.Result_Value.Kind /= CCL.VM.Integer_Value
            or else Value.Result_Value.Integer /= 1)
         then Fail (Unsupported_Version, Cursor);
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
            else
               if not Is_Text (Name, "service") then Fail (Unknown_Declaration, Cursor); end if;
               Atom (Item.Name);
               Expression;
               if Failed then return; end if;
               if not Value.Has_Value or else Value.Has_Text or else Value.Has_Character
                 or else Value.Result_Value.Kind /= CCL.VM.Integer_Value
                 or else Value.Result_Value.Integer not in 1 .. Integer_64 (Unsigned_32'Last)
               then
                  Fail (Invalid_Service_ID, Cursor);
                  return;
               end if;
               Item.ID := Unsigned_32 (Value.Result_Value.Integer);
               Read_Rights (Item.Rights);
               Expect (')');
               for Binding of Services (1 .. Service_Count) loop
                  if Binding.Name = Item.Name or else Binding.ID = Item.ID then
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
         elsif Is_Text (Name, "request-service") then Add_Request;
         else Fail (Unknown_Declaration, Cursor);
         end if;
         Expect (')');
      end loop;
      Expect (')');
      Skip;
      if Cursor <= Last then Fail (Trailing_Input, Cursor); end if;
      if not Have_Identity or else not Have_Version then Fail (Missing_Field, Cursor); end if;
      if Failed then return; end if;

      --  Canonical little-endian wire ABI; no host layout/padding dependence.
      Append (Result.Identity, 16#4449_4243#, 4);
      Append (Result.Identity, 1, 2);
      Append (Result.Identity, 2, 2);
      Pair ("id", Identity);
      Pair ("version", Version);
      Append (Result.Capabilities, 16#4342_4954#, 4);
      Append (Result.Capabilities, 1, 2);
      Append (Result.Capabilities, Unsigned_64 (Count), 2);
      for Item of Requests (1 .. Count) loop
         Append (Result.Capabilities, 2, 1); -- REQ_SERVICE, not a capability grant
         Append (Result.Capabilities, Unsigned_64 (Rights_Kind'Enum_Rep (Item.Rights)), 1);
         Append (Result.Capabilities, Unsigned_64 (Item.Slot), 2);
         Append (Result.Capabilities, Unsigned_64 (Item.Service), 4);
         Append (Result.Capabilities, 0, 8);
      end loop;
      Result.Binding_Count := Count;
      for Index in 1 .. Count loop
         Result.Bindings (Index) := (Name => Requests (Index).Name,
                                     Slot => Requests (Index).Slot);
      end loop;
      Result.Success := True;
   end Compile;
end CCL.Manifests;
