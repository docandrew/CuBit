with Interfaces;
with CCL.Resource_Sections;
with CuBit.Network_Authority;
with CuBit.Launch_Authority;

package body CCL.Manifests.Encoding with SPARK_Mode => On is
   use Interfaces;
   use Model;
   use all type CCL.Resource_Sections.Match_Kind;

   procedure Encode
     (Decl : in out Model.Declaration; Catalog : Model.Catalog_Model;
      Position : Natural; Result : in out Compilation_Result)
   is
      Requests : Request_Array renames Decl.Requests;
      Count : Request_Count renames Decl.Count;
      Scopes : Scope_Array renames Decl.Scopes;
      Scope_Count : Scope_Count_Type renames Decl.Scope_Count;
      Streams : Stream_Array renames Decl.Streams;
      Stream_Order : Stream_Order_Array renames Decl.Stream_Order;
      Stream_Count : Stream_Count_Type renames Decl.Stream_Count;
      Match : Match_Kind renames Decl.Match;
      Match_Values : Match_Value_Array renames Decl.Match_Values;
      Explicit_No_Requests : Boolean renames Decl.Explicit_No_Requests;
      Identity : Metadata_Text renames Decl.Identity;
      Version : Metadata_Text renames Decl.Version;
      Fixed : Binding_Array renames Catalog.Fixed;
      Fixed_Count : Natural renames Catalog.Fixed_Count;
      First_Slot : Slot_Number renames Catalog.First_Slot;
      Last_Slot : Slot_Number renames Catalog.Last_Slot;
      Cursor : constant Natural := Position;
      Failed : Boolean := False;
      Resource_Count, Capability_Count : Request_Count := 0;

      procedure Fail (Code : Diagnostic_Code; At_Position : Natural) is
      begin
         if not Failed then
            Failed := True;
            Result.Diagnostic := Code;
            Result.Position := At_Position;
         end if;
      end Fail;

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

   begin
      for Item of Requests (1 .. Count) loop
         if Item.Kind in Resource_Request then
            Resource_Count := Resource_Count + 1;
         end if;
      end loop;
      Capability_Count := Count - Resource_Count;
      if Explicit_No_Requests and then Capability_Count > 0 then
         Fail (Duplicate_Field, Cursor);
      end if;
      if Failed then return; end if;
      Allocate_Slots;
      if Failed then return; end if;
      --  Resources never take the reserved saved-reply slot.
      for Item of Requests (1 .. Count) loop
         if Item.Kind in Resource_Request and then Item.Slot > Sections.Slot_Number'Last then
            Fail (Invalid_Slot, Cursor);
            return;
         end if;
      end loop;

      --  Canonical little-endian wire ABI; no host layout/padding dependence.
      Append (Result.Identity, 16#4449_4243#, 4);
      Append (Result.Identity, 1, 2);
      Append (Result.Identity, 2, 2);
      Pair ("id", Identity);
      Pair ("version", Version);
      if Capability_Count > 0 or else Explicit_No_Requests then
      Append (Result.Capabilities, 16#4342_4954#, 4);
      Append (Result.Capabilities, 1, 2);
      Append (Result.Capabilities, Unsigned_64 (Capability_Count), 2);
      for Item of Requests (1 .. Count) loop
         if Item.Kind not in Resource_Request then
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
         end if;
      end loop;
      end if;
      --  .cubit.resources: "CBRS", version, entry count; a 16-byte match
      --  (kind, reserved, three 16-bit values, reserved); then 24-byte
      --  entries (kind, rights, slot, index, amount, extra).
      if Resource_Count > 0 then
         Append (Result.Resources, Sections.MAGIC, 4);
         Append (Result.Resources, Sections.VERSION, 2);
         Append (Result.Resources, Unsigned_64 (Resource_Count), 2);
         Append (Result.Resources, Unsigned_64 (Match_Kind'Enum_Rep (Match)), 1);
         Append (Result.Resources, 0, 1);
         for Number of Match_Values loop
            Append (Result.Resources, Unsigned_64 (Number), 2);
         end loop;
         Append (Result.Resources, 0, 8);
         for Item of Requests (1 .. Count) loop
            if Item.Kind in Resource_Request then
               Append (Result.Resources, Unsigned_64 (Request_Kind'Enum_Rep (Item.Kind)), 1);
               Append (Result.Resources, Unsigned_64 (Rights_Kind'Enum_Rep (Item.Rights)), 1);
               Append (Result.Resources, Unsigned_64 (Item.Slot), 2);
               Append (Result.Resources, Unsigned_64 (Item.Index), 4);
               Append (Result.Resources, Item.Amount, 8);
               Append (Result.Resources, Item.Extra, 8);
            end if;
         end loop;
      elsif Match /= No_Match then
         --  A match without resources grants nothing; reject it as a mistake.
         Fail (Missing_Device_Match, Cursor);
         return;
      end if;
      --  Emit only what startup's decoder accepts.
      if Result.Resources.Length > 0 then
         declare
            Bytes : Sections.Byte_Array (1 .. Result.Resources.Length);
            Plan : Sections.Section_Plan;
            Status : Sections.Decode_Status;
            use type Sections.Decode_Status;
         begin
            for I in Bytes'Range loop Bytes (I) := Result.Resources.Data (I); end loop;
            if Bytes'Length > Sections.MAX_SECTION_BYTES then
               Fail (Too_Many_Requests, Cursor);
               return;
            end if;
            Sections.Decode (Bytes, Plan, Status);
            if Status /= Sections.Decoded then
               Fail (Invalid_Device_Resource, Cursor);
               return;
            end if;
         end;
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
      --  .cubit.launch: "LNCH", version, count, then each name as a length
      --  byte and its bytes; emitted only when it is one procmgr accepts.
      if Decl.Launch_Count > 0 then
         Append (Result.Launch, Unsigned_64 (CuBit.Launch_Authority.Magic), 4);
         Append (Result.Launch, CuBit.Launch_Authority.Table_Version, 2);
         Append (Result.Launch, Unsigned_64 (Decl.Launch_Count), 2);
         for Name of Decl.Launches (1 .. Decl.Launch_Count) loop
            Append (Result.Launch, Unsigned_64 (Name.Length), 1);
            Write_Text (Result.Launch, Name.Data (1 .. Name.Length));
         end loop;
         declare
            Bytes : CuBit.Launch_Authority.Table_Bytes (1 .. Result.Launch.Length);
         begin
            for I in Bytes'Range loop Bytes (I) := Result.Launch.Data (I); end loop;
            if Bytes'Length > CuBit.Launch_Authority.Maximum_Table_Bytes
              or else not CuBit.Launch_Authority.Valid (Bytes)
            then
               Result.Success := False;
               Result.Diagnostic := Invalid_Launch;
               Result.Launch := (others => <>);
            end if;
         end;
      end if;
   end Encode;
end CCL.Manifests.Encoding;
