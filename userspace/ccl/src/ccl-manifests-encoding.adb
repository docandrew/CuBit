with Interfaces;
with CuBit.Channel_Declarations;
with CuBit.Network_Authority;
with CuBit.Launch_Authority;
with CuBit.Program_Descriptions;
with CuBit.Startup_Tables;

package body CCL.Manifests.Encoding with SPARK_Mode => On is
   use Interfaces;
   use Model;
   package ST renames CuBit.Startup_Tables;

   --  The startup table holds every request plus the launcher's two self
   --  entries, in a section of the usual size.
   pragma Compile_Time_Error
     (MAX_REQUESTS + 2 > ST.Maximum_Entries or else MAX_NAME_TEXT > ST.Maximum_Name_Bytes
      or else ST.Maximum_Table_Bytes > MAX_SECTION_BYTES,
      "a manifest's startup table must fit CuBit.Startup_Tables and a section");

   --  The startup kind of each request kind.
   function Startup_Kind (Kind : Request_Kind) return ST.Capability_Kind is
     (case Kind is
         when Framebuffer_Request   => ST.Framebuffer,
         when Service_Request       => ST.Endpoint,
         when Notification_Request  => ST.Notification,
         when Network_Request       => ST.Network,
         when Render_Request        => ST.Render,
         when Firmware_Snapshot_Request => ST.Firmware_Snapshot,
         when Device_Memory_Request => ST.Device_Memory,
         when IO_Port_Request       => ST.IO_Ports,
         when Interrupt_Request     => ST.Interrupt,
         when DMA_Request           => ST.DMA,
         when Scheduling_Request    => ST.Scheduling);

   procedure Encode
     (Decl : in out Model.Declaration;
      Position : Natural; Result : in out Compilation_Result)
   is
      Requests : Request_Array renames Decl.Requests;
      Count : Request_Count renames Decl.Count;
      Scopes : Scope_Array renames Decl.Scopes;
      Scope_Count : Scope_Count_Type renames Decl.Scope_Count;
      Basis : Sections.Basis_Info renames Decl.Basis;
      Explicit_No_Requests : Boolean renames Decl.Explicit_No_Requests;
      Identity : Metadata_Text renames Decl.Identity;
      Version : Metadata_Text renames Decl.Version;
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
         Append (Result.Capabilities, 0, 2);   --  reserved
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
      --  .cubit.devices: the drives list (CCL.Device_Sections).
      if Decl.Drives.Count > 0 then
         declare
            Bytes : Devices.Section_Bytes;
            Length : Devices.Section_Length;
         begin
            Devices.Encode (Decl.Drives, Bytes, Length);
            for I in 1 .. Length loop
               Append (Result.Devices, Unsigned_64 (Bytes (I)), 1);
            end loop;
         end;
      end if;
      --  .cubit.resources: "CBRS", version, entry count; a 16-byte basis
      --  (kind, reserved, three 16-bit values, reserved); then 24-byte
      --  entries (kind, rights, reserved u16, index, amount, extra).
      if Resource_Count > 0 then
         Append (Result.Resources, Sections.MAGIC, 4);
         Append (Result.Resources, Sections.VERSION, 2);
         Append (Result.Resources, Unsigned_64 (Resource_Count), 2);
         Append (Result.Resources, Unsigned_64 (Basis_Kind'Enum_Rep (Basis.Kind)), 1);
         Append (Result.Resources, 0, 1);
         for Number of Basis.Values loop
            Append (Result.Resources, Unsigned_64 (Number), 2);
         end loop;
         Append (Result.Resources, 0, 8);
         for Item of Requests (1 .. Count) loop
            if Item.Kind in Resource_Request then
               Append (Result.Resources, Unsigned_64 (Request_Kind'Enum_Rep (Item.Kind)), 1);
               Append (Result.Resources, Unsigned_64 (Rights_Kind'Enum_Rep (Item.Rights)), 1);
               Append (Result.Resources, 0, 2);   --  reserved
               Append (Result.Resources, Unsigned_64 (Item.Index), 4);
               Append (Result.Resources, Item.Amount, 8);
               Append (Result.Resources, Item.Extra, 8);
            end if;
         end loop;
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
      --  An optional render session (either frontend's spelling) is an
      --  optional startup capability.
      for Item of Requests (1 .. Count) loop
         if Item.Kind = Render_Request and then Item.Service = 1 then
            Item.Requirement := ST.Optional;
         end if;
         --  A DMA region is memory devmgr allocates and maps, not a
         --  capability: its startup entry is always Absent (launchers record
         --  it so), so it is optional.
         if Item.Kind = DMA_Request then
            Item.Requirement := ST.Optional;
         end if;
      end loop;
      --  .cubit.startup: the declared table (CuBit.Startup_Tables), every
      --  entry Unassigned, in request order; emitted only when it decodes.
      declare
         Table : ST.Table := ST.Empty_Table;
         Bytes : ST.Bytes (1 .. ST.Maximum_Table_Bytes);
         Length : ST.Table_Length;
         Check : ST.Table;
         Outcome : ST.Decode_Result;
         use type ST.Decode_Result;
      begin
         Table.Limit := Decl.Handles;
         for Index in 1 .. Count loop
            declare
               Name : constant String := Requests (Index).Name.Data (1 .. Requests (Index).Name.Length);
            begin
               if Name = ST.Self_Text or else Name = ST.Self_Process_Text then
                  Fail (Reserved_Binding_Name, Cursor);
                  return;
               end if;
               if not ST.Valid_Name_Text (Name) then
                  Fail (Invalid_Binding_Name, Cursor);
                  return;
               end if;
               Table.Entries (Index) :=
                 (Label => ST.To_Name (Name), Kind => Startup_Kind (Requests (Index).Kind),
                  Requirement => Requests (Index).Requirement, State => ST.Unassigned,
                  Handle => ST.No_Handle);
               Table.Count := Index;
            end;
         end loop;
         if not ST.Valid_Table (Table) then
            Fail (Duplicate_Binding, Cursor);
            return;
         end if;
         ST.Encode (Table, Bytes, Length);
         ST.Decode (Bytes (1 .. Length), Check, Outcome);
         if Outcome /= ST.Decoded then
            Fail (Invalid_Binding_Name, Cursor);
            return;
         end if;
         for I in 1 .. Length loop
            Result.Startup.Data (I) := Bytes (I);
         end loop;
         Result.Startup.Length := Length;
         Result.Handles := Decl.Handles;
         --  .cubit.channels: the declared offers, when there are any.
         declare
            package CD renames CuBit.Channel_Declarations;
            Offers : CD.Table := (Count => 0, Offers => [others => (others => <>)]);
            Raw : CD.Bytes (1 .. CD.Maximum_Table_Bytes);
            Raw_Length : CD.Table_Length;
            Check_Table : CD.Table;
            Check_Outcome : CD.Decode_Result;
            use type CD.Decode_Result;
         begin
            for I in 1 .. Decl.Channel_Count loop
               declare
                  Name : constant String :=
                    Decl.Channels (I).Contract.Data (1 .. Decl.Channels (I).Contract.Length);
               begin
                  Offers.Offers (I).Contract.Length := Name'Length;
                  for K in Name'Range loop
                     Offers.Offers (I).Contract.Text (K - Name'First + 1) := Name (K);
                  end loop;
                  Offers.Offers (I).Per_Client := Decl.Channels (I).Per_Client;
                  Offers.Offers (I).Total := Decl.Channels (I).Total;
                  Offers.Offers (I).Class := Decl.Channels (I).Class;
                  Offers.Count := I;
               end;
            end loop;
            Result.Channel_Offers := Offers;
            Result.Channels.Length := 0;
            if Offers.Count > 0 then
               if not CD.Valid_Table (Offers) then
                  Fail (Invalid_Channel_Limit, Cursor);
                  return;
               end if;
               CD.Encode (Offers, Raw, Raw_Length);
               CD.Decode (Raw (1 .. Raw_Length), Check_Table, Check_Outcome);
               if Check_Outcome /= CD.Decoded then
                  Fail (Invalid_Channel_Limit, Cursor);
                  return;
               end if;
               for I in 1 .. Raw_Length loop
                  Result.Channels.Data (I) := Raw (I);
               end loop;
               Result.Channels.Length := Raw_Length;
            end if;
         end;
      end;
      Result.Binding_Count := Count;
      for Index in 1 .. Count loop
         Result.Bindings (Index) := (Name => Requests (Index).Name,
                                     Kind => Startup_Kind (Requests (Index).Kind),
                                     Requirement => Requests (Index).Requirement,
                                     Client => Requests (Index).Client);
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
      --  .cubit.description: the descriptor CuBit.Program_Descriptions decodes;
      --  emitted only when it decodes.
      if Decl.Description.Parameter_Total > 0 or else Decl.Description.Piece_Total > 0
        or else Decl.Description.Connector_Total > 0
      then
         declare
            package PP renames CuBit.Program_Descriptions;
            use type PP.Piece_Kind;
            S : PP.Signature renames Decl.Description;
         begin
            Write_Text (Result.Description, "PDSC");
            Append (Result.Description, PP.Version, 2);
            Append (Result.Description, Unsigned_64 (S.Parameter_Total), 1);
            Append (Result.Description, Unsigned_64 (S.Piece_Total), 1);
            Append (Result.Description, Unsigned_64 (S.Connector_Total), 1);
            Append (Result.Description, Unsigned_64 (S.Descriptor_Total), 1);
            Append (Result.Description, 0, 2);
            for P of S.Parameters (0 .. S.Parameter_Total - 1) loop
               Append (Result.Description, Unsigned_64 (PP.Kind'Enum_Rep (P.Of_Kind)), 1);
               Append (Result.Description,
                       (if P.Many then Unsigned_64 (PP.Many_Flag) else 0)
                       + (if P.Optional then Unsigned_64 (PP.Optional_Flag) else 0), 1);
               Append (Result.Description, Unsigned_64 (P.Name_Length), 1);
               Write_Text (Result.Description, P.Name (1 .. P.Name_Length));
            end loop;
            for Item of S.Pieces (1 .. S.Piece_Total) loop
               Append (Result.Description, Unsigned_64 (PP.Piece_Kind'Enum_Rep (Item.Of_Kind)), 1);
               Append (Result.Description,
                       (if Item.Of_Kind = PP.Literal then 0 else Unsigned_64 (Item.Parameter)), 1);
               Append (Result.Description,
                       (if Item.Of_Kind = PP.Value then 0 else Unsigned_64 (Item.Text_Length)), 1);
               if Item.Of_Kind /= PP.Value then
                  Write_Text (Result.Description, Item.Text (1 .. Item.Text_Length));
               end if;
            end loop;
            for P of S.Connectors (0 .. S.Connector_Total - 1) loop
               Append (Result.Description, Unsigned_64 (PP.Connector_Direction'Enum_Rep (P.Direction)), 1);
               Append (Result.Description, Unsigned_64 (PP.Element_Kind'Enum_Rep (P.Element)), 1);
               Append (Result.Description, Unsigned_64 (PP.Signal_Kind'Enum_Rep (P.Signal)), 1);
               Append (Result.Description, Unsigned_64 (P.Pages), 1);
               Append (Result.Description, Unsigned_64 (P.Name_Length), 1);
               Write_Text (Result.Description, P.Name (1 .. P.Name_Length));
            end loop;
            for D of S.Descriptors (1 .. S.Descriptor_Total) loop
               Append (Result.Description, Unsigned_64 (D.Number), 1);
               Append (Result.Description, Unsigned_64 (D.Target), 1);
            end loop;
            declare
               Bytes : PP.Bytes (1 .. Result.Description.Length);
               Check : PP.Signature;
               Accepted : Boolean;
            begin
               for I in Bytes'Range loop Bytes (I) := Result.Description.Data (I); end loop;
               if Bytes'Length > PP.Maximum_Descriptor_Bytes then
                  Accepted := False;
               else
                  PP.Decode (Bytes, Check, Accepted);
               end if;
               if not Accepted then
                  Result.Success := False;
                  Result.Diagnostic := Invalid_Parameters;
                  Result.Description := (others => <>);
               end if;
            end;
         end;
      end if;
   end Encode;
end CCL.Manifests.Encoding;
