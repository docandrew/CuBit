------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  The children devmgr launches - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with CCL.Resource_Sections;
with CuBit.Handles.Control;
with CuBit.Input;
with CuBit.Launch_Envelopes;
with CuBit.Messages;
with CuBit.Startup_Builder;
with Devmgr_Children;

package body Devmgr_Launches is

   package HC renames CuBit.Handles.Control;
   package ST renames CuBit.Startup_Tables;
   package SB renames CuBit.Startup_Builder;
   package LE renames CuBit.Launch_Envelopes;
   package LC renames Launch_Configuration;
   use type H.Refusal;
   use type ST.Name;

   Spawner : H.Process;
   Roles   : H.Notification;

   --  Service roles devmgr resolves (the catalog's service numbers fit).
   Maximum_Role : constant := 63;
   Services : array (Unsigned_32 range 0 .. Maximum_Role) of H.Endpoint :=
     [others => H.No_Endpoint];

   type Answer_Kind is (Copy_Answer, Placed_Answer, Withheld);
   type Prepared is record
      Label  : ST.Name;
      Kind   : Answer_Kind := Copy_Answer;
      Copy   : H.Derivation;
      Placed : H.Foreign_Handle;
   end record;
   subtype Prepared_Count is Natural range 0 .. ST.Maximum_Entries;
   type Prepared_Array is array (1 .. ST.Maximum_Entries) of Prepared;

   type Launch is record
      Used        : Boolean := False;
      Spawned     : HC.Spawned_Child;
      Manifest    : Driver_Manifests.Manifest;
      Answers     : Prepared_Array;
      Answer_Total : Prepared_Count := 0;
      Payload     : LC.Encoded;
      Name        : String (1 .. Devmgr_Children.Name_Bytes) := [others => ' '];
      Name_Length : Natural range 0 .. Devmgr_Children.Name_Bytes := 0;
   end record;

   Launches : array (Child range 1 .. Child'Last) of Launch;

   procedure Configure (Process_Root : H.Process; Notifications : H.Notification) is
   begin
      Spawner := Process_Root;
      Roles := Notifications;
   end Configure;

   procedure Provide (Role : Unsigned_32; Service : H.Endpoint) is
   begin
      if Role <= Maximum_Role then
         Services (Role) := Service;
      end if;
   end Provide;

   function Service_Of (Role : Unsigned_32) return H.Endpoint is
     (if Role <= Maximum_Role then Services (Role) else H.No_Endpoint);

   function Spawn
     (Name : String; Image : System.Address; Bytes, Priority : Unsigned_64) return Child
   is
      Slot : Child := No_Child;
   begin
      for C in Launches'Range loop
         if not Launches (C).Used then
            Slot := C;
            exit;
         end if;
      end loop;
      if Slot = No_Child or else Bytes = 0 or else Bytes >= Unsigned_64 (Natural'Last) then
         return No_Child;
      end if;
      declare
         L : Launch renames Launches (Slot);
         Contents : constant CCL.Resource_Sections.Byte_Array (1 .. Natural (Bytes))
           with Import, Address => Image;
      begin
         L := (others => <>);
         L.Manifest := Driver_Manifests.Read (Contents);
         declare
            Answer : constant HC.Spawn_Result :=
              HC.Spawn (Spawner, Image, Bytes, Priority, Name, CuBit.Process_IDs.No_Process,
                        L.Manifest.Declared.Limit);
         begin
            if not Answer.Spawned then
               CuBit.Messages.debugPrint
                 ("devmgr: spawn refused: " & Name & " (" &
                  H.Refusal'Image (Answer.Why) & ")" & ASCII.LF);
               return No_Child;
            end if;
            L.Spawned := Answer.Value;
         end;
         L.Used := True;
         L.Name_Length := Natural'Min (Name'Length, Devmgr_Children.Name_Bytes);
         L.Name (1 .. L.Name_Length) := Name (Name'First .. Name'First + L.Name_Length - 1);
         Devmgr_Children.Remember (L.Spawned.Child, Name);
      end;
      return Slot;
   end Spawn;

   function PID (Item : Child) return Process_ID is (Launches (Item).Spawned.Child);
   function Control (Item : Child) return H.Process is (Launches (Item).Spawned.Control);
   function Mailbox (Item : Child) return H.Endpoint is (Launches (Item).Spawned.Mailbox);
   function Space (Item : Child) return H.Cspace is (Launches (Item).Spawned.Space);
   function Child_Self (Item : Child) return H.Foreign_Handle is
     (Launches (Item).Spawned.Child_Self);
   function Child_Self_Process (Item : Child) return H.Foreign_Handle is
     (Launches (Item).Spawned.Child_Self_Process);

   function Interrupt_Of (Item : Child) return Driver_Manifests.Interrupt_Request is
     (Driver_Manifests.Interrupt_Of (Launches (Item).Manifest));

   function Declares (Item : Child; Name : String) return Boolean is
     (Name'Length <= ST.Maximum_Name_Bytes
      and then Launches (Item).Manifest.Startup_Found
      and then ST.Find (Launches (Item).Manifest.Declared, ST.To_Name (Name)) /= 0);

   procedure Remember_Answer (Item : Child; Answer : Prepared) is
      L : Launch renames Launches (Item);
   begin
      for I in 1 .. L.Answer_Total loop
         if L.Answers (I).Label = Answer.Label then
            L.Answers (I) := Answer;
            return;
         end if;
      end loop;
      if L.Answer_Total < ST.Maximum_Entries then
         L.Answer_Total := L.Answer_Total + 1;
         L.Answers (L.Answer_Total) := Answer;
      end if;
   end Remember_Answer;

   procedure Give (Item : Child; Name : String; Copy : H.Derivation) is
   begin
      if Name'Length in 1 .. ST.Maximum_Name_Bytes then
         Remember_Answer (Item, (Label => ST.To_Name (Name), Kind => Copy_Answer,
                                 Copy => Copy, Placed => <>));
      end if;
   end Give;

   procedure Withhold (Item : Child; Name : String) is
   begin
      if Name'Length in 1 .. ST.Maximum_Name_Bytes then
         Remember_Answer (Item, (Label => ST.To_Name (Name), Kind => Withheld,
                                 Copy => <>, Placed => <>));
      end if;
   end Withhold;

   procedure Give_Placed (Item : Child; Name : String; Placed : H.Foreign_Handle) is
   begin
      if Name'Length in 1 .. ST.Maximum_Name_Bytes then
         Remember_Answer (Item, (Label => ST.To_Name (Name), Kind => Placed_Answer,
                                 Copy => <>, Placed => Placed));
      end if;
   end Give_Placed;

   --  The label of the first declared entry of Kind, or an empty name.
   function First_Of (Item : Child; Kind : ST.Capability_Kind) return ST.Name is
      L : Launch renames Launches (Item);
      use type ST.Capability_Kind;
   begin
      if L.Manifest.Startup_Found then
         for I in 1 .. L.Manifest.Declared.Count loop
            if L.Manifest.Declared.Entries (I).Kind = Kind then
               return L.Manifest.Declared.Entries (I).Label;
            end if;
         end loop;
      end if;
      return (others => <>);
   end First_Of;

   function Give_First
     (Item : Child; Kind : ST.Capability_Kind; Copy : H.Derivation) return Boolean
   is
      Label : constant ST.Name := First_Of (Item, Kind);
   begin
      if Label.Length = 0 then
         return False;
      end if;
      Remember_Answer (Item, (Label => Label, Kind => Copy_Answer, Copy => Copy, Placed => <>));
      return True;
   end Give_First;

   function Give_First_Placed
     (Item : Child; Kind : ST.Capability_Kind; Placed : H.Foreign_Handle) return Boolean
   is
      Label : constant ST.Name := First_Of (Item, Kind);
   begin
      if Label.Length = 0 then
         return False;
      end if;
      Remember_Answer (Item, (Label => Label, Kind => Placed_Answer, Copy => <>, Placed => Placed));
      return True;
   end Give_First_Placed;

   function Place (Item : Child; Copy : H.Derivation) return Unsigned_64 is
      Result : constant H.Install_Result := HC.Install (Launches (Item).Spawned.Space, Copy);
   begin
      if Result.Installed then
         return HC.Table_Word (Result.Placed);
      end if;
      CuBit.Messages.debugPrint
        ("devmgr: install refused for " &
         Launches (Item).Name (1 .. Launches (Item).Name_Length) & " (" &
         H.Refusal'Image (Result.Why) & ")" & ASCII.LF);
      return 0;
   end Place;

   procedure Set_Payload (Item : Child; Block : LC.Encoded) is
   begin
      Launches (Item).Payload := Block;
   end Set_Payload;

   ---------------------------------------------------------------------------
   --  Building the startup table
   ---------------------------------------------------------------------------
   function Rights_Of (Kind : CCL.Resource_Sections.Rights_Kind) return H.Rights is
     (case Kind is
         when CCL.Resource_Sections.Read_Only  => H.Read_Only,
         when CCL.Resource_Sections.Write_Only => H.Write_Only,
         when CCL.Resource_Sections.Read_Write => H.Read_Write);

   function Outcome_Of (Why : H.Refusal) return SB.Mint_Outcome is
     (case Why is
         when H.ABI.Limit_Reached | H.ABI.Table_Full => SB.Limit_Reached,
         when H.ABI.Out_Of_Memory => SB.Out_Of_Memory,
         when others => SB.Refused_By_Policy);

   type Context is record
      Item : Child := No_Child;
   end record;

   procedure Install_Copy
     (Item : Child; Copy : H.Derivation; Handle : out Unsigned_64;
      Outcome : out SB.Mint_Outcome)
   is
      Result : constant H.Install_Result := HC.Install (Launches (Item).Spawned.Space, Copy);
   begin
      if Result.Installed then
         Handle := HC.Table_Word (Result.Placed);
         Outcome := (if Handle = 0 then SB.Refused_By_Policy else SB.Granted);
      else
         Handle := 0;
         Outcome := Outcome_Of (Result.Why);
      end if;
   end Install_Copy;

   procedure Mint
     (Into : in out Context; Item : ST.Startup_Entry; Index : ST.Entry_Index;
      Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
   is
      L : Launch renames Launches (Into.Item);
      Origin : constant Driver_Manifests.Source := Driver_Manifests.Source_Of (L.Manifest, Index);
      use type Driver_Manifests.Source_Kind;
   begin
      Handle := 0;
      Outcome := SB.Not_Available;
      for I in 1 .. L.Answer_Total loop
         if L.Answers (I).Label = Item.Label then
            case L.Answers (I).Kind is
               when Copy_Answer =>
                  Install_Copy (Into.Item, L.Answers (I).Copy, Handle, Outcome);
               when Placed_Answer =>
                  Handle := HC.Table_Word (L.Answers (I).Placed);
                  Outcome := (if Handle = 0 then SB.Not_Available else SB.Granted);
               when Withheld =>
                  Handle := 0;
                  Outcome := SB.Not_Available;
            end case;
            return;
         end if;
      end loop;
      if Origin.Kind /= Driver_Manifests.Capability_Source then
         return;
      end if;
      declare
         Request : constant Driver_Manifests.Capability_Request :=
           L.Manifest.Capabilities (Origin.Index);
      begin
         case Item.Kind is
            when ST.Endpoint =>
               if H.Valid (Service_Of (Request.Service)) then
                  Install_Copy (Into.Item,
                    H.Copy (Service_Of (Request.Service), Rights_Of (Request.Rights)),
                    Handle, Outcome);
               end if;
            when ST.Notification =>
               --  An input role's publish notification (Read: publication;
               --  Write is registration) carries the input badge, which
               --  Desktop requires of every source report.
               if CuBit.Input.Is_Input_Role (Unsigned_64 (Request.Service))
                 and then Request.Rights in CCL.Resource_Sections.Read_Only
               then
                  Install_Copy (Into.Item,
                    H.Badged (Roles, H.Read_Only, Unsigned_64 (Request.Service), 1,
                              CuBit.Input.Publisher_Tag),
                    Handle, Outcome);
               else
                  Install_Copy (Into.Item,
                    H.Narrowed (Roles, Rights_Of (Request.Rights),
                                Unsigned_64 (Request.Service), 1),
                    Handle, Outcome);
               end if;
            when others =>
               null;
         end case;
      end;
   end Mint;

   procedure Report
     (Into : in out Context; Item : ST.Startup_Entry; Index : ST.Entry_Index;
      Outcome : SB.Mint_Outcome)
   is
      pragma Unreferenced (Index);
      use type SB.Mint_Outcome;
      L : Launch renames Launches (Into.Item);
   begin
      if Outcome /= SB.Granted then
         CuBit.Messages.debugPrint
           ("devmgr: " & L.Name (1 .. L.Name_Length) & " startup capability " &
            String (Item.Label.Text (1 .. Item.Label.Length)) & " " &
            SB.Mint_Outcome'Image (Outcome) & ASCII.LF);
      end if;
   end Report;

   procedure Build is new SB.Build (Context, Mint, Report);

   Envelope : LE.Block (1 .. LE.Maximum_Block_Bytes);

   function Install (Item : Child) return Boolean is
      L : Launch renames Launches (Item);
      Length : LE.Block_Length := 0;
      Fits : Boolean := True;
      Into : Context := (Item => Item);
      Built : ST.Table;
      Result : SB.Build_Result;
      use type SB.Build_Status;
   begin
      --  The payload, as little-endian words.
      for W in 1 .. L.Payload.Length loop
         for B in 0 .. LC.Word_Bytes - 1 loop
            Envelope (Length + 1) :=
              Unsigned_8 (Shift_Right (L.Payload.Words (W), 8 * B) and 16#FF#);
            Length := Length + 1;
         end loop;
      end loop;
      if L.Manifest.Startup_Found then
         Build (Into, L.Manifest.Declared,
                HC.Table_Word (L.Spawned.Child_Self),
                HC.Table_Word (L.Spawned.Child_Self_Process), Built, Result);
         if Result.Status /= SB.Ready then
            CuBit.Messages.debugPrint
              ("devmgr: " & L.Name (1 .. L.Name_Length) & " startup table refused: " &
               SB.Build_Status'Image (Result.Status) & ASCII.LF);
            return False;
         end if;
         declare
            Table : ST.Bytes (1 .. ST.Maximum_Table_Bytes);
            Table_Length : ST.Table_Length;
         begin
            ST.Encode (Built, Table, Table_Length);
            LE.Append (Envelope, Length, Table (1 .. Table_Length), Fits);
         end;
         if not Fits then
            return False;
         end if;
      end if;
      if Length = 0 then
         return True;
      end if;
      return HC.Install_Launch_Block
        (L.Spawned.Child, L.Spawned.Control, Envelope'Address, Unsigned_64 (Length));
   end Install;

   Unlimited : constant Unsigned_64 := 0;

   function Resume (Item : Child) return Boolean is
     (HC.Resume (Launches (Item).Spawned.Child, Launches (Item).Spawned.Control,
                 Unlimited, Unlimited));

   procedure Abandon (Item : Child) is
      Ignore : Boolean;
   begin
      Ignore := HC.Kill (Launches (Item).Spawned.Child, Launches (Item).Spawned.Control);
   end Abandon;

   function Start (Item : Child) return Boolean is
   begin
      if Install (Item) and then Resume (Item) then
         return True;
      end if;
      CuBit.Messages.debugPrint
        ("devmgr: start failed: " &
         Launches (Item).Name (1 .. Launches (Item).Name_Length) & ASCII.LF);
      Abandon (Item);
      return False;
   end Start;

end Devmgr_Launches;
