------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  FAT32 volumes (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Directory_Paths;
with CuBit.File_Access;
with CuBit.System_Facts;
with CuBit.Monotonic;
with Fat_Format; use Fat_Format;
with Fat_Names; use Fat_Names;
with Fat_Chains; use Fat_Chains;

package body Fat32 is

   use type System.Address;
   use Volume_Admission;
   package DP renames CuBit.Directory_Pages;

   --  The metadata cache (FAT and directory sectors, write-through), per
   --  volume, and how far a miss reads ahead.
   Cache_Bytes : constant := 1024 * 1024;
   Read_Ahead_Sectors : constant := 8;
   --  Clusters one growth step allocates and links at once.
   Batch_Clusters : constant := 256;
   --  The most a file holds (DIR_FileSize).
   Largest_File : constant Unsigned_64 := Unsigned_64 (Unsigned_32'Last);

   --  The synthesized inode's modes (S_IFDIR 0755, S_IFREG 0644 and 0444).
   Mode_Directory      : constant Unsigned_16 := 8#040755#;
   Mode_File           : constant Unsigned_16 := 8#100644#;
   Mode_Read_Only_File : constant Unsigned_16 := 8#100444#;
   Ext2_Sector_Bytes   : constant := 512;   --  i_blocks counts 512-byte units
   Attributes_Slot     : constant := 1;     --  directBlocks (1): DIR_Attr
   Mode_Type_Shift     : constant := 12;
   Directory_Type      : constant Unsigned_16 := 8#04#;
   Regular_Type        : constant Unsigned_16 := 8#10#;

   function Ext2_Type (Item : Ext2_Inodes.Inode) return Unsigned_16 is
     (Shift_Right (Item.typeAndPermissions, Mode_Type_Shift));

   Requests_Total, Barriers_Total : Unsigned_64 := 0;

   procedure Device_Requests (Requests, Barriers : out Unsigned_64) is
   begin
      Requests := Requests_Total;
      Barriers := Barriers_Total;
   end Device_Requests;

   ---------------------------------------------------------------------------
   --  Private stores
   ---------------------------------------------------------------------------
   type Sector_Tags is array (Natural range <>) of Sector_Number;
   type Slot_Flags is array (Natural range <>) of Boolean;
   --  Slot K (from 1) holds bytes (K - 1) * sector size onwards of Data.
   type Cache_Store (Slots : Positive; Last_Byte : Natural) is record
      Tag   : Sector_Tags (1 .. Slots) := [others => 0];
      Valid : Slot_Flags (1 .. Slots) := [others => False];
      Data  : Byte_Array (0 .. Last_Byte);
   end record;

   type Object is record
      Position  : Unsigned_64 := 0;     --  its short entry: volume byte offset / 32 (0: none)
      Parent    : Unsigned_32 := 0;     --  the directory holding it
      First     : Cluster_Number := No_Cluster;
      Size      : Unsigned_32 := 0;
      Directory : Boolean := False;
      Live      : Boolean := False;     --  a directory entry names it
      Known     : Boolean := False;     --  Length and Tail are its chain's
      Length    : Cluster_Count := 0;
      Tail      : Cluster_Number := No_Cluster;
   end record;
   type Object_Array is array (Positive range <>) of Object;
   type Object_Array_Access is access Object_Array;
   type Key_Array is array (Natural range <>) of Unsigned_64;
   type Key_Access is access Key_Array;
   type Number_Array is array (Natural range <>) of Unsigned_32;
   type Number_Access is access Number_Array;
   Empty_Key : constant Unsigned_64 := 0;               --  no entry is at volume byte 0
   Removed_Key : constant Unsigned_64 := Unsigned_64'Last;

   type Object_Store is record
      Items  : Object_Array_Access;   --  Items (I) is object number I + 1
      Count  : Natural := 0;
      Keys   : Key_Access;            --  open addressing, a power of two long
      Values : Number_Access;
      Used   : Natural := 0;          --  keys in use, removed marks included
   end record;
   First_Objects : constant := 1024;

   function Number_Of (Index : Positive) return Unsigned_32 is (Unsigned_32 (Index) + 1);
   function Index_Of (Number : Unsigned_32) return Natural is
     (if Number < Root_Number or else Number - 1 > Unsigned_32 (Natural'Last) then 0
      else Natural (Number - 1));

   function Hash (Key : Unsigned_64) return Unsigned_64 is
     (Key * 16#9E37_79B9_7F4A_7C15#);

   procedure Place_Key (Store : in out Object_Store; Key : Unsigned_64; Number : Unsigned_32) is
      Mask : constant Unsigned_64 := Unsigned_64 (Store.Keys'Length) - 1;
      Slot : Unsigned_64 := Hash (Key) and Mask;
   begin
      while Store.Keys (Natural (Slot)) not in Empty_Key | Removed_Key loop
         Slot := (Slot + 1) and Mask;
      end loop;
      if Store.Keys (Natural (Slot)) = Empty_Key then
         Store.Used := Store.Used + 1;
      end if;
      Store.Keys (Natural (Slot)) := Key;
      Store.Values (Natural (Slot)) := Number;
   end Place_Key;

   function Find_Key (Store : Object_Store; Key : Unsigned_64) return Unsigned_32 is
      Mask : constant Unsigned_64 := Unsigned_64 (Store.Keys'Length) - 1;
      Slot : Unsigned_64 := Hash (Key) and Mask;
   begin
      for Probe in Store.Keys'Range loop
         exit when Store.Keys (Natural (Slot)) = Empty_Key;
         if Store.Keys (Natural (Slot)) = Key then
            return Store.Values (Natural (Slot));
         end if;
         Slot := (Slot + 1) and Mask;
      end loop;
      return 0;
   end Find_Key;

   procedure Drop_Key (Store : in out Object_Store; Key : Unsigned_64) is
      Mask : constant Unsigned_64 := Unsigned_64 (Store.Keys'Length) - 1;
      Slot : Unsigned_64 := Hash (Key) and Mask;
   begin
      for Probe in Store.Keys'Range loop
         exit when Store.Keys (Natural (Slot)) = Empty_Key;
         if Store.Keys (Natural (Slot)) = Key then
            Store.Keys (Natural (Slot)) := Removed_Key;
            return;
         end if;
         Slot := (Slot + 1) and Mask;
      end loop;
   end Drop_Key;

   --  Rebuild the key table at Length (removed marks dropped).
   procedure Rehash (Store : in out Object_Store; Length : Positive) is
      Old_Keys : constant Key_Access := Store.Keys;
      Old_Values : constant Number_Access := Store.Values;
   begin
      Store.Keys := new Key_Array'(0 .. Length - 1 => Empty_Key);
      Store.Values := new Number_Array'(0 .. Length - 1 => 0);
      Store.Used := 0;
      if Old_Keys /= null then
         for I in Old_Keys'Range loop
            if Old_Keys (I) not in Empty_Key | Removed_Key then
               Place_Key (Store, Old_Keys (I), Old_Values (I));
            end if;
         end loop;
      end if;
   end Rehash;

   --  Map Position to Number, growing the table to stay at most half full.
   procedure Index_Position (Store : in out Object_Store; Position : Unsigned_64; Number : Unsigned_32) is
   begin
      if (Store.Used + 1) * 2 > Store.Keys'Length then
         Rehash (Store, (if Store.Count * 4 > Store.Keys'Length then Store.Keys'Length * 2
                         else Store.Keys'Length));
      end if;
      Place_Key (Store, Position, Number);
   end Index_Position;

   ---------------------------------------------------------------------------
   --  Device transport
   ---------------------------------------------------------------------------
   function Sector_Size (V : Volume) return Positive is (Sector_Bytes (V.G));

   procedure Device_Call
     (V : Volume; Write : Boolean; Block, Blocks : Unsigned_64; OK : out Boolean)
   is
      Msg : Message := NULL_MESSAGE;
      Ignore : MessageTag;
      Block_Bytes : constant Unsigned_64 := Unsigned_64 (V.Device.description.logicalBlockSize);
   begin
      OK := False;
      if Blocks = 0 or else Block >= V.Device.description.blockCount
        or else Blocks > V.Device.description.blockCount - Block
      then
         return;
      end if;
      Msg.tag := (label => (if Write then OP_WRITE_BLOCKS else OP_READ_BLOCKS), length => 4,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words := [0 => V.Device.firstBlock + Block, 1 => V.Device.grant.slot,
                    2 => Blocks, 3 => V.Device.grant.generation];
      Requests_Total := Requests_Total + 1;
      Ignore := capCall (V.Device.endpoint, Msg, CuBit.Messages.Wait_Forever);
      OK := Msg.tag.label = REPLY_OK and then Msg.tag.length = 1 and then Msg.tag.protocol = 0
        and then Msg.tag.reserved = 0 and then Msg.words (0) = Blocks * Block_Bytes;
   end Device_Call;

   --  Count volume sectors from First, to or from Data.
   procedure Transfer
     (V : in out Volume; Write : Boolean; First : Sector_Number; Count : Natural;
      Data : System.Address; OK : out Boolean)
   is
      SB : constant Natural := Sector_Size (V);
      Per_Sector : constant Unsigned_64 := Unsigned_64 (V.Device_Blocks_Per_Sector);
      Per_Call : constant Natural := Natural'Max (1, Natural'Min
        (Natural (V.Device.grantBytes) / SB,
         Natural (Unsigned_64'Min (Unsigned_64 (V.Device.description.maxTransferBlocks) / Per_Sector,
                                   Unsigned_64 (Natural'Last)))));
      Done : Natural := 0;
   begin
      OK := False;
      if Count = 0 then
         OK := True;
         return;
      elsif Byte_Count (First) + Byte_Count (Count) > Byte_Count (V.G.Total_Sectors) then
         return;
      end if;
      while Done < Count loop
         declare
            N : constant Natural := Natural'Min (Count - Done, Per_Call);
            Bytes : constant Storage_Count := Storage_Count (N * SB);
            Grant : Byte_Array (0 .. Natural (Bytes) - 1) with Import, Address => V.Device.grantBuffer;
            Here : Byte_Array (0 .. Natural (Bytes) - 1)
              with Import, Address => Data + Storage_Offset (Done * SB);
         begin
            if Write then
               Grant := Here;
            end if;
            Device_Call (V, Write, (Unsigned_64 (First) + Unsigned_64 (Done)) * Per_Sector,
                         Unsigned_64 (N) * Per_Sector, OK);
            if not OK then
               if Write then
                  V.Quarantine := True;
               end if;
               return;
            end if;
            if not Write then
               Here := Grant;
            end if;
            Done := Done + N;
         end;
      end loop;
      OK := True;
   end Transfer;

   procedure Barrier (V : in out Volume; OK : out Boolean) is
      Msg : Message := NULL_MESSAGE;
   begin
      OK := True;
      if not Has_Volatile_Cache (V.Device.description) then
         return;
      end if;
      Msg.tag := (label => OP_FLUSH_DEVICE, length => 0, protocol => 0, ipc => <>, reserved => 0);
      Requests_Total := Requests_Total + 1;
      Barriers_Total := Barriers_Total + 1;
      Msg.tag := capCall (V.Device.endpoint, Msg, CuBit.Messages.Wait_Forever);
      OK := Msg.tag.label = REPLY_OK and then Msg.tag.length = 1 and then Msg.tag.protocol = 0
        and then Msg.tag.reserved = 0 and then Msg.words (0) = 0;
      if not OK then
         V.Quarantine := True;
      end if;
   end Barrier;

   ---------------------------------------------------------------------------
   --  Metadata cache
   ---------------------------------------------------------------------------
   Largest_Sector : constant := 4096;
   subtype Sector_Buffer is Byte_Array (0 .. Largest_Sector - 1);
   Ahead_Buffer : Byte_Array (0 .. Read_Ahead_Sectors * Largest_Sector - 1) with Alignment => 8;

   function Slot_Of (V : Volume; S : Sector_Number) return Positive is
     (Natural (S mod Sector_Number (V.Cache.Slots)) + 1);
   function Base_Of (V : Volume; Slot : Positive) return Natural is ((Slot - 1) * Sector_Size (V));

   --  The slot holding sector S, read (with read-ahead) on a miss.
   procedure Load (V : in out Volume; S : Sector_Number; Slot : out Positive; OK : out Boolean) is
      SB : constant Natural := Sector_Size (V);
      Ahead : Natural;
   begin
      Slot := Slot_Of (V, S);
      OK := True;
      if V.Cache.Valid (Slot) and then V.Cache.Tag (Slot) = S then
         return;
      end if;
      Ahead := Natural (Sector_Number'Min (Read_Ahead_Sectors, V.G.Total_Sectors - S));
      Transfer (V, False, S, Ahead, Ahead_Buffer'Address, OK);
      if not OK then
         return;
      end if;
      for K in 0 .. Ahead - 1 loop
         declare
            J : constant Positive := Slot_Of (V, S + Sector_Number (K));
         begin
            V.Cache.Data (Base_Of (V, J) .. Base_Of (V, J) + SB - 1) := Ahead_Buffer (K * SB .. K * SB + SB - 1);
            V.Cache.Tag (J) := S + Sector_Number (K);
            V.Cache.Valid (J) := True;
         end;
      end loop;
   end Load;

   --  Sector S's bytes, through the cache.
   procedure Read_Sector (V : in out Volume; S : Sector_Number; Data : out Sector_Buffer; OK : out Boolean) is
      SB : constant Natural := Sector_Size (V);
      Slot : Positive;
   begin
      Data := [others => 0];
      Load (V, S, Slot, OK);
      if OK then
         Data (0 .. SB - 1) := V.Cache.Data (Base_Of (V, Slot) .. Base_Of (V, Slot) + SB - 1);
      end if;
   end Read_Sector;

   --  Write sector S (write-through); a failure quarantines the volume.
   procedure Write_Sector (V : in out Volume; S : Sector_Number; Data : Sector_Buffer; OK : out Boolean) is
      SB : constant Natural := Sector_Size (V);
      Slot : constant Positive := Slot_Of (V, S);
   begin
      Transfer (V, True, S, 1, Data'Address, OK);
      if OK then
         V.Cache.Data (Base_Of (V, Slot) .. Base_Of (V, Slot) + SB - 1) := Data (0 .. SB - 1);
         V.Cache.Tag (Slot) := S;
         V.Cache.Valid (Slot) := True;
      elsif V.Cache.Tag (Slot) = S then
         V.Cache.Valid (Slot) := False;
      end if;
   end Write_Sector;

   --  File data written around the cache: drop what it held of those sectors.
   procedure Forget (V : in out Volume; First : Sector_Number; Count : Natural) is
   begin
      if Count >= V.Cache.Slots then
         for J in V.Cache.Valid'Range loop
            if V.Cache.Valid (J) and then V.Cache.Tag (J) >= First
              and then V.Cache.Tag (J) - First < Sector_Number (Count)
            then
               V.Cache.Valid (J) := False;
            end if;
         end loop;
      else
         for K in 0 .. Count - 1 loop
            declare
               J : constant Positive := Slot_Of (V, First + Sector_Number (K));
            begin
               if V.Cache.Tag (J) = First + Sector_Number (K) then
                  V.Cache.Valid (J) := False;
               end if;
            end;
         end loop;
      end if;
   end Forget;

   ---------------------------------------------------------------------------
   --  FAT entries
   ---------------------------------------------------------------------------
   function Active (V : Volume) return Fat_Index is (if V.G.Mirrored then 0 else V.G.Active_Fat);

   procedure Get_Entry (V : in out Volume; C : Cluster_Number; Raw : out Unsigned_32; OK : out Boolean) is
      Slot : Positive;
   begin
      Raw := 0;
      if C > Last_Cluster (V.G) then
         OK := False;
         return;
      end if;
      Load (V, Fat_Sector (V.G, Active (V), C), Slot, OK);
      if OK then
         Raw := Le32 (V.Cache.Data, Base_Of (V, Slot) + Fat_Offset (V.G, C));
      end if;
   end Get_Entry;

   type Change is record
      C     : Cluster_Number := No_Cluster;
      Value : Unsigned_32 := 0;   --  the low 28 bits to store
   end record;
   type Change_List is array (Positive range <>) of Change;

   --  Apply Changes to every current FAT, the active one first; in each,
   --  sectors are written in the order of their first change.
   --  Where the last read stopped, so a sequential read does not walk its
   --  chain from the start each time; and the last chain Read_Data
   --  checked whole (no cycle, no broken link), with its length. A change
   --  to the FAT forgets the check; freeing a cluster forgets both.
   type Read_Place is record
      Key : Unsigned_64 := 0;   --  the volume (several may be mounted)
      First : Cluster_Number := No_Cluster;
      Index : Cluster_Count := 0;
      Cluster : Cluster_Number := No_Cluster;
   end record;
   Last_Read : Read_Place;
   Checked_Key : Unsigned_64 := 0;
   Checked_First : Cluster_Number := No_Cluster;
   Checked_Length : Cluster_Count := 0;

   procedure Update_Fat (V : in out Volume; Changes : Change_List; OK : out Boolean) is
      SB : constant Natural := Sector_Size (V);
      Applied : array (Changes'Range) of Boolean;
      Work : Sector_Buffer;
      Copies : array (1 .. 2) of Fat_Index := [Active (V), 1 - Active (V)];
      Copy_Count : constant Positive := (if V.G.Mirrored then V.G.Fats else 1);
   begin
      OK := True;
      Checked_First := No_Cluster;
      if (for some C of Changes => C.Value = Free_Value) then
         Last_Read := (others => <>);
      end if;
      if Copy_Count = 1 then
         Copies (2) := Copies (1);
      end if;
      for K in 1 .. Copy_Count loop
         Applied := [others => False];
         for I in Changes'Range loop
            if not Applied (I) then
               declare
                  S : constant Sector_Number := Fat_Sector (V.G, Copies (K), Changes (I).C);
               begin
                  Read_Sector (V, S, Work, OK);
                  if not OK then
                     return;
                  end if;
                  for J in I .. Changes'Last loop
                     if not Applied (J) and then Fat_Sector (V.G, Copies (K), Changes (J).C) = S then
                        declare
                           At_Byte : constant Natural := Fat_Offset (V.G, Changes (J).C);
                        begin
                           Put32 (Work, At_Byte, Encode_Entry (Le32 (Work, At_Byte), Changes (J).Value));
                        end;
                        Applied (J) := True;
                     end if;
                  end loop;
                  Write_Sector (V, S, Work, OK);
                  if not OK then
                     return;
                  end if;
               end;
            end if;
         end loop;
      end loop;
      pragma Unreferenced (SB);
   end Update_Fat;

   ---------------------------------------------------------------------------
   --  Chains (the proved walks of Fat_Chains, over this volume's FAT)
   ---------------------------------------------------------------------------
   Walk_Volume : Volume;   --  the volume the walks read (single-threaded service)

   procedure Walk_Entry (C : Data_Cluster; Raw : out Unsigned_32; OK : out Boolean) is
   begin
      Get_Entry (Walk_Volume, C, Raw, OK);
   end Walk_Entry;
   package Walks is new Fat_Chains.Walks (Walk_Entry);

   --  Clusters already chosen by the allocation in progress read as used.
   Chosen : array (1 .. Batch_Clusters) of Cluster_Number := [others => No_Cluster];
   Chosen_Count : Natural := 0;
   procedure Search_Entry (C : Data_Cluster; Raw : out Unsigned_32; OK : out Boolean) is
   begin
      for I in 1 .. Chosen_Count loop
         if Chosen (I) = C then
            Raw := End_Value;
            OK := True;
            return;
         end if;
      end loop;
      Get_Entry (Walk_Volume, C, Raw, OK);
   end Search_Entry;
   package Searches is new Fat_Chains.Searches (Search_Entry);

   function Chain_Status (Result : Step_Result) return Status is
     (case Result is
        when Advanced | Chain_End => Done,
        when Read_Failed => Device_Error,
        when others => Malformed);

   --  The cluster Index links after First; S Not_Found when the chain ends
   --  first (Found is then its last cluster).
   procedure Seek
     (V : in out Volume; First : Cluster_Number; Index : Cluster_Count; Found : out Cluster_Number;
      S : out Status)
   is
      Reached : Cluster_Count;
      Result : Step_Result;
      At_Cluster : Data_Cluster;
   begin
      Found := First;
      if First < First_Data_Cluster or else First > Last_Cluster (V.G) then
         S := Malformed;
         return;
      end if;
      Walk_Volume := V;
      Walks.Seek (First, Last_Cluster (V.G), Index, At_Cluster, Reached, Result);
      V.Quarantine := V.Quarantine or else Walk_Volume.Quarantine;
      Found := At_Cluster;
      S := (if Result = Advanced then Done elsif Result = Chain_End then Not_Found
            else Chain_Status (Result));
   end Seek;

   procedure Measure
     (V : in out Volume; First : Cluster_Number; Length : out Cluster_Count; Tail : out Cluster_Number;
      S : out Status)
   is
      Result : Step_Result;
      At_Tail : Data_Cluster;
   begin
      Length := 0;
      Tail := No_Cluster;
      if First < First_Data_Cluster or else First > Last_Cluster (V.G) then
         S := Malformed;
         return;
      end if;
      Walk_Volume := V;
      Walks.Measure (First, Last_Cluster (V.G), Length, At_Tail, Result);
      Tail := At_Tail;
      S := Chain_Status (Result);
   end Measure;

   --  Choose Count free clusters (not yet marked) into Chosen.
   procedure Choose (V : in out Volume; Count : Natural; S : out Status) is
      C : Data_Cluster;
      Found, Failed : Boolean;
   begin
      Chosen_Count := 0;
      S := Done;
      if Unsigned_64 (Count) > Unsigned_64 (V.Space.Free) then
         S := No_Space;
         return;
      end if;
      Walk_Volume := V;
      for K in 1 .. Count loop
         Searches.Find_Free (V.Space, C, Found, Failed);
         if Failed then
            S := Device_Error;
            return;
         elsif not Found then
            S := No_Space;   --  the count was a hint after all: the FAT is full
            return;
         end if;
         Chosen_Count := Chosen_Count + 1;
         Chosen (Chosen_Count) := C;
         Take (V.Space, C);
      end loop;
   end Choose;

   --  Give back the clusters chosen but not used.
   procedure Unchoose (V : in out Volume) is
   begin
      for K in 1 .. Chosen_Count loop
         if V.Space.Free < V.Space.Total then
            Give (V.Space);
         end if;
      end loop;
      Chosen_Count := 0;
   end Unchoose;

   --  Free the chain from First in every FAT (after a barrier made the
   --  references to it go away), Batch_Clusters at a time.
   procedure Free_Chain (V : in out Volume; First : Cluster_Number; S : out Status) is
      Current : Cluster_Number := First;
      Raw : Unsigned_32;
      OK : Boolean;
      List : Change_List (1 .. Batch_Clusters);
      Count : Natural;
      Steps : Cluster_Count := 0;
   begin
      S := Done;
      while Current /= No_Cluster loop
         Count := 0;
         while Current /= No_Cluster and then Count < Batch_Clusters loop
            if Current < First_Data_Cluster or else Current > Last_Cluster (V.G) then
               Current := No_Cluster;
               exit;
            end if;
            Get_Entry (V, Current, Raw, OK);
            if not OK then
               S := Device_Error;
               return;
            end if;
            Count := Count + 1;
            List (Count) := (C => Current, Value => Free_Value);
            declare
               Item : constant Decoded_Entry := Decode_Entry (Raw, Last_Cluster (V.G));
            begin
               if Item.Kind = Next_Entry and then Steps < V.G.Clusters then
                  Current := Item.Next;
                  Steps := Steps + 1;
               else
                  Current := No_Cluster;   --  end, or a broken or cyclic tail: stop here
               end if;
            end;
         end loop;
         Update_Fat (V, List (1 .. Count), OK);
         if not OK then
            S := Device_Error;
            return;
         end if;
         for K in 1 .. Count loop
            if V.Space.Free < V.Space.Total then
               Give (V.Space);
            end if;
         end loop;
      end loop;
   end Free_Chain;

   ---------------------------------------------------------------------------
   --  Volume state
   ---------------------------------------------------------------------------
   function Read_Only (V : Volume) return Boolean is (not V.Writable);
   function Quarantined (V : Volume) return Boolean is (V.Quarantine);
   function Session (V : Volume) return CuBit.Block_Devices.Device_Session is (V.Device);
   function Cluster_Bytes (V : Volume) return Unsigned_32 is
     (if V.Mounted then Unsigned_32 (Cluster_Bytes (V.G)) else 0);
   function Total_Clusters (V : Volume) return Unsigned_64 is
     (if V.Mounted then Unsigned_64 (V.G.Clusters) else 0);
   function Free_Clusters (V : Volume) return Unsigned_64 is
     (if V.Mounted then Unsigned_64 (V.Space.Free) else 0);
   function Unflushed (V : Volume) return Boolean is (V.Marked_Dirty);

   procedure Retire (V : in out Volume) is
   begin
      V.Quarantine := True;
   end Retire;

   --  FAT[1]'s clean bit in every current FAT.
   procedure Set_Clean (V : in out Volume; Clean : Boolean; OK : out Boolean) is
      Raw : Unsigned_32;
   begin
      Get_Entry (V, Volume_Flags_Cluster, Raw, OK);
      if not OK then
         return;
      end if;
      Update_Fat (V, [1 => (C => Volume_Flags_Cluster,
                             Value => (if Clean then Marked_Clean (Raw) else Marked_Dirty (Raw))
                                      and Entry_Mask)], OK);
   end Set_Clean;

   --  Before any change: writable, not quarantined, and marked dirty
   --  durably (see the spec).
   procedure Begin_Change (V : in out Volume; S : out Status) is
      OK : Boolean;
   begin
      if not V.Mounted or else not V.Writable then
         S := Read_Only;
         return;
      elsif V.Quarantine then
         S := Recovery_Required;
         return;
      end if;
      S := Done;
      if not V.Marked_Dirty then
         Set_Clean (V, False, OK);
         if OK then
            Barrier (V, OK);
         end if;
         if not OK then
            V.Quarantine := True;
            S := Device_Error;
            return;
         end if;
         V.Marked_Dirty := True;
      end if;
   end Begin_Change;

   procedure Flush (V : in out Volume; S : out Status) is
      OK : Boolean;
      Work : Sector_Buffer;
   begin
      S := Done;
      if not V.Mounted or else not V.Marked_Dirty then
         return;
      elsif V.Quarantine then
         S := Recovery_Required;
         return;
      end if;
      Barrier (V, OK);
      if OK and then V.G.Info_Sector /= 0 then
         Read_Sector (V, V.G.Info_Sector, Work, OK);
         if OK then
            declare
               Info : Boot_Sector := Work (0 .. Boot_Sector_Bytes - 1);
            begin
               Encode_Info (Info, V.Space.Free, V.Space.Cursor);
               Work (0 .. Boot_Sector_Bytes - 1) := Info;
            end;
            Write_Sector (V, V.G.Info_Sector, Work, OK);
         end if;
      end if;
      if OK then
         Set_Clean (V, True, OK);
      end if;
      if OK then
         Barrier (V, OK);
      end if;
      if not OK then
         V.Quarantine := True;
         S := Device_Error;
         return;
      end if;
      V.Marked_Dirty := False;
   end Flush;

   ---------------------------------------------------------------------------
   --  Times and inodes
   ---------------------------------------------------------------------------
   function Now_Seconds return Unsigned_64 is
      Offset : constant Unsigned_64 := CuBit.System_Facts.Wall_Clock_Offset;
      Monotonic : Unsigned_64;
      Per_Second : constant := 1_000;
   begin
      if Offset = 0 or else Offset = Unsigned_64'Last then
         return 0;
      end if;
      Monotonic := CuBit.Monotonic.Milliseconds;
      if Monotonic = Unsigned_64'Last or else Offset > Unsigned_64'Last - Monotonic then
         return 0;
      end if;
      return (Offset + Monotonic) / Per_Second;
   end Now_Seconds;

   function Seconds_32 (S : Unsigned_64) return Unsigned_32 is
     (if S > Unsigned_64 (Unsigned_32'Last) then Unsigned_32'Last else Unsigned_32 (S));

   function Inode_Of (V : Volume; E : Short_Entry; Directory : Boolean; Links : Unsigned_16) return Inode is
      Written : constant Unsigned_32 := Seconds_32 (To_Unix (E.Written_Date, E.Written_Time));
      Cluster : constant Unsigned_64 := Unsigned_64 (Cluster_Bytes (V.G));
      Size : constant Unsigned_64 := (if Directory then 0 else Unsigned_64 (E.Size));
      Item : Inode :=
        (typeAndPermissions =>
           (if Directory then Mode_Directory
            elsif (E.Attributes and Attribute_Read_Only) /= 0 then Mode_Read_Only_File
            else Mode_File),
         uid => 0, gid => 0, sizeLo => Unsigned_32 (Size),
         accessedTime => Seconds_32 (To_Unix (E.Accessed_Date, 0)),
         creationTime => Written, modifiedTime => Written, deletedTime => 0,
         numHardLinks => Links,
         numDiskSectors => Seconds_32 ((Size + Cluster - 1) / Cluster * Cluster / Ext2_Sector_Bytes),
         flags => 0, osSpecific1 => 0, directBlocks => [others => 0],
         singleIndirectBlock => 0, doubleIndirectBlock => 0, tripleIndirectBlock => 0,
         generationNumber => 0, fileACL => 0, sizeHi_DirACL => 0, fragmentBlockAddr => 0,
         osSpecific2A => 0, osSpecific2B => 0, osSpecific2C => 0);
   begin
      Item.directBlocks (0) := Unsigned_32 (E.First);
      Item.directBlocks (Attributes_Slot) := Unsigned_32 (E.Attributes);
      return Item;
   end Inode_Of;

   function Root_Inode (V : Volume) return Inode is
     (Inode_Of (V, (Attributes => Attribute_Directory, First => V.G.Root, others => <>), True, 2));

   ---------------------------------------------------------------------------
   --  Objects
   ---------------------------------------------------------------------------
   procedure Grow_Objects (Store : in out Object_Store) is
      Old : constant Object_Array_Access := Store.Items;
   begin
      Store.Items := new Object_Array (1 .. Old'Length * 2);
      Store.Items (1 .. Old'Length) := Old.all;
   end Grow_Objects;

   function Object_At (V : Volume; Number : Unsigned_32) return Natural is
     (if Index_Of (Number) in 1 .. V.Objects.Count then Index_Of (Number) else 0);

   --  The number of the entry at Position (a new one the first time).
   procedure Number_For
     (V : in out Volume; Position : Unsigned_64; Parent : Unsigned_32; E : Short_Entry;
      Directory : Boolean; Number : out Unsigned_32; S : out Status)
   is
      Store : Object_Store renames V.Objects.all;
      Index : Natural;
   begin
      Number := Find_Key (Store, Position);
      S := Done;
      if Number = 0 then
         if Store.Count >= Maximum_Objects then
            debugPrint ("FAT32: object capacity" & Natural'Image (Maximum_Objects) & " reached" & ASCII.LF);
            S := Capacity;
            return;
         elsif Store.Count = Store.Items'Length then
            Grow_Objects (Store);
         end if;
         Store.Count := Store.Count + 1;
         Number := Number_Of (Store.Count);
         Store.Items (Store.Count) := (Position => Position, Live => True, others => <>);
         Index_Position (Store, Position, Number);
      end if;
      Index := Index_Of (Number);
      declare
         Item : Object renames Store.Items (Index);
      begin
         if Item.First /= E.First or else Item.Directory /= Directory then
            Item.Known := False;
         end if;
         if Parent /= 0 then
            Item.Parent := Parent;
         end if;
         Item.First := E.First;
         Item.Size := (if Directory then 0 else E.Size);
         Item.Directory := Directory;
      end;
   end Number_For;

   ---------------------------------------------------------------------------
   --  Directories
   ---------------------------------------------------------------------------
   function Entries_Per_Cluster (V : Volume) return Positive is (Cluster_Bytes (V.G) / Entry_Bytes);
   function Entries_Per_Sector_Of (V : Volume) return Positive is (Sector_Size (V) / Entry_Bytes);

   --  Where a directory's entries are being read: the cluster holding
   --  entry Cluster_Index * Entries_Per_Cluster.
   type Directory_Walk is record
      First   : Cluster_Number := No_Cluster;
      Cluster : Cluster_Number := No_Cluster;
      Index   : Natural := 0;
   end record;

   --  The sector and byte offset of entry Index of the directory W reads.
   --  S Not_Found past the directory's chain (or its 65536 entries).
   procedure Locate
     (V : in out Volume; W : in out Directory_Walk; Index : Natural;
      Sector : out Sector_Number; Offset : out Natural; S : out Status)
   is
      EPC : constant Positive := Entries_Per_Cluster (V);
      Wanted : constant Natural := Index / EPC;
      Found : Cluster_Number;
   begin
      Sector := 0;
      Offset := 0;
      if Index >= Maximum_Directory_Entries then
         S := Not_Found;
         return;
      end if;
      if W.Cluster = No_Cluster or else Wanted < W.Index then
         W.Cluster := W.First;
         W.Index := 0;
      end if;
      if Wanted > W.Index then
         Seek (V, W.Cluster, Cluster_Count (Wanted - W.Index), Found, S);
         if S /= Done then
            return;
         end if;
         W.Cluster := Found;
         W.Index := Wanted;
      elsif W.Cluster < First_Data_Cluster or else W.Cluster > Last_Cluster (V.G) then
         S := Malformed;
         return;
      end if;
      declare
         Within : constant Natural := Index mod EPC;
      begin
         Sector := Cluster_Sector (V.G, W.Cluster) + Sector_Number (Within / Entries_Per_Sector_Of (V));
         Offset := Within mod Entries_Per_Sector_Of (V) * Entry_Bytes;
      end;
      S := Done;
   end Locate;

   function Position_Of (V : Volume; Sector : Sector_Number; Offset : Natural) return Unsigned_64 is
     ((Unsigned_64 (Sector) * Unsigned_64 (Sector_Size (V)) + Unsigned_64 (Offset)) / Entry_Bytes);

   --  One named entry of a directory.
   type Record_Info is record
      Short       : Short_Entry;
      Directory   : Boolean := False;
      First_Index : Natural := 0;   --  its first slot (the first long-name part)
      Short_Index : Natural := 0;   --  its short entry's slot
      Position    : Unsigned_64 := 0;
      Has_Long    : Boolean := False;
      Long        : Long_Units := [others => Name_Padding];
      Long_Length : Fat_Format.Long_Length := 0;
      Alias       : Long_Units := [others => Name_Padding];
      Alias_Length : Fat_Format.Long_Length := 0;
      Alias_OK    : Boolean := False;
      Name        : Name_Text := [others => ' '];   --  its CuBit name
      Name_Length : Fat_Names.Name_Length := 0;
      Named       : Boolean := False;               --  representable here
   end record;

   --  The next file or directory entry at or after Index (dots, labels,
   --  deleted and malformed entries skipped); Index moves past it. S
   --  Not_Found at the directory's end.
   procedure Next_Record
     (V : in out Volume; W : in out Directory_Walk; Index : in out Natural; Info : out Record_Info;
      S : out Status)
   is
      Gathered : Long_Name_State := Idle;
      Set_Start : Natural := Index;
      Sector : Sector_Number;
      Offset : Natural;
      Work : Sector_Buffer;
      Part : Long_Part;
      Valid : Boolean;
      Valid_Cluster : Boolean;
      Loaded : Sector_Number := Sector_Number'Last;
   begin
      Info := (others => <>);
      loop
         Locate (V, W, Index, Sector, Offset, S);
         if S /= Done then
            return;
         end if;
         if Sector /= Loaded then
            Read_Sector (V, Sector, Work, Valid);
            if not Valid then
               S := Device_Error;
               return;
            end if;
            Loaded := Sector;
         end if;
         case Classify (Work, Offset) is
            when End_Of_Directory =>
               S := Not_Found;
               return;
            when Long_Part_Entry =>
               Decode_Long (Work, Offset, Part, Valid);
               if not Valid then
                  Gathered := Idle;
               else
                  if Part.Last then
                     Set_Start := Index;
                  end if;
                  Gather (Gathered, Part);
               end if;
            when File_Entry | Subdirectory_Entry =>
               Decode_Short (Work, Offset, Info.Short, Valid_Cluster);
               if Valid_Cluster and then Info.Short.First <= Last_Cluster (V.G)
                 and then Info.Short.First /= 1
               then
                  Info.Directory := Classify (Work, Offset) = Subdirectory_Entry;
                  Info.Short_Index := Index;
                  Info.Position := Position_Of (V, Sector, Offset);
                  Info.First_Index := Index;
                  if Complete_For (Gathered, Info.Short.Name) then
                     Gathered_Name (Gathered, Info.Long, Info.Long_Length, Info.Has_Long);
                     if Info.Has_Long then
                        Info.First_Index := Set_Start;
                     end if;
                  end if;
                  Short_Units (Info.Short.Name, Info.Short.Case_Flags, Info.Alias, Info.Alias_Length,
                               Info.Alias_OK);
                  if Info.Has_Long then
                     From_Units (Info.Long, Info.Long_Length, Info.Name, Info.Name_Length, Info.Named);
                  end if;
                  if not Info.Named and then Info.Alias_OK then
                     From_Units (Info.Alias, Info.Alias_Length, Info.Name, Info.Name_Length, Info.Named);
                  end if;
                  Index := Index + 1;
                  S := Done;
                  return;
               end if;
               Gathered := Idle;
            when others =>
               Gathered := Idle;
         end case;
         Index := Index + 1;
      end loop;
   end Next_Record;

   --  Name's entry in the directory starting at Directory_First.
   procedure Find
     (V : in out Volume; Directory_First : Cluster_Number; Units : Long_Units;
      Length : Fat_Format.Long_Length; Info : out Record_Info; S : out Status)
   is
      W : Directory_Walk := (First => Directory_First, others => <>);
      Index : Natural := 0;
   begin
      loop
         Next_Record (V, W, Index, Info, S);
         if S /= Done then
            return;
         end if;
         exit when (Info.Has_Long and then Same_Name (Units, Length, Info.Long, Info.Long_Length))
           or else (Info.Alias_OK and then Same_Name (Units, Length, Info.Alias, Info.Alias_Length));
      end loop;
   end Find;

   --  A path component's units (for a lookup, or Strict for a new name).
   procedure Units_Of
     (Name : String; Strict : Boolean; Units : out Long_Units; Length : out Fat_Format.Long_Length;
      S : out Status)
   is
      Check : Name_Check;
   begin
      Units := [others => Name_Padding];
      Length := 0;
      if Name'Length = 0 or else Name'Length > Fat_Names.Maximum_Name_Bytes then
         S := Invalid_Name;
         return;
      end if;
      declare
         Copy : constant String (1 .. Name'Length) := Name;
      begin
         To_Units (Copy, Strict, Units, Length, Check);
      end;
      S := (if Check = Name_Valid then Done else Invalid_Name);
   end Units_Of;

   --  The directory first cluster of object Number (S Not_Directory if not one).
   procedure Directory_Of (V : Volume; Number : Unsigned_32; First : out Cluster_Number; S : out Status) is
      Index : constant Natural := Object_At (V, Number);
   begin
      First := No_Cluster;
      if Index = 0 then
         S := Not_Found;
      elsif not V.Objects.Items (Index).Directory then
         S := Not_Directory;
      elsif not V.Objects.Items (Index).Live and then Number /= Root_Number then
         S := Not_Found;
      else
         First := V.Objects.Items (Index).First;
         S := Done;
      end if;
   end Directory_Of;

   function Valid_Path (Path : String) return Boolean is
     (Path'Length <= CuBit.Directory_Paths.Maximum_Bytes and then
      (Path'Length = 0 or else CuBit.File_Access.Valid_Path (Path)));

   --  Look Name up in directory Parent (an object number).
   procedure Step_Into
     (V : in out Volume; Parent : Unsigned_32; Name : String; Number : out Unsigned_32;
      Info : out Record_Info; S : out Status)
   is
      First : Cluster_Number;
      Units : Long_Units;
      Length : Fat_Format.Long_Length;
   begin
      Number := 0;
      Info := (others => <>);
      if not CuBit.Directory_Paths.Valid_Child_Name (Name) then
         S := Malformed;
         return;
      end if;
      Directory_Of (V, Parent, First, S);
      if S /= Done then
         return;
      end if;
      Units_Of (Name, False, Units, Length, S);
      if S /= Done then
         S := Not_Found;   --  no entry can have a name that is not UTF-8
         return;
      end if;
      Find (V, First, Units, Length, Info, S);
      if S = Done then
         Number_For (V, Info.Position, Parent, Info.Short, Info.Directory, Number, S);
      end if;
   end Step_Into;

   procedure Resolve (V : in out Volume; Path : String; Number : out Unsigned_32; S : out Status) is
      Current : Unsigned_32 := Root_Number;
      First : Natural := Path'First;
      Last : Natural;
      Info : Record_Info;
   begin
      Number := 0;
      if not V.Mounted or else not Valid_Path (Path) then
         S := Malformed;
         return;
      end if;
      while First <= Path'Last loop
         if Path (First) = '/' then
            First := First + 1;
         else
            Last := First;
            while Last < Path'Last and then Path (Last + 1) /= '/' loop
               Last := Last + 1;
            end loop;
            Step_Into (V, Current, Path (First .. Last), Current, Info, S);
            if S /= Done then
               return;
            end if;
            First := Last + 1;
         end if;
      end loop;
      Number := Current;
      S := Done;
   end Resolve;

   --  Path's parent directory and the last component's bounds.
   procedure Resolve_Parent
     (V : in out Volume; Path : String; Parent : out Unsigned_32; Name_First, Name_Last : out Natural;
      S : out Status)
   is
      Last : Natural := Path'Last;
      Slash : Natural;
   begin
      Parent := 0;
      Name_First := Path'First;
      Name_Last := Path'First - 1;
      while Last >= Path'First and then Path (Last) = '/' loop
         Last := Last - 1;
      end loop;
      if Last < Path'First then
         S := Invalid_Name;   --  the root has no name
         return;
      end if;
      Slash := Last;
      while Slash >= Path'First and then Path (Slash) /= '/' loop
         Slash := Slash - 1;
      end loop;
      Name_First := Slash + 1;
      Name_Last := Last;
      Resolve (V, Path (Path'First .. Slash), Parent, S);
   end Resolve_Parent;

   procedure Lookup (V : in out Volume; Directory : Inode; Name : String; Number : out Unsigned_32;
                     S : out Status)
   is
      Units : Long_Units;
      Length : Fat_Format.Long_Length;
      Info : Record_Info;
   begin
      Number := 0;
      if not V.Mounted then
         S := Malformed;
         return;
      end if;
      Units_Of (Name, False, Units, Length, S);
      if S /= Done then
         S := Not_Found;
         return;
      end if;
      Find (V, Cluster_Number (Directory.directBlocks (0) and Entry_Mask), Units, Length, Info, S);
      if S = Done then
         --  The parent is only known by number when it was met as one.
         Number_For (V, Info.Position, 0, Info.Short, Info.Directory, Number, S);
      end if;
   end Lookup;

   procedure Read_Object (V : in out Volume; Number : Unsigned_32; Item : out Inode; S : out Status) is
      Index : Natural;
   begin
      Item := Root_Inode (V);
      if not V.Mounted then
         S := Malformed;
         return;
      elsif Number = Root_Number then
         S := Done;
         return;
      end if;
      Index := Object_At (V, Number);
      if Index = 0 then
         S := Out_Of_Range;
         return;
      end if;
      declare
         O : constant Object := V.Objects.Items (Index);
         E : Short_Entry;
         Work : Sector_Buffer;
         OK, Valid_Cluster : Boolean;
         SB : constant Unsigned_64 := Unsigned_64 (Sector_Size (V));
         Byte : constant Unsigned_64 := O.Position * Entry_Bytes;
      begin
         if not O.Live then
            --  Unlinked while open: what it held, with no links.
            Item := Inode_Of (V, (Attributes => (if O.Directory then Attribute_Directory else 0),
                                  First => O.First, Size => O.Size, others => <>), O.Directory, 0);
            S := Done;
            return;
         end if;
         Read_Sector (V, Sector_Number (Byte / SB), Work, OK);
         if not OK then
            S := Device_Error;
            return;
         end if;
         case Classify (Work, Natural (Byte mod SB)) is
            when File_Entry | Subdirectory_Entry =>
               Decode_Short (Work, Natural (Byte mod SB), E, Valid_Cluster);
               if not Valid_Cluster or else E.First > Last_Cluster (V.G) then
                  S := Malformed;
                  return;
               end if;
               Item := Inode_Of (V, E, O.Directory, 1);
               S := Done;
            when others =>
               S := Malformed;
         end case;
      end;
   end Read_Object;

   procedure Read_Directory_Page
     (V : in out Volume; Directory : Inode; Cursor : Unsigned_64;
      Entries : out Listed_Records; Count : out Natural; Next : out Unsigned_64; S : out Status)
   is
      First : constant Cluster_Number := Cluster_Number (Directory.directBlocks (0) and Entry_Mask);
      W : Directory_Walk := (First => First, others => <>);
      Wanted : constant Unsigned_64 := Cursor / Entry_Bytes;
      Index : Natural;
      Info : Record_Info;
      Used : Natural := DP.Header_Bytes;
      Number : Unsigned_32;
      Mark : Natural;
   begin
      Entries := [others => (others => <>)];
      Count := 0;
      Next := Cursor;
      if not V.Mounted or else Ext2_Type (Directory) /= Directory_Type then
         S := Malformed;
         return;
      elsif Wanted >= Maximum_Directory_Entries then
         S := Listing_End;
         return;
      end if;
      --  From the start of the cursor's cluster, so that a long name begun
      --  there is whole; only records whose short entry is at or after the
      --  cursor are listed.
      Index := Natural (Wanted) - Natural (Wanted) mod Entries_Per_Cluster (V);
      loop
         Mark := Index;
         Next_Record (V, W, Index, Info, S);
         if S = Not_Found then
            Next := Unsigned_64 (Index) * Entry_Bytes;
            S := Listing_End;
            return;
         elsif S /= Done then
            return;
         end if;
         if Unsigned_64 (Info.Short_Index) >= Wanted and then Info.Named then
            if Count = DP.Maximum_Entries or else Used > DP.Page_Bytes - DP.Record_Bytes (Info.Name_Length) then
               Next := Unsigned_64 (Mark) * Entry_Bytes;
               S := Done;
               return;
            end if;
            Number_For (V, Info.Position, 0, Info.Short, Info.Directory, Number, S);
            if S /= Done then
               return;
            end if;
            Used := Used + DP.Record_Bytes (Info.Name_Length);
            Entries (Count).Number := Number;
            Entries (Count).Kind := (if Info.Directory then DP.Kind_Directory else DP.Kind_File);
            Entries (Count).Length := Info.Name_Length;
            for K in 1 .. Info.Name_Length loop
               Entries (Count).Name (K) := Character'Pos (Info.Name (K));
            end loop;
            Count := Count + 1;
         end if;
      end loop;
   end Read_Directory_Page;

   ---------------------------------------------------------------------------
   --  Writing entries
   ---------------------------------------------------------------------------
   Maximum_Slots : constant := Maximum_Parts + 1;
   subtype Slot_Bytes is Byte_Array (0 .. Maximum_Slots * Entry_Bytes - 1);

   --  Write Count entries (Data) at directory slot Start: in slot order,
   --  so that the short entry (last) is in the last sector written.
   procedure Put_Entries
     (V : in out Volume; Directory_First : Cluster_Number; Start, Count : Natural; Data : Slot_Bytes;
      S : out Status)
   is
      W : Directory_Walk := (First => Directory_First, others => <>);
      K : Natural := 0;
      Sector, Other : Sector_Number;
      Offset, Other_Offset : Natural;
      Work : Sector_Buffer;
      OK : Boolean;
   begin
      while K < Count loop
         Locate (V, W, Start + K, Sector, Offset, S);
         if S /= Done then
            S := (if S = Not_Found then Malformed else S);
            return;
         end if;
         Read_Sector (V, Sector, Work, OK);
         if not OK then
            S := Device_Error;
            return;
         end if;
         loop
            Work (Offset .. Offset + Entry_Bytes - 1) := Data (K * Entry_Bytes .. K * Entry_Bytes + Entry_Bytes - 1);
            K := K + 1;
            exit when K = Count;
            Locate (V, W, Start + K, Other, Other_Offset, S);
            if S /= Done then
               S := (if S = Not_Found then Malformed else S);
               return;
            end if;
            exit when Other /= Sector;
            Offset := Other_Offset;
         end loop;
         Write_Sector (V, Sector, Work, OK);
         if not OK then
            S := Device_Error;
            return;
         end if;
      end loop;
      S := Done;
   end Put_Entries;

   --  Mark slots First_Index .. Short_Index deleted: the short entry's
   --  sector first, then the long-name parts' (see the spec).
   procedure Delete_Entries
     (V : in out Volume; Directory_First : Cluster_Number; First_Index, Short_Index : Natural;
      S : out Status)
   is
      W : Directory_Walk := (First => Directory_First, others => <>);
      Short_Sector, Sector : Sector_Number;
      Offset : Natural;
      Work : Sector_Buffer;
      OK : Boolean;
      Done_Sectors : array (1 .. Maximum_Slots) of Sector_Number := [others => Sector_Number'Last];
      Done_Count : Natural := 0;

      procedure Mark (In_Sector : Sector_Number) is
         W2 : Directory_Walk := (First => Directory_First, others => <>);
         Sec : Sector_Number;
         Off : Natural;
         St : Status;
      begin
         Read_Sector (V, In_Sector, Work, OK);
         if not OK then
            S := Device_Error;
            return;
         end if;
         for I in First_Index .. Short_Index loop
            Locate (V, W2, I, Sec, Off, St);
            if St = Done and then Sec = In_Sector then
               Work (Off) := Free_Marker;
            end if;
         end loop;
         Write_Sector (V, In_Sector, Work, OK);
         if not OK then
            S := Device_Error;
            return;
         end if;
         Done_Count := Done_Count + 1;
         Done_Sectors (Done_Count) := In_Sector;
      end Mark;
   begin
      S := Done;
      Locate (V, W, Short_Index, Short_Sector, Offset, S);
      if S /= Done then
         S := (if S = Not_Found then Malformed else S);
         return;
      end if;
      Mark (Short_Sector);
      for I in reverse First_Index .. Short_Index loop
         exit when S /= Done;
         Locate (V, W, I, Sector, Offset, S);
         if S /= Done then
            S := (if S = Not_Found then Malformed else S);
            return;
         end if;
         if (for all D of Done_Sectors (1 .. Done_Count) => D /= Sector) then
            Mark (Sector);
         end if;
      end loop;
   end Delete_Entries;

   --  A zeroed cluster (with Head entries at its start) written through
   --  the cache.
   procedure Write_Cluster (V : in out Volume; C : Cluster_Number; Head : Byte_Array; S : out Status) is
      Work : Sector_Buffer := [others => 0];
      First : constant Sector_Number := Cluster_Sector (V.G, C);
      OK : Boolean;
   begin
      for K in 0 .. Cluster_Sectors (V.G) - 1 loop
         Work := [others => 0];
         if K = 0 then
            Work (0 .. Head'Length - 1) := Head;
         end if;
         Write_Sector (V, First + Sector_Number (K), Work, OK);
         if not OK then
            S := Device_Error;
            return;
         end if;
      end loop;
      S := Done;
   end Write_Cluster;

   --  Slots Count in a row in the directory, free (deleted or past its
   --  end marker); the directory grows by zeroed clusters when it has
   --  none (a zeroed cluster, a barrier, then the link). The same scan
   --  finds the numeric tails Base uses (Taken) and whether the plain or
   --  exact alias is in use (Plain_Taken), ignoring the entry at Ignore
   --  (a renamed object's own). At_End: the run reaches past the end
   --  marker, so the slot after it must be made to read as the end.
   Tail_Limit : constant := Maximum_Directory_Entries + 1;
   Taken : array (1 .. Tail_Limit) of Boolean;

   procedure Find_Slots
     (V : in out Volume; Directory_First : Cluster_Number; Count : Positive; Base : Basis;
      Exact : Short_Name; Has_Exact : Boolean; Ignore : Unsigned_64;
      Start : out Natural; Plain_Taken : out Boolean; At_End : out Boolean; S : out Status)
   is
      W : Directory_Walk := (First => Directory_First, others => <>);
      Index : Natural := 0;
      Run_Start : Natural := 0;
      Run : Natural := 0;
      Found : Boolean := False;
      Sector : Sector_Number;
      Offset : Natural;
      Work : Sector_Buffer;
      Loaded : Sector_Number := Sector_Number'Last;
      OK : Boolean;
      Ended : Boolean := False;
      Free_Slot : Boolean;
      Plain : constant Short_Name := Plain_Alias (Base);
      Length : Cluster_Count;
      Tail, Added : Cluster_Number;
   begin
      Start := 0;
      Plain_Taken := False;
      At_End := False;
      Taken := [others => False];
      loop
         Locate (V, W, Index, Sector, Offset, S);
         exit when S = Not_Found;
         if S /= Done then
            return;
         end if;
         if Sector /= Loaded then
            Read_Sector (V, Sector, Work, OK);
            if not OK then
               S := Device_Error;
               return;
            end if;
            Loaded := Sector;
         end if;
         Free_Slot := Ended;
         if not Ended then
            case Classify (Work, Offset) is
               when End_Of_Directory =>
                  Ended := True;
                  Free_Slot := True;
               when Deleted =>
                  Free_Slot := True;
               when Long_Part_Entry =>
                  null;
               when File_Entry | Subdirectory_Entry | Volume_Label | Dot_Entry | Dot_Dot_Entry
                  | Malformed_Entry =>
                  if Position_Of (V, Sector, Offset) /= Ignore then
                     declare
                        Name : constant Short_Name := Work (Offset .. Offset + Short_Name_Bytes - 1);
                        Tail_Number : constant Natural := Tail_Of (Name, Base);
                     begin
                        if Tail_Number in Taken'Range then
                           Taken (Tail_Number) := True;
                        end if;
                        if Name = Plain or else (Has_Exact and then Name = Exact) then
                           Plain_Taken := True;
                        end if;
                     end;
                  end if;
            end case;
         end if;
         if Free_Slot then
            if Run = 0 then
               Run_Start := Index;
            end if;
            Run := Run + 1;
            if Run >= Count and then not Found then
               Found := True;
               Start := Run_Start;
               At_End := Ended;
            end if;
            --  Past the end marker nothing more is named: done.
            exit when Found and then Ended;
         else
            Run := 0;
         end if;
         Index := Index + 1;
      end loop;
      S := Done;
      if Found then
         return;
      end if;
      --  Grow from the free run at the chain's end (if any).
      Start := (if Run > 0 then Run_Start else Index);
      At_End := True;
      if Start + Count > Maximum_Directory_Entries then
         S := Range_Unsupported;
         return;
      end if;
      while Index - Start < Count loop
         Measure (V, Directory_First, Length, Tail, S);
         if S /= Done then
            return;
         end if;
         Choose (V, 1, S);
         if S /= Done then
            Unchoose (V);
            return;
         end if;
         Added := Chosen (1);
         Chosen_Count := 0;
         Write_Cluster (V, Added, [1 .. 0 => 0], S);
         if S = Done then
            Update_Fat (V, [1 => (C => Added, Value => End_Value)], OK);
            if OK then
               Barrier (V, OK);
            end if;
            if OK then
               Update_Fat (V, [1 => (C => Tail, Value => Unsigned_32 (Added))], OK);
            end if;
            S := (if OK then Done else Device_Error);
         end if;
         if S /= Done then
            return;
         end if;
         Index := Index + Entries_Per_Cluster (V);
      end loop;
      pragma Unreferenced (Length);
   end Find_Slots;

   --  The entries naming a new object (long-name parts, then Item) for
   --  Name in the directory at Directory_First: written, their slots and
   --  position returned. Ignore: as Find_Slots.
   procedure Add_Name
     (V : in out Volume; Directory_First : Cluster_Number; Units : Long_Units;
      Length : Fat_Format.Long_Length; Ignore : Unsigned_64; Item : in out Short_Entry;
      First_Index, Short_Index : out Natural; Position : out Unsigned_64; S : out Status)
   is
      Exact : Short_Name;
      Is_Exact : Boolean;
      Base : Basis;
      Parts : Natural;
      Start : Natural;
      Plain_Taken, At_End : Boolean;
      Data : Slot_Bytes := [others => 0];
      Sum : Unsigned_8;
      W : Directory_Walk := (First => Directory_First, others => <>);
      Sector : Sector_Number;
      Offset : Natural;
   begin
      First_Index := 0;
      Short_Index := 0;
      Position := 0;
      Exact_Short (Units, Length, Exact, Is_Exact);
      Base := Make_Basis (Units, Length);
      Parts := (if Is_Exact then 0 else (Length + Units_Per_Part - 1) / Units_Per_Part);
      Find_Slots (V, Directory_First, Parts + 1, Base, Exact, Is_Exact, Ignore, Start, Plain_Taken,
                  At_End, S);
      if S /= Done then
         return;
      end if;
      if Is_Exact then
         if Plain_Taken then
            S := Exists;
            return;
         end if;
         Item.Name := Exact;
      elsif not Base.Lossy and then not Plain_Taken then
         Item.Name := Plain_Alias (Base);
      else
         declare
            N : Natural := 0;
         begin
            for K in Taken'Range loop
               if not Taken (K) then
                  N := K;
                  exit;
               end if;
            end loop;
            if N = 0 then
               S := Range_Unsupported;
               return;
            end if;
            Item.Name := Alias (Base, N);
         end;
      end if;
      Sum := Checksum (Item.Name);
      for P in 1 .. Parts loop
         --  Slot P - 1 holds part Parts - P + 1 (the last part first).
         declare
            Ordinal : constant Part_Ordinal := Parts - P + 1;
            Part : Long_Part := (Ordinal => Ordinal, Last => P = 1, Checksum => Sum, others => <>);
         begin
            for U in Part_Unit_Index loop
               declare
                  At_Unit : constant Natural := (Ordinal - 1) * Units_Per_Part + U;
               begin
                  Part.Units (U) := (if At_Unit <= Length then Units (At_Unit)
                                     elsif At_Unit = Length + 1 then Name_Terminator
                                     else Name_Padding);
               end;
            end loop;
            Encode_Long (Data, (P - 1) * Entry_Bytes, Part);
         end;
      end loop;
      Encode_Short (Data, Parts * Entry_Bytes, Item);
      --  Past the end marker the slot after the new entries must read as
      --  the end (it is zero in a cluster this driver zeroed; another
      --  writer may have left anything there).
      if At_End then
         declare
            After : Sector_Number;
            After_Offset : Natural;
            Work : Sector_Buffer;
            OK : Boolean;
            St : Status;
         begin
            Locate (V, W, Start + Parts + 1, After, After_Offset, St);
            if St = Done then
               Read_Sector (V, After, Work, OK);
               if not OK then
                  S := Device_Error;
                  return;
               elsif Work (After_Offset) /= End_Marker then
                  Work (After_Offset) := End_Marker;
                  Write_Sector (V, After, Work, OK);
                  if not OK then
                     S := Device_Error;
                     return;
                  end if;
               end if;
            end if;
         end;
      end if;
      Put_Entries (V, Directory_First, Start, Parts + 1, Data, S);
      if S /= Done then
         return;
      end if;
      First_Index := Start;
      Short_Index := Start + Parts;
      Locate (V, W, Short_Index, Sector, Offset, S);
      if S = Done then
         Position := Position_Of (V, Sector, Offset);
      end if;
   end Add_Name;

   ---------------------------------------------------------------------------
   --  Data
   ---------------------------------------------------------------------------
   Zeroes : constant Byte_Array (0 .. 65_535) := [others => 0];

   --  Bytes at volume byte Position, to or from Data: whole sectors go
   --  directly, partial ones read-modify-write through a sector buffer.
   procedure Byte_IO
     (V : in out Volume; Write : Boolean; Position : Unsigned_64; Count : Unsigned_64;
      Data : System.Address; OK : out Boolean)
   is
      SB : constant Unsigned_64 := Unsigned_64 (Sector_Size (V));
      Here : Unsigned_64 := Position;
      Done_Bytes : Unsigned_64 := 0;
      Work : Sector_Buffer;
   begin
      OK := True;
      while Done_Bytes < Count loop
         declare
            Sector : constant Sector_Number := Sector_Number (Here / SB);
            Within : constant Unsigned_64 := Here mod SB;
            Left : constant Unsigned_64 := Count - Done_Bytes;
         begin
            if Within = 0 and then Left >= SB then
               declare
                  Whole : constant Natural := Natural (Unsigned_64'Min (Left / SB, 1024));
               begin
                  Transfer (V, Write, Sector, Whole, Data + Storage_Offset (Done_Bytes), OK);
                  if not OK then
                     return;
                  end if;
                  if Write then
                     Forget (V, Sector, Whole);
                  end if;
                  Here := Here + Unsigned_64 (Whole) * SB;
                  Done_Bytes := Done_Bytes + Unsigned_64 (Whole) * SB;
               end;
            else
               declare
                  Part : constant Unsigned_64 := Unsigned_64'Min (Left, SB - Within);
                  Bytes : Byte_Array (0 .. Natural (Part) - 1)
                    with Import, Address => Data + Storage_Offset (Done_Bytes);
               begin
                  Transfer (V, False, Sector, 1, Work'Address, OK);
                  if not OK then
                     return;
                  end if;
                  if Write then
                     Work (Natural (Within) .. Natural (Within + Part) - 1) := Bytes;
                     Transfer (V, True, Sector, 1, Work'Address, OK);
                     if not OK then
                        return;
                     end if;
                     Forget (V, Sector, 1);
                  else
                     Bytes := Work (Natural (Within) .. Natural (Within + Part) - 1);
                  end if;
                  Here := Here + Part;
                  Done_Bytes := Done_Bytes + Part;
               end;
            end if;
         end;
      end loop;
   end Byte_IO;

   function Cluster_Position (V : Volume; C : Cluster_Number) return Unsigned_64 is
     (Unsigned_64 (Cluster_Sector (V.G, C)) * Unsigned_64 (Sector_Size (V)));


   --  The cluster holding file cluster Index of the chain from First.
   procedure Cluster_At
     (V : in out Volume; First : Cluster_Number; Index : Cluster_Count; C : out Cluster_Number;
      S : out Status)
   is
      From : Cluster_Number := First;
      Skip : Cluster_Count := Index;
   begin
      if Last_Read.Key = V.Device.volumeKey and then Last_Read.First = First and then First /= No_Cluster
        and then Last_Read.Index <= Index
      then
         From := Last_Read.Cluster;
         Skip := Index - Last_Read.Index;
      end if;
      Seek (V, From, Skip, C, S);
      if S = Done then
         Last_Read := (Key => V.Device.volumeKey, First => First, Index => Index, Cluster => C);
      elsif S = Not_Found then
         S := Malformed;   --  the chain is shorter than the size says
      end if;
   end Cluster_At;

   --  Read or write Count bytes at file offset Offset of the chain from
   --  First (all of it allocated), a run of contiguous clusters at a time.
   procedure File_IO
     (V : in out Volume; Write : Boolean; First : Cluster_Number; Offset, Count : Unsigned_64;
      Data : System.Address; S : out Status)
   is
      CB : constant Unsigned_64 := Unsigned_64 (Cluster_Bytes (V.G));
      Done_Bytes : Unsigned_64 := 0;
      C, Next_C : Cluster_Number;
      Raw : Unsigned_32;
      OK : Boolean;
   begin
      S := Done;
      if Count = 0 then
         return;
      end if;
      Cluster_At (V, First, Cluster_Count (Offset / CB), C, S);
      if S /= Done then
         return;
      end if;
      while Done_Bytes < Count loop
         declare
            At_Byte : constant Unsigned_64 := Offset + Done_Bytes;
            Within : constant Unsigned_64 := At_Byte mod CB;
            Run_Start : constant Cluster_Number := C;
            Run_Bytes : Unsigned_64 := Unsigned_64'Min (CB - Within, Count - Done_Bytes);
            Run_Index : Cluster_Count := Cluster_Count (At_Byte / CB);
         begin
            --  Extend the run while the next cluster follows on disk.
            while Done_Bytes + Run_Bytes < Count loop
               Get_Entry (V, C, Raw, OK);
               if not OK then
                  S := Device_Error;
                  return;
               end if;
               declare
                  Item : constant Decoded_Entry := Decode_Entry (Raw, Last_Cluster (V.G));
               begin
                  if Item.Kind /= Next_Entry then
                     S := Malformed;
                     return;
                  end if;
                  Next_C := Item.Next;
               end;
               Run_Index := Run_Index + 1;
               exit when Next_C /= C + 1;
               C := Next_C;
               Run_Bytes := Run_Bytes + Unsigned_64'Min (CB, Count - Done_Bytes - Run_Bytes);
               Last_Read := (Key => V.Device.volumeKey, First => First, Index => Run_Index, Cluster => C);
            end loop;
            Byte_IO (V, Write, Cluster_Position (V, Run_Start) + Within, Run_Bytes,
                     Data + Storage_Offset (Done_Bytes), OK);
            if not OK then
               S := Device_Error;
               return;
            end if;
            Done_Bytes := Done_Bytes + Run_Bytes;
            if Done_Bytes < Count then
               C := Next_C;
               Last_Read := (Key => V.Device.volumeKey, First => First, Index => Run_Index, Cluster => C);
            end if;
         end;
      end loop;
   end File_IO;

   procedure Read_Data
     (V : in out Volume; Item : Inode; Offset : Unsigned_64; Buffer : System.Address;
      Count : Unsigned_64; Done_Bytes : out Unsigned_64; S : out Status)
   is
      Size : constant Unsigned_64 := Unsigned_64 (Item.sizeLo);
      First : constant Cluster_Number := Cluster_Number (Item.directBlocks (0) and Entry_Mask);
      Wanted : Unsigned_64;
   begin
      Done_Bytes := 0;
      S := Done;
      if not V.Mounted or else Ext2_Type (Item) /= Regular_Type then
         S := Wrong_Object;
         return;
      elsif Offset >= Size or else Count = 0 then
         return;
      end if;
      Wanted := Unsigned_64'Min (Count, Size - Offset);
      --  The chain is walked whole once (cycles, breaks) before any read
      --  follows it, and must hold the size.
      if Checked_First /= First or else Checked_Key /= V.Device.volumeKey or else First = No_Cluster then
         declare
            Length : Cluster_Count;
            Tail : Cluster_Number;
         begin
            Measure (V, First, Length, Tail, S);
            if S /= Done then
               return;
            end if;
            Checked_Key := V.Device.volumeKey;
            Checked_First := First;
            Checked_Length := Length;
         end;
      end if;
      if Unsigned_64 (Checked_Length) * Unsigned_64 (Cluster_Bytes (V.G)) < Size then
         S := Malformed;
         return;
      end if;
      File_IO (V, False, First, Offset, Wanted, Buffer, S);
      if S = Done then
         Done_Bytes := Wanted;
      end if;
   end Read_Data;

   --  The object's directory entry, decoded, and where it is.
   procedure Get_Entry_Of
     (V : in out Volume; O : Object; E : out Short_Entry; Sector : out Sector_Number; Offset : out Natural;
      S : out Status)
   is
      SB : constant Unsigned_64 := Unsigned_64 (Sector_Size (V));
      Byte : constant Unsigned_64 := O.Position * Entry_Bytes;
      Work : Sector_Buffer;
      OK, Valid_Cluster : Boolean;
   begin
      E := (others => <>);
      Sector := Sector_Number (Byte / SB);
      Offset := Natural (Byte mod SB);
      Read_Sector (V, Sector, Work, OK);
      if not OK then
         S := Device_Error;
         return;
      end if;
      if Classify (Work, Offset) not in File_Entry | Subdirectory_Entry then
         S := Malformed;
         return;
      end if;
      Decode_Short (Work, Offset, E, Valid_Cluster);
      S := (if Valid_Cluster then Done else Malformed);
   end Get_Entry_Of;

   --  Rewrite the object's short entry with E.
   procedure Put_Entry_Of (V : in out Volume; O : Object; E : Short_Entry; S : out Status) is
      SB : constant Unsigned_64 := Unsigned_64 (Sector_Size (V));
      Byte : constant Unsigned_64 := O.Position * Entry_Bytes;
      Work : Sector_Buffer;
      OK : Boolean;
   begin
      Read_Sector (V, Sector_Number (Byte / SB), Work, OK);
      if OK then
         Encode_Short (Work, Natural (Byte mod SB), E);
         Write_Sector (V, Sector_Number (Byte / SB), Work, OK);
      end if;
      S := (if OK then Done else Device_Error);
   end Put_Entry_Of;

   procedure Stamp (E : in out Short_Entry) is
   begin
      From_Unix (Now_Seconds, E.Written_Date, E.Written_Time);
      E.Accessed_Date := E.Written_Date;
      E.Attributes := E.Attributes or Attribute_Archive;
   end Stamp;

   --  The object's chain length and last cluster (cached), checked whole.
   procedure Chain_Of (V : in out Volume; Index : Positive; S : out Status) is
      O : Object renames V.Objects.Items (Index);
   begin
      S := Done;
      if O.Known then
         return;
      elsif O.First = No_Cluster then
         O.Length := 0;
         O.Tail := No_Cluster;
      else
         Measure (V, O.First, O.Length, O.Tail, S);
         if S /= Done then
            return;
         end if;
      end if;
      --  A chain too short for the size is a broken file: no change to it.
      if Unsigned_64 (O.Length) * Unsigned_64 (Cluster_Bytes (V.G)) < Unsigned_64 (O.Size) then
         S := Malformed;
         return;
      end if;
      O.Known := True;
   end Chain_Of;

   --  Grow object Index's chain to Wanted clusters: the new clusters'
   --  chain in every FAT, then the link from the old last cluster. The
   --  new clusters are past the size until the caller has written every
   --  byte up to its new size and made it durable (Write_Data).
   procedure Grow (V : in out Volume; Index : Positive; Wanted : Cluster_Count; S : out Status) is
      O : Object renames V.Objects.Items (Index);
      List : Change_List (1 .. Batch_Clusters);
      OK : Boolean;
   begin
      Chain_Of (V, Index, S);
      if S /= Done then
         return;
      end if;
      while O.Length < Wanted loop
         declare
            Count : constant Natural := Natural (Cluster_Count'Min (Wanted - O.Length, Batch_Clusters));
         begin
            Choose (V, Count, S);
            if S /= Done then
               Unchoose (V);
               return;
            end if;
            for K in 1 .. Count loop
               List (K) := (C => Chosen (K),
                            Value => (if K = Count then End_Value else Unsigned_32 (Chosen (K + 1))));
            end loop;
            Update_Fat (V, List (1 .. Count), OK);
            if OK and then O.First /= No_Cluster then
               Update_Fat (V, [1 => (C => O.Tail, Value => Unsigned_32 (Chosen (1)))], OK);
            end if;
            if not OK then
               Chosen_Count := 0;
               S := Device_Error;
               return;
            end if;
            if O.First = No_Cluster then
               O.First := Chosen (1);
            end if;
            O.Tail := Chosen (Count);
            O.Length := O.Length + Cluster_Count (Count);
            Chosen_Count := 0;
         end;
      end loop;
      S := Done;
   end Grow;

   function Object_Item (V : Volume; O : Object; E : Short_Entry) return Inode is
     (Inode_Of (V, E, O.Directory, (if O.Live then 1 else 0)));

   --  Make object Index Size bytes long with E its entry (when live).
   procedure Publish (V : in out Volume; Index : Positive; E : in out Short_Entry; S : out Status) is
      O : Object renames V.Objects.Items (Index);
   begin
      E.First := O.First;
      E.Size := O.Size;
      Stamp (E);
      S := Done;
      if O.Live then
         Put_Entry_Of (V, O, E, S);
      end if;
   end Publish;

   procedure Write_Data
     (V : in out Volume; Number : Unsigned_32; Item : in out Inode; Offset : Unsigned_64;
      Buffer : System.Address; Count : Unsigned_64; Done_Bytes : out Unsigned_64; S : out Status)
   is
      Index : constant Natural := (if V.Mounted then Object_At (V, Number) else 0);
      E : Short_Entry;
      Sector : Sector_Number;
      Offset_In : Natural;
      CB : Unsigned_64;
      Old_Size, New_End : Unsigned_64;
      OK : Boolean;
   begin
      Done_Bytes := 0;
      if Index = 0 or else V.Objects.Items (Index).Directory then
         S := Wrong_Object;
         return;
      elsif Offset > Largest_File or else Count > Largest_File - Offset then
         S := Range_Unsupported;
         return;
      elsif Count = 0 then
         S := Done;
         return;
      end if;
      Begin_Change (V, S);
      if S /= Done then
         return;
      end if;
      CB := Unsigned_64 (Cluster_Bytes (V.G));
      declare
         O : Object renames V.Objects.Items (Index);
      begin
         if O.Live then
            Get_Entry_Of (V, O, E, Sector, Offset_In, S);
            if S /= Done then
               return;
            end if;
         else
            E := (First => O.First, Size => O.Size, others => <>);
         end if;
         Chain_Of (V, Index, S);
         if S /= Done then
            Item := Object_Item (V, O, E);
            return;
         end if;
         Old_Size := Unsigned_64 (O.Size);
         New_End := Offset + Count;
         if New_End > Unsigned_64 (O.Length) * CB then
            Grow (V, Index, Cluster_Count ((New_End + CB - 1) / CB), S);
            if S /= Done then
               Item := Object_Item (V, O, E);
               return;
            end if;
         end if;
         --  Every byte up to the new size is written before the size is
         --  published: zeroes between the old end and the write.
         if Offset > Old_Size then
            declare
               Gap : Unsigned_64 := Offset - Old_Size;
               At_Byte : Unsigned_64 := Old_Size;
            begin
               while Gap > 0 loop
                  File_IO (V, True, O.First, At_Byte, Unsigned_64'Min (Gap, Zeroes'Length),
                           Zeroes'Address, S);
                  if S /= Done then
                     Item := Object_Item (V, O, E);
                     return;
                  end if;
                  At_Byte := At_Byte + Unsigned_64'Min (Gap, Zeroes'Length);
                  Gap := Gap - Unsigned_64'Min (Gap, Zeroes'Length);
               end loop;
            end;
         end if;
         File_IO (V, True, O.First, Offset, Count, Buffer, S);
         if S /= Done then
            Item := Object_Item (V, O, E);
            return;
         end if;
         if New_End > Old_Size then
            --  The data, then the size (and a new first cluster).
            Barrier (V, OK);
            if not OK then
               S := Device_Error;
               Item := Object_Item (V, O, E);
               return;
            end if;
            O.Size := Unsigned_32 (New_End);
         end if;
         Publish (V, Index, E, S);
         Item := Object_Item (V, O, E);
         if S = Done then
            Done_Bytes := Count;
         end if;
      end;
   end Write_Data;

   --  Shrink object Index to Size: the entry first, a barrier, then the
   --  released clusters freed (see the spec).
   procedure Shrink (V : in out Volume; Index : Positive; Size : Unsigned_64; E : in out Short_Entry;
                     S : out Status)
   is
      O : Object renames V.Objects.Items (Index);
      CB : constant Unsigned_64 := Unsigned_64 (Cluster_Bytes (V.G));
      Keep : constant Cluster_Count := Cluster_Count ((Size + CB - 1) / CB);
      Release : Cluster_Number := No_Cluster;
      New_Tail, Raw_Next : Cluster_Number;
      Raw : Unsigned_32;
      OK : Boolean;
   begin
      Chain_Of (V, Index, S);
      if S /= Done then
         return;
      end if;
      if Keep = 0 then
         Release := O.First;
         O.First := No_Cluster;
         O.Length := 0;
         O.Tail := No_Cluster;
      elsif Keep < O.Length then
         Seek (V, O.First, Keep - 1, New_Tail, S);
         if S /= Done then
            S := (if S = Not_Found then Malformed else S);
            return;
         end if;
         Get_Entry (V, New_Tail, Raw, OK);
         if not OK then
            S := Device_Error;
            return;
         end if;
         declare
            Item : constant Decoded_Entry := Decode_Entry (Raw, Last_Cluster (V.G));
         begin
            Raw_Next := (if Item.Kind = Next_Entry then Item.Next else No_Cluster);
         end;
         Update_Fat (V, [1 => (C => New_Tail, Value => End_Value)], OK);
         if not OK then
            S := Device_Error;
            return;
         end if;
         Release := Raw_Next;
         O.Length := Keep;
         O.Tail := New_Tail;
      end if;
      O.Size := Unsigned_32 (Size);
      Publish (V, Index, E, S);
      if S /= Done or else Release = No_Cluster then
         return;
      end if;
      Barrier (V, OK);
      if not OK then
         S := Device_Error;
         return;
      end if;
      Free_Chain (V, Release, S);
      Last_Read := (others => <>);
   end Shrink;

   procedure Resize
     (V : in out Volume; Number : Unsigned_32; Size : Unsigned_64; Item : out Inode; S : out Status)
   is
      Index : constant Natural := (if V.Mounted then Object_At (V, Number) else 0);
      E : Short_Entry;
      Sector : Sector_Number;
      Offset : Natural;
      Done_Bytes : Unsigned_64;
   begin
      Item := Root_Inode (V);
      if Index = 0 or else V.Objects.Items (Index).Directory then
         S := Wrong_Object;
         return;
      elsif Size > Largest_File then
         S := Range_Unsupported;
         return;
      end if;
      Begin_Change (V, S);
      if S /= Done then
         return;
      end if;
      declare
         O : Object renames V.Objects.Items (Index);
      begin
         if O.Live then
            Get_Entry_Of (V, O, E, Sector, Offset, S);
            if S /= Done then
               return;
            end if;
         else
            E := (First => O.First, Size => O.Size, others => <>);
         end if;
         if Size > Unsigned_64 (O.Size) then
            --  Growth: zeroes, written as data.
            declare
               Item_Now : Inode := Object_Item (V, O, E);
               Old : constant Unsigned_64 := Unsigned_64 (O.Size);
            begin
               if Size - Old <= Zeroes'Length then
                  Write_Data (V, Number, Item_Now, Old, Zeroes'Address, Size - Old, Done_Bytes, S);
               else
                  Write_Data (V, Number, Item_Now, Size - 1, Zeroes'Address, 1, Done_Bytes, S);
               end if;
               Item := Item_Now;
               return;
            end;
         end if;
         Shrink (V, Index, Size, E, S);
         Item := Object_Item (V, O, E);
      end;
   end Resize;

   ---------------------------------------------------------------------------
   --  Names
   ---------------------------------------------------------------------------
   procedure Create_Object
     (V : in out Volume; Parent : Unsigned_32; Name : String; Directory : Boolean;
      Number : out Unsigned_32; S : out Status)
   is
      Parent_First : Cluster_Number;
      Units : Long_Units;
      Length : Fat_Format.Long_Length;
      Info : Record_Info;
      E : Short_Entry;
      First_Index, Short_Index : Natural;
      Position : Unsigned_64;
      Added : Cluster_Number := No_Cluster;
      OK : Boolean;
   begin
      Number := 0;
      Directory_Of (V, Parent, Parent_First, S);
      if S /= Done then
         return;
      end if;
      Units_Of (Name, True, Units, Length, S);
      if S /= Done then
         return;
      end if;
      Find (V, Parent_First, Units, Length, Info, S);
      if S = Done then
         S := Exists;
         return;
      elsif S /= Not_Found then
         return;
      end if;
      Begin_Change (V, S);
      if S /= Done then
         return;
      end if;
      E.Attributes := (if Directory then Attribute_Directory else Attribute_Archive);
      From_Unix (Now_Seconds, E.Created_Date, E.Created_Time);
      E.Written_Date := E.Created_Date;
      E.Written_Time := E.Created_Time;
      E.Accessed_Date := E.Created_Date;
      if Directory then
         --  Its cluster ("." and "..", zeros) and end of chain, a barrier,
         --  then the parent's entry.
         Choose (V, 1, S);
         if S /= Done then
            Unchoose (V);
            return;
         end if;
         Added := Chosen (1);
         Chosen_Count := 0;
         declare
            Head : Byte_Array (0 .. 2 * Entry_Bytes - 1) := [others => 0];
            Dot_Item : Short_Entry := E;
         begin
            Dot_Item.Name := [0 => Dot, others => Space];
            Dot_Item.First := Added;
            Encode_Short (Head, 0, Dot_Item);
            Dot_Item.Name := [0 | 1 => Dot, others => Space];
            --  ".." of a directory in the root is cluster 0 (FATGEN section 6.4).
            Dot_Item.First := (if Parent = Root_Number then No_Cluster else Parent_First);
            Encode_Short (Head, Entry_Bytes, Dot_Item);
            Write_Cluster (V, Added, Head, S);
         end;
         if S = Done then
            Update_Fat (V, [1 => (C => Added, Value => End_Value)], OK);
            if OK then
               Barrier (V, OK);
            end if;
            S := (if OK then Done else Device_Error);
         end if;
         if S /= Done then
            return;
         end if;
         E.First := Added;
      end if;
      Add_Name (V, Parent_First, Units, Length, 0, E, First_Index, Short_Index, Position, S);
      if S /= Done then
         if Added /= No_Cluster then
            --  The name was not written: the new cluster is unreferenced.
            declare
               Ignore : Status;
            begin
               Free_Chain (V, Added, Ignore);
            end;
         end if;
         return;
      end if;
      Number_For (V, Position, Parent, E, Directory, Number, S);
      if S = Done and then Directory then
         V.Objects.Items (Index_Of (Number)).Known := True;
         V.Objects.Items (Index_Of (Number)).Length := 1;
         V.Objects.Items (Index_Of (Number)).Tail := Added;
      elsif S = Done then
         V.Objects.Items (Index_Of (Number)).Known := True;
         V.Objects.Items (Index_Of (Number)).Length := 0;
      end if;
   end Create_Object;

   procedure Create_File
     (V : in out Volume; Directory : Unsigned_32; Name : String; Number : out Unsigned_32; S : out Status) is
   begin
      Number := 0;
      if not V.Mounted then
         S := Malformed;
         return;
      end if;
      Create_Object (V, Directory, Name, False, Number, S);
   end Create_File;

   procedure Make_Directory
     (V : in out Volume; Path : String; Number : out Unsigned_32; Parent_Status : out Status;
      S : out Status)
   is
      Parent : Unsigned_32;
      First, Last : Natural;
   begin
      Number := 0;
      S := Malformed;
      if not V.Mounted then
         Parent_Status := Malformed;
         return;
      end if;
      Resolve_Parent (V, Path, Parent, First, Last, Parent_Status);
      if Parent_Status /= Done then
         return;
      end if;
      Create_Object (V, Parent, Path (First .. Last), True, Number, S);
   end Make_Directory;

   --  The entry Path names, with its parent's number and first cluster.
   procedure Locate_Name
     (V : in out Volume; Path : String; Parent : out Unsigned_32; Parent_First : out Cluster_Number;
      Info : out Record_Info; Number : out Unsigned_32; S : out Status)
   is
      First, Last : Natural;
   begin
      Number := 0;
      Parent_First := No_Cluster;
      Info := (others => <>);
      Resolve_Parent (V, Path, Parent, First, Last, S);
      if S /= Done then
         return;
      end if;
      Directory_Of (V, Parent, Parent_First, S);
      if S /= Done then
         return;
      end if;
      Step_Into (V, Parent, Path (First .. Last), Number, Info, S);
   end Locate_Name;

   --  The directory at First holds nothing but "." and "..".
   procedure Empty_Directory (V : in out Volume; First : Cluster_Number; Empty : out Boolean; S : out Status) is
      W : Directory_Walk := (First => First, others => <>);
      Index : Natural := 0;
      Info : Record_Info;
   begin
      Next_Record (V, W, Index, Info, S);
      Empty := S = Not_Found;
      if S = Not_Found then
         S := Done;
      end if;
   end Empty_Directory;

   --  Remove the name; its object loses its entry. Its clusters stay.
   procedure Remove_Name
     (V : in out Volume; Parent_First : Cluster_Number; Info : Record_Info; Number : Unsigned_32;
      S : out Status)
   is
      Index : constant Natural := Object_At (V, Number);
   begin
      Delete_Entries (V, Parent_First, Info.First_Index, Info.Short_Index, S);
      if Index /= 0 then
         Drop_Key (V.Objects.all, V.Objects.Items (Index).Position);
         V.Objects.Items (Index).Live := False;
         V.Objects.Items (Index).Position := 0;
      end if;
   end Remove_Name;

   procedure Release (V : in out Volume; First : Cluster_Number; S : out Status) is
      OK : Boolean;
   begin
      S := Done;
      if First = No_Cluster then
         return;
      end if;
      Barrier (V, OK);
      if not OK then
         S := Device_Error;
         return;
      end if;
      Free_Chain (V, First, S);
      Last_Read := (others => <>);
   end Release;

   procedure Unlink
     (V : in out Volume; Path : String; Keep : Boolean; Number : out Unsigned_32; Removed : out Inode;
      S : out Status)
   is
      Parent : Unsigned_32;
      Parent_First : Cluster_Number;
      Info : Record_Info;
   begin
      Removed := Root_Inode (V);
      Number := 0;
      if not V.Mounted then
         S := Malformed;
         return;
      end if;
      Locate_Name (V, Path, Parent, Parent_First, Info, Number, S);
      if S /= Done then
         Number := 0;
         return;
      elsif Info.Directory then
         Number := 0;
         S := Is_Directory;
         return;
      end if;
      Begin_Change (V, S);
      if S /= Done then
         Number := 0;
         return;
      end if;
      Remove_Name (V, Parent_First, Info, Number, S);
      if S /= Done then
         return;
      end if;
      declare
         O : Object renames V.Objects.Items (Index_Of (Number));
      begin
         Removed := Inode_Of (V, Info.Short, False, 0);
         if not Keep then
            Release (V, O.First, S);
            O.First := No_Cluster;
            O.Size := 0;
            O.Known := False;
         end if;
      end;
   end Unlink;

   procedure Reclaim (V : in out Volume; Number : Unsigned_32; S : out Status) is
      Index : constant Natural := (if V.Mounted then Object_At (V, Number) else 0);
   begin
      if Index = 0 or else V.Objects.Items (Index).Live then
         S := Wrong_Object;
         return;
      end if;
      Begin_Change (V, S);
      if S /= Done then
         return;
      end if;
      Release (V, V.Objects.Items (Index).First, S);
      V.Objects.Items (Index).First := No_Cluster;
      V.Objects.Items (Index).Size := 0;
      V.Objects.Items (Index).Known := False;
   end Reclaim;

   procedure Remove_Directory (V : in out Volume; Path : String; Number : out Unsigned_32; S : out Status) is
      Parent : Unsigned_32;
      Parent_First : Cluster_Number;
      Info : Record_Info;
      Empty : Boolean;
   begin
      Number := 0;
      if not V.Mounted then
         S := Malformed;
         return;
      end if;
      Locate_Name (V, Path, Parent, Parent_First, Info, Number, S);
      if S /= Done then
         Number := 0;
         return;
      elsif not Info.Directory then
         Number := 0;
         S := Not_Directory;
         return;
      end if;
      Empty_Directory (V, Info.Short.First, Empty, S);
      if S /= Done then
         return;
      elsif not Empty then
         S := Not_Empty;
         return;
      end if;
      Begin_Change (V, S);
      if S /= Done then
         return;
      end if;
      Remove_Name (V, Parent_First, Info, Number, S);
      if S = Done then
         Release (V, Info.Short.First, S);
         V.Objects.Items (Index_Of (Number)).First := No_Cluster;
      end if;
   end Remove_Directory;

   --  Point the moved directory's ".." at its new parent.
   procedure Repoint_Parent
     (V : in out Volume; Directory_First : Cluster_Number; New_Parent : Unsigned_32;
      New_Parent_First : Cluster_Number; S : out Status)
   is
      Sector : constant Sector_Number := Cluster_Sector (V.G, Directory_First);
      Work : Sector_Buffer;
      OK, Valid_Cluster : Boolean;
      E : Short_Entry;
   begin
      Read_Sector (V, Sector, Work, OK);
      if not OK then
         S := Device_Error;
         return;
      elsif Classify (Work, Entry_Bytes) /= Dot_Dot_Entry then
         S := Malformed;
         return;
      end if;
      Decode_Short (Work, Entry_Bytes, E, Valid_Cluster);
      E.First := (if New_Parent = Root_Number then No_Cluster else New_Parent_First);
      Encode_Short (Work, Entry_Bytes, E);
      Write_Sector (V, Sector, Work, OK);
      S := (if OK then Done else Device_Error);
   end Repoint_Parent;

   procedure Rename
     (V : in out Volume; Old_Path, New_Path : String; Keep : Boolean;
      Replaced_Number : out Unsigned_32; Replaced : out Inode; S : out Status)
   is
      Old_Parent, New_Parent, Source, Target : Unsigned_32;
      Old_First_Cluster, New_First_Cluster : Cluster_Number;
      Source_Info, Target_Info : Record_Info;
      First, Last : Natural;
      Units : Long_Units;
      Length : Fat_Format.Long_Length;
      OK : Boolean;
   begin
      Replaced_Number := 0;
      Replaced := Root_Inode (V);
      if not V.Mounted then
         S := Malformed;
         return;
      end if;
      Locate_Name (V, Old_Path, Old_Parent, Old_First_Cluster, Source_Info, Source, S);
      if S /= Done then
         return;
      end if;
      Resolve_Parent (V, New_Path, New_Parent, First, Last, S);
      if S /= Done then
         S := (if S = Invalid_Name then Invalid_Name else Not_Found);
         return;
      end if;
      Directory_Of (V, New_Parent, New_First_Cluster, S);
      if S /= Done then
         return;
      end if;
      Units_Of (New_Path (First .. Last), True, Units, Length, S);
      if S /= Done then
         return;
      end if;
      --  A directory may not move into its own subtree.
      if Source_Info.Directory then
         declare
            Up : Unsigned_32 := New_Parent;
            Steps : Natural := 0;
         begin
            while Up /= Root_Number and then Up /= 0 and then Steps <= V.Objects.Count loop
               if Up = Source then
                  S := Invalid_Move;
                  return;
               end if;
               exit when Object_At (V, Up) = 0;
               Up := V.Objects.Items (Object_At (V, Up)).Parent;
               Steps := Steps + 1;
            end loop;
         end;
      end if;
      Find (V, New_First_Cluster, Units, Length, Target_Info, S);
      if S = Done and then Target_Info.Position = Source_Info.Position then
         S := Not_Found;   --  the same entry under another case: a new name
      elsif S = Done then
         Number_For (V, Target_Info.Position, New_Parent, Target_Info.Short, Target_Info.Directory,
                     Target, S);
         if S /= Done then
            return;
         end if;
         if Source_Info.Directory and then not Target_Info.Directory then
            S := Not_Directory;
            return;
         elsif not Source_Info.Directory and then Target_Info.Directory then
            S := Is_Directory;
            return;
         elsif Target_Info.Directory then
            declare
               Empty : Boolean;
            begin
               Empty_Directory (V, Target_Info.Short.First, Empty, S);
               if S /= Done then
                  return;
               elsif not Empty then
                  S := Not_Empty;
                  return;
               end if;
            end;
         end if;
         Begin_Change (V, S);
         if S /= Done then
            return;
         end if;
         --  The target's short entry takes the source's object, keeping its name.
         declare
            E : Short_Entry := Source_Info.Short;
            T : Object renames V.Objects.Items (Index_Of (Target));
            Old_Target : constant Short_Entry := Target_Info.Short;
         begin
            E.Name := Target_Info.Short.Name;
            E.Case_Flags := Target_Info.Short.Case_Flags;
            Put_Entry_Of (V, T, E, S);
            if S = Done and then Source_Info.Directory and then Old_Parent /= New_Parent then
               Repoint_Parent (V, Source_Info.Short.First, New_Parent, New_First_Cluster, S);
            end if;
            if S = Done then
               Barrier (V, OK);
               S := (if OK then Done else Device_Error);
            end if;
            if S /= Done then
               return;
            end if;
            Delete_Entries (V, Old_First_Cluster, Source_Info.First_Index, Source_Info.Short_Index, S);
            if S /= Done then
               return;
            end if;
            --  Numbers follow the objects: the source's moves to the target's
            --  entry; the target's object loses its name.
            Drop_Key (V.Objects.all, Source_Info.Position);
            Drop_Key (V.Objects.all, Target_Info.Position);
            T.Live := False;
            T.Position := 0;
            declare
               O : Object renames V.Objects.Items (Index_Of (Source));
            begin
               O.Position := Target_Info.Position;
               O.Parent := New_Parent;
               Index_Position (V.Objects.all, O.Position, Source);
            end;
            Replaced := Inode_Of (V, Old_Target, Target_Info.Directory, 0);
            if Keep and then not Target_Info.Directory then
               Replaced_Number := Target;
            else
               Release (V, Old_Target.First, S);
               T.First := No_Cluster;
               T.Size := 0;
               T.Known := False;
            end if;
         end;
         return;
      end if;
      if S /= Not_Found then
         return;
      end if;
      Begin_Change (V, S);
      if S /= Done then
         return;
      end if;
      --  A new name for the object, then the old one goes.
      declare
         E : Short_Entry := Source_Info.Short;
         New_First, New_Short : Natural;
         Position : Unsigned_64;
      begin
         E.Case_Flags := 0;
         Add_Name (V, New_First_Cluster, Units, Length, Source_Info.Position, E, New_First, New_Short, Position, S);
         if S = Done and then Source_Info.Directory and then Old_Parent /= New_Parent then
            Repoint_Parent (V, Source_Info.Short.First, New_Parent, New_First_Cluster, S);
         end if;
         if S = Done then
            Barrier (V, OK);
            S := (if OK then Done else Device_Error);
         end if;
         if S /= Done then
            return;
         end if;
         --  The old slots may have moved if the new name went in the same
         --  directory before them: they did not (new names only take free
         --  slots), so the old indexes still hold.
         Delete_Entries (V, Old_First_Cluster, Source_Info.First_Index, Source_Info.Short_Index, S);
         if S /= Done then
            return;
         end if;
         Drop_Key (V.Objects.all, Source_Info.Position);
         declare
            O : Object renames V.Objects.Items (Index_Of (Source));
         begin
            O.Position := Position;
            O.Parent := New_Parent;
            Index_Position (V.Objects.all, Position, Source);
         end;
      end;
   end Rename;

   ---------------------------------------------------------------------------
   --  Mount
   ---------------------------------------------------------------------------
   procedure Mount
     (V          : out Volume;
      Endpoint   : CuBit.Handles.Endpoint;
      Key        : Unsigned_64;
      Grant      : CuBit.Memory_Grants.Grant_Reference;
      Buffer     : System.Address;
      Buffer_Bytes : Unsigned_32;
      Window     : Volume_Window;
      Result     : out Volume_Admission.Admission_Result)
   is
      Msg : Message := NULL_MESSAGE;
      Description : Device_Description;
      Candidate : Volume;
      OK : Boolean;
      Boot : Boot_Sector;
      Decoded : Boot_Result;
   begin
      V := (others => <>);
      Result := Invalid_Session;
      if Key = 0 or else not CuBit.Handles.Valid (Endpoint) or else Buffer = System.Null_Address
        or else Buffer_Bytes = 0
      then
         return;
      end if;
      Msg.tag := (label => OP_DESCRIBE_DEVICE, length => 0, protocol => 0, ipc => <>, reserved => 0);
      Msg.tag := capCall (Endpoint, Msg, CuBit.Messages.Wait_Forever);
      if Msg.tag.label /= REPLY_OK then
         Result := Device_Error;
         return;
      elsif Msg.tag.length /= 4 or else Msg.tag.protocol /= 0 or else Msg.tag.reserved /= 0
        or else not Decode_Description (Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3),
                                        Description)
      then
         Result := Invalid_Description;
         return;
      elsif Buffer_Bytes < Unsigned_32 (Description.logicalBlockSize)
        or else Description.logicalBlockSize > Largest_Sector
      then
         Result := Invalid_Session;
         return;
      end if;
      if Window.Blocks > 0 then
         if Window.First >= Description.blockCount
           or else Window.Blocks > Description.blockCount - Window.First
         then
            Result := Invalid_Description;
            return;
         end if;
         Description.blockCount := Window.Blocks;
      end if;
      Candidate.Device := (endpoint => Endpoint, volumeKey => Key, grant => Grant, grantBuffer => Buffer,
                           grantBytes => Buffer_Bytes, firstBlock => Window.First,
                           description => Description);
      --  The boot sector, through one device block.
      Device_Call (Candidate, False, 0, 1, OK);
      if not OK then
         Result := Device_Error;
         return;
      end if;
      declare
         Block : Byte_Array (0 .. Boot_Sector_Bytes - 1) with Import, Address => Buffer;
      begin
         Boot := Block;
      end;
      Decode_Boot (Boot, Device_Block_Bytes (Description.logicalBlockSize),
                   Byte_Count (Unsigned_64'Min (Description.blockCount, 2 ** 40))
                     * Byte_Count (Description.logicalBlockSize),
                   Candidate.G, Decoded);
      case Decoded is
         when Boot_Valid => null;
         when No_Boot_Signature | Bad_Jump | Not_Fat32_Layout | Fat12_Or_16 | Bad_Sector_Size
            | Bad_Media =>
            Result := Unsupported_Filesystem;
            return;
         when others =>
            debugPrint ("FAT32: boot sector refused: " & Boot_Result'Image (Decoded) & ASCII.LF);
            Result := Invalid_Filesystem;
            return;
      end case;
      if Natural (Buffer_Bytes) < Sector_Bytes (Candidate.G) then
         Result := Invalid_Session;
         return;
      end if;
      Candidate.Device_Blocks_Per_Sector := Sector_Bytes (Candidate.G) / Natural (Description.logicalBlockSize);
      Candidate.Cache := new Cache_Store (Slots => Cache_Bytes / Sector_Bytes (Candidate.G),
                                          Last_Byte => Cache_Bytes - 1);
      Candidate.Mounted := True;
      --  Free clusters, counted from the active FAT (FSInfo is a hint).
      declare
         SB : constant Natural := Sector_Bytes (Candidate.G);
         EPS : constant Natural := Entries_Per_Sector (Candidate.G);
         Total : constant Natural := Natural (Candidate.G.Clusters) + 2;
         First_Sector : constant Sector_Number := Fat_Sector (Candidate.G, Active (Candidate), 0);
         Sectors : constant Natural := (Total + EPS - 1) / EPS;
         Chunk : constant Natural := Natural'Max (1, Natural'Min (Ahead_Buffer'Length / SB, Sectors));
         Done_Sectors : Natural := 0;
         Free : Natural := 0;
         Info_Sector_Data : Sector_Buffer;
         Hints : Info_Hints;
         Flags : Unsigned_32;
      begin
         while Done_Sectors < Sectors loop
            declare
               N : constant Natural := Natural'Min (Chunk, Sectors - Done_Sectors);
               First_Entry : constant Natural := Done_Sectors * EPS;
               Count : constant Natural := Natural'Min (N * EPS, Total - First_Entry);
            begin
               Transfer (Candidate, False, First_Sector + Sector_Number (Done_Sectors), N,
                         Ahead_Buffer'Address, OK);
               if not OK then
                  Result := Device_Error;
                  return;
               end if;
               for K in 0 .. N - 1 loop
                  declare
                     Base : constant Natural := First_Entry + K * EPS;
                     Low : constant Natural := Natural'Max (Base, 2);
                     High : constant Natural := Natural'Min (Base + EPS, First_Entry + Count);
                     Sector : constant Byte_Array (0 .. SB - 1) := Ahead_Buffer (K * SB .. K * SB + SB - 1);
                  begin
                     if High > Low then
                        Free := Free + Free_Entries (Sector, Low - Base, High - Low);
                     end if;
                  end;
               end loop;
               Done_Sectors := Done_Sectors + N;
            end;
         end loop;
         Hints := (others => <>);
         if Candidate.G.Info_Sector /= 0 then
            Read_Sector (Candidate, Candidate.G.Info_Sector, Info_Sector_Data, OK);
            if not OK then
               Result := Device_Error;
               return;
            end if;
            Hints := Decode_Info (Info_Sector_Data (0 .. Boot_Sector_Bytes - 1), Candidate.G);
            if Hints.Next_Known and then Hints.Next > Last_Cluster (Candidate.G) then
               Hints.Next_Known := False;
            end if;
         end if;
         Candidate.Space := Opening (Candidate.G.Clusters, Cluster_Count (Free), Hints);
         --  Clean, error-free and on a writable device: read-write.
         Get_Entry (Candidate, Volume_Flags_Cluster, Flags, OK);
         if not OK then
            Result := Device_Error;
            return;
         end if;
         Candidate.Writable := Volume_Clean (Flags) and then not Boot_Dirty (Boot)
           and then not Is_Read_Only (Description);
         if not Candidate.Writable and then not Is_Read_Only (Description) then
            debugPrint ("FAT32: volume not cleanly unmounted; mounted read-only (check it first)"
                        & ASCII.LF);
         end if;
      end;
      --  The root directory's chain must end well.
      declare
         Length : Cluster_Count;
         Tail : Cluster_Number;
         S : Status;
      begin
         Measure (Candidate, Candidate.G.Root, Length, Tail, S);
         if S /= Done then
            Result := (if S = Device_Error then Device_Error else Invalid_Filesystem);
            debugPrint ("FAT32: root directory chain is broken" & ASCII.LF);
            return;
         end if;
      end;
      Candidate.Objects := new Object_Store;
      Candidate.Objects.Items := new Object_Array (1 .. First_Objects);
      Rehash (Candidate.Objects.all, 2 * First_Objects);
      Candidate.Objects.Count := 1;
      Candidate.Objects.Items (1) := (Position => 0, Parent => Root_Number, First => Candidate.G.Root,
                                      Directory => True, Live => True, others => <>);
      Last_Read := (others => <>);
      V := Candidate;
      Result := Admitted;
   end Mount;

end Fat32;
