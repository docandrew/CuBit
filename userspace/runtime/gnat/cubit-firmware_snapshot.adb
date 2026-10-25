pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Handles.Startup;
with CuBit.Handles.Firmware_Snapshots;
with CuBit.Startup_Tables;

package body CuBit.Firmware_Snapshot with SPARK_Mode is

   package ABI renames Firmware_Snapshot_ABI;

   function Named (Label : String) return Authority is
     ((Handle => CuBit.Handles.Startup.Firmware_Snapshot_Named
                   (Label, CuBit.Startup_Tables.Firmware_Snapshot)));

   function To_Outcome (Item : ABI.Outcome) return Outcome is
     (case Item is
         when ABI.Ok               => Ok,
         when ABI.Not_Ready        => Not_Ready,
         when ABI.Unknown_Table    => Unknown_Table,
         when ABI.Beyond_End       => Beyond_End,
         when ABI.Empty_Before_End => Empty_Before_End,
         when ABI.Copy_Failed      => Copy_Failed,
         when ABI.Not_Authorized   => Not_Authorized,
         when ABI.Malformed        => Malformed);

   --  The call itself: the kernel writes Reply (only for Ok) and, for a
   --  copy, Into (Destination) through its own checks of this process's
   --  pages.
   procedure Kernel_Call
     (A : Authority; Words : ABI.Request_Words; Reply : out ABI.Reply; Result : out ABI.Outcome)
   with Global => null, Pre => Words.Destination = 0;

   procedure Kernel_Call
     (A : Authority; Words : ABI.Request_Words; Reply : out ABI.Reply; Result : out ABI.Outcome)
   with SPARK_Mode => Off
   is
      Answer : aliased ABI.Reply := ABI.Empty_Reply;
   begin
      Result := ABI.To_Outcome
        (CuBit.Handles.Firmware_Snapshots.Call (A.Handle, Words, Answer'Address));
      Reply := Answer;
   end Kernel_Call;

   procedure Kernel_Copy
     (A : Authority; Table, Offset : Unsigned_64; Into : in out Byte_Array;
      Reply : out ABI.Reply; Result : out ABI.Outcome)
   with Global => null, Pre => Into'Length <= Chunk_Max;

   procedure Kernel_Copy
     (A : Authority; Table, Offset : Unsigned_64; Into : in out Byte_Array;
      Reply : out ABI.Reply; Result : out ABI.Outcome)
   with SPARK_Mode => Off
   is
      use System.Storage_Elements;
      Answer : aliased ABI.Reply := ABI.Empty_Reply;
   begin
      Result := ABI.To_Outcome
        (CuBit.Handles.Firmware_Snapshots.Call
           (A.Handle,
            (Op          => ABI.Operation'Enum_Rep (ABI.Copy_Table_Part),
             Table       => Table,
             Offset      => Offset,
             Destination => (if Into'Length = 0 then 0
                             else Unsigned_64 (To_Integer (Into'Address))),
             Capacity    => Unsigned_64 (Into'Length)),
            Answer'Address));
      Reply := Answer;
   end Kernel_Copy;

   procedure Info (A : Authority; Result : out Catalog_Info; Status : out Outcome) is
      Reply : ABI.Reply;
      Answer : ABI.Outcome;
      Item : ABI.Catalog;
      Valid : Boolean;
   begin
      Result := (others => <>);
      Kernel_Call (A, (Op => ABI.Operation'Enum_Rep (ABI.Catalog_Info), others => 0),
                   Reply, Answer);
      Status := To_Outcome (Answer);
      if Status /= Ok then
         return;
      end if;
      ABI.Decode_Info (Reply, Item, Valid);
      if not Valid then
         Status := Malformed;
         return;
      end if;
      Result := (Version       => ABI.Interface_Version,
                 Table_Count   => Item.Table_Count,
                 Ready         => Item.Ready,
                 Largest_Table => Byte_Count (Item.Largest),
                 Total_Bytes   => Byte_Count (Item.Total));
   end Info;

   procedure Describe
     (A : Authority; Table : Table_ID; Result : out Table_Description; Status : out Outcome)
   is
      Reply : ABI.Reply;
      Answer : ABI.Outcome;
      Item : ABI.Description;
      Valid : Boolean;
   begin
      Result := (others => <>);
      Kernel_Call (A, (Op => ABI.Operation'Enum_Rep (ABI.Describe_Table),
                       Table => Unsigned_64 (Table), others => 0), Reply, Answer);
      Status := To_Outcome (Answer);
      if Status /= Ok then
         return;
      end if;
      ABI.Decode_Description (Reply, ABI.Table_Number (Table), Item, Valid);
      if not Valid then
         Status := Malformed;
         return;
      end if;
      Result := (Signature => [for I in Signature_Index => Item.Signature (I)],
                 Revision  => Item.Revision,
                 Extent    => Byte_Count (Item.Extent));
   end Describe;

   procedure Copy_Part
     (A : Authority; Table : Table_ID; Offset : Byte_Count;
      Into : out Byte_Array; Copied : out Byte_Count; Status : out Outcome)
   is
      Reply : ABI.Reply;
      Answer : ABI.Outcome;
      Count : ABI.Capacity_Bytes;
      Valid : Boolean;
   begin
      Into := [others => 0];
      Copied := 0;
      Kernel_Copy (A, Unsigned_64 (Table), Unsigned_64 (Offset), Into, Reply, Answer);
      Status := To_Outcome (Answer);
      if Status = Ok then
         ABI.Decode_Copy (Reply, Unsigned_64 (Into'Length), Count, Valid);
         if Valid then
            Copied := Byte_Count (Count);
            return;
         end if;
         Status := Malformed;
      end if;
      --  A refusal or a failed copy: nothing of it is kept.
      Into := [others => 0];
   end Copy_Part;

end CuBit.Firmware_Snapshot;
