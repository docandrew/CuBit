pragma Ada_2022;
package body Firmware_Snapshot_ABI with SPARK_Mode is

   function To_Outcome (Word : Unsigned_64) return Outcome is
   begin
      for Item in Outcome loop
         if Word = Outcome_Word (Item) then
            return Item;
         end if;
         pragma Loop_Invariant (for all Seen in Outcome'First .. Item => Word /= Outcome_Word (Seen));
      end loop;
      return Malformed;
   end To_Outcome;

   procedure Decode (Words : Request_Words; Item : out Request; Valid : out Boolean) is
   begin
      Item := (others => <>);
      Valid := Well_Formed (Words);
      if Valid then
         Item := Decoded (Words);
         pragma Assert (Operation'Enum_Rep (Item.Op) = Words.Op);
         pragma Assert (Unsigned_64 (Item.Table) = Words.Table);
         pragma Assert (Unsigned_64 (Item.Capacity) = Words.Capacity);
         pragma Assert (Encode (Item) = Words);
      end if;
   end Decode;

   procedure Lemma_Encode_Decodes (Item : Request) is
      W : constant Request_Words := Encode (Item);
   begin
      case Item.Op is
         when Catalog_Info    => pragma Assert (W.Op = 1);
         when Describe_Table  => pragma Assert (W.Op = 2);
         when Copy_Table_Part => pragma Assert (W.Op = 3);
      end case;
      pragma Assert (Known_Operation (W.Op) and then Operation_Of (W.Op) = Item.Op);
      pragma Assert (Well_Formed (W));
   end Lemma_Encode_Decodes;

   function Plan_Copy
     (Extent, Offset : Unsigned_64; Capacity : Capacity_Bytes) return Copy_Plan is
   begin
      if Offset > Extent then
         return (Beyond_End, 0);
      elsif Capacity = 0 and then Offset < Extent then
         return (Empty_Before_End, 0);
      elsif Extent - Offset < Unsigned_64 (Capacity) then
         return (Ok, Capacity_Bytes (Extent - Offset));
      else
         return (Ok, Capacity);
      end if;
   end Plan_Copy;

   procedure Decode_Info (Words : Reply; Item : out Catalog; Valid : out Boolean) is
   begin
      Item := (others => <>);
      Valid := Info_Well_Formed (Words);
      if Valid then
         Item := (Ready => Words (Info_Ready) = 1,
                  Table_Count => Unsigned_32 (Words (Info_Count)),
                  Largest => Words (Info_Largest), Total => Words (Info_Total));
      end if;
   end Decode_Info;

   procedure Lemma_Signature (Word : Unsigned_64)
   with Ghost, Global => null,
        Pre  => Word <= Unsigned_64 (Unsigned_32'Last),
        Post => Signature_Word (Signature_Of (Word)) = Word;
   procedure Lemma_Signature (Word : Unsigned_64) is null;

   procedure Decode_Description
     (Words : Reply; Table : Table_Number; Item : out Description; Valid : out Boolean) is
   begin
      Item := (others => <>);
      Valid := Description_Well_Formed (Words, Table);
      if Valid then
         Lemma_Signature (Words (Describe_Signature));
         Item := (Table => Table,
                  Signature => Signature_Of (Words (Describe_Signature)),
                  Revision => Unsigned_8 (Words (Describe_Revision)),
                  Extent => Words (Describe_Extent));
      end if;
   end Decode_Description;

   procedure Decode_Copy
     (Words : Reply; Capacity : Capacity_Bytes; Copied : out Capacity_Bytes; Valid : out Boolean) is
   begin
      Copied := 0;
      Valid := Copy_Well_Formed (Words, Capacity);
      if Valid then
         Copied := Capacity_Bytes (Words (Copy_Copied));
      end if;
   end Decode_Copy;

   procedure Lemma_Info_Round_Trip (Item : Catalog) is null;
   procedure Lemma_Description_Round_Trip (Item : Description) is
      W : constant Unsigned_64 := Signature_Word (Item.Signature);
   begin
      pragma Assert (W <= Unsigned_64 (Unsigned_32'Last));
      pragma Assert ((W and Byte_Mask) = Unsigned_64 (Item.Signature (1)));
      pragma Assert ((Shift_Right (W, Byte_Bits) and Byte_Mask) = Unsigned_64 (Item.Signature (2)));
      pragma Assert ((Shift_Right (W, 2 * Byte_Bits) and Byte_Mask) = Unsigned_64 (Item.Signature (3)));
      pragma Assert ((Shift_Right (W, 3 * Byte_Bits) and Byte_Mask) = Unsigned_64 (Item.Signature (4)));
   end Lemma_Description_Round_Trip;
   procedure Lemma_Copy_Round_Trip (Copied, Capacity : Capacity_Bytes) is null;

end Firmware_Snapshot_ABI;
