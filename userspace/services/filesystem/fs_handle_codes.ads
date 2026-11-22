------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The filesystem service's handle table codes, proved (SPARK): the opaque
--  wire handle (slot and generation), a handle's dirty-entry tag, object
--  identities, the inode-version and shared-object hashes, and directory
--  resume tokens. Slots, generations, wire handles, tags and tokens are
--  distinct types, so one cannot be passed for another unconverted.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Volume_List;

package Fs_Handle_Codes with SPARK_Mode is

   --  Handle table: slots below Extended_Handles also carry directory and
   --  optical-file state; regular files use the rest.
   Maximum_Handles  : constant := 2_048;
   Extended_Handles : constant := 64;
   type Handle_Slot is range 0 .. Maximum_Handles - 1;
   subtype Extended_Slot is Handle_Slot range 0 .. Extended_Handles - 1;
   subtype Plain_Slot is Handle_Slot range Extended_Handles .. Handle_Slot'Last;

   --  How often a slot was reused. Never wraps: a slot whose generation
   --  reached Generation'Last is retired instead.
   type Generation is new Unsigned_32;
   First_Generation : constant Generation := 1;

   function Retires (Current : Generation) return Boolean is
     (Current = Generation'Last);
   function Next (Current : Generation) return Generation is (Current + 1)
     with Pre => not Retires (Current);

   ---------------------------------------------------------------------------
   --  Wire handles: the one-based slot in the low word, the generation in
   --  the high word. Zero is never a handle.
   ---------------------------------------------------------------------------
   type Handle_Word is new Unsigned_64;
   No_Handle : constant Handle_Word := 0;
   Slot_Code_Bits : constant := 32;
   Slot_Code_Mask : constant Handle_Word := 2 ** Slot_Code_Bits - 1;

   function Slot_Code (Word : Handle_Word) return Handle_Word is
     (Word and Slot_Code_Mask);
   function Generation_Of (Word : Handle_Word) return Generation is
     (Generation (Shift_Right (Word, Slot_Code_Bits)));

   function Valid_Slot_Code (Word : Handle_Word) return Boolean is
     (Slot_Code (Word) in 1 .. Maximum_Handles);

   function Encode (Slot : Handle_Slot; Current : Generation) return Handle_Word is
     (Shift_Left (Handle_Word (Current), Slot_Code_Bits) or (Handle_Word (Slot) + 1))
     with Post => Encode'Result /= No_Handle and then
                  Valid_Slot_Code (Encode'Result) and then
                  Generation_Of (Encode'Result) = Current;

   function Slot_Of (Word : Handle_Word) return Handle_Slot is
     (Handle_Slot (Slot_Code (Word) - 1))
     with Pre => Valid_Slot_Code (Word);

   --  Round trip, both ways.
   procedure Lemma_Decode_Encode (Slot : Handle_Slot; Current : Generation)
     with Ghost, Global => null,
          Post => Slot_Of (Encode (Slot, Current)) = Slot;
   procedure Lemma_Encode_Decode (Word : Handle_Word)
     with Ghost, Global => null,
          Pre  => Valid_Slot_Code (Word),
          Post => Encode (Slot_Of (Word), Generation_Of (Word)) = Word;

   ---------------------------------------------------------------------------
   --  Dirty-entry tags (CuBit.Filesystem_Queues.Dirty_Slot_At): the slot in
   --  the low Tag_Slot_Bits, the low Tag_Generation_Bits of its generation
   --  above. Every slot fits (the service checks the bit widths against the
   --  queue layout's).
   ---------------------------------------------------------------------------
   Tag_Slot_Bits       : constant := 11;
   Tag_Generation_Bits : constant := 21;
   pragma Compile_Time_Error
     (2 ** Tag_Slot_Bits < Maximum_Handles, "a tag must hold every slot");
   type Dirty_Tag is new Unsigned_32;
   Tag_Slot_Mask       : constant Dirty_Tag := 2 ** Tag_Slot_Bits - 1;
   Tag_Generation_Mask : constant Generation := 2 ** Tag_Generation_Bits - 1;

   function Tag_Of (Slot : Handle_Slot; Current : Generation) return Dirty_Tag is
     (Shift_Left (Dirty_Tag (Current and Tag_Generation_Mask), Tag_Slot_Bits) or
      Dirty_Tag (Slot));

   function Tag_Slot (Tag : Dirty_Tag) return Handle_Slot is
     (Handle_Slot (Tag and Tag_Slot_Mask));

   procedure Lemma_Tag_Slot (Slot : Handle_Slot; Current : Generation)
     with Ghost, Global => null,
          Post => Tag_Slot (Tag_Of (Slot, Current)) = Slot;

   ---------------------------------------------------------------------------
   --  Object identities: the volume in the high word, the inode number in
   --  the low (delegations, change events, directory pages, inspections).
   ---------------------------------------------------------------------------
   Volume_Shift : constant := 32;

   function Object_Id
     (Volume : Volume_List.Volume_Index; Inode : Unsigned_32) return Unsigned_64 is
     (Shift_Left (Unsigned_64 (Volume), Volume_Shift) or Unsigned_64 (Inode))
     with Post => Unsigned_32 (Object_Id'Result and 16#FFFF_FFFF#) = Inode and then
                  Shift_Right (Object_Id'Result, Volume_Shift) = Unsigned_64 (Volume);

   ---------------------------------------------------------------------------
   --  Hashes. Fibonacci hashing: the top bits of a product with 2**32 over
   --  the golden ratio.
   ---------------------------------------------------------------------------
   Golden_Ratio_32 : constant := 16#9E37_79B9#;
   --  The second multiplier of the shared-object hash (MurmurHash3's).
   Mix_Multiplier : constant := 16#85EB_CA6B#;

   --  The open-object table's hash of an inode identity.
   function Identity_Hash
     (Volume : Volume_List.Volume_Index; Inode : Unsigned_32) return Unsigned_32 is
     (Inode * Golden_Ratio_32 xor Unsigned_32 (Volume) * Mix_Multiplier);

   --  The inode-version table: entries are found from a home entry by
   --  probing a short window after it.
   Version_Entries : constant := 1024;
   Version_Probes  : constant := 8;
   Version_Hash_Shift : constant := 32 - 10;
   pragma Compile_Time_Error
     (2 ** (32 - Version_Hash_Shift) /= Version_Entries,
      "the version hash must cover the table exactly");
   type Version_Slot is range 0 .. Version_Entries - 1;
   type Version_Probe is range 0 .. Version_Probes - 1;

   function Version_Home
     (Volume : Volume_List.Volume_Index; Inode : Unsigned_32) return Version_Slot is
     (Version_Slot (Shift_Right ((Inode xor Unsigned_32 (Volume)) * Golden_Ratio_32,
                                 Version_Hash_Shift)));

   function Probe (Home : Version_Slot; Step : Version_Probe) return Version_Slot is
     (Version_Slot ((Natural (Home) + Natural (Step)) mod Version_Entries));

   function Next_Probe (Step : Version_Probe) return Version_Probe is
     (if Step = Version_Probe'Last then Version_Probe'First else Step + 1);

   ---------------------------------------------------------------------------
   --  Directory resume tokens (Directory.Page.V2): the next record's byte
   --  position in the low Token_Position_Bits, and above it a check naming
   --  the directory, so a token handed back for another directory is
   --  refused. The position itself is checked by Ext2 whatever its origin.
   ---------------------------------------------------------------------------
   type Directory_Check is new Unsigned_32;
   --  Spreads the inode, volume and generation over the word: a mix-up
   --  detector, not authority (the handle is that).
   Directory_Golden : constant := 16#9E37_79B1#;
   Directory_Volume_Shift : constant := 24;

   --  Generation_Mix: the directory inode's generation, or 0 when the
   --  format has none.
   function Check_Of
     (Inode : Unsigned_32; Volume : Volume_List.Volume_Index;
      Generation_Mix : Unsigned_32) return Directory_Check
     with Post => Check_Of'Result /= 0;

   Token_Position_Bits : constant := 32;
   Token_Position_Mask : constant Unsigned_64 := 2 ** Token_Position_Bits - 1;
   type Resume_Token is new Unsigned_64;
   No_Token : constant Resume_Token := 0;

   function Token_Of (Check : Directory_Check; Position : Unsigned_64) return Resume_Token is
     (Shift_Left (Resume_Token (Check), Token_Position_Bits) or
      Resume_Token (Position and Token_Position_Mask));

   function Check_In (Token : Resume_Token) return Directory_Check is
     (Directory_Check (Shift_Right (Token, Token_Position_Bits)));

   function Position_In (Token : Resume_Token) return Unsigned_64 is
     (Unsigned_64 (Token) and Token_Position_Mask)
     with Post => Position_In'Result <= Token_Position_Mask;

   procedure Lemma_Token (Check : Directory_Check; Position : Unsigned_64)
     with Ghost, Global => null,
          Pre  => Check /= 0 and then Position <= Token_Position_Mask,
          Post => Check_In (Token_Of (Check, Position)) = Check and then
                  Position_In (Token_Of (Check, Position)) = Position and then
                  Token_Of (Check, Position) /= No_Token;

end Fs_Handle_Codes;
