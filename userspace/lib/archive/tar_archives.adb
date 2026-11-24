------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The tar walker (see the spec). Field offsets and widths are the ustar
--  header block's (POSIX.1-2017, pax, "ustar Interchange Format", Table
--  "ustar Header Block").
------------------------------------------------------------------------------
pragma Ada_2022;

package body Tar_Archives with SPARK_Mode is
   use type Interfaces.Unsigned_8;
   subtype U64 is Byte_Count;

   --  ustar header fields: offset and width in bytes.
   NAME_AT : constant := 0;
   NAME_WIDTH : constant := 100;
   SIZE_AT : constant := 124;
   SIZE_WIDTH : constant := 12;
   CHECKSUM_AT : constant := 148;
   CHECKSUM_WIDTH : constant := 8;
   TYPEFLAG_AT : constant := 156;
   MAGIC_AT : constant := 257;
   MAGIC_WIDTH : constant := 8;   --  magic (6) and version (2)
   PREFIX_AT : constant := 345;
   PREFIX_WIDTH : constant := 155;

   NUL : constant Byte := 0;
   SPACE : constant Byte := Character'Pos (' ');
   --  GNU tar's base-256 numbers set the first byte's high bit.
   BASE_256_FLAG : constant Byte := 16#80#;

   type Magic_Bytes is array (0 .. MAGIC_WIDTH - 1) of Byte;
   --  POSIX: "ustar" NUL, version "00". Old GNU: "ustar  " NUL.
   POSIX_MAGIC : constant Magic_Bytes :=
     [Character'Pos ('u'), Character'Pos ('s'), Character'Pos ('t'), Character'Pos ('a'),
      Character'Pos ('r'), 0, Character'Pos ('0'), Character'Pos ('0')];
   GNU_MAGIC : constant Magic_Bytes :=
     [Character'Pos ('u'), Character'Pos ('s'), Character'Pos ('t'), Character'Pos ('a'),
      Character'Pos ('r'), SPACE, SPACE, 0];

   --  Typeflags (POSIX.1-2017 ustar; GNU tar's 'L' and 'K').
   function Kind_Of (Flag : Byte) return Entry_Kind is
     (case Flag is
         when 0 | Character'Pos ('0') | Character'Pos ('7') => Regular_File,
         when Character'Pos ('1') => Hard_Link,
         when Character'Pos ('2') => Symbolic_Link,
         when Character'Pos ('3') => Character_Device,
         when Character'Pos ('4') => Block_Device,
         when Character'Pos ('5') => Directory,
         when Character'Pos ('6') => FIFO,
         when Character'Pos ('x') => Pax_Extended,
         when Character'Pos ('g') => Pax_Global,
         when Character'Pos ('L') => GNU_Long_Name,
         when Character'Pos ('K') => GNU_Long_Link,
         when others => Unsupported_Kind);

   function All_Zero (B : Block) return Boolean is (for all X of B => X = 0);

   function Magic_Is (B : Block; M : Magic_Bytes) return Boolean is
     (for all K in M'Range => B (MAGIC_AT + K) = M (K));

   procedure Read_Octal
     (B : Block; First : Block_Offset; Width : Positive; Value : out Field_Value; Why : out Refusal)
   is
      Last : constant Block_Offset := First + Width - 1;
      I : Natural := First;
      V : Field_Value := 0;
      Digits_Read : Natural := 0;
   begin
      Value := 0;
      if B (First) >= BASE_256_FLAG then
         Why := Base_256_Number;
         return;
      end if;
      --  Leading spaces, then octal digits, then only NUL or space.
      while I <= Last and then B (I) = SPACE loop
         pragma Loop_Invariant (I in First .. Last);
         pragma Loop_Variant (Increases => I);
         I := I + 1;
      end loop;
      while I <= Last and then B (I) in Character'Pos ('0') .. Character'Pos ('7') loop
         pragma Loop_Invariant (I in First .. Last and then Digits_Read <= I - First);
         pragma Loop_Variant (Increases => I);
         if V > (MAXIMUM_FIELD_VALUE - 7) / 8 then
            --  Not reachable within twelve digits; it bounds V for the prover.
            Why := Bad_Number;
            return;
         end if;
         V := V * 8 + U64 (B (I) - Character'Pos ('0'));
         Digits_Read := Digits_Read + 1;
         I := I + 1;
      end loop;
      if Digits_Read = 0 then
         Why := Bad_Number;
         return;
      end if;
      while I <= Last loop
         pragma Loop_Invariant (I in First .. Last);
         pragma Loop_Variant (Increases => I);
         if B (I) /= NUL and then B (I) /= SPACE then
            Why := Bad_Number;
            return;
         end if;
         I := I + 1;
      end loop;
      Value := V;
      Why := No_Refusal;
   end Read_Octal;

   --  The checksum: the unsigned sum of the header's bytes, its own field
   --  counted as eight spaces.
   function Checksum_Matches (B : Block) return Boolean is
      Sum : Natural := 0;
      Stored : Field_Value;
      Why : Refusal;
   begin
      for I in Block_Offset loop
         pragma Loop_Invariant (Sum <= I * Natural (Byte'Last));
         Sum := Sum +
           (if I in CHECKSUM_AT .. CHECKSUM_AT + CHECKSUM_WIDTH - 1 then Natural (SPACE) else Natural (B (I)));
      end loop;
      Read_Octal (B, CHECKSUM_AT, CHECKSUM_WIDTH, Stored, Why);
      return Why = No_Refusal and then Stored = U64 (Sum);
   end Checksum_Matches;

   procedure Refuse (W : in out Walker; Why : Refusal; E : out Event)
   with Pre => Why /= No_Refusal and then Valid (W) and then not Failed (W),
        Post => Valid (W) and then Failed (W) and then W.Why = Why and then E.Kind = Refused
          and then E.Why = Why and then W.Ceiling = W.Ceiling'Old and then W.Entries = W.Entries'Old
          and then W.Bytes = W.Bytes'Old and then W.Current = W.Current'Old
          and then W.Current_Length = W.Current_Length'Old
   is
   begin
      W.Phase := Failed_Phase;
      W.Why := Why;
      W.Remaining := 0;
      E := (Kind => Refused, Why => Why, others => <>);
   end Refuse;

   procedure Start (W : out Walker; Ceiling : Limits; Specials : Specials_Policy) is
   begin
      W := (Ceiling => Ceiling, Specials => Specials, others => <>);
   end Start;

   --  A header's name: the prefix (POSIX magic only), '/', and the name,
   --  each up to its first NUL.
   procedure Append_Field
     (B : Block; First : Block_Offset; Width : Positive; Into : in out Path_Buffer; Length : in out Path_Length)
   with Pre => Width <= PREFIX_WIDTH and then First <= BLOCK_BYTES - Width
                 and then Length <= MAXIMUM_PATH_BYTES - Width,
        Post => Length in Length'Old .. Length'Old + Width
   is
   begin
      for K in 0 .. Width - 1 loop
         pragma Loop_Invariant (Length in Length'Loop_Entry .. Length'Loop_Entry + K);
         exit when B (First + K) = NUL;
         Length := Length + 1;
         Into (Length) := Character'Val (B (First + K));
      end loop;
   end Append_Field;

   --  Raw (1 .. Raw_Length) as the entry's path: leading "./" dropped, and a
   --  directory's one trailing '/'. Empty is the archive's own root ("."):
   --  Empty True and nothing set. Otherwise Current holds it when Confined;
   --  Why says why not.
   procedure Set_Path
     (W : in out Walker; Raw : Path_Buffer; Raw_Length : Path_Length; Is_Directory : Boolean;
      Empty : out Boolean; Why : out Refusal)
   with Post => W.Phase = W.Phase'Old and then W.Why = W.Why'Old and then W.Ceiling = W.Ceiling'Old
                  and then W.Entries = W.Entries'Old and then W.Bytes = W.Bytes'Old
                  and then W.Remaining = W.Remaining'Old and then W.Zero_Blocks = W.Zero_Blocks'Old
                  and then W.Meta_Length = W.Meta_Length'Old and then W.Specials = W.Specials'Old
                  and then (if Why = No_Refusal and then not Empty then Has_Path (W))
   is
      F : Positive := 1;
      L : Natural := Raw_Length;
   begin
      Empty := False;
      Why := No_Refusal;
      while F < L and then Raw (F) = '.' and then Raw (F + 1) = '/' loop
         pragma Loop_Invariant (F in 1 .. L - 1);
         F := F + 2;
      end loop;
      if F = L and then Raw (F) = '.' then
         F := F + 1;
      end if;
      if Is_Directory and then L >= F and then Raw (L) = '/' then
         L := L - 1;
      end if;
      if L < F then
         Empty := True;
         return;
      end if;
      W.Current_Length := L - F + 1;
      W.Current (1 .. W.Current_Length) := Raw (F .. L);
      if not Confined (W.Current (1 .. W.Current_Length)) then
         W.Current_Length := 0;
         W.Rejected_Length := Raw_Length;
         W.Rejected (1 .. Raw_Length) := Raw (1 .. Raw_Length);
         Why := Unsafe_Path;
      end if;
   end Set_Path;

   --  One pax extended header's records, "LENGTH KEY=VALUE\n" (LENGTH in
   --  decimal counts the whole record): "path" names the next entry; a
   --  "size" override or GNU sparse map is refused (the data would not be
   --  what the header says); other keys (times, owners, linkpath) change
   --  nothing here.
   procedure Parse_Pax (W : in out Walker; Why : out Refusal)
   with Post => W.Phase = W.Phase'Old and then W.Why = W.Why'Old and then W.Ceiling = W.Ceiling'Old
                  and then W.Entries = W.Entries'Old and then W.Bytes = W.Bytes'Old
                  and then W.Remaining = W.Remaining'Old and then W.Zero_Blocks = W.Zero_Blocks'Old
                  and then W.Meta_Length = W.Meta_Length'Old and then W.Specials = W.Specials'Old
                  and then W.Current = W.Current'Old and then W.Current_Length = W.Current_Length'Old
   is
      Length : constant Meta_Length := W.Meta_Length;
      P : Positive := 1;
      SPARSE_PREFIX : constant String := "GNU.sparse.";
   begin
      Why := No_Refusal;
      while P <= Length loop
         pragma Loop_Invariant (P in 1 .. Length);
         pragma Loop_Invariant (Why = No_Refusal);
         pragma Loop_Variant (Increases => P);
         declare
            Q : Positive := P;
            Record_Length : Natural := 0;
            Record_Last, Equals : Positive;
            Found : Boolean := False;
         begin
            while Q <= Length and then W.Meta (Q) in '0' .. '9' loop
               pragma Loop_Invariant (Q in P .. Length and then Record_Length <= MAXIMUM_META_BYTES);
               Record_Length := Record_Length * 10 + (Character'Pos (W.Meta (Q)) - Character'Pos ('0'));
               if Record_Length > MAXIMUM_META_BYTES then
                  Why := Bad_Pax_Record;
                  return;
               end if;
               Q := Q + 1;
            end loop;
            --  Digits, a space, a key of one byte or more, '=', '\n'.
            if Q = P or else Q > Length or else W.Meta (Q) /= ' ' or else Record_Length < Q - P + 4
              or else Record_Length > Length - P + 1
            then
               Why := Bad_Pax_Record;
               return;
            end if;
            Record_Last := P + Record_Length - 1;
            if W.Meta (Record_Last) /= ASCII.LF then
               Why := Bad_Pax_Record;
               return;
            end if;
            Equals := Q + 1;
            for K in Q + 2 .. Record_Last - 1 loop
               pragma Loop_Invariant (not Found);
               if W.Meta (K) = '=' then
                  Equals := K;
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               Why := Bad_Pax_Record;
               return;
            end if;
            declare
               Key : constant String := W.Meta (Q + 1 .. Equals - 1);
               Value_Length : constant Natural := Record_Last - 1 - Equals;
            begin
               if Key = "path" then
                  if Value_Length > MAXIMUM_PATH_BYTES then
                     Why := Path_Too_Long;
                     return;
                  end if;
                  --  An empty value removes an earlier path.
                  W.Pending_Length := Value_Length;
                  W.Pending (1 .. Value_Length) := W.Meta (Equals + 1 .. Record_Last - 1);
               elsif Key = "size"
                 or else (Key'Length >= SPARSE_PREFIX'Length
                          and then Key (Key'First .. Key'First + SPARSE_PREFIX'Length - 1) = SPARSE_PREFIX)
               then
                  Why := Unsupported_Pax;
                  return;
               end if;
            end;
            exit when Record_Last = Length;
            P := Record_Last + 1;
         end;
      end loop;
   end Parse_Pax;

   --  A GNU long name: its text up to the first NUL names the next entry.
   procedure Take_Long_Name (W : in out Walker; Why : out Refusal)
   with Post => W.Phase = W.Phase'Old and then W.Why = W.Why'Old and then W.Ceiling = W.Ceiling'Old
                  and then W.Entries = W.Entries'Old and then W.Bytes = W.Bytes'Old
                  and then W.Remaining = W.Remaining'Old and then W.Zero_Blocks = W.Zero_Blocks'Old
                  and then W.Meta_Length = W.Meta_Length'Old and then W.Specials = W.Specials'Old
                  and then W.Current = W.Current'Old and then W.Current_Length = W.Current_Length'Old
   is
      Length : Meta_Length := 0;
   begin
      for K in 1 .. W.Meta_Length loop
         pragma Loop_Invariant (Length = K - 1);
         exit when W.Meta (K) = ASCII.NUL;
         Length := K;
      end loop;
      if Length = 0 then
         Why := Empty_Name;
      elsif Length > MAXIMUM_PATH_BYTES then
         Why := Path_Too_Long;
      else
         W.Pending_Length := Length;
         W.Pending (1 .. Length) := W.Meta (1 .. Length);
         Why := No_Refusal;
      end if;
   end Take_Long_Name;

   --  A non-zero block where a header belongs.
   procedure Header (W : in out Walker; B : Block; E : out Event)
   with Pre => Valid (W) and then W.Phase = Expect_Header,
        Post => Valid (W) and then W.Ceiling = W.Ceiling'Old
          and then W.Phase in Expect_Header | In_Data | In_Skip | In_Meta | Failed_Phase
          and then (E.Kind = Refused) = Failed (W)
          and then (if E.Kind = Refused then E.Why = W.Why and then E.Why /= No_Refusal)
          and then E.Kind in Nothing | Directory_Entry | File_Start | Skipped_Special | Refused
          and then (if E.Kind in Directory_Entry | File_Start | Skipped_Special then Has_Path (W))
          and then (if E.Kind = File_Start then W.Remaining = E.Size)
   is
      Size : Field_Value;
      Why : Refusal;
      Kind : Entry_Kind;
      Posix : Boolean;
      Raw : Path_Buffer := [others => ' '];
      Raw_Length : Path_Length := 0;
      Empty : Boolean;
   begin
      E := (others => <>);
      if W.Zero_Blocks = 1 then
         Refuse (W, Lone_Zero_Block, E);
         return;
      elsif not Checksum_Matches (B) then
         Refuse (W, Bad_Checksum, E);
         return;
      end if;
      Posix := Magic_Is (B, POSIX_MAGIC);
      if not Posix and then not Magic_Is (B, GNU_MAGIC) then
         Refuse (W, Not_Ustar, E);
         return;
      end if;
      Read_Octal (B, SIZE_AT, SIZE_WIDTH, Size, Why);
      if Why /= No_Refusal then
         Refuse (W, Why, E);
         return;
      end if;
      Kind := Kind_Of (B (TYPEFLAG_AT));

      case Kind is
         when Unsupported_Kind =>
            Refuse (W, Unsupported_Entry, E);
            return;
         when Pax_Extended | GNU_Long_Name =>
            if Size > MAXIMUM_META_BYTES then
               Refuse (W, Meta_Too_Large, E);
            elsif Size = 0 and then Kind = GNU_Long_Name then
               Refuse (W, Empty_Name, E);
            elsif Size > 0 then
               W.Phase := In_Meta;
               W.Meta_Kind := (if Kind = Pax_Extended then Pax_Meta else Long_Name_Meta);
               W.Meta_Length := 0;
               W.Remaining := Size;
            end if;
            return;
         when Pax_Global | GNU_Long_Link =>
            --  Global attributes and link targets change nothing here.
            if Size > 0 then
               W.Phase := In_Skip;
               W.Remaining := Size;
            end if;
            return;
         when Regular_File | Directory | Special_Kind =>
            null;
      end case;

      --  The entry's raw name: a pending long name or pax path, else the
      --  header's own.
      if W.Pending_Length > 0 then
         Raw_Length := W.Pending_Length;
         Raw (1 .. Raw_Length) := W.Pending (1 .. Raw_Length);
         W.Pending_Length := 0;
      else
         if Posix and then B (PREFIX_AT) /= NUL then
            Append_Field (B, PREFIX_AT, PREFIX_WIDTH, Raw, Raw_Length);
            Raw_Length := Raw_Length + 1;
            Raw (Raw_Length) := '/';
         end if;
         Append_Field (B, NAME_AT, NAME_WIDTH, Raw, Raw_Length);
      end if;
      if Raw_Length = 0 then
         Refuse (W, Empty_Name, E);
         return;
      end if;
      Set_Path (W, Raw, Raw_Length, Kind = Directory, Empty, Why);
      if Why /= No_Refusal then
         Refuse (W, Why, E);
         return;
      end if;

      case Kind is
         when Directory =>
            if Size /= 0 then
               Refuse (W, Directory_Data, E);
            elsif Empty then
               --  "./": the target itself, which exists already.
               null;
            elsif W.Entries >= W.Ceiling.Maximum_Entries then
               Refuse (W, Too_Many_Entries, E);
            else
               W.Entries := W.Entries + 1;
               E := (Kind => Directory_Entry, others => <>);
            end if;
         when Regular_File =>
            if Empty then
               Refuse (W, Empty_Name, E);
            elsif W.Entries >= W.Ceiling.Maximum_Entries then
               Refuse (W, Too_Many_Entries, E);
            elsif Size > W.Ceiling.Maximum_Bytes - W.Bytes then
               Refuse (W, Too_Many_Bytes, E);
            else
               W.Entries := W.Entries + 1;
               W.Bytes := W.Bytes + Size;
               W.Remaining := Size;
               W.Phase := (if Size > 0 then In_Data else Expect_Header);
               E := (Kind => File_Start, Size => Size, others => <>);
            end if;
         when Special_Kind =>
            if Empty then
               Refuse (W, Empty_Name, E);
            elsif W.Specials = Refuse_Specials then
               Refuse (W, Special_Entry, E);
            else
               W.Remaining := Size;
               W.Phase := (if Size > 0 then In_Skip else Expect_Header);
               E := (Kind => Skipped_Special, others => <>);
            end if;
         when others =>
            null;
      end case;
   end Header;

   procedure Feed (W : in out Walker; B : Block; E : out Event) is
   begin
      E := (others => <>);
      case W.Phase is
         when Failed_Phase =>
            E := (Kind => Refused, Why => W.Why, others => <>);
         when Ended_Phase =>
            --  Padding to the archive's record size.
            null;
         when Expect_Header =>
            if All_Zero (B) then
               if W.Zero_Blocks = 1 then
                  W.Phase := Ended_Phase;
                  W.Zero_Blocks := 0;
                  E := (Kind => Finished, others => <>);
               else
                  W.Zero_Blocks := 1;
               end if;
            else
               Header (W, B, E);
            end if;
         when In_Data | In_Skip | In_Meta =>
            declare
               Before : constant Entry_Bytes := W.Remaining;
               N : Piece_Length;
               Why : Refusal;
            begin
               if Before >= BLOCK_BYTES then
                  N := BLOCK_BYTES;
               else
                  N := Natural (Before);
               end if;
               pragma Assert (N >= 1 and then U64 (N) <= Before);
               W.Remaining := Before - U64 (N);
               if W.Phase = In_Data then
                  E := (Kind => File_Data, Data_Length => N, Last_Piece => W.Remaining = 0, others => <>);
                  if W.Remaining = 0 then
                     W.Phase := Expect_Header;
                  end if;
               elsif W.Phase = In_Skip then
                  if W.Remaining = 0 then
                     W.Phase := Expect_Header;
                  end if;
               else
                  pragma Assert (W.Meta_Length + N <= MAXIMUM_META_BYTES);
                  for K in 0 .. N - 1 loop
                     pragma Loop_Invariant (W.Meta_Length = W.Meta_Length'Loop_Entry + K);
                     W.Meta_Length := W.Meta_Length + 1;
                     W.Meta (W.Meta_Length) := Character'Val (B (K));
                  end loop;
                  pragma Assert (U64 (W.Meta_Length) + W.Remaining <= MAXIMUM_META_BYTES);
                  if W.Remaining = 0 then
                     if W.Meta_Kind = Pax_Meta then
                        Parse_Pax (W, Why);
                     else
                        Take_Long_Name (W, Why);
                     end if;
                     W.Phase := Expect_Header;
                     if Why /= No_Refusal then
                        Refuse (W, Why, E);
                     end if;
                  end if;
               end if;
            end;
      end case;
   end Feed;

   procedure Finish (W : in out Walker; E : out Event) is
   begin
      if W.Phase = Ended_Phase then
         E := (Kind => Finished, others => <>);
      elsif W.Phase = Failed_Phase then
         E := (Kind => Refused, Why => W.Why, others => <>);
      else
         Refuse (W, Truncated, E);
      end if;
   end Finish;
end Tar_Archives;
