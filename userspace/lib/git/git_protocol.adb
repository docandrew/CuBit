------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The protocol v2 readers (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;

package body Git_Protocol with SPARK_Mode is
   LF : constant Byte := 10;
   SPACE : constant Byte := Character'Pos (' ');
   HEX_CHARACTERS : constant String (1 .. 16) := "0123456789abcdef";

   --  gitprotocol-v2: side-band channels in the packfile section.
   PACK_CHANNEL : constant := 1;
   PROGRESS_CHANNEL : constant := 2;
   ERROR_CHANNEL : constant := 3;

   function Nibble (C : Character) return Byte is
     (if C in '0' .. '9' then Character'Pos (C) - Character'Pos ('0')
      else Character'Pos (C) - Character'Pos ('a') + 10)
   with Pre => Lower_Hex (C), Post => Nibble'Result <= 15;

   function To_Id (S : String; F : Object_Format) return Object_Id is
      Id : Object_Id := NO_ID;
   begin
      for K in 1 .. Id_Bytes (F) loop
         Id (K) := Nibble (S (S'First + 2 * (K - 1))) * 16 + Nibble (S (S'First + 2 * (K - 1) + 1));
         pragma Loop_Invariant (for all J in Id_Bytes (F) + 1 .. MAXIMUM_ID_BYTES => Id (J) = 0);
      end loop;
      return Id;
   end To_Id;

   function To_Hex (Id : Object_Id; F : Object_Format) return String is
      H : String (1 .. Hex_Digits (F)) := [others => '0'];
   begin
      for K in 1 .. Id_Bytes (F) loop
         H (2 * K - 1) := HEX_CHARACTERS (Natural (Id (K) / 16) + 1);
         H (2 * K) := HEX_CHARACTERS (Natural (Id (K) mod 16) + 1);
         pragma Loop_Invariant (for all J in 1 .. 2 * K => Lower_Hex (H (J)));
      end loop;
      return H;
   end To_Hex;

   function Packet (Text : String) return String is
      N : constant Natural := Text'Length + LENGTH_DIGITS;
      Head : constant String (1 .. LENGTH_DIGITS) :=
        [HEX_CHARACTERS (N / 4096 mod 16 + 1), HEX_CHARACTERS (N / 256 mod 16 + 1), HEX_CHARACTERS (N / 16 mod 16 + 1),
         HEX_CHARACTERS (N mod 16 + 1)];
   begin
      return Head & Text;
   end Packet;

   --  The payload's line: without one trailing LF.
   function Line_Last (P : Byte_Array) return Natural is
     (if P'Length = 0 then 0 elsif P (P'Last) = LF then P'Last - 1 else P'Last)
   with Pre => P'First = 1 and then P'Last >= 0, Post => Line_Last'Result in P'First - 1 .. P'Last;

   function Matches (P : Byte_Array; First : Positive; Last : Natural; S : String) return Boolean is
     (Last - First + 1 = S'Length
      and then (for all K in 0 .. S'Length - 1 => P (First + K) = Character'Pos (S (S'First + K))))
   with Pre => First < Positive'Last and then Last >= First - 1
                 and then (if Last >= First then First in P'Range and then Last in P'Range);

   function Is_Lower_Hex (B : Byte) return Boolean is
     (B in Character'Pos ('0') .. Character'Pos ('9') | Character'Pos ('a') .. Character'Pos ('f'));

   function Hex_At (P : Byte_Array; First : Positive; F : Object_Format) return Boolean is
     (for all K in 0 .. Hex_Digits (F) - 1 => Is_Lower_Hex (P (First + K)))
   with Pre => First in P'Range and then P'Last >= Hex_Digits (F) - 1
                 and then First <= P'Last - (Hex_Digits (F) - 1);

   function Id_At (P : Byte_Array; First : Positive; F : Object_Format) return Object_Id
   with Pre => First in P'Range and then P'Last >= Hex_Digits (F) - 1
                 and then First <= P'Last - (Hex_Digits (F) - 1) and then Hex_At (P, First, F)
   is
      S : String (1 .. Hex_Digits (F)) := [others => '0'];
   begin
      for K in S'Range loop
         S (K) := Character'Val (P (First + (K - 1)));
         pragma Loop_Invariant (for all J in 1 .. K => Lower_Hex (S (J)));
      end loop;
      return To_Id (S, F);
   end Id_At;

   ---------------------------------------------------------------------------
   --  The capability advertisement.

   function Key_Character (B : Byte) return Boolean is
     (B in Character'Pos ('a') .. Character'Pos ('z') | Character'Pos ('A') .. Character'Pos ('Z')
         | Character'Pos ('0') .. Character'Pos ('9') | Character'Pos ('-') | Character'Pos ('_'));

   function Value_Character (B : Byte) return Boolean is (B in 16#20# .. 16#7E#);

   procedure Start (A : out Advertisement_Reader) is
   begin
      A := (others => <>);
   end Start;

   procedure Feed (A : in out Advertisement_Reader; Kind : Packet_Kind; Payload : Byte_Array; Reply : out Reply_Kind) is
      procedure Refuse (Why : Advertisement_Refusal) with
        Pre => Why /= No_Refusal,
        Post => Failed (A) and then Valid (A) and then Failure (A) = Why and then Reply = Refused,
        Global => (In_Out => A, Output => Reply)
      is
      begin
         A.Phase := Failed_Phase;
         A.Why := Why;
         Reply := Refused;
      end Refuse;

      Last : constant Natural := Line_Last (Payload);
   begin
      Reply := Need_More;
      case A.Phase is
         when Failed_Phase =>
            Reply := Refused;
         when Done_Phase =>
            Refuse (Unexpected_Packet);
         when Expect_Version =>
            if Kind /= Data_Packet then
               Refuse (Not_Version_2);
            elsif not A.Service_Seen and then Matches (Payload, Payload'First, Last, "# service=git-upload-pack") then
               A.Service_Seen := True;
               A.Phase := Expect_Service_Flush;
            elsif Matches (Payload, Payload'First, Last, "version 2") then
               A.Phase := In_Capabilities;
            else
               Refuse (Not_Version_2);
            end if;
         when Expect_Service_Flush =>
            if Kind = Flush_Packet then
               A.Phase := Expect_Version;
            else
               Refuse (Unexpected_Packet);
            end if;
         when In_Capabilities =>
            if Kind = Flush_Packet then
               if not A.Ls_Refs then
                  Refuse (No_Ls_Refs);
               elsif not A.Fetch then
                  Refuse (No_Fetch);
               else
                  A.Phase := Done_Phase;
                  Reply := Ended;
               end if;
            elsif Kind /= Data_Packet then
               Refuse (Unexpected_Packet);
            else
               --  key [ "=" value ]
               declare
                  Key_Last : Natural := Payload'First - 1;
               begin
                  while Key_Last < Last and then Key_Character (Payload (Key_Last + 1)) loop
                     pragma Loop_Invariant (Key_Last in Payload'First - 1 .. Last - 1);
                     Key_Last := Key_Last + 1;
                  end loop;
                  if Key_Last < Payload'First then
                     Refuse (Bad_Capability);
                  elsif Key_Last < Last
                    and then (Payload (Key_Last + 1) /= Character'Pos ('=') or else Key_Last + 1 = Last
                              or else (for some I in Key_Last + 2 .. Last => not Value_Character (Payload (I))))
                  then
                     Refuse (Bad_Capability);
                  elsif Matches (Payload, Payload'First, Key_Last, "ls-refs") then
                     A.Ls_Refs := True;
                  elsif Matches (Payload, Payload'First, Key_Last, "fetch") then
                     A.Fetch := True;
                  elsif Matches (Payload, Payload'First, Key_Last, "agent") then
                     A.Agent := True;
                  elsif Matches (Payload, Payload'First, Key_Last, "object-format") then
                     if Key_Last < Last and then Matches (Payload, Key_Last + 2, Last, "sha1") then
                        A.Object_Format := True;
                        A.Format := Sha1_Format;
                     elsif Key_Last < Last and then Matches (Payload, Key_Last + 2, Last, "sha256") then
                        A.Object_Format := True;
                        A.Format := Sha256_Format;
                     else
                        Refuse (Unsupported_Object_Format);
                     end if;
                  end if;
               end;
            end if;
      end case;
   end Feed;

   ---------------------------------------------------------------------------
   --  ls-refs.

   procedure Start (L : out Refs_Reader; Maximum : Ref_Count; Format : Object_Format) is
   begin
      L := (Maximum => Maximum, Format => Format, others => <>);
   end Start;

   --  Copy a name of at most MAXIMUM_REF_NAME_BYTES into Buffer.
   procedure Copy_Name
     (P : Byte_Array; First, Last : Positive; Buffer : out Ref_Buffer; Length : out Ref_Name_Length)
   with Pre => First <= Last and then Last in P'Range and then First in P'Range
                 and then Last - First + 1 <= MAXIMUM_REF_NAME_BYTES,
        Post => Length = Last - First + 1
   is
   begin
      Buffer := [others => ' '];
      Length := Last - First + 1;
      for K in 1 .. Length loop
         Buffer (K) := Character'Val (P (First + (K - 1)));
      end loop;
   end Copy_Name;

   procedure Feed (L : in out Refs_Reader; Kind : Packet_Kind; Payload : Byte_Array; Reply : out Ref_Reply_Kind) is
      procedure Refuse (Why : Refs_Refusal) with
        Pre => Why /= No_Refusal and then L.Count <= L.Maximum,
        Post => Failed (L) and then Valid (L) and then Failure (L) = Why and then Reply = Refs_Refused
          and then not L.Has_Ref and then L.Count = L.Count'Old and then L.Maximum = L.Maximum'Old
          and then L.Format = L.Format'Old,
        Global => (In_Out => L, Output => Reply)
      is
      begin
         L.Phase := Failed_Phase;
         L.Why := Why;
         L.Has_Ref := False;
         L.Has_Symref := False;
         Reply := Refs_Refused;
      end Refuse;

      Last : constant Natural := Line_Last (Payload);
      Name_First : Positive;
      Name_Last : Natural;
      I : Positive;
      Name : Ref_Buffer;
      Name_Length : Ref_Name_Length;
      Target : Ref_Buffer := [others => ' '];
      Target_Length : Ref_Name_Length := 0;
      Has_Target : Boolean := False;
      Has_Peeled : Boolean := False;
      Peeled : Object_Id := NO_ID;
      Id_Digits : constant Positive := Hex_Digits (L.Format);
      SYMREF_TARGET : constant String := "symref-target:";
      PEELED_PREFIX : constant String := "peeled:";
   begin
      Reply := Ref_Read;
      if L.Phase = Failed_Phase then
         Reply := Refs_Refused;
         L.Has_Ref := False;
         L.Has_Symref := False;
         return;
      end if;
      L.Has_Ref := False;
      L.Has_Symref := False;
      L.Has_Peeled := False;
      if L.Phase = Done_Phase or else Kind in Delimiter_Packet | Response_End_Packet then
         Refuse (Unexpected_Packet);
         return;
      elsif Kind = Flush_Packet then
         L.Phase := Done_Phase;
         Reply := Refs_Ended;
         return;
      end if;
      --  "<id> <name>"
      if Payload'Length < Id_Digits + 2 or else Last < Payload'First + Id_Digits + 1 then
         Refuse (Bad_Ref_Line);
         return;
      elsif not Hex_At (Payload, Payload'First, L.Format) then
         Refuse (Bad_Object_Id);
         return;
      elsif Payload (Payload'First + Id_Digits) /= SPACE then
         Refuse (Bad_Ref_Line);
         return;
      elsif L.Count >= L.Maximum then
         Refuse (Too_Many_Refs);
         return;
      end if;
      Name_First := Payload'First + Id_Digits + 1;
      Name_Last := Name_First - 1;
      while Name_Last < Last and then Payload (Name_Last + 1) /= SPACE loop
         pragma Loop_Invariant (Name_Last in Name_First - 1 .. Last - 1);
         Name_Last := Name_Last + 1;
      end loop;
      if Name_Last < Name_First or else Name_Last - Name_First + 1 > MAXIMUM_REF_NAME_BYTES then
         Refuse (Bad_Ref_Name);
         return;
      end if;
      Copy_Name (Payload, Name_First, Name_Last, Name, Name_Length);
      if not Safe_Ref_Name (Name (1 .. Name_Length)) then
         Refuse (Bad_Ref_Name);
         return;
      end if;
      --  Attributes: " symref-target:<name>" and " peeled:<id>", each once.
      I := Name_Last + 1;
      while I <= Last loop
         pragma Loop_Invariant (I in Name_Last + 1 .. Last);
         pragma Loop_Invariant (if Has_Target then Safe_Ref_Name (Target (1 .. Target_Length)));
         pragma Loop_Invariant (L.Count < L.Maximum and then not L.Has_Ref and then L.Phase = In_Refs
                                and then L.Why = No_Refusal and then L.Format = L.Format'Loop_Entry);
         --  Payload (I) is the space before an attribute.
         declare
            Value_First : constant Positive := I + 1;
            Value_Last : Natural := I;
         begin
            while Value_Last < Last and then Payload (Value_Last + 1) /= SPACE loop
               pragma Loop_Invariant (Value_Last in I .. Last - 1);
               Value_Last := Value_Last + 1;
            end loop;
            if Value_Last - I > SYMREF_TARGET'Length
              and then Matches (Payload, Value_First, Value_First + SYMREF_TARGET'Length - 1, SYMREF_TARGET)
              and then not Has_Target
              and then Value_Last - (Value_First + SYMREF_TARGET'Length) + 1 <= MAXIMUM_REF_NAME_BYTES
            then
               Copy_Name (Payload, Value_First + SYMREF_TARGET'Length, Value_Last, Target, Target_Length);
               if not Safe_Ref_Name (Target (1 .. Target_Length)) then
                  Refuse (Bad_Ref_Name);
                  return;
               end if;
               Has_Target := True;
            elsif Value_Last - I = PEELED_PREFIX'Length + Id_Digits
              and then Matches (Payload, Value_First, Value_First + PEELED_PREFIX'Length - 1, PEELED_PREFIX)
              and then not Has_Peeled
            then
               if not Hex_At (Payload, Value_First + PEELED_PREFIX'Length, L.Format) then
                  Refuse (Bad_Object_Id);
                  return;
               end if;
               Peeled := Id_At (Payload, Value_First + PEELED_PREFIX'Length, L.Format);
               Has_Peeled := True;
            else
               Refuse (Bad_Attribute);
               return;
            end if;
            I := Value_Last + 1;
         end;
      end loop;
      L.Id := Id_At (Payload, Payload'First, L.Format);
      L.Name := Name;
      L.Name_Length := Name_Length;
      L.Has_Symref := Has_Target;
      L.Target := Target;
      L.Target_Length := Target_Length;
      L.Has_Peeled := Has_Peeled;
      L.Peeled := Peeled;
      L.Has_Ref := True;
      L.Count := L.Count + 1;
   end Feed;

   ---------------------------------------------------------------------------
   --  The fetch response.

   procedure Start (F : out Fetch_Reader) is
   begin
      F := (others => <>);
   end Start;

   procedure Feed (F : in out Fetch_Reader; Kind : Packet_Kind; Payload : Byte_Array; Reply : out Fetch_Reply) is
      procedure Refuse (Why : Fetch_Refusal; First : Positive; Last : Natural) with
        Pre => Why /= No_Refusal and then (if Last >= First then First in Payload'Range and then Last in Payload'Range),
        Post => Failed (F) and then Valid (F) and then Failure (F) = Why and then Reply.Kind = Fetch_Refused,
        Global => (Output => (F, Reply), Input => Payload)
      is
         --  The message: printable ASCII only, at most MAXIMUM_MESSAGE_BYTES.
         N : Message_Length := 0;
      begin
         F.Text := [others => ' '];
         if Last >= First then
            for K in First .. Last loop
               pragma Loop_Invariant (N <= K - First);
               exit when N = MAXIMUM_MESSAGE_BYTES;
               if Payload (K) in 16#20# .. 16#7E# then
                  N := N + 1;
                  F.Text (N) := Character'Val (Payload (K));
               end if;
            end loop;
         end if;
         F.Text_Length := N;
         F.Phase := Failed_Phase;
         F.Why := Why;
         Reply := (Kind => Fetch_Refused, others => <>);
      end Refuse;

      Last : constant Natural := Line_Last (Payload);
   begin
      Reply := (others => <>);
      case F.Phase is
         when Failed_Phase =>
            Reply := (Kind => Fetch_Refused, others => <>);
         when Done_Phase =>
            if Kind = Response_End_Packet then
               Reply := (Kind => Fetch_Ended, others => <>);
            else
               Refuse (Unexpected_Packet, 1, 0);
            end if;
         when Expect_Section =>
            if Kind /= Data_Packet then
               Refuse (Unexpected_Packet, 1, 0);
            elsif Matches (Payload, Payload'First, Last, "packfile") then
               F.Phase := In_Packfile;
            else
               Refuse (Unexpected_Section, Payload'First, Last);
            end if;
         when In_Packfile =>
            if Kind = Flush_Packet then
               F.Phase := Done_Phase;
               Reply := (Kind => Fetch_Ended, others => <>);
            elsif Kind /= Data_Packet or else Payload'Length = 0 then
               Refuse ((if Kind = Data_Packet then Bad_Sideband else Unexpected_Packet), 1, 0);
            elsif Payload (Payload'First) = PACK_CHANNEL then
               if Payload'Length > 1 then
                  Reply := (Kind => Pack_Data, First => Payload'First + 1, Last => Payload'Last);
               end if;
            elsif Payload (Payload'First) = PROGRESS_CHANNEL then
               if Payload'Length > 1 then
                  Reply := (Kind => Progress, First => Payload'First + 1, Last => Payload'Last);
               end if;
            elsif Payload (Payload'First) = ERROR_CHANNEL then
               Refuse (Remote_Error, Payload'First + 1, Payload'Last);
            else
               Refuse (Bad_Sideband, 1, 0);
            end if;
      end case;
   end Feed;
end Git_Protocol;
