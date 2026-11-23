------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The packfile reader (see the spec). Headers are read a byte per step,
--  entry data a run per step; every byte taken before the trailer is
--  hashed, and every byte of an entry goes into its CRC-32.
------------------------------------------------------------------------------
pragma Ada_2022;
package body Git_Packs with SPARK_Mode is
   subtype U32 is Interfaces.Unsigned_32;

   --  gitformat-pack: the signature, versions and object type numbers.
   type Signature_Bytes is array (1 .. 4) of Byte;
   SIGNATURE : constant Signature_Bytes :=
     [Character'Pos ('P'), Character'Pos ('A'), Character'Pos ('C'), Character'Pos ('K')];
   FIRST_VERSION : constant := 2;
   LAST_VERSION : constant := 3;
   TYPE_COMMIT : constant := 1;
   TYPE_TREE : constant := 2;
   TYPE_BLOB : constant := 3;
   TYPE_TAG : constant := 4;
   TYPE_OFS_DELTA : constant := 6;
   TYPE_REF_DELTA : constant := 7;
   --  Base-128 numbers: the continuation bit and a group's range; the
   --  first byte of an entry header holds four size bits.
   CONTINUATION : constant := 16#80#;
   GROUP : constant := 128;
   GROUP_BITS : constant := 7;
   FIRST_SIZE_GROUP : constant := 16;
   FIRST_SIZE_BITS : constant := 4;
   subtype U64 is Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_64;
   --  The largest OFS_DELTA distance before one more group.
   MAXIMUM_DISTANCE_BEFORE_GROUP : constant := 2 ** 54;

   --  CRC-32 (ISO 3309 / ITU-T V.42, as zlib's crc32 and git's index),
   --  reflected, bit by bit.
   CRC_POLYNOMIAL : constant U32 := 16#EDB8_8320#;
   CRC_START : constant U32 := 16#FFFF_FFFF#;

   function CRC_Byte (CRC : U32; B : Byte) return U32 is
      C : U32 := CRC xor U32 (B);
   begin
      for K in 1 .. 8 loop
         C := (if (C and 1) = 1 then Interfaces.Shift_Right (C, 1) xor CRC_POLYNOMIAL
               else Interfaces.Shift_Right (C, 1));
      end loop;
      return C;
   end CRC_Byte;

   function Kind_Of (Code : Natural) return Object_Kind is
     (case Code is
         when TYPE_COMMIT => Commit_Object,
         when TYPE_TREE => Tree_Object,
         when TYPE_BLOB => Blob_Object,
         when TYPE_TAG => Tag_Object,
         when TYPE_OFS_DELTA => Ofs_Delta,
         when others => Ref_Delta)
   with Pre => Code in TYPE_COMMIT .. TYPE_TAG | TYPE_OFS_DELTA | TYPE_REF_DELTA;

   procedure Start (P : out Pack_Reader; Ceiling : Limits; Format : Git_Protocol.Object_Format) is
   begin
      P := (Ceiling => Ceiling, Format => Format, others => <>);
      Inflate_Streams.Start (P.Z, Zlib_Stream, 0);
      Git_Hashes.Start (P.Hash, Format);
   end Start;

   procedure Refuse (P : in out Pack_Reader; Why : Refusal; E : out Event_Kind)
   with Pre => Why /= No_Refusal,
        Post => P = (P'Old with delta Phase => Failed_Phase, Why => Why) and then E = Refused
   is
   begin
      P.Phase := Failed_Phase;
      P.Why := Why;
      E := Refused;
   end Refuse;

   --  The header is complete: the entry's data follows.
   procedure Begin_Data (P : in out Pack_Reader; E : out Event_Kind)
   with Pre => Valid (P) and then P.Phase in Entry_Header | Ofs_Offset | Ref_Base
                 and then (if P.Kind = Ofs_Delta then P.Base_At >= HEADER_BYTES and then P.Base_At < P.Entry_At),
        Post => Valid (P) and then P.Phase = Entry_Data and then E = Entry_Start and then P.Delivered = 0
          and then P = (P'Old with delta Phase => P.Phase, Z => P.Z, Delivered => P.Delivered)
   is
   begin
      Inflate_Streams.Start (P.Z, Zlib_Stream, P.Size);
      P.Delivered := 0;
      P.Phase := Entry_Data;
      E := Entry_Start;
   end Begin_Data;

   --  The next entry, or the trailer after the last.
   procedure Next_Entry (P : in out Pack_Reader)
   with Pre => P.Done_Entries <= P.Count,
        Post => P.Phase in Entry_Header | Trailer and then (P.Phase = Entry_Header) = (P.Done_Entries < P.Count)
          and then P = (P'Old with delta Phase => P.Phase, Header_Started => P.Header_Started,
                        Shift => P.Shift, Trailer_Read => P.Trailer_Read)
          and then not P.Header_Started and then P.Shift = 0 and then P.Trailer_Read = 0
   is
   begin
      P.Header_Started := False;
      P.Shift := 0;
      P.Trailer_Read := 0;
      P.Phase := (if P.Done_Entries < P.Count then Entry_Header else Trailer);
   end Next_Entry;

   --  The pack's checksum: the hash of every byte before the trailer.
   procedure Finalize (P : in out Pack_Reader)
   with Post => P = (P'Old with delta Hash => P.Hash, Sum => P.Sum)
                  and then Git_Hashes.Format (P.Hash) = Git_Hashes.Format (P.Hash'Old)
   is
   begin
      Git_Hashes.Finish (P.Hash, P.Sum);
   end Finalize;

   --  Bytes taken in phase Before: the offset moves past them; they are
   --  hashed unless they are the trailer's, and go into the entry's CRC.
   procedure Account (P : in out Pack_Reader; Run : Byte_Array; Before : Phase_Kind)
   with Pre => Run'Last < Positive'Last - 128
                 and then P.Offset + Byte_Count (Run'Length) <= P.Ceiling.Maximum_Pack_Bytes,
        Post => P = (P'Old with delta Offset => P.Offset, Hash => P.Hash, CRC => P.CRC)
          and then P.Offset = P.Offset'Old + Byte_Count (Run'Length)
          and then Git_Hashes.Format (P.Hash) = Git_Hashes.Format (P.Hash'Old)
   is
   begin
      if Run'Length > 0 then
         P.Offset := P.Offset + Byte_Count (Run'Length);
         if Before /= Trailer then
            Git_Hashes.Update (P.Hash, Run);
         end if;
         if Before in Entry_Header | Ofs_Offset | Ref_Base | Entry_Data then
            for K in Run'Range loop
               pragma Loop_Invariant (P = (P'Loop_Entry with delta CRC => P.CRC));
               P.CRC := CRC_Byte (P.CRC, Run (K));
            end loop;
         end if;
      end if;
   end Account;

   --  What one header or trailer byte means (Offset still counts the
   --  bytes before B).
   procedure Decide_Byte (P : in out Pack_Reader; B : Byte; E : out Event_Kind)
   with Pre => Valid (P) and then P.Phase in Pack_Header | Entry_Header | Ofs_Offset | Ref_Base | Trailer,
        Post => Valid (P) and then P.Offset = P.Offset'Old and then P.Ceiling = P.Ceiling'Old
          and then P.Format = P.Format'Old
          and then P.Done_Entries = P.Done_Entries'Old
          and then E in Need_Input | Entry_Start | Pack_End | Refused
          and then (E = Refused) = (P.Phase = Failed_Phase)
          and then (if E = Refused then P.Why /= No_Refusal)
          and then (E = Pack_End) = (P.Phase = Done_Phase)
          and then (P.Phase = Entry_Data) = (E = Entry_Start)
          and then (if E = Entry_Start then
                      P.Delivered = 0 and then P.Size <= P.Ceiling.Maximum_Object_Bytes
                      and then (if P.Kind = Ofs_Delta then
                                  P.Base_At >= HEADER_BYTES and then P.Base_At < P.Entry_At))
   is
   begin
      E := Need_Input;
      case P.Phase is
         when Pack_Header =>
            P.Header_Read := P.Header_Read + 1;
            P.Header (P.Header_Read) := B;
            if P.Header_Read = HEADER_BYTES then
               declare
                  Version : constant U32 :=
                    Interfaces.Shift_Left (U32 (P.Header (5)), 24) or Interfaces.Shift_Left (U32 (P.Header (6)), 16)
                    or Interfaces.Shift_Left (U32 (P.Header (7)), 8) or U32 (P.Header (8));
                  Objects : constant U32 :=
                    Interfaces.Shift_Left (U32 (P.Header (9)), 24) or Interfaces.Shift_Left (U32 (P.Header (10)), 16)
                    or Interfaces.Shift_Left (U32 (P.Header (11)), 8) or U32 (P.Header (12));
               begin
                  if (for some K in SIGNATURE'Range => P.Header (K) /= SIGNATURE (K)) then
                     Refuse (P, Bad_Signature, E);
                  elsif Version not in FIRST_VERSION .. LAST_VERSION then
                     Refuse (P, Bad_Version, E);
                  elsif Objects > U32 (Natural'Last) or else Natural (Objects) > P.Ceiling.Maximum_Objects then
                     Refuse (P, Too_Many_Objects, E);
                  else
                     P.Count := Natural (Objects);
                     Next_Entry (P);
                  end if;
               end;
            end if;

         when Entry_Header =>
            if not P.Header_Started then
               declare
                  Code : constant Natural := Natural (B / FIRST_SIZE_GROUP) mod 8;
               begin
                  if Code not in TYPE_COMMIT .. TYPE_TAG | TYPE_OFS_DELTA | TYPE_REF_DELTA then
                     Refuse (P, Bad_Object_Type, E);
                     return;
                  end if;
                  P.Kind := Kind_Of (Code);
                  P.Entry_At := P.Offset;
                  P.CRC := CRC_START;
                  P.Pending := U64 (B mod FIRST_SIZE_GROUP);
                  P.Shift := FIRST_SIZE_BITS;
                  P.Header_Started := True;
               end;
            elsif P.Shift > MAXIMUM_SHIFT then
               Refuse (P, Bad_Size, E);
               return;
            else
               P.Pending := P.Pending or Interfaces.Shift_Left (U64 (B mod GROUP), P.Shift);
               P.Shift := P.Shift + GROUP_BITS;
            end if;
            if B < CONTINUATION then
               --  The size is whole.
               if P.Pending > U64 (Byte_Count'Last) then
                  Refuse (P, Bad_Size, E);
                  return;
               elsif Byte_Count (P.Pending) > P.Ceiling.Maximum_Object_Bytes then
                  Refuse (P, Object_Too_Large, E);
                  return;
               end if;
               P.Delivered := 0;
               P.Size := Byte_Count (P.Pending);
               P.Distance := 0;
               P.Header_Started := False;
               case P.Kind is
                  when Ofs_Delta =>
                     P.Phase := Ofs_Offset;
                  when Ref_Delta =>
                     P.Base_Read := 0;
                     P.Phase := Ref_Base;
                  when Whole_Kind =>
                     Begin_Data (P, E);
               end case;
            end if;

         when Ofs_Offset =>
            if not P.Header_Started then
               P.Distance := Byte_Count (B mod GROUP);
               P.Header_Started := True;
            elsif P.Distance >= MAXIMUM_DISTANCE_BEFORE_GROUP then
               Refuse (P, Bad_Delta_Offset, E);
               return;
            else
               P.Distance := (P.Distance + 1) * GROUP + Byte_Count (B mod GROUP);
            end if;
            if B < CONTINUATION then
               if P.Distance = 0 or else P.Entry_At < HEADER_BYTES or else P.Distance > P.Entry_At - HEADER_BYTES then
                  Refuse (P, Bad_Delta_Offset, E);
                  return;
               end if;
               P.Base_At := P.Entry_At - P.Distance;
               P.Header_Started := False;
               Begin_Data (P, E);
            end if;

         when Ref_Base =>
            P.Base (P.Base_Read + 1) := B;
            if P.Base_Read + 1 = Git_Protocol.Id_Bytes (P.Format) then
               P.Base_Read := 0;
               Begin_Data (P, E);
            else
               P.Base_Read := P.Base_Read + 1;
            end if;

         when Trailer =>
            P.Trailer_Read := P.Trailer_Read + 1;
            P.Trailer (P.Trailer_Read) := B;
            if P.Trailer_Read = Git_Protocol.Id_Bytes (P.Format) then
               if Git_Protocol."=" (P.Trailer, P.Sum) then
                  P.Phase := Done_Phase;
                  E := Pack_End;
               else
                  Refuse (P, Bad_Checksum, E);
               end if;
            end if;

         when Entry_Data | Done_Phase | Failed_Phase =>
            --  Excluded by the precondition.
            Refuse (P, Truncated, E);
      end case;
   end Decide_Byte;

   --  One header or trailer byte.
   procedure Header_Byte (P : in out Pack_Reader; B : Byte; E : out Event_Kind)
   with Pre => Valid (P) and then P.Phase in Pack_Header | Entry_Header | Ofs_Offset | Ref_Base | Trailer
                 and then P.Offset < P.Ceiling.Maximum_Pack_Bytes,
        Post => Valid (P) and then P.Offset = P.Offset'Old + 1 and then P.Ceiling = P.Ceiling'Old
          and then P.Format = P.Format'Old
          and then P.Done_Entries = P.Done_Entries'Old
          and then E in Need_Input | Entry_Start | Pack_End | Refused
          and then (E = Refused) = (P.Phase = Failed_Phase)
          and then (if E = Refused then P.Why /= No_Refusal)
          and then (E = Pack_End) = (P.Phase = Done_Phase)
          and then (P.Phase = Entry_Data) = (E = Entry_Start)
          and then (if E = Entry_Start then
                      P.Delivered = 0 and then P.Size <= P.Ceiling.Maximum_Object_Bytes
                      and then (if P.Kind = Ofs_Delta then
                                  P.Base_At >= HEADER_BYTES and then P.Base_At < P.Entry_At))
   is
      Before : constant Phase_Kind := P.Phase;
      Byte_Run : constant Byte_Array (1 .. 1) := [B];
   begin
      Decide_Byte (P, B, E);
      Account (P, Byte_Run, Before);
      if P.Phase = Trailer and then Before /= Trailer then
         Finalize (P);
      end if;
   end Header_Byte;

   --  A run of the entry's zlib stream.
   procedure Data_Run
     (P : in out Pack_Reader;
      Input : Byte_Array;
      In_Next : in out Positive;
      Output : in out Byte_Array;
      Out_Next : in out Positive;
      E : out Event_Kind)
   with
     Pre => Valid (P) and then P.Phase = Entry_Data
       and then Input'Last < Positive'Last - 128 and then Output'Last < Positive'Last
       and then In_Next in Input'First .. Input'Last + 1
       and then Out_Next in Output'First .. Output'Last + 1
       and then P.Offset + Byte_Count (Input'Last + 1 - In_Next) <= P.Ceiling.Maximum_Pack_Bytes,
     Post =>
       Valid (P)
       and then P.Ceiling = P.Ceiling'Old and then P.Format = P.Format'Old
       and then In_Next in In_Next'Old .. Input'Last + 1
       and then Out_Next in Out_Next'Old .. Output'Last + 1
       and then P.Offset = P.Offset'Old + Byte_Count (In_Next - In_Next'Old)
       and then P.Delivered = P.Delivered'Old + Byte_Count (Out_Next - Out_Next'Old)
       and then (for all I in Output'Range =>
                   (if I < Out_Next'Old or else I >= Out_Next then Output (I) = Output'Old (I)))
       and then E in Need_Input | Output_Full | Entry_End | Refused
       and then (E = Refused) = (P.Phase = Failed_Phase)
       and then (if E = Refused then P.Why /= No_Refusal)
       and then (if E = Need_Input then In_Next = Input'Last + 1 and then P.Phase = Entry_Data)
       and then (if E = Output_Full then Out_Next = Output'Last + 1 and then P.Phase = Entry_Data)
       and then (if E = Entry_End then P.Delivered = P.Size and then P.Done_Entries >= 1
                   and then P.Phase in Entry_Header | Trailer)
       and then P.Phase /= Done_Phase
   is
      R : Progress;
      First : constant Positive := In_Next;
   begin
      Inflate_Streams.Inflate (P.Z, Input, In_Next, Output, Out_Next, R);
      P.Delivered := Inflate_Streams.Total_Out (P.Z);
      pragma Assert (Byte_Count (In_Next - First) <= Byte_Count (Input'Last + 1 - First));
      Account (P, Input (First .. In_Next - 1), Entry_Data);
      case R is
         when Need_Input =>
            E := Need_Input;
         when Output_Full =>
            E := Output_Full;
         when Finished =>
            if P.Delivered /= P.Size then
               Refuse (P, Size_Mismatch, E);
            else
               P.Done_Entries := P.Done_Entries + 1;
               Next_Entry (P);
               if P.Phase = Trailer then
                  Finalize (P);
               end if;
               E := Entry_End;
            end if;
         when Refused =>
            P.Inner := Inflate_Streams.Failure (P.Z);
            Refuse (P, (if P.Inner = Inflate_Streams.Too_Large then Size_Mismatch else Bad_Compression), E);
      end case;
   end Data_Run;

   procedure Step
     (P : in out Pack_Reader;
      Input : Byte_Array;
      In_Next : in out Positive;
      Output : in out Byte_Array;
      Out_Next : in out Positive;
      E : out Event_Kind)
   is
      Allowance : constant Byte_Count := P.Ceiling.Maximum_Pack_Bytes - P.Offset;
      --  The input past the pack's ceiling is not taken.
      Last : constant Natural :=
        (if Byte_Count (Input'Last + 1 - In_Next) > Allowance then In_Next - 1 + Natural (Allowance)
         else Input'Last);
   begin
      E := Need_Input;
      if P.Phase = Failed_Phase then
         E := Refused;
         return;
      elsif P.Phase = Done_Phase then
         E := Pack_End;
         return;
      elsif P.Phase = Entry_Data then
         Data_Run (P, Input (Input'First .. Last), In_Next, Output, Out_Next, E);
      else
         while In_Next <= Last loop
            pragma Loop_Invariant (Valid (P) and then P.Ceiling = P.Ceiling'Loop_Entry
                                   and then P.Format = P.Format'Loop_Entry);
            pragma Loop_Invariant (In_Next in In_Next'Loop_Entry .. Last);
            pragma Loop_Invariant (P.Offset = P.Offset'Loop_Entry + Byte_Count (In_Next - In_Next'Loop_Entry));
            pragma Loop_Invariant (P.Phase in Pack_Header | Entry_Header | Ofs_Offset | Ref_Base | Trailer);
            pragma Loop_Invariant (E = Need_Input);
            Header_Byte (P, Input (In_Next), E);
            In_Next := In_Next + 1;
            exit when E /= Need_Input;
         end loop;
      end if;
      if E = Need_Input and then Last < Input'Last then
         Refuse (P, Pack_Too_Large, E);
      end if;
   end Step;

   procedure Input_Ended (P : in out Pack_Reader; E : out Event_Kind) is
   begin
      if P.Phase = Done_Phase then
         E := Pack_End;
      elsif P.Phase = Failed_Phase then
         E := Refused;
      else
         Refuse (P, Truncated, E);
      end if;
   end Input_Ended;
end Git_Packs;
