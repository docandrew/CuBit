--  Linux-hosted differential test: TCP_Header (the proved codec netstack
--  uses) against the RecordFlux parser generated from specs/tcp.rflx, on
--  the same segments - built well formed, then mutated, then random.
--  They must agree on every field whenever RecordFlux accepts a segment.
--  The codec may accept a segment RecordFlux rejects only when the same
--  segment with its option bytes replaced by NOPs passes RecordFlux: the
--  one intended difference (option contents are TCP_Wire's, which skips
--  unknown kinds as RFC 9293 3.1 requires).
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Interfaces;   use Interfaces;
with TCP_Header;
with Net.RFLX_Builtin_Types; use Net.RFLX_Builtin_Types;
with Net.RFLX_Types;
with Net.TCP;
with Net.TCP.Segment;

procedure Differential is
   Failures, Both, Only_Codec, Neither, Cases : Natural := 0;
   Seed : Unsigned_32 := 20260926;

   function Rand return Unsigned_32 is
   begin
      Seed := Seed * 1_103_515_245 + 12_345;
      return Shift_Right (Seed, 8);
   end Rand;

   --  RecordFlux's verdict and fields.
   procedure RFLX_Parse (B : TCP_Header.Bytes; Valid : out Boolean; H : out TCP_Header.Header) is
      Buf : Bytes_Ptr := new Bytes (1 .. Index (Natural'Max (B'Length, 1)));
      Ctx : Net.TCP.Segment.Context;
   begin
      H := (others => <>);
      if B'Length < 20 then   --  below Segment_Length'First: invalid by the spec
         Valid := False;
         Net.RFLX_Types.Free (Buf);
         return;
      end if;
      for I in B'Range loop
         Buf (Index (I + 1)) := Byte (B (I));
      end loop;
      Net.TCP.Segment.Initialize
        (Ctx, Buf, Segment_Length => Net.TCP.Segment_Length (B'Length),
         Written_Last => Net.RFLX_Types.Bit_Length (B'Length) * 8);
      Net.TCP.Segment.Verify_Message (Ctx);
      Valid := Net.TCP.Segment.Well_Formed_Message (Ctx);
      if Valid then
         H.Source_Port := Unsigned_16 (Net.TCP.Segment.Get_Source_Port (Ctx));
         H.Destination_Port := Unsigned_16 (Net.TCP.Segment.Get_Destination_Port (Ctx));
         H.Seq_No := Unsigned_32 (Net.TCP.Segment.Get_Sequence_Number (Ctx));
         H.Ack_No := Unsigned_32 (Net.TCP.Segment.Get_Acknowledgment_Number (Ctx));
         H.Size := Natural (Net.TCP.Segment.Get_Data_Offset (Ctx)) * 4;
         H.NS := Net.TCP.Segment.Get_NS (Ctx);
         H.CWR := Net.TCP.Segment.Get_CWR (Ctx);
         H.ECE := Net.TCP.Segment.Get_ECN (Ctx);
         H.URG := Net.TCP.Segment.Get_URG (Ctx);
         H.ACK := Net.TCP.Segment.Get_ACK (Ctx);
         H.PSH := Net.TCP.Segment.Get_PSH (Ctx);
         H.RST := Net.TCP.Segment.Get_RST (Ctx);
         H.SYN := Net.TCP.Segment.Get_SYN (Ctx);
         H.FIN := Net.TCP.Segment.Get_FIN (Ctx);
         H.Window := Unsigned_16 (Net.TCP.Segment.Get_Window (Ctx));
         H.Checksum := Unsigned_16 (Net.TCP.Segment.Get_Checksum (Ctx));
         H.Urgent_Pointer := Unsigned_16 (Net.TCP.Segment.Get_Urgent_Pointer (Ctx));
      end if;
      Net.TCP.Segment.Take_Buffer (Ctx, Buf);
      Net.RFLX_Types.Free (Buf);
   end RFLX_Parse;

   procedure Check (B : TCP_Header.Bytes) is
      R_Valid : Boolean;
      R, C : TCP_Header.Header;
      C_Valid : constant Boolean := TCP_Header.Well_Formed (B);
   begin
      Cases := Cases + 1;
      RFLX_Parse (B, R_Valid, R);
      if C_Valid then
         TCP_Header.Parse (B, C);
      end if;
      if R_Valid and then not C_Valid then
         Put_Line ("FAIL: RecordFlux accepts, codec rejects, length" & B'Length'Image);
         Failures := Failures + 1;
      elsif R_Valid then
         Both := Both + 1;
         if TCP_Header."/=" (R, C) then
            Put_Line ("FAIL: fields differ, length" & B'Length'Image);
            Failures := Failures + 1;
         end if;
      elsif C_Valid then
         --  Only option contents may explain it.
         declare
            N : TCP_Header.Bytes := B;
            N_Valid : Boolean;
            Ignore : TCP_Header.Header;
         begin
            for I in TCP_Header.Fixed_Size .. C.Size - 1 loop
               N (I) := 1;
            end loop;
            RFLX_Parse (N, N_Valid, Ignore);
            if N_Valid then
               Only_Codec := Only_Codec + 1;
            else
               Put_Line ("FAIL: codec accepts a header RecordFlux rejects, length" & B'Length'Image);
               Failures := Failures + 1;
            end if;
         end;
      else
         Neither := Neither + 1;
      end if;
   end Check;

   --  A well-formed segment: random fields, options (NOPs, MSS, window
   --  scale, SACK-permitted, timestamps or an unknown kind), random data.
   procedure Well_Formed_Case is
      Opt_Words : constant Natural := Natural (Rand mod 11);
      Data_Len  : constant Natural := Natural (Rand mod 64);
      Size      : constant Natural := 20 + 4 * Opt_Words;
      B : TCP_Header.Bytes (0 .. Size + Data_Len - 1) := [others => 0];
      H : TCP_Header.Header;
      P : Natural := 20;
   begin
      H := (Source_Port => Unsigned_16 (Rand mod 65_536), Destination_Port => Unsigned_16 (Rand mod 65_536),
            Seq_No => Rand * 256 + Rand mod 256, Ack_No => Rand * 256 + Rand mod 256,
            Size => Size,
            NS => Rand mod 2 = 0, CWR => Rand mod 2 = 0, ECE => Rand mod 2 = 0,
            URG => Rand mod 2 = 0, ACK => Rand mod 2 = 0, PSH => Rand mod 2 = 0,
            RST => Rand mod 2 = 0, SYN => Rand mod 2 = 0, FIN => Rand mod 2 = 0,
            Window => Unsigned_16 (Rand mod 65_536), Checksum => Unsigned_16 (Rand mod 65_536),
            Urgent_Pointer => 0);
      if H.URG then
         H.Urgent_Pointer := Unsigned_16 (Rand mod 65_536);
      end if;
      TCP_Header.Write (B, H);
      while P < Size loop
         case Rand mod 7 is
            when 0 | 1 => B (P) := 1; P := P + 1;
            when 2 => if Size - P >= 4 then B (P .. P + 3) := [2, 4, 5, 180]; P := P + 4; else B (P) := 1; P := P + 1; end if;
            when 3 => if Size - P >= 3 then B (P .. P + 2) := [3, 3, 7]; P := P + 3; else B (P) := 1; P := P + 1; end if;
            when 4 => if Size - P >= 2 then B (P .. P + 1) := [4, 2]; P := P + 2; else B (P) := 1; P := P + 1; end if;
            when 5 => if Size - P >= 10 then B (P .. P + 9) := [8, 10, 1, 2, 3, 4, 5, 6, 7, 8]; P := P + 10;
                      else B (P) := 1; P := P + 1; end if;
            when others => if Size - P >= 4 then B (P .. P + 3) := [30, 4, 9, 9]; P := P + 4;   --  unknown kind
                           else B (P) := 1; P := P + 1; end if;
         end case;
      end loop;
      for I in Size .. B'Last loop
         B (I) := Unsigned_8 (Rand mod 256);
      end loop;
      Check (B);
      --  Mutations: one byte of the header or options, or the length cut.
      for M in 1 .. 8 loop
         declare
            X : TCP_Header.Bytes := B;
         begin
            X (Natural (Rand) mod Size) := Unsigned_8 (Rand mod 256);
            Check (X);
            if B'Length > 1 then
               Check (X (0 .. Natural (Rand) mod B'Length));
            end if;
         end;
      end loop;
   end Well_Formed_Case;
begin
   for N in 1 .. 20_000 loop
      Well_Formed_Case;
   end loop;
   --  Random bytes, every length from 0 to 80.
   for N in 1 .. 20_000 loop
      declare
         B : TCP_Header.Bytes (0 .. Natural (Rand mod 81) - 1);
      begin
         for I in B'Range loop
            B (I) := Unsigned_8 (Rand mod 256);
         end loop;
         Check (B);
      end;
   end loop;
   Put_Line ("segments:" & Cases'Image & ", both accept:" & Both'Image & ", codec only (option contents):" &
             Only_Codec'Image & ", both reject:" & Neither'Image);
   --  Cost of one parse of a 1,480-byte segment (20-byte header, 1,460
   --  bytes of data): RecordFlux's verify-and-get against the codec.
   declare
      Rounds : constant := 200_000;
      B : TCP_Header.Bytes (0 .. 1_479) := [others => 16#AB#];
      H : TCP_Header.Header := (Size => 20, ACK => True, others => <>);
      Buf : Bytes_Ptr := new Bytes (1 .. 1_480);
      Ctx : Net.TCP.Segment.Context;
      Sum : Unsigned_64 := 0;
      T0, T1, T2 : Time;
   begin
      TCP_Header.Write (B, H);
      for I in B'Range loop
         Buf (Index (I + 1)) := Byte (B (I));
      end loop;
      T0 := Clock;
      for R in 1 .. Rounds loop
         Net.TCP.Segment.Initialize
           (Ctx, Buf, Segment_Length => 1_480, Written_Last => 1_480 * 8);
         Net.TCP.Segment.Verify_Message (Ctx);
         if Net.TCP.Segment.Well_Formed_Message (Ctx) then
            Sum := Sum + Unsigned_64 (Net.TCP.Segment.Get_Sequence_Number (Ctx)) +
                   Unsigned_64 (Net.TCP.Segment.Get_Window (Ctx)) +
                   (if Net.TCP.Segment.Get_ACK (Ctx) then 1 else 0);
         end if;
         Net.TCP.Segment.Take_Buffer (Ctx, Buf);
      end loop;
      T1 := Clock;
      for R in 1 .. Rounds loop
         B (4) := Unsigned_8 (R mod 256);   --  defeat hoisting
         if TCP_Header.Well_Formed (B) then
            TCP_Header.Parse (B, H);
            Sum := Sum + Unsigned_64 (H.Seq_No) + Unsigned_64 (H.Window) + (if H.ACK then 1 else 0);
         end if;
      end loop;
      T2 := Clock;
      Put_Line ("parse, ns per segment: RecordFlux" &
                Long_Float'Image (Long_Float (To_Duration (T1 - T0)) * 1.0E9 / Long_Float (Rounds)) &
                ", codec" & Long_Float'Image (Long_Float (To_Duration (T2 - T1)) * 1.0E9 / Long_Float (Rounds)) &
                "  (checksum" & Sum'Image & ")");
   end;
   Put_Line (if Failures = 0 then "NET-HEADERS: PASS" else "NET-HEADERS: FAIL");
end Differential;
