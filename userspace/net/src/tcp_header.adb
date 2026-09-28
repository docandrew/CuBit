------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Header with SPARK_Mode is

   function Flag (V : Boolean; N : Natural) return Unsigned_8 is
     (if V then Shift_Left (Unsigned_8'(1), N) else 0)
   with Pre => N < 8, Post => Bit (Flag'Result, N) = V;

   procedure Parse (B : Bytes; H : out Header) is
   begin
      H := (Source_Port      => U16 (B, 0),
            Destination_Port => U16 (B, 2),
            Seq_No           => U32 (B, 4),
            Ack_No           => U32 (B, 8),
            Size             => Stated_Size (B),
            NS  => Bit (B (12), 0),
            CWR => Bit (B (13), 7),
            ECE => Bit (B (13), 6),
            URG => Bit (B (13), 5),
            ACK => Bit (B (13), 4),
            PSH => Bit (B (13), 3),
            RST => Bit (B (13), 2),
            SYN => Bit (B (13), 1),
            FIN => Bit (B (13), 0),
            Window         => U16 (B, 14),
            Checksum       => U16 (B, 16),
            Urgent_Pointer => U16 (B, 18));
   end Parse;

   procedure Put16 (B : in out Bytes; I : Natural; V : Unsigned_16) with
     Pre  => I >= B'First and then I < B'Last,
     Post => U16 (B, I) = V and then
             (for all J in B'Range => (if J /= I and then J /= I + 1 then B (J) = B'Old (J)))
   is
   begin
      B (I) := Unsigned_8 (Shift_Right (V, 8));
      B (I + 1) := Unsigned_8 (V and 16#FF#);
   end Put16;

   procedure Put32 (B : in out Bytes; I : Natural; V : Unsigned_32) with
     Pre  => I >= B'First and then B'Last >= 3 and then I <= B'Last - 3,
     Post => U32 (B, I) = V and then
             (for all J in B'Range => (if J < I or else J > I + 3 then B (J) = B'Old (J)))
   is
   begin
      B (I) := Unsigned_8 (Shift_Right (V, 24));
      B (I + 1) := Unsigned_8 (Shift_Right (V, 16) and 16#FF#);
      B (I + 2) := Unsigned_8 (Shift_Right (V, 8) and 16#FF#);
      B (I + 3) := Unsigned_8 (V and 16#FF#);
   end Put32;

   procedure Write (B : in out Bytes; H : Header) is
   begin
      Put16 (B, 0, H.Source_Port);
      Put16 (B, 2, H.Destination_Port);
      Put32 (B, 4, H.Seq_No);
      Put32 (B, 8, H.Ack_No);
      B (12) := Shift_Left (Unsigned_8 (H.Size / 4), 4) or Flag (H.NS, 0);
      B (13) := Flag (H.CWR, 7) or Flag (H.ECE, 6) or Flag (H.URG, 5) or Flag (H.ACK, 4) or
                Flag (H.PSH, 3) or Flag (H.RST, 2) or Flag (H.SYN, 1) or Flag (H.FIN, 0);
      Put16 (B, 14, H.Window);
      Put16 (B, 16, H.Checksum);
      Put16 (B, 18, H.Urgent_Pointer);
   end Write;

   procedure Write_SYN_Options (B : in out Bytes; MSS : Unsigned_16; Scale : Boolean;
                                Shift : Unsigned_8) is
   begin
      B (20) := 2;   --  MSS, length 4
      B (21) := 4;
      Put16 (B, 22, MSS);
      if Scale then
         B (24) := 1;   --  NOP, then window scale, length 3
         B (25) := 3;
         B (26) := 3;
         B (27) := Shift;
      end if;
   end Write_SYN_Options;

end TCP_Header;
