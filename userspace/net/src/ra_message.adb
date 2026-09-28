------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body RA_Message with SPARK_Mode is

   function U32 (B : Bytes; I : Natural) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (B (I)), 24) or Shift_Left (Unsigned_32 (B (I + 1)), 16) or
      Shift_Left (Unsigned_32 (B (I + 2)), 8) or Unsigned_32 (B (I + 3)))
   with Pre => I >= B'First and then B'Last >= 3 and then I <= B'Last - 3;

   procedure Parse
     (B : Bytes; Hop_Limit : Unsigned_8; Source : Address;
      A : out Advertisement; OK : out Boolean)
   is
      P : Natural := Fixed_Size;
      Option_Bytes : Natural;
      Candidate : Prefix;
   begin
      A := (others => <>);
      OK := False;
      if Hop_Limit /= Required_Hop_Limit or else not Is_Link_Local (Source) or else
        B'Length < Fixed_Size or else B (0) /= Advertisement_Type or else B (1) /= 0
      then
         return;
      end if;
      A.Router_Lifetime := IPv6_Header.U16 (B, 6);
      while P < B'Length loop
         pragma Loop_Invariant (P >= Fixed_Size and then P < B'Length);
         pragma Loop_Invariant (for all I in 1 .. A.Count => Usable (A.Prefixes (I)));
         pragma Loop_Variant (Increases => P);
         if B'Length - P < 2 or else B (P + 1) = 0 then
            return;   --  a truncated option, or one of length zero
         end if;
         Option_Bytes := Natural (B (P + 1)) * 8;
         if Option_Bytes > B'Length - P then
            return;   --  an option past the end of the message
         end if;
         if B (P) = Prefix_Option and then Option_Bytes = Prefix_Option_Size and then
           B (P + 2) = SLAAC_Prefix and then (B (P + 3) and Autonomous_Flag) /= 0 and then
           A.Count < Maximum_Prefixes
         then
            Candidate :=
              (Network   => [0 .. 7 => 0, 8 .. 15 => 0],
               Valid     => U32 (B, P + 4),
               Preferred => U32 (B, P + 8));
            for K in 0 .. 7 loop
               Candidate.Network (K) := B (P + 16 + K);
               pragma Loop_Invariant (for all J in 8 .. 15 => Candidate.Network (J) = 0);
            end loop;
            if Usable (Candidate) then
               A.Count := A.Count + 1;
               A.Prefixes (A.Count) := Candidate;
            end if;
         end if;
         P := P + Option_Bytes;
      end loop;
      OK := True;
   end Parse;

end RA_Message;
