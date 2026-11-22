------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Syn_Cookies with SPARK_Mode is
   use type SipHash.Byte_Array;

   function Index_For (MSS : Unsigned_32) return MSS_Index is
   begin
      for I in reverse MSS_Index range 1 .. MSS_Index'Last loop
         if MSS_Table (I) <= MSS then
            return I;
         end if;
         pragma Loop_Invariant (for all J in I .. MSS_Index'Last => MSS_Table (J) > MSS);
      end loop;
      return 0;
   end Index_For;

   function Tag (K : SipHash.Key; E : TCP_Isn.Endpoints; Client_ISN : Seq;
                 T : Unsigned_32) return Unsigned_32
   is
      V : constant Unsigned_32 := Unsigned_32 (Client_ISN);
      Byte : constant := 16#FF#;
      --  The client's ISN in network order, then the counter.
      Extra : constant SipHash.Byte_Array (0 .. 4) :=
        [Unsigned_8 (Shift_Right (V, 24)), Unsigned_8 (Shift_Right (V, 16) and Byte),
         Unsigned_8 (Shift_Right (V, 8) and Byte), Unsigned_8 (V and Byte),
         Unsigned_8 (T and Byte)];
   begin
      return Unsigned_32 (SipHash.Hash (K, TCP_Isn.Serialize (E) & Extra) and Tag_Mask);
   end Tag;

   procedure Lemma_Round_Trip (K : SipHash.Key; E : TCP_Isn.Endpoints; Client_ISN : Seq;
                               Sent, Now : Unsigned_64; MSS : Unsigned_32)
   is
      T : constant Unsigned_32 := Counter (Sent);
      C : constant Unsigned_32 := Cookie_Word (K, E, Client_ISN, Sent, MSS);
   begin
      pragma Assert (Unsigned_32 (Make (K, E, Client_ISN, Sent, MSS)) = C);
      pragma Assert (Shift_Right (C, Counter_Shift) = T);
      pragma Assert ((C and Tag_Mask) = Tag (K, E, Client_ISN, T));
      pragma Assert ((Shift_Right (C, Index_Shift) and Index_Mask) = Index_For (MSS));
      pragma Assert ((Counter (Now) - T) mod Counter_Steps <= Periods_Valid);
   end Lemma_Round_Trip;

   procedure Lemma_Expired (K : SipHash.Key; E : TCP_Isn.Endpoints; Client_ISN : Seq;
                            Sent, Now : Unsigned_64; MSS : Unsigned_32)
   is
      T : constant Unsigned_32 := Counter (Sent);
      C : constant Unsigned_32 := Cookie_Word (K, E, Client_ISN, Sent, MSS);
   begin
      pragma Assert (Unsigned_32 (Make (K, E, Client_ISN, Sent, MSS)) = C);
      pragma Assert (Shift_Right (C, Counter_Shift) = T);
      pragma Assert ((Counter (Now) - T) mod Counter_Steps in Periods_Valid + 1 .. Counter_Steps - 1);
   end Lemma_Expired;
end TCP_Syn_Cookies;
