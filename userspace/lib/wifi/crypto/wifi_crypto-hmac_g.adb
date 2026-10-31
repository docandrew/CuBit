--  HMAC, generic over a streaming hash (body). See the spec.

package body WiFi_Crypto.HMAC_G with
   SPARK_Mode => On
is
   pragma Warnings
     (GNATprove, Off, "is set by ""Fin*"" but not used after the call",
      Reason => "a hash or HMAC state is dead after its final step");

   Inner_Pad_Byte : constant Byte := 16#36#;
   Outer_Pad_Byte : constant Byte := 16#5C#;

   subtype Block_Index is N32 range 0 .. Block_Bytes - 1;
   subtype Block is Byte_Seq (Block_Index);

   procedure Set_Key (KS : out Keyed_State; K : Byte_Seq) is
      IPad : Block := (others => Inner_Pad_Byte);
      OPad : Block := (others => Outer_Pad_Byte);
      Key  : Block := (others => 0);
      Key_Digest : Digest;
      Ctx  : Context;
   begin
      --  Key longer than block size: hash it first
      if K'Length > Block_Bytes then
         Init (Ctx);
         Update (Ctx, K);
         Final (Ctx, Key_Digest);
         Key (Digest_Index) := Key_Digest;
      elsif K'Length > 0 then
         Key (0 .. N32 (K'Length) - 1) := K;
      end if;

      for I in Block_Index loop
         IPad (I) := IPad (I) xor Key (I);
         OPad (I) := OPad (I) xor Key (I);
      end loop;

      Init (KS.Inner);
      Update (KS.Inner, IPad);
      Init (KS.Outer);
      Update (KS.Outer, OPad);
   end Set_Key;

   procedure Start (MS : out Message_State; KS : Keyed_State) is
   begin
      MS := (Inner => KS.Inner, Outer => KS.Outer);
   end Start;

   procedure Absorb (MS : in out Message_State; Data : Byte_Seq) is
   begin
      Update (MS.Inner, Data);
   end Absorb;

   procedure Finish (MS : in out Message_State; Output : out Digest) is
      Inner_Hash : Digest;
   begin
      --  Inner hash: H(ipad || message); outer: H(opad || inner hash)
      Final (MS.Inner, Inner_Hash);
      Update (MS.Outer, Inner_Hash);
      Final (MS.Outer, Output);
   end Finish;

   procedure Compute (Output : out Digest; M : Byte_Seq; K : Byte_Seq) is
      KS : Keyed_State;
      MS : Message_State;
   begin
      Set_Key (KS, K);
      Start (MS, KS);
      Absorb (MS, M);
      Finish (MS, Output);
   end Compute;

end WiFi_Crypto.HMAC_G;
