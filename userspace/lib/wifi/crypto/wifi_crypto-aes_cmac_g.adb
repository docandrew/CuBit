--  AES-CMAC (RFC 4493), generic over the AES block cipher (body).


package body WiFi_Crypto.AES_CMAC_G with
   SPARK_Mode => On
is
   pragma Warnings
     (GNATprove, Off, "is set by ""Finish"" but not used after the call",
      Reason => "the MAC state is dead after Finish");

   --  R_128 (RFC 4493 2.3 "const_Rb"): the low byte of x^128 reduced.
   Rb : constant Byte := 16#87#;
   --  10* padding (RFC 4493 2.4, padding (x)).
   Pad_Byte : constant Byte := 16#80#;

   Byte_Bits : constant := 8;
   Top_Bit_Shift : constant := Byte_Bits - 1;

   --  Output := Input << 1 in GF(2^128), reduced by Rb without a branch.
   function Double (Input : Bytes_16) return Bytes_16 is
      Output : Bytes_16;
      Carry  : Byte := 0;
      Top    : constant Byte := Shift_Right (Input (0), Top_Bit_Shift);
   begin
      for I in reverse Bytes_16'Range loop
         Output (I) := Shift_Left (Input (I), 1) or Carry;
         Carry := Shift_Right (Input (I), Top_Bit_Shift);
      end loop;
      --  0 - Top is 16#FF# when the top bit was set, else 0.
      Output (Output'Last) := Output (Output'Last) xor ((0 - Top) and Rb);
      return Output;
   end Double;

   procedure Encipher_Into_X (S : in out State; RK : Round_Keys; Block : Bytes_16)
   is
      Input : Bytes_16;
   begin
      for I in Bytes_16'Range loop
         Input (I) := S.X (I) xor Block (I);
      end loop;
      Cipher (S.X, Input, RK);
   end Encipher_Into_X;

   procedure Start (S : out State; RK : Round_Keys) is
      Zero : constant Bytes_16 := (others => 0);
      L    : Bytes_16;
   begin
      Cipher (L, Zero, RK);
      S := (K1     => Double (L),
            K2     => Double (Double (L)),
            X      => Zero,
            Buffer => Zero,
            Fill   => 0);
   end Start;

   procedure Absorb (S : in out State; RK : Round_Keys; Data : Byte_Seq) is
      Full : Bytes_16;
   begin
      for I in Data'Range loop
         --  A full buffer is not the last block: more data follows. It is
         --  copied out so the block does not alias the state it updates.
         if S.Fill = Block_Bytes then
            Full := S.Buffer;
            Encipher_Into_X (S, RK, Full);
            S.Fill := 0;
         end if;
         S.Buffer (S.Fill) := Data (I);
         S.Fill := S.Fill + 1;
      end loop;
   end Absorb;

   procedure Finish (S : in out State; RK : Round_Keys; Output : out Tag) is
      Last : Bytes_16 := S.Buffer;
   begin
      if S.Fill = Block_Bytes then
         for I in Bytes_16'Range loop
            Last (I) := Last (I) xor S.K1 (I);
         end loop;
      else
         Last (S.Fill) := Pad_Byte;
         for I in S.Fill + 1 .. Bytes_16'Last loop
            Last (I) := 0;
         end loop;
         for I in Bytes_16'Range loop
            Last (I) := Last (I) xor S.K2 (I);
         end loop;
      end if;
      Encipher_Into_X (S, RK, Last);
      Output := S.X;
   end Finish;

   procedure Compute (Output : out Tag; RK : Round_Keys; M : Byte_Seq) is
      S : State;
   begin
      Start (S, RK);
      Absorb (S, RK, M);
      Finish (S, RK, Output);
   end Compute;

end WiFi_Crypto.AES_CMAC_G;
