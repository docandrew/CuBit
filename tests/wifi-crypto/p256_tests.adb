with Interfaces; use Interfaces;
with SPARKNaCl; use SPARKNaCl;
with SPARKTLSCrypto.HKDF;
with SPARKTLSCrypto.Hashing.SHA256;
with SPARKTLSCrypto.P256; use SPARKTLSCrypto.P256;
with WiFi_Crypto.P256_Field; use WiFi_Crypto.P256_Field;
with SPARKTLSCrypto.P256.Point; use SPARKTLSCrypto.P256.Point;
with WiFi_Crypto.P256_SSWU; use WiFi_Crypto.P256_SSWU;
with Checks; use Checks;

package body P256_Tests is

   Field_Bytes : constant := 32;
   Point_Bytes : constant := 2 * Field_Bytes;

   H2C_DST : constant String := "QUUX-V01-CS02-with-P256_XMD:SHA-256_SSWU_RO_";
   XMD_DST : constant String := "QUUX-V01-CS02-with-expander-SHA256-128";

   function Repeat_Char (C : Character; N : Natural) return String is
     [1 .. N => C];

   function FE (Hex_Value : String) return P256_FE is
      R : P256_FE;
   begin
      Bytes_To_FE (R, Hex (Hex_Value));
      return R;
   end FE;

   function Bytes (A : P256_FE) return Byte_Seq is
      R : Byte_Seq (0 .. Field_Bytes - 1);
   begin
      FE_To_Bytes (R, A);
      return R;
   end Bytes;

   function Affine_Bytes (X, Y : P256_FE) return Byte_Seq is (Bytes (X) & Bytes (Y));

   ---------------------------------------------------------------
   --  Field helpers
   ---------------------------------------------------------------

   procedure Field_Cases is
      Three : constant P256_FE := FE (Repeat_Char ('0', 62) & "03");
      R     : P256_FE;
      Is_Square : U32;
      P_Minus_1 : constant String :=
        "ffffffff00000001000000000000000000000000fffffffffffffffffffffffe";

      procedure Legendre_Case (Name : String; A : String; Expected : Legendre_Symbol) is
      begin
         Check ("Legendre (" & Name & ")", Legendre_F256 (FE (A)) = Expected);
      end Legendre_Case;

      function Small (V : Natural) return String is
         Digit : constant String := "0123456789abcdef";
      begin
         return Repeat_Char ('0', 62) & Digit (V / 16 + 1) & Digit (V mod 16 + 1);
      end Small;
   begin
      --  Expected values from Python's pow (x, e, p).
      Check ("Inv (3)", Bytes (Inv_F256 (Three)),
             Hex ("aaaaaaaa00000000aaaaaaaaaaaaaaaaaaaaaaab555555555555555555555555"));
      Check ("3 * Inv (3) = 1", FE_Equal (Mul_F256 (Three, Inv_F256 (Three)), FE_One) = 1);
      Check ("Inv (0) = 0", FE_Zero_Flag (Inv_F256 (FE_Zero)) = 1);

      Sqrt_F256 (R, Is_Square, FE (Small (4)));
      Check ("Sqrt (4) is square", Is_Square = 1);
      Check ("Sqrt (4) = 2", Bytes (R), Hex (Small (2)));
      Sqrt_F256 (R, Is_Square, FE (Small (2)));
      Check ("Sqrt (2) is square", Is_Square = 1);
      Check ("Sqrt (2)", Bytes (R),
             Hex ("af8bbdfe8cdd5577acbf345b543d28cf402f4e94d3865b97ea0787f2d3aa5d22"));
      Sqrt_F256 (R, Is_Square, FE (Small (3)));
      Check ("Sqrt (3) is not square", Is_Square = 0);
      Sqrt_F256 (R, Is_Square, FE_Zero);
      Check ("Sqrt (0) = 0", Is_Square = 1 and FE_Zero_Flag (R) = 1);

      Legendre_Case ("0", Small (0), 0);
      Legendre_Case ("1", Small (1), 1);
      Legendre_Case ("2", Small (2), 1);
      Legendre_Case ("3", Small (3), -1);
      Legendre_Case ("5", Small (5), 1);
      Legendre_Case ("7", Small (7), 1);
      Legendre_Case ("10", Small (10), 1);
      Legendre_Case ("p - 1", P_Minus_1, -1);

      Check ("sgn0 (1) = 1", Sgn0_F256 (FE_One) = 1);
      Check ("sgn0 (2) = 0", Sgn0_F256 (FE (Small (2))) = 0);
      Check ("sgn0 (p - 1) = 0", Sgn0_F256 (FE (P_Minus_1)) = 0);
   end Field_Cases;

   ---------------------------------------------------------------
   --  RFC 9380
   ---------------------------------------------------------------

   procedure XMD_Case (Msg : String; Length : Positive; Expected : String) is
      Output : Byte_Seq (0 .. N32 (Length) - 1);
   begin
      Expand_Message_XMD (Output, Text (Msg), Text (XMD_DST));
      Check ("RFC 9380 K.1 expand_message_xmd len" & Length'Image & " msg "
             & Msg (Msg'First .. Natural'Min (Msg'Last, Msg'First + 15)),
             Output, Hex (Expected));
   end XMD_Case;

   procedure H2C_Case (Name, Msg, U0, U1, Q0, Q1, P : String) is
      Uniform : Byte_Seq (0 .. 2 * Wide_Bytes - 1);
      U_0, U_1 : P256_FE;
      X, Y : P256_FE;
      R : P256_Jacobian;
   begin
      Expand_Message_XMD (Uniform, Text (Msg), Text (H2C_DST));
      Reduce_Wide (U_0, Uniform (0 .. Wide_Bytes - 1));
      Reduce_Wide (U_1, Uniform (Wide_Bytes .. Uniform'Last));
      Check ("RFC 9380 J.1.1 u[0] msg " & Name, Bytes (U_0), Hex (U0));
      Check ("RFC 9380 J.1.1 u[1] msg " & Name, Bytes (U_1), Hex (U1));
      Map_To_Curve (X, Y, U_0);
      Check ("RFC 9380 J.1.1 Q0 msg " & Name, Affine_Bytes (X, Y), Hex (Q0));
      Map_To_Curve (X, Y, U_1);
      Check ("RFC 9380 J.1.1 Q1 msg " & Name, Affine_Bytes (X, Y), Hex (Q1));
      Hash_To_Curve (R, Text (Msg), Text (H2C_DST));
      Check ("RFC 9380 J.1.1 P msg " & Name, Affine_Bytes (R.X, R.Y), Hex (P));
   end H2C_Case;

   --  map_to_curve lands on the curve, and the doubling case of
   --  Map_Pair_To_Curve agrees with the generic addition, for many u.
   procedure Sweep is
      Count : constant := 256;
      Uniform : Byte_Seq (0 .. Wide_Bytes - 1);
      U : P256_FE;
      X, Y : P256_FE;
      Seed : Byte_Seq (0 .. 3);
      All_On_Curve : Boolean := True;
      Sqrt_Agrees  : Boolean := True;
      R : P256_FE;
      Is_Square : U32;
      Doubled, Expected : P256_Jacobian;
   begin
      for I in 0 .. Count - 1 loop
         Seed := [0, 0, Byte (I / 256), Byte (I mod 256)];
         Expand_Message_XMD (Uniform, Seed, Text ("CuBit-sweep"));
         Reduce_Wide (U, Uniform);
         Map_To_Curve (X, Y, U);
         All_On_Curve := All_On_Curve and On_Curve (X, Y) = 1;
         Sqrt_F256 (R, Is_Square, U);
         Sqrt_Agrees := Sqrt_Agrees
           and (Is_Square = 1) = (Legendre_F256 (U) /= -1)
           and (Is_Square = 0 or else FE_Equal (Square_F256 (R), U) = 1);
      end loop;
      Check ("map_to_curve on curve for" & Count'Image & " u", All_On_Curve);
      Check ("Sqrt self-check = Legendre for" & Count'Image & " u", Sqrt_Agrees);

      --  SSWU (u) + SSWU (u) takes the masked doubling.
      Map_Pair_To_Curve (Doubled, U, U);
      Map_To_Curve (X, Y, U);
      Expected := (X => X, Y => Y, Z => FE_One);
      P256_Double (Expected);
      P256_To_Affine (Expected);
      Check ("Map_Pair_To_Curve (u, u) = 2 * SSWU (u)",
             Affine_Bytes (Doubled.X, Doubled.Y), Affine_Bytes (Expected.X, Expected.Y));
   end Sweep;

   ---------------------------------------------------------------
   --  SAE hash-to-element, IEEE Std 802.11-2020 J.10 (group 19), as
   --  hostap's sae_tests runs it: PT from the password, then
   --  PWE = val * PT for the two addresses.
   ---------------------------------------------------------------

   procedure SAE_H2E_Case is
      use SPARKTLSCrypto.HKDF;
      SSID       : constant Byte_Seq := Text ("byteme");
      Password   : constant Byte_Seq := Text ("mekmitasdigoat");
      Identifier : constant Byte_Seq := Text ("psk4internet");
      Addr_Max   : constant Byte_Seq := Hex ("000b6bd90246");  --  MAX (addresses)
      Addr_Min   : constant Byte_Seq := Hex ("00095b66ec1e");
      --  The group order n minus 1, big-endian.
      Order_Minus_1 : constant Byte_Seq :=
        Hex ("ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632550");

      Seed : SPARKTLSCrypto.Hashing.SHA256.Digest;
      Wide : OKM_Seq (0 .. Wide_Bytes - 1);
      Wide_Bytes_Copy : Byte_Seq (0 .. Wide_Bytes - 1);
      U1, U2 : P256_FE;
      PT : P256_Jacobian;
      Val : SPARKTLSCrypto.Hashing.SHA256.Digest;
      Ge  : Boolean;
      Borrow, Carry : Integer;
      Encoded : Byte_Seq (0 .. Point_Bytes);
      Uncompressed_Tag : constant := 1;

      procedure Expand (Label : String; U : out P256_FE) is
      begin
         Expand (Wide, Seed, Text (Label));
         for I in Wide'Range loop
            Wide_Bytes_Copy (I) := Wide (I);
         end loop;
         Reduce_Wide (U, Wide_Bytes_Copy);
      end Expand;
   begin
      --  pwd-seed = HKDF-Extract (ssid, password || identifier)
      Extract (Seed, Password & Identifier, SSID);
      Expand ("SAE Hash to Element u1 P1", U1);
      Expand ("SAE Hash to Element u2 P2", U2);
      Map_Pair_To_Curve (PT, U1, U2);
      --  Intermediate from a Python model of 12.4.4.2.3 that reproduces
      --  the J.10 PWE below; not itself a published value.
      Check ("SAE H2E PT (model)", Affine_Bytes (PT.X, PT.Y), Hex
        ("b6e38c98750c684b5d17c3d8c9a4100b39931279187ca6cced5f37ef46ddfa97"
         & "5687e972e50f73e3898861e7edad21bea7d5f622df88243bb804920ae8e647fa"));

      --  val = HKDF-Extract (0^32, MAX || MIN) mod (n - 1) + 1. val < 2^256
      --  < 2 (n - 1), so one conditional subtraction reduces it (public
      --  data: addresses only).
      Extract (Val, Addr_Max & Addr_Min, Byte_Seq'(0 .. 31 => 0));
      Ge := True;
      for I in Val'Range loop
         if Val (I) /= Order_Minus_1 (I) then
            Ge := Val (I) > Order_Minus_1 (I);
            exit;
         end if;
      end loop;
      if Ge then
         Borrow := 0;
         for I in reverse Val'Range loop
            declare
               D : constant Integer := Integer (Val (I)) - Integer (Order_Minus_1 (I)) - Borrow;
            begin
               Val (I) := Byte ((D + 256) mod 256);
               Borrow := (if D < 0 then 1 else 0);
            end;
         end loop;
      end if;
      Carry := 1;
      for I in reverse Val'Range loop
         declare
            S : constant Integer := Integer (Val (I)) + Carry;
         begin
            Val (I) := Byte (S mod 256);
            Carry := S / 256;
         end;
      end loop;
      Check ("SAE H2E val (model)", Val, Hex
        ("bb7f9cac5aa8b72c02b5daacc2771abe74e72604612295eca2ce18363ae9a927"));

      P256_Mul (PT, Val, Val'Length);
      P256_To_Affine (PT);
      P256_Encode (Encoded, PT);
      Check ("802.11-2020 J.10 SAE H2E PWE (group 19)",
             Encoded (Uncompressed_Tag .. Encoded'Last), Hex
        ("c93049b9e64000f848201649e999f2b5c22dea69b5632c9df4d633b8aa1f6c1e"
         & "73634e94b53d82e7383a8d258199d9dc1a5ee8269d060382ccbf33e614ff59a0"));
   end SAE_H2E_Case;

   procedure Run is
   begin
      Field_Cases;

      --  RFC 9380 K.1 (expand_message_xmd, SHA-256)
      XMD_Case ("", 32,
        "68a985b87eb6b46952128911f2a4412bbc302a9d759667f87f7a21d803f07235");
      XMD_Case ("abc", 32,
        "d8ccab23b5985ccea865c6c97b6e5b8350e794e603b4b97902f53a8a0d605615");
      XMD_Case ("abcdef0123456789", 32,
        "eff31487c770a893cfb36f912fbfcbff40d5661771ca4b2cb4eafe524333f5c1");
      XMD_Case ("q128_" & Repeat_Char ('q', 128), 32,
        "b23a1d2b4d97b2ef7785562a7e8bac7eed54ed6e97e29aa51bfe3f12ddad1ff9");
      XMD_Case ("a512_" & Repeat_Char ('a', 512), 32,
        "4623227bcc01293b8c130bf771da8c298dede7383243dc0993d2d94823958c4c");
      XMD_Case ("", 128,
        "af84c27ccfd45d41914fdff5df25293e221afc53d8ad2ac06d5e3e29485dadbe"
          & "e0d121587713a3e0dd4d5e69e93eb7cd4f5df4cd103e188cf60cb02edc3edf18"
          & "eda8576c412b18ffb658e3dd6ec849469b979d444cf7b26911a08e63cf31f9dc"
          & "c541708d3491184472c2c29bb749d4286b004ceb5ee6b9a7fa5b646c993f0ced");
      XMD_Case ("abc", 128,
        "abba86a6129e366fc877aab32fc4ffc70120d8996c88aee2fe4b32d6c7b6437a"
          & "647e6c3163d40b76a73cf6a5674ef1d890f95b664ee0afa5359a5c4e07985635"
          & "bbecbac65d747d3d2da7ec2b8221b17b0ca9dc8a1ac1c07ea6a1e60583e2cb00"
          & "058e77b7b72a298425cd1b941ad4ec65e8afc50303a22c0f99b0509b4c895f40");
      XMD_Case ("abcdef0123456789", 128,
        "ef904a29bffc4cf9ee82832451c946ac3c8f8058ae97d8d629831a74c6572bd9"
          & "ebd0df635cd1f208e2038e760c4994984ce73f0d55ea9f22af83ba4734569d4b"
          & "c95e18350f740c07eef653cbb9f87910d833751825f0ebefa1abe5420bb52be1"
          & "4cf489b37fe1a72f7de2d10be453b2c9d9eb20c7e3f6edc5a60629178d9478df");
      XMD_Case ("q128_" & Repeat_Char ('q', 128), 128,
        "80be107d0884f0d881bb460322f0443d38bd222db8bd0b0a5312a6fedb49c1bb"
          & "d88fd75d8b9a09486c60123dfa1d73c1cc3169761b17476d3c6b7cbbd727acd0"
          & "e2c942f4dd96ae3da5de368d26b32286e32de7e5a8cb2949f866a0b80c58116b"
          & "29fa7fabb3ea7d520ee603e0c25bcaf0b9a5e92ec6a1fe4e0391d1cdbce8c68a");
      XMD_Case ("a512_" & Repeat_Char ('a', 512), 128,
        "546aff5444b5b79aa6148bd81728704c32decb73a3ba76e9e75885cad9def1d0"
          & "6d6792f8a7d12794e90efed817d96920d728896a4510864370c207f99bd4a608"
          & "ea121700ef01ed879745ee3e4ceef777eda6d9e5e38b90c86ea6fb0b36504ba4"
          & "a45d22e86f6db5dd43d98a294bebb9125d5b794e9d2a81181066eb954966a487");

      --  RFC 9380 J.1.1 (P256_XMD:SHA-256_SSWU_RO_)
      H2C_Case ("", "",
        U0 => "ad5342c66a6dd0ff080df1da0ea1c04b96e0330dd89406465eeba11582515009",
        U1 => "8c0f1d43204bd6f6ea70ae8013070a1518b43873bcd850aafa0a9e220e2eea5a",
        Q0 => "ab640a12220d3ff283510ff3f4b1953d09fad35795140b1c5d64f313967934d5"
          & "dccb558863804a881d4fff3455716c836cef230e5209594ddd33d85c565b19b1",
        Q1 => "51cce63c50d972a6e51c61334f0f4875c9ac1cd2d3238412f84e31da7d980ef5"
          & "b45d1a36d00ad90e5ec7840a60a4de411917fbe7c82c3949a6e699e5a1b66aac",
        P  => "2c15230b26dbc6fc9a37051158c95b79656e17a1a920b11394ca91c44247d3e4"
          & "8a7a74985cc5c776cdfe4b1f19884970453912e9d31528c060be9ab5c43e8415");
      H2C_Case ("abc", "abc",
        U0 => "afe47f2ea2b10465cc26ac403194dfb68b7f5ee865cda61e9f3e07a537220af1",
        U1 => "379a27833b0bfe6f7bdca08e1e83c760bf9a338ab335542704edcd69ce9e46e0",
        Q0 => "5219ad0ddef3cc49b714145e91b2f7de6ce0a7a7dc7406c7726c7e373c58cb48"
          & "7950144e52d30acbec7b624c203b1996c99617d0b61c2442354301b191d93ecf",
        Q1 => "019b7cb4efcfeaf39f738fe638e31d375ad6837f58a852d032ff60c69ee3875f"
          & "589a62d2b22357fed5449bc38065b760095ebe6aeac84b01156ee4252715446e",
        P  => "0bb8b87485551aa43ed54f009230450b492fead5f1cc91658775dac4a3388a0f"
          & "5c41b3d0731a27a7b14bc0bf0ccded2d8751f83493404c84a88e71ffd424212e");
      H2C_Case ("abcdef0123456789", "abcdef0123456789",
        U0 => "0fad9d125a9477d55cf9357105b0eb3a5c4259809bf87180aa01d651f53d312c",
        U1 => "b68597377392cd3419d8fcc7d7660948c8403b19ea78bbca4b133c9d2196c0fb",
        Q0 => "a17bdf2965eb88074bc01157e644ed409dac97cfcf0c61c998ed0fa45e79e4a2"
          & "4f1bc80c70d411a3cc1d67aeae6e726f0f311639fee560c7f5a664554e3c9c2e",
        Q1 => "7da48bb67225c1a17d452c983798113f47e438e4202219dd0715f8419b274d66"
          & "b765696b2913e36db3016c47edb99e24b1da30e761a8a3215dc0ec4d8f96e6f9",
        P  => "65038ac8f2b1def042a5df0b33b1f4eca6bff7cb0f9c6c1526811864e544ed80"
          & "cad44d40a656e7aff4002a8de287abc8ae0482b5ae825822bb870d6df9b56ca3");
      H2C_Case ("q128_...", "q128_" & Repeat_Char ('q', 128),
        U0 => "3bbc30446f39a7befad080f4d5f32ed116b9534626993d2cc5033f6f8d805919",
        U1 => "76bb02db019ca9d3c1e02f0c17f8baf617bbdae5c393a81d9ce11e3be1bf1d33",
        Q0 => "c76aaa823aeadeb3f356909cb08f97eee46ecb157c1f56699b5efebddf0e6398"
          & "776a6f45f528a0e8d289a4be12c4fab80762386ec644abf2bffb9b627e4352b1",
        Q1 => "418ac3d85a5ccc4ea8dec14f750a3a9ec8b85176c95a7022f391826794eb5a75"
          & "fd6604f69e9d9d2b74b072d14ea13050db72c932815523305cb9e807cc900aff",
        P  => "4be61ee205094282ba8a2042bcb48d88dfbb609301c49aa8b078533dc65a0b5d"
          & "98f8df449a072c4721d241a3b1236d3caccba603f916ca680f4539d2bfb3c29e");
      H2C_Case ("a512_...", "a512_" & Repeat_Char ('a', 512),
        U0 => "4ebc95a6e839b1ae3c63b847798e85cb3c12d3817ec6ebc10af6ee51adb29fec",
        U1 => "4e21af88e22ea80156aff790750121035b3eefaa96b425a8716e0d20b4e269ee",
        Q0 => "d88b989ee9d1295df413d4456c5c850b8b2fb0f5402cc5c4c7e815412e926db8"
          & "bb4a1edeff506cf16def96afff41b16fc74f6dbd55c2210e5b8f011ba32f4f40",
        Q1 => "a281e34e628f3a4d2a53fa87ff973537d68ad4fbc28d3be5e8d9f6a2571c5a4b"
          & "f6ed88a7aab56a488100e6f1174fa9810b47db13e86be999644922961206e184",
        P  => "457ae2981f70ca85d8e24c308b14db22f3e3862c5ea0f652ca38b5e49cd64bc5"
          & "ecb9f0eadc9aeed232dabc53235368c1394c78de05dd96893eefa62b0f4757dc");

      Sweep;
      SAE_H2E_Case;
   end Run;
end P256_Tests;
