with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Interfaces;
with Storehouse;
with Storehouse.Canonical;
with Storehouse.Hashing;
with Storehouse.Hex;
with Storehouse.Paths;

--  Hosted checks of the Storehouse core (docs/ccl-build.md): hex names,
--  the canonical encoding against RFC 8949 appendix A and its refusals,
--  SHA-256 known answers, and the path rules.
procedure Core_Tests is
   use Ada.Text_IO;
   use Interfaces;
   use Storehouse;
   use Storehouse.Canonical;

   Failures : Natural := 0;
   Checks : Natural := 0;
   Random_Values : constant := 200_000;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL " & What);
      end if;
   end Check;

   function Bytes_Of (Hex_Text : String) return Byte_Array is
      Result : Byte_Array (1 .. Hex_Text'Length / 2);
   begin
      for I in Result'Range loop
         Result (I) := Byte'Value ("16#" & Hex_Text (Hex_Text'First + 2 * (I - 1) .. Hex_Text'First + 2 * I - 1) & "#");
      end loop;
      return Result;
   end Bytes_Of;

   function Image (Data : Byte_Array) return String is
      Digits_Text : constant String := "0123456789abcdef";
      Result : String (1 .. 2 * Data'Length);
   begin
      for I in Data'Range loop
         Result (2 * (I - Data'First) + 1) := Digits_Text (Natural (Data (I) / 16) + 1);
         Result (2 * (I - Data'First) + 2) := Digits_Text (Natural (Data (I) mod 16) + 1);
      end loop;
      return Result;
   end Image;

   procedure Encodes (Major : Major_Type; Value : Unsigned_64; Expected : String) is
      Got : constant Byte_Array := Head (Major, Value);
      M : Major_Type;
      V : Unsigned_64;
      Next : Positive;
      Valid : Boolean;
   begin
      Check (Image (Got) = Expected, "head " & Major'Image & Value'Image & " = " & Image (Got) & ", want " & Expected);
      Read_Head (Got, 1, M, V, Next, Valid);
      Check (Valid and then M = Major and then V = Value and then Next = Got'Length + 1,
             "read back " & Expected);
   end Encodes;

   procedure Refuses (Hex_Text : String; Why : String) is
      Data : constant Byte_Array := Bytes_Of (Hex_Text);
      M : Major_Type;
      V : Unsigned_64;
      Next : Positive;
      Valid : Boolean;
   begin
      Read_Head (Data, 1, M, V, Next, Valid);
      Check (not Valid, "refuse " & Hex_Text & " (" & Why & ")");
   end Refuses;

   function Text_Bytes (S : String) return Byte_Array is
      Result : Byte_Array (1 .. S'Length);
   begin
      for I in S'Range loop
         Result (I - S'First + 1) := Character'Pos (S (I));
      end loop;
      return Result;
   end Text_Bytes;

   package Random_64 is new Ada.Numerics.Discrete_Random (Unsigned_64);
   Generator : Random_64.Generator;
begin
   --  RFC 8949 appendix A.
   Encodes (Unsigned_Integer, 0, "00");
   Encodes (Unsigned_Integer, 23, "17");
   Encodes (Unsigned_Integer, 24, "1818");
   Encodes (Unsigned_Integer, 100, "1864");
   Encodes (Unsigned_Integer, 1000, "1903e8");
   Encodes (Unsigned_Integer, 1_000_000, "1a000f4240");
   Encodes (Unsigned_Integer, 1_000_000_000_000, "1b000000e8d4a51000");
   Encodes (Unsigned_Integer, Unsigned_64'Last, "1bffffffffffffffff");
   Encodes (Text_String, 0, "60");
   Encodes (Text_String, 4, "64");
   Encodes (Byte_String, 4, "44");
   Encodes (Array_Of, 0, "80");
   Encodes (Array_Of, 3, "83");
   Encodes (Array_Of, 25, "9819");
   --  Boundaries of each length.
   Encodes (Unsigned_Integer, 255, "18ff");
   Encodes (Unsigned_Integer, 256, "190100");
   Encodes (Unsigned_Integer, 65_535, "19ffff");
   Encodes (Unsigned_Integer, 65_536, "1a00010000");
   Encodes (Unsigned_Integer, 16#FFFF_FFFF#, "1affffffff");
   Encodes (Unsigned_Integer, 16#1_0000_0000#, "1b0000000100000000");

   --  Non-shortest forms are second encodings: refused.
   Refuses ("1817", "23 in one byte");
   Refuses ("190017", "23 in two bytes");
   Refuses ("1900ff", "255 in two bytes");
   Refuses ("1a0000ffff", "65535 in four bytes");
   Refuses ("1b00000000ffffffff", "a 32-bit value in eight bytes");
   --  Outside the profile.
   Refuses ("20", "negative integer");
   Refuses ("a0", "map");
   Refuses ("c0", "tag");
   Refuses ("f4", "simple value");
   Refuses ("f93c00", "float");
   Refuses ("5f", "indefinite byte string");
   Refuses ("9f", "indefinite array");
   Refuses ("1c", "reserved additional information");
   --  Truncated.
   Refuses ("19ff", "two-byte argument cut short");
   Refuses ("1b000000e8d4a510", "eight-byte argument cut short");

   --  Round trip of random values of every length.
   Random_64.Reset (Generator, 1);
   for I in 1 .. Random_Values loop
      declare
         Raw : constant Unsigned_64 := Random_64.Random (Generator);
         Value : constant Unsigned_64 := Shift_Right (Raw, Natural (Raw mod 64));
         Major : constant Major_Type := Major_Type'Val (I mod 4);
         Got : constant Byte_Array := Head (Major, Value);
         M : Major_Type;
         V : Unsigned_64;
         Next : Positive;
         Valid : Boolean;
      begin
         Read_Head (Got, 1, M, V, Next, Valid);
         if not (Valid and then M = Major and then V = Value and then Got'Length = Length_Of (Value)) then
            Check (False, "random round trip" & Value'Image);
         end if;
      end;
   end loop;
   Checks := Checks + 1;

   --  SHA-256 known answers (FIPS 180-4 examples).
   Check (Hex.Image (Hashing.Of_Bytes (Text_Bytes (""))) =
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", "sha256 empty");
   Check (Hex.Image (Hashing.Of_Bytes (Text_Bytes ("abc"))) =
          "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad", "sha256 abc");
   declare
      Million : constant Byte_Array (1 .. 1_000_000) := [others => Character'Pos ('a')];
      Hash : Hashing.Context;
      Pieces : Digest;
   begin
      Check (Hex.Image (Hashing.Of_Bytes (Million)) =
             "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0", "sha256 million a");
      --  Uneven pieces give the same identity as one call.
      Hashing.Start (Hash);
      Hashing.Add (Hash, Million (1 .. 1));
      Hashing.Add (Hash, Million (2 .. 70_001));
      Hashing.Add (Hash, Million (70_002 .. 1_000_000));
      Hashing.Finish (Hash, Pieces);
      Check (Pieces = Hashing.Of_Bytes (Million), "sha256 in pieces");
   end;

   --  Hex names: one spelling per identity.
   declare
      Name : constant Hex.Text := "2a08982cf8a935d57b31d69bddb0d0bbaa53774bb2fb96e8f98e7990a53bae1f";
   begin
      Check (Hex.Is_Name (Name) and then Hex.Image (Hex.Parse (Name)) = Name, "hex round trip");
      Check (not Hex.Is_Name ("2A08982cf8a935d57b31d69bddb0d0bbaa53774bb2fb96e8f98e7990a53bae1f"), "hex uppercase");
      Check (not Hex.Is_Name (Name (1 .. 63)), "hex short");
   end;

   --  Paths.
   Check (Paths.Is_Relative ("userspace/apps/sleep/main.adb"), "relative path");
   Check (Paths.Is_Relative ("a"), "one component");
   Check (Paths.Is_Relative ("lib/.hidden/x..y"), "dots inside names");
   Check (not Paths.Is_Relative (""), "empty path");
   Check (not Paths.Is_Relative ("/etc/passwd"), "absolute path");
   Check (not Paths.Is_Relative ("a/../b"), "dot-dot component");
   Check (not Paths.Is_Relative (".."), "dot-dot alone");
   Check (not Paths.Is_Relative ("./a"), "dot component");
   Check (not Paths.Is_Relative ("a/."), "trailing dot component");
   Check (not Paths.Is_Relative ("a//b"), "empty component");
   Check (not Paths.Is_Relative ("a/"), "trailing separator");
   Check (not Paths.Is_Relative ("a b"), "space");
   Check (not Paths.Is_Relative ("a\b"), "backslash");
   Check (Paths.Is_Literal ("-Wl,-z,stack-size=16777216"), "literal switch");
   Check (not Paths.Is_Literal ("-I/usr/include"), "literal with a path");
   Check (not Paths.Is_Literal ("-ffile-prefix-map=/home=."), "literal with a host path");
   Check (not Paths.Is_Literal ("include/x.h"), "literal with a relative path");

   Put_Line ("core:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures > 0 then
      raise Program_Error with "core tests failed";
   end if;
end Core_Tests;
