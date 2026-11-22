with SPARKNaCl;

package body Storehouse.Hashing with SPARK_Mode is
   package SHA256 renames SPARKTLSCrypto.Hashing.SHA256;
   use type SPARKNaCl.N32;

   --  Bytes handed to SHA-256 per call; any size works, this bounds the
   --  copy into the crate's index type.
   Piece_Bytes : constant := 65_536;

   procedure Start (Hash : out Context) is
   begin
      SHA256.Init (Hash.State);
   end Start;

   procedure Add (Hash : in out Context; Data : Byte_Array) is
      First : Positive;
      Count : Natural;
      Piece : SPARKNaCl.Byte_Seq (0 .. Piece_Bytes - 1) := [others => 0];
   begin
      --  An empty array's bounds need not lie in Positive.
      if Data'Length = 0 then
         return;
      end if;
      First := Data'First;
      loop
         pragma Loop_Invariant (First in Data'Range);
         pragma Loop_Variant (Increases => First);
         Count := Natural'Min (Piece_Bytes, Data'Last - First + 1);
         for I in 0 .. Count - 1 loop
            Piece (SPARKNaCl.N32 (I)) := Data (First + I);
         end loop;
         SHA256.Update (Hash.State, Piece (0 .. SPARKNaCl.N32 (Count - 1)));
         exit when Data'Last - First < Piece_Bytes;
         First := First + Piece_Bytes;
      end loop;
   end Add;

   procedure Finish (Hash : in out Context; Result : out Digest) is
      Output : SHA256.Digest;
   begin
      SHA256.Final (Hash.State, Output);
      Result := [for I in Digest_Index => Output (SPARKNaCl.N32 (I - 1))];
   end Finish;

   function Of_Bytes (Data : Byte_Array) return Digest is
      Hash : Context;
      Result : Digest;
   begin
      Start (Hash);
      Add (Hash, Data);
      Finish (Hash, Result);
      return Result;
   end Of_Bytes;

end Storehouse.Hashing;
