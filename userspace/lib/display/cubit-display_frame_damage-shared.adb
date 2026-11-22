pragma Ada_2022;
package body CuBit.Display_Frame_Damage.Shared with SPARK_Mode => Off is
   function Snapshot_And_Decode (Frame : Unsigned_64; Area : DP.Rectangle) return Decoding is
      Copy : Word_Array;
   begin
      for I in Word_Array'Range loop
         Copy (I) := Read (I);
      end loop;
      return Decode (Copy, Frame, Area);
   end Snapshot_And_Decode;

   function Decode_At (Location : System.Address; Frame : Unsigned_64; Area : DP.Rectangle)
     return Decoding
   is
      Words : array (Word_Index) of Unsigned_64
        with Import, Volatile_Components, Address => Location;
      function Read (Index : Word_Index) return Unsigned_64 is (Words (Index));
      function Decode_Record is new Snapshot_And_Decode (Read);
   begin
      return Decode_Record (Frame, Area);
   end Decode_At;
end CuBit.Display_Frame_Damage.Shared;
