pragma Ada_2022;
with I2C_Transfer_Wire;
package I2C_Channel_Wire with Pure, SPARK_Mode is
   package W renames I2C_Transfer_Wire;
   subtype Word is W.Word;
   use type Word;
   Maximum_Payload : constant W.Capacity := W.Capacity'Last;
   -- Small copied channel metadata. Payload bytes live in the persistent
   -- single-writer areas; there are no peer-provided pointers or grant IDs.
   Header_Bytes : constant := 32;
   type Raw_Request is record
      Counts, Deadline, Reserved_A, Reserved_B : Word := 0;
   end record with Size => Header_Bytes * 8, Alignment => 8;
   for Raw_Request use record
      Counts at 0 range 0 .. 63;
      Deadline at 8 range 0 .. 63;
      Reserved_A at 16 range 0 .. 63;
      Reserved_B at 24 range 0 .. 63;
   end record;
   type Request is record
      Write_Count, Read_Count : W.Count;
      Limit : W.Millisecond;
   end record;
   function Fits (Item : Request; Available : W.Capacity) return Boolean is
     (Word (Item.Write_Count) + Word (Item.Read_Count) in 1 .. Word (Available));
   type Decode_Status is (Decoded, Reserved_Header, Reserved_Counts, Invalid_Size, Infinite_Deadline);
   type Decoding (Status : Decode_Status := Invalid_Size) is record
      case Status is
         when Decoded => Value : Request;
         when others => null;
      end case;
   end record;
   function Decode (Raw : Raw_Request; Available : W.Capacity) return Decoding
   with Post => (if Decode'Result.Status = Decoded then Fits (Decode'Result.Value, Available));
   function Encode (Item : Request; Available : W.Capacity) return Raw_Request
   with Pre => Fits (Item, Available),
        Post => Encode'Result.Reserved_A = 0 and then Encode'Result.Reserved_B = 0
          and then Decode (Encode'Result, Available).Status = Decoded
          and then Decode (Encode'Result, Available).Value = Item;
   -- Zero is an expired but structurally finite deadline. The controller owner
   -- must check its clock/deadline before any FIFO/target access. Forever is
   -- refused, not replaced with a fresh budget. Target and bus speed come only
   -- from the admitted endpoint/channel badge, never this metadata.
end I2C_Channel_Wire;
