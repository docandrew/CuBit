pragma Ada_2022;
with Interfaces;
-- Wire representation only: no namespace, interpreter, resource authority or
-- startup capability dependency. Labels are scoped to the backend endpoint.
package ACPI_Region_Protocol with SPARK_Mode, Pure is
   use Interfaces;
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   type Packet is record
      Label : Unsigned_32 := 0;
      Length : Unsigned_8 := 4;
      Flags : Unsigned_8 := 0;
      Reserved : Unsigned_16 := 0;
      Data : Words := [others => 0];
   end record;
   Read_Operation : constant Unsigned_32 := 0;
   Write_Operation : constant Unsigned_32 := 1;
   Operation_Reply : constant Unsigned_32 := 16#F002#;
   type Outcome is (Denied, Malformed, Done, Indeterminate, Backend_Fault);
   type Register_ID is new Unsigned_64;
   No_Register : constant Register_ID := 0;
   type Epoch_Number is new Unsigned_64;
   No_Epoch : constant Epoch_Number := 0;
   type Register_Request is record
      Epoch : Epoch_Number := No_Epoch;
      Register : Register_ID := No_Register;
      For_Write : Boolean := False;
      Value : Unsigned_64 := 0;
   end record;
   function Valid_Request (Item : Register_Request) return Boolean is
     (Item.Epoch /= No_Epoch and then Item.Register /= No_Register
      and then (Item.For_Write or else Item.Value = 0));
   function Encode (Item : Register_Request) return Packet is
     ((Label => (if Item.For_Write then Write_Operation else Read_Operation),
      Data => [Unsigned_64 (Item.Epoch), Unsigned_64 (Item.Register), Item.Value, 0],
      others => <>))
     with Pre => Valid_Request (Item),
       Post => Encode'Result.Label = (if Item.For_Write then Write_Operation else Read_Operation)
         and then Encode'Result.Length = 4 and then Encode'Result.Flags = 0
         and then Encode'Result.Reserved = 0
         and then Encode'Result.Data = Words'
           [Unsigned_64 (Item.Epoch), Unsigned_64 (Item.Register), Item.Value, 0];
   -- A malformed response may follow an executed write. Callers must retain
   -- uncertainty, never turn Valid=False into permission to replay a request.
   type Decoded_Reply (Valid : Boolean := False) is record
      case Valid is
         when True => Status : Outcome; Value : Unsigned_64;
         when False => null;
      end case;
   end record;
   Half_Mask : constant Unsigned_64 := 16#FFFF_FFFF#;
   Half_Bits : constant := 32;
   function Valid_Reply (Item : Packet; For_Write : Boolean) return Boolean is
     (Item.Label = Operation_Reply and then Item.Length = 4
      and then Item.Flags = 0 and then Item.Reserved = 0
      and then Item.Data (0) <= Outcome'Pos (Outcome'Last)
      and then Item.Data (1) <= Half_Mask and then Item.Data (2) <= Half_Mask
      and then Item.Data (3) = 0
      and then (if Item.Data (0) /= Outcome'Pos (Done) or else For_Write then
        Item.Data (1) = 0 and then Item.Data (2) = 0));
   function Decode (Item : Packet; For_Write : Boolean) return Decoded_Reply is
     (if Valid_Reply (Item, For_Write) then
        (Valid => True, Status => Outcome'Val (Item.Data (0)),
         Value => Item.Data (1) or Shift_Left (Item.Data (2), Half_Bits))
      else (Valid => False))
     with Post => Decode'Result.Valid = Valid_Reply (Item, For_Write)
       and then (if Decode'Result.Valid then
         Decode'Result.Status = Outcome'Val (Item.Data (0))
         and then Decode'Result.Value = (Item.Data (1) or Shift_Left (Item.Data (2), Half_Bits))
         and then (Decode'Result.Value and Half_Mask) = Item.Data (1)
         and then Shift_Right (Decode'Result.Value, Half_Bits) = Item.Data (2)
         and then (if For_Write or else Decode'Result.Status /= Done then
           Decode'Result.Value = 0));
   -- Local transport facts, not wire fields or hardware authority.
   type Time_Instant is new Unsigned_64;
   No_Finite_Deadline : constant Time_Instant := Time_Instant'Last;
   type Delivery is (Not_Dispatched, Reply_Received, Completion_Unknown);
end ACPI_Region_Protocol;
