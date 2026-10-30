pragma Ada_2022;
with Interfaces;
with CuBit.Grant_References;
package I2C_Transfer_Wire with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   subtype Operation is Interfaces.Unsigned_32;
   use type Word;
   Transfer_Operation : constant Operation := 16#0226#;
   Transfer_Reply_Operation : constant Operation := 16#0227#;
   Protocol_Version : constant Word := 1;
   type Count is range 0 .. 2 ** 16 - 1;
   subtype Capacity is Count range 1 .. Count'Last;
   Count_Base : constant Word := 2 ** 16;
   Reserved_Base : constant Word := Count_Base * Count_Base;
   subtype Millisecond is Word range 0 .. Word'Last - 1;
   type Deadline_Kind is (Within, Forever);
   type Deadline (Kind : Deadline_Kind := Within) is record
      case Kind is
         when Within => At_Millisecond : Millisecond := 0;
         when Forever => null;
      end case;
   end record;
   function Encode_Deadline (Item : Deadline) return Word is
     (if Item.Kind = Forever then Word'Last else Item.At_Millisecond);
   function Decode_Deadline (Item : Word) return Deadline is
     (if Item = Word'Last then (Kind => Forever)
      else (Kind => Within, At_Millisecond => Item));
   type Request is record
      Input, Output : CuBit.Grant_References.Reference;
      Write_Count, Read_Count : Count;
      Limit : Deadline;
   end record;
   function Fits (Item : Request; Available : Capacity) return Boolean is
     (Word (Item.Write_Count) + Word (Item.Read_Count) in 1 .. Word (Available));
   type Message_Words is array (Natural range <>) of Word;
   Input_Index : constant Natural := 0;
   Output_Index : constant Natural := 1;
   Counts_Index : constant Natural := 2;
   Deadline_Index : constant Natural := 3;
   Request_Word_Count : constant Natural := 4;
   subtype Request_Words is Message_Words (Input_Index .. Deadline_Index);
   function Encode (Item : Request) return Request_Words;
   type Decode_Status is (Decoded, Wrong_Operation, Wrong_Protocol,
      Wrong_Length, Reserved_Count_Bits, Invalid_Size, Invalid_Input,
      Invalid_Output);
   type Decoding (Status : Decode_Status := Wrong_Length) is record
      case Status is
         when Decoded => Value : Request;
         when others => null;
      end case;
   end record;
   function Decode
     (Label : Operation; Protocol : Word; Words : Message_Words;
      Available : Capacity) return Decoding
     with Post => (if Decode'Result.Status = Decoded then
                    Fits (Decode'Result.Value, Available));
   -- Completion counts have deliberately different meanings: Requested_Writes
   -- is the request size retained by the owner, not an acknowledged byte count;
   -- Delivered_Reads counts only bytes successfully copied into the client grant.
   type Completion_Status is
     (Completed, Unauthorized, Invalid_Channel, Unsupported_Channel, Busy,
      Invalid_Size, Input_Refused, Input_Uncertain, Admission_Refused,
      Output_Refused, Output_Uncertain, Internal_Mismatch, Invalid_Frame);
   for Completion_Status use
     (Completed => 0, Unauthorized => 1, Invalid_Channel => 2,
      Unsupported_Channel => 3, Busy => 4, Invalid_Size => 5,
      Input_Refused => 6, Input_Uncertain => 7, Admission_Refused => 8,
      Output_Refused => 9, Output_Uncertain => 10, Internal_Mismatch => 11,
      Invalid_Frame => 12);
   type Completion is record
      Status : Completion_Status;
      Ticket : Word;
      Hardware_Succeeded, Effects_Possible : Boolean;
      Requested_Writes, Delivered_Reads : Count;
   end record;
   Status_Radix : constant Word := 2 ** 8;
   Hardware_Succeeded_Bit : constant Word := Status_Radix;
   Effects_Possible_Bit : constant Word := 2 * Status_Radix;
   Completion_Reserved_Base : constant Word := 4 * Status_Radix;
   Completion_Status_Index : constant Natural := 0;
   Ticket_Index : constant Natural := 1;
   Completion_Counts_Index : constant Natural := 2;
   Completion_Reserved_Index : constant Natural := 3;
   Completion_Word_Count : constant Natural := 4;
   subtype Completion_Words is Message_Words (0 .. Completion_Word_Count - 1);
   function Encode_Completion (Item : Completion) return Completion_Words;
   type Completion_Decode_Status is (Completion_Decoded,
      Completion_Wrong_Operation, Completion_Wrong_Protocol,
      Completion_Wrong_Length, Completion_Reserved_Bits, Unknown_Status);
   type Completion_Decoding
     (Status : Completion_Decode_Status := Completion_Wrong_Length) is record
      case Status is
         when Completion_Decoded => Value : Completion;
         when others => null;
      end case;
   end record;
   function Decode_Completion
     (Label : Operation; Protocol : Word; Words : Message_Words)
      return Completion_Decoding;
   -- References are identities, never authority or addresses. Native code must
   -- acquire with the exact kernel-stamped sender as expected owner. Unused
   -- references are encoded as zero. No target, speed or work quota is carried.
   -- Decode checks the header before inspecting payload. Its finite deadline
   -- is absolute GETTIME milliseconds; zero remains an expired finite value.
end I2C_Transfer_Wire;
