pragma Ada_2022;
with Interfaces;
package GPIO_Control_Wire with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   subtype Operation is Interfaces.Unsigned_32;
   use type Word;
   use type Operation;
   Service_Role : constant := 34;
   OP_GPIO_ARM : constant Operation := 16#02A0#;
   OP_GPIO_RELEASE : constant Operation := 16#02A1#;
   OP_GPIO_EVENT : constant Operation := 16#02A2#;
   OP_GPIO_ACK : constant Operation := 16#02A3#;
   Protocol_Version : constant Word := 1;
   No_Identity : constant Word := 0;
   subtype Identity is Word range 1 .. Word'Last;
   Wait_Forever_Word : constant Word := Word'Last;
   subtype Millisecond is Word range 0 .. Word'Last - 1;
   type Deadline_Kind is (Within, Forever);
   type Deadline (Kind : Deadline_Kind := Within) is record
      case Kind is
         when Within => At_Millisecond : Millisecond := 0;
         when Forever => null;
      end case;
   end record;
   function Encode_Deadline (Value : Deadline) return Word is
     (if Value.Kind = Forever then Wait_Forever_Word else Value.At_Millisecond);
   function Decode_Deadline (Value : Word) return Deadline is
     (if Value = Wait_Forever_Word then (Kind => Forever)
      else (Kind => Within, At_Millisecond => Value));
   type Control_Kind is (Arm, Release_Pin, Acknowledge);
   function Label_Of (Kind : Control_Kind) return Operation is
     (case Kind is when Arm => OP_GPIO_ARM, when Release_Pin => OP_GPIO_RELEASE,
        when Acknowledge => OP_GPIO_ACK);
   function Is_Control (Label : Operation) return Boolean is
     (Label in OP_GPIO_ARM | OP_GPIO_RELEASE | OP_GPIO_ACK);
   type Request (Kind : Control_Kind := Arm) is record
      Limit : Deadline;
      case Kind is
         when Arm => null;
         when Release_Pin => Lease_To_Release : Identity;
         when Acknowledge => Lease_To_Ack, Generation_To_Ack : Identity;
      end case;
   end record;
   type Message_Words is array (Natural range <>) of Word;
   First_Index : constant := 0;
   Second_Index : constant := 1;
   Third_Index : constant := 2;
   Short_Word_Count : constant := 2;
   Ack_Word_Count : constant := 3;
   Completion_Word_Count : constant := 3;
   function Encode (Value : Request) return Message_Words;
   type Decode_Status is (Decoded, Wrong_Operation, Wrong_Protocol, Wrong_Length,
      Reserved_Word, Missing_Lease, Missing_Generation, Unknown_Status,
      Invalid_Completion);
   type Request_Decoding (Status : Decode_Status := Wrong_Length) is record
      case Status is
         when Decoded => Value : Request;
         when others => null;
      end case;
   end record;
   function Decode (Label : Operation; Protocol : Word; Words : Message_Words)
      return Request_Decoding;
   type Completion_Status is (Completed, Unauthorized, Invalid_Channel,
      Unsupported_Channel, Busy, Invalid_Frame, Clock_Unavailable, Expired,
      Refused, Uncertain, Internal_Mismatch);
   for Completion_Status use (Completed => 0, Unauthorized => 1,
      Invalid_Channel => 2, Unsupported_Channel => 3, Busy => 4,
      Invalid_Frame => 5, Clock_Unavailable => 6, Expired => 7, Refused => 8,
      Uncertain => 9, Internal_Mismatch => 10);
   function Status_Code (Status : Completion_Status) return Word is
     (Word (Completion_Status'Enum_Rep (Status)));
   type Completion is record
      Kind : Control_Kind;
      Status : Completion_Status;
      Lease, Generation : Word;
   end record;
   function Valid (Value : Completion) return Boolean is
     ((Value.Generation = No_Identity or else Value.Lease /= No_Identity)
      and then (Value.Kind = Acknowledge or else Value.Generation = No_Identity)
      and then (Value.Status /= Completed or else
         (Value.Lease /= No_Identity and then
          (Value.Kind /= Acknowledge or else Value.Generation /= No_Identity))));
   function Encode_Completion (Value : Completion) return Message_Words
      with Pre => Valid (Value);
   type Completion_Decoding (Status : Decode_Status := Wrong_Length) is record
      case Status is
         when Decoded => Value : Completion;
         when others => null;
      end case;
   end record;
   function Decode_Completion
     (Label : Operation; Protocol : Word; Words : Message_Words)
      return Completion_Decoding
      with Post => (if Decode_Completion'Result.Status = Decoded then
         Valid (Decode_Completion'Result.Value));
   type Event is record
      Lease, Generation : Identity;
   end record;
   function Encode_Event (Value : Event) return Message_Words;
   type Event_Decoding (Status : Decode_Status := Wrong_Length) is record
      case Status is
         when Decoded => Value : Event;
         when others => null;
      end case;
   end record;
   function Decode_Event (Label : Operation; Protocol : Word; Words : Message_Words)
      return Event_Decoding;
   -- No pin/address/process ID in payload. Authority is the kernel-stamped GPIO
   -- endpoint badge. Native IPC entry kind, sender, saved reply token, operation and exact
   -- lease/event identity must be matched by native adapters, beyond this codec.
   -- Completed arm means listening confirmed; release echoes the retired lease;
   -- ACK completion confirms rearm. Refused/Uncertain may retain a known lease.
   -- Pending stays local; never send it as a completion. Events are asynchronous.
end GPIO_Control_Wire;
