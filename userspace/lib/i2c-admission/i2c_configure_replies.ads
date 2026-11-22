pragma Ada_2022;
with Interfaces;
package I2C_Configure_Replies with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   Version : constant Word := 1;
   type Message_Words is array (Natural range <>) of Word;
   use type Word;
   subtype Register_Value is Interfaces.Unsigned_32;
   type Reply_Status is (Ready, Refused, Failed_No_Effects, Failed_Effects_Possible);
   subtype Failure_Status is Reply_Status range Refused .. Failed_Effects_Possible;
   -- Diagnostic stage codes are the positions in this version-one enumeration.
   -- They describe startup failure, not a request to perform an operation.
   type Startup_Stage is
     (Frame_Validation, Launch_Validation, Authority_Validation, Mapping,
      Controller_Reset, Component_Probe, Interrupt_Mask, Controller_Disable,
      Disabled_Confirmation, Idle_Confirmation, TX_Confirmation, RX_Confirmation,
      Abort_Check, Status_Clear);
   type Reply (Status : Reply_Status := Refused) is record
      case Status is
         when Ready => null;
         when others =>
            Stage : Startup_Stage := Frame_Validation;
            Last_Register_Value : Register_Value := 0;
      end case;
   end record;
   Version_Index : constant Natural := 0;
   Status_Index : constant Natural := 1;
   Stage_Index : constant Natural := 2;
   Value_Index : constant Natural := 3;
   Reply_Word_Count : constant Natural := 4;
   subtype Reply_Words is Message_Words (Version_Index .. Value_Index);
   function Encode (Value : Reply) return Reply_Words
     with Post => Encode'Result (Version_Index) = Version;
   type Decode_Status is (Decoded, Wrong_Length, Wrong_Version, Invalid_Status,
      Invalid_Stage, Invalid_Register_Value, Noncanonical_Ready);
   type Decoding (Status : Decode_Status := Wrong_Length) is record
      case Status is
         when Decoded => Value : Reply;
         when others => null;
      end case;
   end record;
   function Valid_Words (Words : Message_Words) return Boolean;
   function Decode (Words : Message_Words) return Decoding
     with Post =>
       (Decode'Result.Status = Decoded) = Valid_Words (Words)
       and then (if Decode'Result.Status = Decoded then
          Word (Words'Length) = Word (Reply_Word_Count)
          and then Encode (Decode'Result.Value) = Words);
   procedure Round_Trip (Value : Reply)
     with Ghost,
       Post => Decode (Encode (Value)).Status = Decoded
         and then Decode (Encode (Value)).Value = Value;
   -- Ready has no failure fields and encodes only zero in both diagnostic words.
   -- No failed/refused reply aliases Ready. Native code is responsible for the
   -- truth of Ready/quiescence and for selecting the effects classification from
   -- actual retained hardware evidence, including writes preceding clock loss.
   -- No operation IDs, endpoints, addresses, reply authority or hardware IO here.
private
   Ready_Code : constant Word := 0;
   Refused_Code : constant Word := 1;
   Failed_No_Effects_Code : constant Word := 2;
   Failed_Effects_Possible_Code : constant Word := 3;
   function Status_Code (Value : Reply_Status) return Word is
     (case Value is
         when Ready => Ready_Code,
         when Refused => Refused_Code,
         when Failed_No_Effects => Failed_No_Effects_Code,
         when Failed_Effects_Possible => Failed_Effects_Possible_Code);
   function Status_Of (Value : Word) return Failure_Status is
     (case Value is
         when Refused_Code => Refused,
         when Failed_No_Effects_Code => Failed_No_Effects,
         when others => Failed_Effects_Possible)
     with Pre => Value in Refused_Code .. Failed_Effects_Possible_Code,
       Post => Status_Code (Status_Of'Result) = Value;
end I2C_Configure_Replies;
