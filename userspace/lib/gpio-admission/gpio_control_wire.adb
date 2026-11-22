pragma Ada_2022;
package body GPIO_Control_Wire with SPARK_Mode is
   function Kind_Of (Label : Operation) return Control_Kind is
     (if Label = OP_GPIO_ARM then Arm
      elsif Label = OP_GPIO_RELEASE then Release_Pin else Acknowledge)
     with Pre => Is_Control (Label);
   function Encode (Value : Request) return Message_Words is
   begin
      case Value.Kind is
         when Arm => return [No_Identity, Encode_Deadline (Value.Limit)];
         when Release_Pin =>
            return [Value.Lease_To_Release, Encode_Deadline (Value.Limit)];
         when Acknowledge =>
            return [Value.Lease_To_Ack, Value.Generation_To_Ack,
               Encode_Deadline (Value.Limit)];
      end case;
   end Encode;
   function Decode (Label : Operation; Protocol : Word; Words : Message_Words)
      return Request_Decoding
   is
      Kind : Control_Kind;
   begin
      if not Is_Control (Label) then return (Status => Wrong_Operation); end if;
      if Protocol /= Protocol_Version then return (Status => Wrong_Protocol); end if;
      Kind := Kind_Of (Label);
      if (Kind = Acknowledge and then Words'Length /= Ack_Word_Count)
        or else (Kind /= Acknowledge and then Words'Length /= Short_Word_Count)
      then return (Status => Wrong_Length); end if;
      if Kind = Arm then
         if Words (Words'First) /= No_Identity then
            return (Status => Reserved_Word);
         end if;
         return (Status => Decoded, Value => (Kind => Arm,
            Limit => Decode_Deadline (Words (Words'First + Second_Index))));
      end if;
      if Words (Words'First) = No_Identity then return (Status => Missing_Lease); end if;
      if Kind = Release_Pin then
         return (Status => Decoded, Value => (Kind => Release_Pin,
            Limit => Decode_Deadline (Words (Words'First + Second_Index)),
            Lease_To_Release => Words (Words'First)));
      end if;
      if Words (Words'First + Second_Index) = No_Identity then
         return (Status => Missing_Generation);
      end if;
      return (Status => Decoded, Value => (Kind => Acknowledge,
         Limit => Decode_Deadline (Words (Words'First + Third_Index)),
         Lease_To_Ack => Words (Words'First),
         Generation_To_Ack => Words (Words'First + Second_Index)));
   end Decode;
   function Encode_Completion (Value : Completion) return Message_Words is
     ([Status_Code (Value.Status), Value.Lease, Value.Generation]);
   function Decode_Completion
     (Label : Operation; Protocol : Word; Words : Message_Words)
      return Completion_Decoding
   is
      Value : Completion;
      Code : Word;
   begin
      if not Is_Control (Label) then return (Status => Wrong_Operation); end if;
      if Protocol /= Protocol_Version then return (Status => Wrong_Protocol); end if;
      if Words'Length /= Completion_Word_Count then return (Status => Wrong_Length); end if;
      Code := Words (Words'First);
      if Code > Status_Code (Completion_Status'Last) then
         return (Status => Unknown_Status);
      end if;
      Value := (Kind => Kind_Of (Label),
         Status => Completion_Status'Val (Natural (Code)),
         Lease => Words (Words'First + Second_Index),
         Generation => Words (Words'First + Third_Index));
      if not Valid (Value) then return (Status => Invalid_Completion); end if;
      return (Status => Decoded, Value => Value);
   end Decode_Completion;
   function Encode_Event (Value : Event) return Message_Words is
     ([Value.Lease, Value.Generation]);
   function Decode_Event (Label : Operation; Protocol : Word; Words : Message_Words)
      return Event_Decoding
   is
   begin
      if Label /= OP_GPIO_EVENT then return (Status => Wrong_Operation); end if;
      if Protocol /= Protocol_Version then return (Status => Wrong_Protocol); end if;
      if Words'Length /= Short_Word_Count then return (Status => Wrong_Length); end if;
      if Words (Words'First) = No_Identity then return (Status => Missing_Lease); end if;
      if Words (Words'First + Second_Index) = No_Identity then
         return (Status => Missing_Generation);
      end if;
      return (Status => Decoded, Value =>
         (Lease => Words (Words'First),
          Generation => Words (Words'First + Second_Index)));
   end Decode_Event;
end GPIO_Control_Wire;
