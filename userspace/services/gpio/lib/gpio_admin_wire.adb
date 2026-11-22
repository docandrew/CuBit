pragma Ada_2022;
package body GPIO_Admin_Wire with SPARK_Mode is
   use type W.Word, W.Operation;
   function Encode (Value : Request) return W.Message_Words is
   begin
      case Value.Kind is
         when Admit_Sink => return [1 .. 0 => 0];
         when Retire_Sink => return [Value.Badge, P.To_Word (Value.Owner)];
      end case;
   end Encode;
   function Decode (Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words; Has_Offer : Boolean) return Decoding is
      Owner : P.Process_ID;
   begin
      if Label not in OP_GPIO_ADMIT_SINK | OP_GPIO_RETIRE_SINK then return (Status => Wrong_Operation); end if;
      if Protocol /= Protocol_Version then return (Status => Wrong_Protocol); end if;
      if Label = OP_GPIO_ADMIT_SINK then
         if Words'Length /= 0 then return (Status => Wrong_Length); end if;
         if not Has_Offer then return (Status => Missing_Offer); end if;
         return (Decoded, (Kind => Admit_Sink));
      end if;
      if Has_Offer then return (Status => Unexpected_Offer); end if;
      if Words'Length /= Retire_Word_Count then return (Status => Wrong_Length); end if;
      if not GPIO_Channel_Badges.Valid_Word (Words (Words'First)) then return (Status => Invalid_Badge); end if;
      Owner := P.From_Word (Words (Words'First + 1));
      if not P.Is_Process (Owner) then return (Status => Invalid_Owner); end if;
      return (Decoded, (Retire_Sink, Words (Words'First), Owner));
   end Decode;
   function Encode_Reply (Status : W.Completion_Status) return W.Message_Words is ([W.Status_Code (Status)]);
   function Decode_Reply (Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words) return Reply_Decoding is
   begin
      if Label not in OP_GPIO_ADMIT_SINK | OP_GPIO_RETIRE_SINK then return (Status => Wrong_Operation); end if;
      if Protocol /= Protocol_Version then return (Status => Wrong_Protocol); end if;
      if Words'Length /= Reply_Word_Count then return (Status => Wrong_Length); end if;
      if Words (Words'First) > W.Status_Code (W.Internal_Mismatch) then return (Status => Unknown_Status); end if;
      return (Decoded, W.Completion_Status'Enum_Val (Words (Words'First)));
   end Decode_Reply;
end GPIO_Admin_Wire;
