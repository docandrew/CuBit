pragma Ada_2022;
with CuBit.Process_IDs;
with GPIO_Control_Wire;
with GPIO_Channel_Badges;
package GPIO_Admin_Wire with Pure, SPARK_Mode is
   package W renames GPIO_Control_Wire;
   package P renames CuBit.Process_IDs;
   OP_GPIO_ADMIT_SINK : constant W.Operation := 16#02A4#;
   OP_GPIO_RETIRE_SINK : constant W.Operation := 16#02A5#;
   Protocol_Version : constant W.Word := W.Protocol_Version;
   Retire_Word_Count : constant := 2;
   Reply_Word_Count : constant := 1;
   type Request_Kind is (Admit_Sink, Retire_Sink);
   function Label_Of (Kind : Request_Kind) return W.Operation is
     (if Kind = Admit_Sink then OP_GPIO_ADMIT_SINK else OP_GPIO_RETIRE_SINK);
   type Request (Kind : Request_Kind := Admit_Sink) is record
      case Kind is
         when Admit_Sink => null;
         when Retire_Sink =>
            Badge : W.Word;
            Owner : P.Process_ID;
      end case;
   end record;
   function Valid (Value : Request) return Boolean is
     (Value.Kind = Admit_Sink or else
       (GPIO_Channel_Badges.Valid_Word (Value.Badge) and then P.Is_Process (Value.Owner)));
   function Encode (Value : Request) return W.Message_Words with Pre => Valid (Value);
   type Decode_Status is (Decoded, Wrong_Operation, Wrong_Protocol, Wrong_Length,
      Missing_Offer, Unexpected_Offer, Invalid_Badge, Invalid_Owner, Unknown_Status);
   type Decoding (Status : Decode_Status := Wrong_Length) is record
      case Status is
         when Decoded => Value : Request;
         when others => null;
      end case;
   end record;
   function Decode (Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words; Has_Offer : Boolean) return Decoding
      with Post => (if Decode'Result.Status = Decoded then Valid (Decode'Result.Value));
   -- Admin replies are one typed GPIO completion-status word. Completed means
   -- binding/retirement really finished; Uncertain means unresolved effects.
   -- No Pending wire reply and no implicit retry after a timed-out call.
   function Encode_Reply (Status : W.Completion_Status) return W.Message_Words;
   type Reply_Decoding (Status : Decode_Status := Wrong_Length) is record
      case Status is
         when Decoded => Value : W.Completion_Status;
         when others => null;
      end case;
   end record;
   function Decode_Reply (Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words) return Reply_Decoding;
   -- Only a kernel-authenticated launcher may administer channels. ADMIT has
   -- zero words and exactly one offered endpoint. Owner/badge come from kernel
   -- Describe of that accepted Write-only sink, not from payload. RETIRE has
   -- [badge,full opaque owner lifetime], no offer, and must match the current
   -- binding exactly. These IDs correlate a retirement; they grant no authority.
end GPIO_Admin_Wire;
