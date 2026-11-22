with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;

--  The Messages API through llm.svc (AGENT-001 slice 1, docs/agent-harness.md):
--    (llm.ask (Llm_Request prompt => "hello" model => Llm_Model.Default
--       max_tokens => 1024 deadline_ms => 60000
--       cost_ceiling => (Llm_Ceiling.Nano_Usd 50000000) system => ""
--       mission => "console"))
--                    Llm_Exchange: the exchange, started at once. Every
--                    limit is spelled out (no defaults); Llm_Model.Default
--                    names claude-opus-5-5 (Llm_Requests.Default_Model),
--                    (Llm_Model.Named "...") any other model.
--    (llm.text x)    Stream<String>: the answer's text as it streams, one
--                    element per line; the newest line grows in place.
--    (llm.reply x)   Task<Llm_Reply>: how it ended (a typed Llm_Stop),
--                    usage, cost and duration, once it has.
--    (llm.cancel x)  stops the exchange (Llm_Stop.Cancelled).
--  The API key never appears here: it is the secret anthropic-api-key in
--  secrets.svc, which only llm.svc may use. The types are CCL source; the
--  digest and keys are SHA-256 of it (then "#" and the type's name), and
--  tests/ccl-console checks them.
package CCL.Interfaces.Llm with SPARK_Mode is

   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 3;
   PUBLISHED_TYPES : constant := 9;
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Llm_Model (variant (Default) (Named String))) " &
     "(type Llm_Ceiling (variant (Nano_Usd Integer) (Unlimited))) " &
     "(type Llm_Tokens (range 1 128000)) (type Llm_Milliseconds (range 1 600000)) " &
     "(type Llm_Request (record (prompt String) (model Llm_Model) (max_tokens Llm_Tokens) " &
     "(deadline_ms Llm_Milliseconds) (cost_ceiling Llm_Ceiling) (system String) " &
     "(mission String))) (type Llm_Exchange (record (id Integer))) (type Llm_Stop (enum " &
     "End_Turn Max_Tokens Stop_Sequence Tool_Use Pause_Turn Refusal Context_Window_Exceeded " &
     "Unknown_Stop_Reason Cost_Budget Deadline Cancelled Api_Error Malformed_Response " &
     "Disconnected Connect_Failed)) (type Llm_Cost (variant (Priced Integer) (Unpriced))) " &
     "(type Llm_Reply (record (stop Llm_Stop) (error String) (problem String) (status " &
     "Integer) (model String) (input_tokens Integer) (output_tokens Integer) " &
     "(cache_read_tokens Integer) (cache_write_tokens Integer) (cost Llm_Cost) " &
     "(duration_ms Integer) (text_bytes Integer) (detail String)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#F184_EE1D_5606_482D#,
      16#1FAC_2118_3CC5_CE0A#,
      16#CD3C_8AFD_C064_5D39#,
      16#8F58_880A_DB14_077E#];
   REQUEST_KEY : constant CCL.Objects.Schema_Key :=
     [16#59B1_143F_65E8_B6C6#,
      16#E790_4D4F_CEBF_B9B2#,
      16#6156_C57B_6E15_2ED7#,
      16#1546_6843_8CDE_D6CF#];
   EXCHANGE_KEY : constant CCL.Objects.Schema_Key :=
     [16#5365_1AF6_1F48_6A77#,
      16#81DC_0487_AC95_FFC7#,
      16#654A_F282_3646_6C95#,
      16#80F6_E54E_59DC_137F#];
   REPLY_KEY : constant CCL.Objects.Schema_Key :=
     [16#D00B_73FD_E67C_054F#,
      16#3D26_FF0C_6898_3F41#,
      16#F3B8_340E_5A91_EA3D#,
      16#CCE8_C90F_D87F_8BEE#];

   --  Llm_Tokens and Llm_Milliseconds (CuBit.Llm_Protocol's limits).
   MAXIMUM_TOKENS : constant := 128_000;
   MAXIMUM_DEADLINE_MS : constant := 600_000;

   --  Fields, in declaration order (1-based, as CCL.Objects.Views numbers them).
   REQUEST_PROMPT : constant := 1;
   REQUEST_MODEL : constant := 2;
   REQUEST_MAX_TOKENS : constant := 3;
   REQUEST_DEADLINE : constant := 4;
   REQUEST_CEILING : constant := 5;
   REQUEST_SYSTEM : constant := 6;
   REQUEST_MISSION : constant := 7;
   EXCHANGE_FIELDS : constant := 1;
   REPLY_FIELDS : constant := 13;
   --  Alternatives, numbered from 1.
   MODEL_DEFAULT : constant := 1;
   MODEL_NAMED : constant := 2;
   CEILING_NANO_USD : constant := 1;
   CEILING_UNLIMITED : constant := 2;
   COST_PRICED : constant := 1;
   COST_UNPRICED : constant := 2;

   type Operation is (Ask, Text, Reply, Cancel);
   function Name (Item : Operation) return String is
     (case Item is
         when Ask => "ask", when Text => "text", when Reply => "reply", when Cancel => "cancel");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Llm);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Request, Exchange, Reply : CCL.Objects.Binding;
   end record;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Llm;
