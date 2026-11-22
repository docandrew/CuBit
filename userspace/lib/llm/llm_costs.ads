------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What a Messages API exchange costs, from its usage (AGENT-001 slice 1).
--
--  @description
--  Prices are in nano-USD per token: Anthropic's first-party rates, as the
--  claude-api reference recorded them (cached 2026-10-06), with a 5-minute
--  cache write at 1.25x the input rate. A model not in the table has no
--  price: a request with a cost ceiling is refused for it, never guessed.
--  Arithmetic cannot overflow: counts are bounded by Maximum_Token_Count
--  and the dearest rate by Maximum_Rate.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Llm_Costs with Pure, SPARK_Mode is

   --  A trillion tokens: far beyond any exchange, small enough that a sum
   --  of four products at Maximum_Rate fits 64 bits.
   Maximum_Token_Count : constant := 2**40;
   subtype Token_Count is Unsigned_64 range 0 .. Maximum_Token_Count;

   --  Nano-USD per token. 50_000 is USD 50 per million tokens.
   Maximum_Rate : constant := 2**16;
   subtype Rate is Unsigned_64 range 0 .. Maximum_Rate;

   --  The rates per token, by kind of token.
   type Rates is record
      Input, Output, Cache_Write, Cache_Read : Rate := 0;
   end record;

   --  A model's price. Some models charge a second set of rates for a
   --  long prompt: when the prompt (input, cache write and cache read
   --  tokens together) is over Long_Above tokens, every token of the
   --  exchange is charged at Long. Long_Above = 0: one set of rates.
   type Price is record
      Known : Boolean := False;
      Base : Rates;
      Long_Above : Token_Count := 0;
      Long : Rates;
   end record;

   --  The table: exact model identifiers only, never a prefix.
   function Price_Of (Model : String) return Price;

   type Usage is record
      Input, Output, Cache_Write, Cache_Read : Token_Count := 0;
   end record;

   Maximum_Cost : constant := 4 * Maximum_Token_Count * Maximum_Rate;
   subtype Nano_Usd is Unsigned_64 range 0 .. Maximum_Cost;

   --  The prompt's tokens (what a long-prompt tier is decided on).
   function Prompt_Tokens (U : Usage) return Unsigned_64 is
     (U.Input + U.Cache_Write + U.Cache_Read);

   --  The rates an exchange with this usage is charged at.
   function Rates_For (P : Price; U : Usage) return Rates is
     (if P.Long_Above > 0 and then Prompt_Tokens (U) > P.Long_Above then P.Long else P.Base);

   function Cost (P : Price; U : Usage) return Nano_Usd is
     (Rates_For (P, U).Input * U.Input + Rates_For (P, U).Output * U.Output +
      Rates_For (P, U).Cache_Write * U.Cache_Write + Rates_For (P, U).Cache_Read * U.Cache_Read);

   --  The most an exchange can cost once its input usage is known: that
   --  input, and every output token it may still produce (Max_Output).
   --  The prompt decides the tier, and output never changes it.
   function Worst_Case (P : Price; U : Usage; Max_Output : Token_Count) return Nano_Usd is
     (Rates_For (P, U).Input * U.Input + Rates_For (P, U).Output * Max_Output +
      Rates_For (P, U).Cache_Write * U.Cache_Write + Rates_For (P, U).Cache_Read * U.Cache_Read);
end Llm_Costs;
