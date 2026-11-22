pragma Ada_2022;
package body Llm_Costs with SPARK_Mode is

   --  USD per million tokens times 1_000 = nano-USD per token.
   --  A 5-minute cache write is 1.25x the input rate.
   Opus_5_5   : constant Price :=
     (Known => True, Base => (Input => 4_000, Output => 20_000, Cache_Write => 5_000, Cache_Read => 200),
      others => <>);
   Sonnet_5_5 : constant Price :=
     (Known => True, Base => (Input => 2_000, Output => 10_000, Cache_Write => 2_500, Cache_Read => 200),
      others => <>);
   --  USD 0.10 / 0.50 for prompts up to 100K tokens, 0.50 / 2.50 beyond;
   --  cache reads at 0.1x the input rate.
   Haiku_Long_Prompt : constant := 100_000;
   Haiku_5_5  : constant Price :=
     (Known => True, Base => (Input => 100, Output => 500, Cache_Write => 125, Cache_Read => 10),
      Long_Above => Haiku_Long_Prompt,
      Long => (Input => 500, Output => 2_500, Cache_Write => 625, Cache_Read => 50));
   Fable_5_1  : constant Price :=
     (Known => True, Base => (Input => 10_000, Output => 50_000, Cache_Write => 12_500, Cache_Read => 250),
      others => <>);

   function Price_Of (Model : String) return Price is
   begin
      if Model = "claude-opus-5-5" then
         return Opus_5_5;
      elsif Model = "claude-sonnet-5-5" then
         return Sonnet_5_5;
      elsif Model = "claude-haiku-5-5" then
         return Haiku_5_5;
      elsif Model = "claude-fable-5-1" then
         return Fable_5_1;
      end if;
      return (others => <>);
   end Price_Of;
end Llm_Costs;
