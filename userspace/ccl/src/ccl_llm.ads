with Ada.Unchecked_Deallocation;
with Interfaces;

--  llm.svc for llm.* (CCL.Interfaces.Llm), one body per platform: native/
--  asks the service and follows each exchange with submitted pulls whose
--  completions arrive in the console's event loop (never blocking it);
--  the Linux preview's host/ has no llm.svc (Unavailable), a stand-in, not
--  CuBit.
package CCL_Llm is
   --  Exchanges one program follows at once (a named ceiling; llm.svc has
   --  its own).
   MAXIMUM_EXCHANGES : constant := 4;
   --  Named ceilings (LIM-001; CuBit.Llm_Protocol's). Each exchange's
   --  window is sized from its ask, and a captured exchange's content
   --  grows on the heap as it arrives; reaching a ceiling is a typed
   --  capacity error (Ask: Capacity; Take_Summary: Content_Too_Large),
   --  never a silent cut. The longest ask: model, mission, system, tools
   --  and conversation together, less the request header.
   MAXIMUM_ASK_TEXT : constant := 4 * 2**20 - 48;
   --  The most content one captured exchange returns.
   MAXIMUM_CONTENT : constant := 8 * 2**20;
   MAXIMUM_SHORT : constant := 160;

   type Text_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, Text_Access);

   type Ceiling_Kind is (Unlimited, Nano_Usd);

   --  How an exchange ended, in CCL's Llm_Stop order.
   type Stop is
     (End_Turn, Max_Tokens, Stop_Sequence, Tool_Use, Pause_Turn, Refusal,
      Context_Window_Exceeded, Unknown_Stop_Reason, Cost_Budget, Deadline,
      Cancelled, Api_Error, Malformed_Response, Disconnected, Connect_Failed);

   --  Capacity: the ask is longer than a ceiling (compact the
   --  conversation first).
   type Ask_Result is (Started, Not_Granted, Unavailable, Refused, Capacity);

   --  Starts an exchange; Why says why not (Refused, Unavailable). The
   --  request is provider-neutral: Tools and Conversation are
   --  CuBit.Llm_Protocol's tools and conversation sections (typed tool
   --  definitions, a conversation of typed blocks). Cache asks for the
   --  stable prefix to be cached; Capture for the response's content
   --  blocks (Take_Summary).
   procedure Ask
     (Model, Mission, System, Tools, Conversation : String;
      Cache, Capture : Boolean;
      Max_Tokens, Deadline_Ms : Positive;
      Ceiling : Ceiling_Kind; Ceiling_Nano_Usd : Interfaces.Unsigned_64;
      Exchange : out Interfaces.Unsigned_64; Result : out Ask_Result;
      Why : out String; Why_Length : out Natural);

   --  Text that arrived for Exchange and was not yet taken (Last <
   --  Into'First: none).
   procedure Take_Text (Exchange : Interfaces.Unsigned_64; Into : out String; Last : out Natural);

   type Short_Text is record
      Data : String (1 .. MAXIMUM_SHORT) := [others => ' '];
      Length : Natural range 0 .. MAXIMUM_SHORT := 0;
   end record;
   type Summary is record
      Ended_As : Stop := Disconnected;
      Error, Problem, Model, Detail : Short_Text;
      Status : Natural := 0;
      Input, Output, Cache_Read, Cache_Write : Interfaces.Unsigned_64 := 0;
      Priced : Boolean := False;
      Cost_Nano_Usd, Duration_Ms, Text_Bytes : Interfaces.Unsigned_64 := 0;
      Tool_Calls : Natural := 0;
      --  The content did not arrive whole (beyond MAXIMUM_CONTENT, or out
      --  of memory): Content is null and the exchange counts as failed.
      Content_Lost : Boolean := False;
   end record;
   --  Once Exchange ended and all its text was taken: how it ended, and a
   --  captured exchange's content blocks (null: none). The caller owns
   --  Content and frees it. The exchange is then let go (Ended True once).
   procedure Take_Summary
     (Exchange : Interfaces.Unsigned_64; Result : out Summary;
      Content : out Text_Access; Ended : out Boolean);

   procedure Cancel (Exchange : Interfaces.Unsigned_64; Done : out Boolean);
   --  Whether Exchange is one this program follows.
   function Following (Exchange : Interfaces.Unsigned_64) return Boolean;
end CCL_Llm;
