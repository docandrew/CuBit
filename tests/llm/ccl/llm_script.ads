--  What the scripted CCL_Llm (fake_ccl_llm.adb) delivers and records.
with Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with CCL_Llm;
package Llm_Script is
   --  llm.*: one exchange whose text arrives in pieces as the test
   --  releases them, ending when it says.
   Pieces : constant := 3;
   --  How many pieces the exchange has delivered so far, and whether it ended.
   Released : Natural range 0 .. Pieces := 0;
   Ended : Boolean := False;
   function Piece_Text (N : Positive) return String is
     (case N is
         when 1 => "Hello from",
         when 2 => " the script." & ASCII.LF & "Second ",
         when others => "line, END.");

   Asked : Boolean := False;
   Last_Model_Length, Last_Mission_Length, Last_System_Length : Natural := 0;
   Last_Max_Tokens, Last_Deadline : Natural := 0;
   Last_Bounded : Boolean := False;
   Last_Ceiling : Interfaces.Unsigned_64 := 0;
   Last_Cache, Last_Capture : Boolean := False;
   Last_Conversation, Last_Tools, Last_System, Last_Model : Unbounded_String;
   Asks : Natural := 0;
   Capacity_Refusals : Natural := 0;

   --  agent.*: each ask is answered by the next scripted turn, at once (its
   --  text, then its typed stop, usage and content blocks).
   Agent_Mode : Boolean := False;
   type Turn is record
      Text : Unbounded_String;
      Stop : CCL_Llm.Stop := CCL_Llm.End_Turn;
      Content : Unbounded_String;
      Input, Output, Cache_Read : Interfaces.Unsigned_64 := 0;
      Cost : Interfaces.Unsigned_64 := 0;
   end record;
   Maximum_Turns : constant := 16;
   Turns : array (1 .. Maximum_Turns) of Turn;
   Turn_Count : Natural := 0;
   Next_Turn : Positive := 1;
   --  Hold the next turn's end until the test releases it (Hold False).
   Hold : Boolean := False;
end Llm_Script;
