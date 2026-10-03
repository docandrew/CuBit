with Compositor_Input_Queue;
package Compositor_Input_Acknowledgment with SPARK_Mode, Pure is
   package IQ renames Compositor_Input_Queue;
   use type IQ.Word;
   use type IQ.Event;
   procedure Apply (Q : in out IQ.Queue; Close : in out IQ.Word; After : IQ.Word)
   with Post => Close = (if Close'Old <= After then 0 else Close'Old) and then
     (for all I in IQ.Index => Q (I) =
       (if Q'Old (I).Valid and then Q'Old (I).Serial <= After
        then IQ.Cleared (Q'Old (I)) else Q'Old (I)));
end Compositor_Input_Acknowledgment;
