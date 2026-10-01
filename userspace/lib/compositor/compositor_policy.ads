package Compositor_Policy with SPARK_Mode, Pure is
   type State is (Legacy, Ready, Reading, Disabled, Restart_Required);
   type Completion is (Rendered, Rejected, Failed_Quiescent, Access_Unknown);
   function Initial (Opt_In, Initialized : Boolean) return State is
     (if Opt_In and Initialized then Ready else Legacy);
   function May_Retire (S : State) return Boolean is
     (S in Legacy | Ready | Disabled);
   function May_Fallback (S : State) return Boolean is
     (S in Legacy | Disabled);
   procedure Begin_Draw (S : in out State)
     with Pre => S = Ready, Post => S = Reading and not May_Retire (S);
   procedure Finish_Draw (S : in out State; Result : Completion)
     with Pre => S = Reading,
       Post =>
         (if Result = Rendered then S = Ready
          elsif Result = Access_Unknown then
            S = Restart_Required and not May_Retire (S) and not May_Fallback (S)
          else S = Disabled and May_Retire (S) and May_Fallback (S));
end Compositor_Policy;
