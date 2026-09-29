with Interfaces;
package Intel_GPU_Log_Completion with SPARK_Mode is
   use Interfaces;
   -- Called only after the publisher accepts the matching completion token.
   -- A rate-limit reply confirms no acquisition; cumulative losses do not
   -- indicate whether the current shared page is safe to reuse.
   function Recoverable
     (Transport_OK, Pending : Boolean;
      Label, Length, Flags, Reserved : Unsigned_64;
      Words_Zero : Boolean) return Boolean is
     (Transport_OK and not Pending and Words_Zero and
      Label in 16#F000# | 16#F008# and Length = 4 and
      Flags = 0 and Reserved = 0);
end Intel_GPU_Log_Completion;
