with Interfaces;
package Intel_GPU_Log_Completion with SPARK_Mode is
   use Interfaces;
   -- Called only after the publisher accepts the matching completion token.
   -- A rate-limit reply confirms no acquisition; cumulative losses do not
   -- indicate whether the current shared page is safe to reuse.
   -- Successful and below-minimum replies advertise Severity position 0..5;
   -- all other words remain reserved. Rate-limit replies remain all-zero.
   function Recoverable
     (Transport_OK, Pending : Boolean;
      Label, Length, Flags, Reserved, Minimum : Unsigned_64;
      Remaining_Words_Zero : Boolean) return Boolean is
     (Transport_OK and not Pending and Remaining_Words_Zero and
      Length = 4 and Flags = 0 and Reserved = 0 and
      ((Label in 16#F000# | 16#F009# and Minimum <= 5) or
       (Label = 16#F008# and Minimum = 0)));
end Intel_GPU_Log_Completion;
