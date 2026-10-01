with Interfaces;

--  The real-time reservations the kernel can ever admit (kernel
--  Realtime_Admission: Microseconds'Last and Realtime_Share). Manifests and
--  startup policy reject anything else when they are compiled.
package CCL.Scheduling_Limits with Pure, SPARK_Mode => On is
   use type Interfaces.Integer_64;
   MAX_MICROSECONDS : constant := 2 ** 30;
   REALTIME_SHARE_PPM : constant := 700_000;
   PARTS_PER_MILLION : constant := 1_000_000;
   subtype Microseconds is Interfaces.Integer_64 range 1 .. MAX_MICROSECONDS;

   --  Budget every Period, no more than the real-time share of one CPU.
   function Admissible (Budget, Period : Microseconds) return Boolean is
     (Budget <= Period
      and then Budget * PARTS_PER_MILLION <= Period * REALTIME_SHARE_PPM);

   --  Whether a ceiling of Cap_Budget every Cap_Period covers a request of
   --  Budget every Period: no larger a share of the CPU (as kernel
   --  Realtime_Admission.Covers).
   function Covers (Cap_Budget, Cap_Period, Budget, Period : Microseconds) return Boolean is
     (Budget * Cap_Period <= Cap_Budget * Period);
end CCL.Scheduling_Limits;
