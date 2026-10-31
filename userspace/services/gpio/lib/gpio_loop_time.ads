pragma Ada_2022;
with Intel_GPIO_IRQ;
package GPIO_Loop_Time with SPARK_Mode, Pure is
   subtype Tick is Intel_GPIO_IRQ.Tick;
   use type Tick;
   subtype Finite_Time is Tick range 0 .. Tick'Last - 1;
   subtype Duration is Finite_Time range 1 .. Finite_Time'Last;
   function After (Now, Window : Tick) return Finite_Time;
   -- Saturates below the ABI forever sentinel. No modular wrap to an early
   -- deadline. A clock at/beyond the finite limit yields an expired limit.
   type Wait_Kind is (Activity, Until_Time, Relative_Delay);
   type Wait_Plan (Kind : Wait_Kind := Activity) is record
      case Kind is
         when Activity => null;
         when Until_Time => Deadline : Finite_Time;
         when Relative_Delay => Backoff : Duration;
      end case;
   end record;
end GPIO_Loop_Time;
