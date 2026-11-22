with Interfaces;
package body Desktop_Startup_Progress with SPARK_Mode => Off,
  Refined_State => (State => Value) is
   use type Interfaces.Unsigned_8;
   Value : Interfaces.Unsigned_8 := Stage'Pos (Stage'First)
     with Atomic, Volatile;
   procedure Enter (Point : Stage) is
   begin
      Value := Stage'Pos (Point);
   end Enter;
   function Current return Stage is
      Seen : constant Interfaces.Unsigned_8 := Value;
   begin
      return (if Seen <= Stage'Pos (Stage'Last) then Stage'Val (Seen) else Stage'First);
   end Current;
end Desktop_Startup_Progress;
