------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Channel_Schedule with SPARK_Mode is

   procedure Open (S : in out State; I : Index; C : Class) is
   begin
      S.Active (I) := True;
      S.Ready (I) := False;
      S.Served (I) := False;
      S.Class_Of (I) := C;
   end Open;

   procedure Close (S : in out State; I : Index) is
   begin
      S.Active (I) := False;
      S.Ready (I) := False;
   end Close;

   procedure Mark_Ready (S : in out State; I : Index) is
   begin
      S.Ready (I) := True;
   end Mark_Ready;

   procedure Served (S : in out State; I : Index; Still_Ready : Boolean) is
   begin
      S.Served (I) := True;
      S.Ready (I) := Still_Ready;
   end Served;

   procedure Begin_Turn (S : in out State) is
   begin
      S.Served := [others => False];
   end Begin_Turn;

   procedure Grow (From : State; Into : out State) is
   begin
      Into.Active := [others => False];
      Into.Ready := [others => False];
      Into.Served := [others => False];
      Into.Class_Of := [others => Class'First];
      Into.Active (1 .. From.Size) := From.Active;
      Into.Ready (1 .. From.Size) := From.Ready;
      Into.Served (1 .. From.Size) := From.Served;
      Into.Class_Of (1 .. From.Size) := From.Class_Of;
      Into.Cursor := From.Cursor;
   end Grow;

   function First_After (S : State; C : Class; After : Natural) return Index is
   begin
      for J in After + 1 .. S.Size loop
         if Eligible (S, J) and then S.Class_Of (J) = C then
            return J;
         end if;
         pragma Loop_Invariant
           (for all K in After + 1 .. J => not (Eligible (S, K) and then S.Class_Of (K) = C));
      end loop;
      for J in 1 .. S.Size loop
         if Eligible (S, J) and then S.Class_Of (J) = C then
            return J;
         end if;
         pragma Loop_Invariant
           (for all K in 1 .. J => not (Eligible (S, K) and then S.Class_Of (K) = C));
      end loop;
      raise Program_Error;  --  unreachable: Any_Eligible
   end First_After;

   procedure Next (S : in out State; Found : out Boolean; I : out Index) is
   begin
      I := Index'First;
      Found := False;
      for C in Class loop
         if Any_Eligible (S, C) then
            I := First_After (S, C, S.Cursor (C));
            S.Cursor (C) := I;
            Found := True;
            return;
         end if;
         pragma Loop_Invariant (for all D in Class'First .. C => not Any_Eligible (S, D));
         pragma Loop_Invariant (S = S'Loop_Entry);
      end loop;
   end Next;

end CuBit.Channel_Schedule;
