pragma Ada_2022;
package body Events is

   Log : String (1 .. 2 * Max_Events);
   Used : Natural := 0;

   procedure Record_Event (Kind : Character; Id : Object_Id) is
   begin
      if Used + 2 <= Log'Length then
         Log (Used + 1) := Kind;
         Log (Used + 2) := Character'Val (Character'Pos ('0') + Id);
         Used := Used + 2;
      end if;
   end Record_Event;

   procedure Clear is
   begin
      Used := 0;
   end Clear;

   function Trace return String is (Log (1 .. Used));

   function Count (Kind : Character) return Natural is
      N : Natural := 0;
   begin
      for I in 1 .. Used / 2 loop
         if Log (2 * I - 1) = Kind then
            N := N + 1;
         end if;
      end loop;
      return N;
   end Count;

   overriding procedure Initialize (T : in out Tracked) is
   begin
      Record_Event ('I', T.Id);
   end Initialize;

   overriding procedure Adjust (T : in out Tracked) is
   begin
      Record_Event ('A', T.Id);
   end Adjust;

   overriding procedure Finalize (T : in out Tracked) is
   begin
      Record_Event ('F', T.Id);
   end Finalize;

   overriding procedure Initialize (R : in out Resource) is
   begin
      Record_Event ('I', R.Id);
   end Initialize;

   overriding procedure Finalize (R : in out Resource) is
   begin
      Record_Event ('F', R.Id);
   end Finalize;

end Events;
