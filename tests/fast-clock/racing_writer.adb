pragma Ada_2022;
with Clock_Publication; use Clock_Publication;
with Clock_Publication.Sample;

package body Racing_Writer is

   --  Every version the writer publishes reads Expected_Time at
   --  Reader_Counter: Base_Ticks moves forward by the same nanoseconds as
   --  Base_Time moves back (one nanosecond per tick). A snapshot mixing
   --  two versions reads a different time.
   Reader_Counter : constant := 1_000_000_000;
   Expected_Time  : constant := 2_000_000_000;
   Versions       : constant := 200_000;
   Reads          : constant := 2_000_000;

   Shared_Sequence   : Unsigned_64 := 0 with Atomic;
   Shared_Version    : constant Unsigned_64 := Layout_Version;
   Shared_Frequency  : constant Unsigned_64 := Minimum_Hz;
   Shared_Scale      : constant Unsigned_64 := One_Nanosecond;
   Shared_Base_Ticks : Unsigned_64 := 0 with Atomic;
   Shared_Base_Time  : Unsigned_64 := Expected_Time - Reader_Counter
     with Atomic;
   Writer_Done : Boolean := False with Atomic;
   Stable_Count : Natural := 0 with Atomic;

   function Load_Sequence return Unsigned_64 is (Shared_Sequence);
   function Load_Fields return Parameters is
     ((Version    => Shared_Version,
       Frequency  => Shared_Frequency,
       Scale      => Shared_Scale,
       Base_Ticks => Shared_Base_Ticks,
       Base_Time  => Shared_Base_Time));
   function Load_Counter return Unsigned_64 is (Reader_Counter);

   procedure Read is new Clock_Publication.Sample
     (Load_Sequence, Load_Fields, Load_Counter);

   task Writer;
   task body Writer is
      Spin : Unsigned_64 with Volatile;
   begin
      for V in 1 .. Versions loop
         Shared_Sequence := Opened (Shared_Sequence);
         Shared_Base_Ticks := Unsigned_64 (V);
         --  Widen the window between the two stores.
         for I in 1 .. 50 loop
            Spin := Unsigned_64 (I);
         end loop;
         Shared_Base_Time := Expected_Time - Reader_Counter + Unsigned_64 (V);
         Shared_Sequence := Closed (Shared_Sequence);
      end loop;
      Writer_Done := True;
   end Writer;

   function Stable_Reads return Natural is (Stable_Count);

   procedure Run (Torn : out Unsigned_64) is
      Value : Unsigned_64;
      Ok    : Boolean;
   begin
      Torn := 0;
      for I in 1 .. Reads loop
         Read (Value, Ok);
         if Ok then
            Stable_Count := Stable_Count + 1;
            if Value /= Expected_Time then
               Torn := Torn + 1;
            end if;
         end if;
         exit when Writer_Done and then I > Reads / 2;
      end loop;
      while not Writer_Done loop
         null;
      end loop;
   end Run;

end Racing_Writer;
