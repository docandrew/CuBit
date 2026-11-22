------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
package body CuBit.Channel_Rings with SPARK_Mode is

   --  Sizes are powers of two, so a mask, not a division.
   function Position (Value : Index; Size : Ring_Size) return Natural is
     (Natural (Value and Index (Size - 1)));

   procedure Accept_Consumed
     (P : in out Producer; Value : Index; OK : out Boolean)
   is
      Freed : constant Count := Released (P, Value);
   begin
      OK := Freed <= Count (P.Fill);
      if OK then
         P.Fill := P.Fill - Natural (Freed);
      end if;
   end Accept_Consumed;

   procedure Free_Slices
     (P : Producer; First, First_Length, Second_Length : out Natural)
   is
      Free : constant Natural := Space (P);
   begin
      First := Position (P.Produced, P.Size);
      First_Length := Natural'Min (Free, P.Size - First);
      Second_Length := Free - First_Length;
   end Free_Slices;

   procedure Commit (P : in out Producer; N : Natural) is
   begin
      P.Produced := P.Produced + Index (N);
      P.Fill := P.Fill + N;
   end Commit;

   procedure Write
     (P : in out Producer; Ring : in out Bytes; Data : Bytes;
      Written : out Natural)
   is
      First, First_Length, Second_Length : Natural;
      N1, N2 : Natural;
      D : constant Integer := Data'First;
   begin
      Free_Slices (P, First, First_Length, Second_Length);
      Written := Natural'Min (Data'Length, First_Length + Second_Length);
      N1 := Natural'Min (Written, First_Length);
      N2 := Written - N1;
      if N1 > 0 then
         Ring (First .. First + N1 - 1) := Data (D .. D + (N1 - 1));
      end if;
      if N2 > 0 then
         Ring (0 .. N2 - 1) := Data (D + N1 .. D + N1 + (N2 - 1));
      end if;
      Commit (P, Written);
   end Write;

   procedure Accept_Produced
     (C : in out Consumer; Value : Index; OK : out Boolean)
   is
      Ahead : constant Count := Distance (C.Consumed, Value);
   begin
      OK := Ahead <= Count (C.Size) and then Ahead >= Count (C.Available);
      if OK then
         C.Available := Natural (Ahead);
      end if;
   end Accept_Produced;

   procedure Data_Slices
     (C : Consumer; First, First_Length, Second_Length : out Natural)
   is
      Ready : constant Natural := C.Available;
   begin
      First := Position (C.Consumed, C.Size);
      First_Length := Natural'Min (Ready, C.Size - First);
      Second_Length := Ready - First_Length;
   end Data_Slices;

   procedure Consume (C : in out Consumer; N : Natural) is
   begin
      C.Consumed := C.Consumed + Index (N);
      C.Available := C.Available - N;
   end Consume;

   procedure Read
     (C : in out Consumer; Ring : Bytes; Data : in out Bytes;
      Copied : out Natural)
   is
      First, First_Length, Second_Length : Natural;
      N1, N2 : Natural;
      D : constant Integer := Data'First;
   begin
      Data_Slices (C, First, First_Length, Second_Length);
      Copied := Natural'Min (Data'Length, First_Length + Second_Length);
      N1 := Natural'Min (Copied, First_Length);
      N2 := Copied - N1;
      if N1 > 0 then
         Data (D .. D + (N1 - 1)) := Ring (First .. First + N1 - 1);
      end if;
      if N2 > 0 then
         Data (D + N1 .. D + N1 + (N2 - 1)) := Ring (0 .. N2 - 1);
      end if;
      Consume (C, Copied);
   end Read;

end CuBit.Channel_Rings;
