------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Channel_Rings; use CuBit.Channel_Rings;

package body CuBit.Channel_Rings_C is

   function Well_Formed (R : Ring) return Boolean;
   function To_Producer (R : Ring) return Producer;
   function To_Consumer (R : Ring) return Consumer;
   procedure Store (R : in out Ring; P : Producer);
   procedure Store (R : in out Ring; C : Consumer);

   function Well_Formed (R : Ring) return Boolean is
     (Valid_Size (Unsigned_64 (R.Size)) and then R.Count <= R.Size);

   function To_Producer (R : Ring) return Producer is
     ((Size => Natural (R.Size), Produced => Index (R.Own),
       Fill => Natural (R.Count)));

   function To_Consumer (R : Ring) return Consumer is
     ((Size => Natural (R.Size), Consumed => Index (R.Own),
       Available => Natural (R.Count)));

   procedure Store (R : in out Ring; P : Producer) is
   begin
      R.Own := Unsigned_32 (P.Produced);
      R.Count := Unsigned_32 (P.Fill);
   end Store;

   procedure Store (R : in out Ring; C : Consumer) is
   begin
      R.Own := Unsigned_32 (C.Consumed);
      R.Count := Unsigned_32 (C.Available);
   end Store;

   function Accept_Consumed (P : access Ring; Value : Unsigned_32)
     return Interfaces.C.int
   is
      V  : Producer;
      OK : Boolean;
   begin
      if not Well_Formed (P.all) then
         return 0;
      end if;
      V := To_Producer (P.all);
      Accept_Consumed (V, Index (Value), OK);
      Store (P.all, V);
      return (if OK then 1 else 0);
   end Accept_Consumed;

   function Accept_Produced (C : access Ring; Value : Unsigned_32)
     return Interfaces.C.int
   is
      V  : Consumer;
      OK : Boolean;
   begin
      if not Well_Formed (C.all) then
         return 0;
      end if;
      V := To_Consumer (C.all);
      Accept_Produced (V, Index (Value), OK);
      Store (C.all, V);
      return (if OK then 1 else 0);
   end Accept_Produced;

   procedure Free_Slices
     (P : access constant Ring;
      First, Length_1, Length_2 : access Unsigned_32)
   is
      F, L1, L2 : Natural := 0;
   begin
      if Well_Formed (P.all) then
         Free_Slices (To_Producer (P.all), F, L1, L2);
      end if;
      First.all := Unsigned_32 (F);
      Length_1.all := Unsigned_32 (L1);
      Length_2.all := Unsigned_32 (L2);
   end Free_Slices;

   procedure Data_Slices
     (C : access constant Ring;
      First, Length_1, Length_2 : access Unsigned_32)
   is
      F, L1, L2 : Natural := 0;
   begin
      if Well_Formed (C.all) then
         Data_Slices (To_Consumer (C.all), F, L1, L2);
      end if;
      First.all := Unsigned_32 (F);
      Length_1.all := Unsigned_32 (L1);
      Length_2.all := Unsigned_32 (L2);
   end Data_Slices;

   function Commit
     (P : access Ring; N : Unsigned_32) return Interfaces.C.int
   is
      V : Producer;
   begin
      if not Well_Formed (P.all) or else N > P.Size - P.Count then
         return 0;
      end if;
      V := To_Producer (P.all);
      Commit (V, Natural (N));
      Store (P.all, V);
      return 1;
   end Commit;

   function Consume
     (C : access Ring; N : Unsigned_32) return Interfaces.C.int
   is
      V : Consumer;
   begin
      if not Well_Formed (C.all) or else N > C.Count then
         return 0;
      end if;
      V := To_Consumer (C.all);
      Consume (V, Natural (N));
      Store (C.all, V);
      return 1;
   end Consume;

   function Valid_Size (Size : Unsigned_32) return Interfaces.C.int is
     (if Valid_Size (Unsigned_64 (Size)) then 1 else 0);

end CuBit.Channel_Rings_C;
