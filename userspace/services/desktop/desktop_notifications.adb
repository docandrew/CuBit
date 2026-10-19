------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Deskbar's notification centre (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;

package body Desktop_Notifications with SPARK_Mode is

   --  Source cut to Into'Length characters; Length is how many were kept.
   procedure Copy (Source : String; Into : out String; Length : out Natural)
     with Pre  => Into'First = 1 and then Into'Last < Natural'Last,
          Post => Length <= Into'Length
   is
   begin
      Into := [others => ' '];
      Length := 0;
      for C of Source loop
         exit when Length = Into'Length;
         pragma Loop_Invariant (Length < Into'Length);
         Length := Length + 1;
         Into (Length) := C;
      end loop;
   end Copy;

   function Make
     (Kind    : Source_Kind;
      Process : Interfaces.Unsigned_64;
      Name    : String;
      Level   : Urgency;
      Icon    : Desktop_Icons.Icon_ID;
      Accent  : Accent_Color;
      Now_Ms  : Interfaces.Unsigned_64;
      At_Wall : Wall_Time;
      Title   : String;
      Detail  : String) return Notification
   is
      Result : Notification;
      Used   : Natural;
   begin
      Result.Source.Kind := Kind;
      Result.Source.Process := Process;
      Copy (Name, Result.Source.Name, Used);
      Result.Source.Length := Used;
      Result.Level := Level;
      Result.Icon := Icon;
      Result.Accent := Accent;
      Result.Posted_Ms := Now_Ms;
      Result.Posted_At := At_Wall;
      Copy (Title, Result.Title, Used);
      Result.Title_Len := Used;
      Copy (Detail, Result.Detail, Used);
      Result.Detail_Len := Used;
      return Result;
   end Make;

   function Find (H : History; Id : Serial) return Count is
   begin
      for P in 1 .. H.Used loop
         if H.Items (P).Id = Id then
            return P;
         end if;
         pragma Loop_Invariant (for all Q in 1 .. P => H.Items (Q).Id /= Id);
      end loop;
      return 0;
   end Find;

   function Toast_Length (Level : Urgency) return Interfaces.Unsigned_64 is
     (case Level is
         when Low      => Toast_Low_Ms,
         when Normal   => Toast_Normal_Ms,
         when Critical => Never);

   procedure Post
     (H : in out History; N : Notification; Now_Ms : Interfaces.Unsigned_64;
      Id : out Serial)
   is
      Keep : constant Count := Count'Min (H.Used, Capacity - 1);
      Span : constant Interfaces.Unsigned_64 := Toast_Length (N.Level);
   begin
      if H.Last_Id = Serial'Last then
         Id := No_Notification;
         return;
      end if;
      if H.Used = Capacity then
         if H.Evicted < Interfaces.Unsigned_64'Last then
            H.Evicted := H.Evicted + 1;
         end if;
      end if;
      H.Last_Id := H.Last_Id + 1;
      Id := H.Last_Id;
      H.Items (2 .. Keep + 1) := H.Items (1 .. Keep);
      H.Items (1) := N;
      H.Items (1).Id := Id;
      H.Used := Keep + 1;
      H.Unseen := Count'Min (H.Unseen + 1, H.Used);
      if not H.Is_Quiet or else N.Level = Critical then
         H.Toasted := Id;
         H.Toast_End :=
           (if Span = Never or else Now_Ms >= Never - Span then Never else Now_Ms + Span);
      end if;
   end Post;

   procedure Dismiss (H : in out History; Id : Serial) is
      At_Position : constant Count := Find (H, Id);
   begin
      if H.Toasted = Id then
         H.Toasted := No_Notification;
         H.Toast_End := Never;
      end if;
      if At_Position = 0 then
         pragma Assert (for all P in 1 .. H.Used => H.Items (P).Id /= Id);
         return;
      end if;
      --  Ids strictly decrease: newer records' are above Id, older below.
      pragma Assert (for all Q in 1 .. At_Position - 1 => H.Items (Q).Id > Id);
      pragma Assert (for all Q in At_Position + 1 .. H.Used => H.Items (Q).Id < Id);
      declare
         Before : constant Slots := H.Items with Ghost;
      begin
         H.Items (At_Position .. H.Used - 1) := H.Items (At_Position + 1 .. H.Used);
         pragma Assert (for all Q in 1 .. At_Position - 1 => H.Items (Q).Id = Before (Q).Id);
         pragma Assert (for all Q in At_Position .. H.Used - 1 => H.Items (Q).Id = Before (Q + 1).Id);
         pragma Assert (for all Q in 1 .. H.Used - 1 => H.Items (Q).Id /= Id);
         pragma Assert (for all Q in 1 .. H.Used - 1 => H.Items (Q).Id /= No_Notification);
         pragma Assert (for all Q in 1 .. H.Used - 1 => H.Items (Q).Id <= H.Last_Id);
         pragma Assert
           (for all P in 1 .. H.Used - 1 =>
              (for all Q in P + 1 .. H.Used - 1 => H.Items (P).Id > H.Items (Q).Id));
      end;
      if At_Position <= H.Unseen then
         H.Unseen := H.Unseen - 1;
      end if;
      H.Used := H.Used - 1;
      pragma Assert (H.Unseen <= H.Used);
      pragma Assert (for all P in 1 .. H.Used => H.Items (P).Id /= Id);
   end Dismiss;

   procedure Clear (H : in out History) is
   begin
      H.Used := 0;
      H.Unseen := 0;
      H.Toasted := No_Notification;
      H.Toast_End := Never;
   end Clear;

   procedure Mark_Read (H : in out History) is
   begin
      H.Unseen := 0;
   end Mark_Read;

   procedure Set_Quiet (H : in out History; On : Boolean) is
   begin
      H.Is_Quiet := On;
   end Set_Quiet;

   procedure Expire_Toast
     (H : in out History; Now_Ms : Interfaces.Unsigned_64; Changed : out Boolean) is
   begin
      Changed := H.Toasted /= No_Notification and then Now_Ms >= H.Toast_End;
      if Changed then
         H.Toasted := No_Notification;
         H.Toast_End := Never;
      end if;
   end Expire_Toast;

   procedure Hide_Toast (H : in out History) is
   begin
      H.Toasted := No_Notification;
      H.Toast_End := Never;
   end Hide_Toast;

end Desktop_Notifications;
