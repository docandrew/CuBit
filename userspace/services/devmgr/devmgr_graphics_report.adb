------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;

package body Devmgr_Graphics_Report with SPARK_Mode is
   use type GR.Generation, GR.Subject_Kind;

   Hex_Digits : constant String (1 .. 16) := "0123456789abcdef";

   function Hex4 (Value : Unsigned_16) return String is
     [1 => Hex_Digits (Natural (Shift_Right (Value, 12) and 15) + 1),
      2 => Hex_Digits (Natural (Shift_Right (Value, 8) and 15) + 1),
      3 => Hex_Digits (Natural (Shift_Right (Value, 4) and 15) + 1),
      4 => Hex_Digits (Natural (Value and 15) + 1)];

   function GPU_Entry (Facts : GPU_Facts) return GR.Report_Entry is
      Subject : constant GR.Name :=
        (if Facts.Display_Present then GR.To_Name (Hex4 (Facts.Vendor) & ":" & Hex4 (Facts.Device))
         else GR.To_Name ("none"));
      Driver_Name : constant GR.Name :=
        (case Facts.Driver is
            when No_Driver => GR.To_Name ("cpu"),
            when Intel_GPU => GR.To_Name ("intel-gpu"),
            when Virtio_GPU => GR.To_Name ("virtio-gpu"));
      Base : constant GR.Report_Entry :=
        (Kind => GR.GPU, Id => Facts.Location, Subject => Subject, Path => GR.Software,
         Why => GR.No_GPU_Device, Detail => 0, Device => Driver_Name, Since => Facts.Since,
         Stale => False);
   begin
      if not Facts.Display_Present then
         return Base;
      elsif Facts.Driver = No_Driver then
         return (Base with delta Why => GR.No_Driver_Bound);
      elsif not Facts.Running then
         return (Base with delta Why => GR.Driver_Failed, Detail => GR.Driver_Stage'Pos (GR.Init));
      elsif Facts.Driver = Virtio_GPU then
         return (Base with delta Why => GR.Venus_Not_Offered, Detail => GR.Venus_Gap'Pos (GR.No_3D));
      else
         return (Base with delta Path => GR.Hardware, Why => GR.None);
      end if;
   end GPU_Entry;

   procedure Advance (R : in out Report) is
   begin
      if R.Current < GR.Generation'Last then
         R.Current := R.Current + 1;
      else
         R.Current := 1;
      end if;
   end Advance;

   procedure Post (R : in out Report; Item : GR.Report_Entry; Poster : Poster_Id) is
      Fresh : GR.Report_Entry := Item;
   begin
      Fresh.Stale := False;
      for I in 1 .. R.Used loop
         if GR.Same_Subject (R.Slots (I).Value, Fresh) then
            R.Slots (I) := (Value => Fresh, Poster => Poster);
            Advance (R);
            return;
         end if;
      end loop;
      if R.Used < Capacity then
         R.Used := R.Used + 1;
         R.Slots (R.Used) := (Value => Fresh, Poster => Poster);
         Advance (R);
      elsif R.Lost < Natural'Last then
         R.Lost := R.Lost + 1;
      end if;
   end Post;

   procedure Poster_Gone (R : in out Report; Poster : Poster_Id) is
      Changed : Boolean := False;
   begin
      if Poster = No_Poster then
         return;
      end if;
      for I in 1 .. R.Used loop
         if not R.Slots (I).Value.Stale
           and then (R.Slots (I).Poster = Poster
                     or else (R.Slots (I).Value.Kind = GR.Program
                              and then Unsigned_64 (R.Slots (I).Value.Id) = Unsigned_64 (Poster)))
         then
            R.Slots (I).Value.Stale := True;
            Changed := True;
         end if;
      end loop;
      if Changed then
         Advance (R);
      end if;
   end Poster_Gone;

   function Refused (Request : Unsigned_64; Why : GR.Refusal) return Answer is
     ((Reply => True, Label => GR.Refused_Label, Length => GR.Refusal_Words,
       Values => [0 => Request, 1 => GR.Refusal'Enum_Rep (Why), others => 0]));

   procedure Hold (Halves : in out Pending; Poster : Poster_Id; Values : Words) is
      Free : Natural := 0;
   begin
      for I in Halves'Range loop
         if Halves (I).Poster = Poster then
            Halves (I).Values := Values;
            return;
         elsif Halves (I).Poster = No_Poster and then Free = 0 then
            Free := I;
         end if;
      end loop;
      if Free /= 0 then
         Halves (Free) := (Poster => Poster, Values => Values);
      end if;
   end Hold;

   procedure Complete
     (R : in out Report; Halves : in out Pending; Poster : Poster_Id; Values : Words)
   is
   begin
      for I in Halves'Range loop
         if Halves (I).Poster = Poster then
            declare
               First : constant Words := Halves (I).Values;
               Wire : constant GR.Entry_Wire :=
                 [0 => First (0) and GR.Maximum_Word, 1 => First (1) and GR.Maximum_Word,
                  2 => First (2) and GR.Maximum_Word, 3 => First (3) and GR.Maximum_Word,
                  4 => Values (0) and GR.Maximum_Word, 5 => Values (1) and GR.Maximum_Word,
                  6 => Values (2) and GR.Maximum_Word, 7 => Values (3) and GR.Maximum_Word];
            begin
               Halves (I) := (others => <>);
               --  A pair that does not decode is dropped.
               if GR.Valid (Wire) then
                  Post (R, GR.Decode (Wire), Poster);
               end if;
            end;
            return;
         end if;
      end loop;
   end Complete;

   procedure Handle
     (R : in out Report; Halves : in out Pending; Label : Unsigned_32; Length : Natural;
      Values : Words; Badge : Unsigned_64; Sender : Poster_Id; From_Parent : Boolean;
      Result : out Answer; Handled : out Boolean)
   is
      Request : constant Unsigned_64 := Values (GR.Request_Word);
   begin
      Result := (others => <>);
      Handled := True;
      if Label = GR.Post_Label or else Label = GR.Post_End_Label then
         --  Acknowledged at once, taken or not: a poster learns nothing
         --  from the answer but that devmgr has read it.
         Result := (Reply => True, Label => Label, Length => 0, Values => [others => 0]);
         if (GR.Is_Poster (Badge) or else From_Parent) and then Sender /= No_Poster
           and then Length = GR.Post_Words
         then
            if Label = GR.Post_Label then
               Hold (Halves, Sender, Values);
            else
               Complete (R, Halves, Sender, Values);
            end if;
         end if;
      elsif Label = GR.Gone_Label then
         Result := (Reply => True, Label => Label, Length => 0, Values => [others => 0]);
         if From_Parent and then Length = GR.Gone_Words and then Values (0) /= 0 then
            Poster_Gone (R, Poster_Id (Values (0)));
         end if;
      elsif Label = GR.Summary_Label then
         if not GR.Is_Observer (Badge) then
            Result := Refused (Request, GR.Not_Approved);
         elsif Length /= GR.Request_Words or else Request = 0 then
            Result := Refused (Request, GR.Malformed);
         else
            Result := (Reply => True, Label => GR.Summary_Label, Length => GR.Summary_Words,
                       Values => [0 => Request, 1 => Unsigned_64 (R.Current),
                                  2 => Unsigned_64 (R.Used), 3 => Unsigned_64 (R.Lost)]);
         end if;
      elsif Label = GR.Entry_Label then
         if not GR.Is_Observer (Badge) then
            Result := Refused (Request, GR.Not_Approved);
         elsif Length /= GR.Entry_Request_Words or else Request = 0
           or else Values (GR.Index_Word) not in 1 .. Unsigned_64 (R.Used)
           or else Values (GR.Page_Word) > Unsigned_64 (GR.Page_Index'Last)
         then
            Result := Refused (Request, GR.Malformed);
         elsif Values (GR.Generation_Word) /= Unsigned_64 (R.Current) then
            Result := Refused (Request, GR.Stale_Generation);
         else
            declare
               Wire : constant GR.Entry_Wire :=
                 GR.Encode (R.Slots (Entry_Index (Values (GR.Index_Word))).Value);
               Page : constant GR.Page_Index := GR.Page_Index (Values (GR.Page_Word));
            begin
               Result := (Reply => True, Label => GR.Entry_Label, Length => GR.Reply_Words,
                          Values => [0 => Request, 1 => Unsigned_64 (R.Current),
                                     2 => GR.Page_Word_Of (Wire, Page, 0),
                                     3 => GR.Page_Word_Of (Wire, Page, 1)]);
            end;
         end if;
      else
         Handled := False;
      end if;
   end Handle;

end Devmgr_Graphics_Report;
