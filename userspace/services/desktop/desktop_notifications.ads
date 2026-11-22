------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Deskbar's notification centre (docs/development-backlog.md UI-027):
--  typed notification records, a bounded history, and the toast that shows
--  the newest one briefly above the status button.
--
--  @description
--  A record names its source (Desktop itself, or an application or service
--  by its kernel-authenticated process identity), an urgency, when it was
--  posted (monotonic and, when the clock knew it, wall time) and its words.
--  Records arrive as events Desktop already receives asynchronously (a
--  procmgr reply, a kernel child-exit notice): nothing calls into Desktop
--  to post one, and posting never blocks.
--
--  The history keeps the newest Capacity records, newest first. A post
--  into a full history evicts the oldest record and counts it (Dropped),
--  so an eviction is visible, never silent. The person dismisses one record
--  or clears them all; opening the panel marks every record read.
--
--  The toast shows the newest post for Toast_Low_Ms or Toast_Normal_Ms; a
--  Critical post stays until dismissed. In Quiet mode (do not disturb) only
--  Critical posts are toasted; every post still reaches the history.
--
--  Each record carries a security-context mark (CTX-001). Until contexts
--  exist every record is the person's own; the mark is part of the record
--  so the panel can show it later without a new record shape.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with Desktop_Icons;

package Desktop_Notifications with SPARK_Mode is
   use type Interfaces.Unsigned_64;

   Capacity       : constant := 32;
   Maximum_Name   : constant := 32;
   Maximum_Title  : constant := 64;
   Maximum_Detail : constant := 96;

   --  How long a toast shows, by urgency (Critical: until dismissed).
   Toast_Low_Ms    : constant := 4_000;
   Toast_Normal_Ms : constant := 8_000;
   Never           : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;

   type Urgency is (Low, Normal, Critical);

   --  Who posted: Desktop itself, an application or a service.
   type Source_Kind is (Desktop, Application, Service);

   subtype Name_Length is Natural range 0 .. Maximum_Name;
   subtype Title_Length is Natural range 0 .. Maximum_Title;
   subtype Detail_Length is Natural range 0 .. Maximum_Detail;
   subtype Name_Text is String (1 .. Maximum_Name);
   subtype Title_Text is String (1 .. Maximum_Title);
   subtype Detail_Text is String (1 .. Maximum_Detail);

   --  Process: the source's process identity word (0 for Desktop itself).
   --  Name: what the panel calls it (the app's launcher label).
   type Source_Identity is record
      Kind    : Source_Kind := Desktop;
      Process : Interfaces.Unsigned_64 := 0;
      Name    : Name_Text := [others => ' '];
      Length  : Name_Length := 0;
   end record;

   --  The source app's identity colour (16#RRGGBB#), or the theme accent.
   subtype RGB is Natural range 0 .. 16#FF_FFFF#;
   type Accent_Color (Declared : Boolean := False) is record
      case Declared is
         when True  => Value : RGB := 0;
         when False => null;
      end case;
   end record;

   --  CTX-001 security contexts; every record is the person's own today.
   type Context_Mark is (Own_Context);

   Minutes_Per_Day : constant := 24 * 60;
   type Minute_Of_Day is range 0 .. Minutes_Per_Day - 1;
   type Wall_Time (Known : Boolean := False) is record
      case Known is
         when True  => Minute : Minute_Of_Day := 0;
         when False => null;
      end case;
   end record;

   type Serial is new Interfaces.Unsigned_64;
   No_Notification : constant Serial := 0;

   type Notification is record
      Id         : Serial := No_Notification;
      Source     : Source_Identity;
      Level      : Urgency := Normal;
      Context    : Context_Mark := Own_Context;
      Icon       : Desktop_Icons.Icon_ID := Desktop_Icons.Start;
      Accent     : Accent_Color;
      Posted_Ms  : Interfaces.Unsigned_64 := 0;
      Posted_At  : Wall_Time;
      Title      : Title_Text := [others => ' '];
      Title_Len  : Title_Length := 0;
      Detail     : Detail_Text := [others => ' '];
      Detail_Len : Detail_Length := 0;
   end record;

   --  A record (Id not yet assigned) with Name, Title and Detail cut to
   --  their bounds.
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
     with Post => Make'Result.Id = No_Notification;

   function Name_Of (N : Notification) return String is
     (N.Source.Name (1 .. N.Source.Length));
   function Title_Of (N : Notification) return String is
     (N.Title (1 .. N.Title_Len));
   function Detail_Of (N : Notification) return String is
     (N.Detail (1 .. N.Detail_Len));

   type Count is range 0 .. Capacity;
   subtype Position is Count range 1 .. Capacity;

   type History is private;

   function Valid (H : History) return Boolean;
   function Length (H : History) return Count;
   function Unread (H : History) return Count
     with Post => Unread'Result <= Length (H);
   --  Records evicted by posts into a full history.
   function Dropped (H : History) return Interfaces.Unsigned_64;
   function Quiet (H : History) return Boolean;

   --  The P-th newest record (1 = newest).
   function Item (H : History; P : Position) return Notification
     with Pre => Valid (H) and then P <= Length (H),
          Post => Item'Result.Id /= No_Notification;

   --  Where the record Id is (0 if it is not in the history).
   function Find (H : History; Id : Serial) return Count
     with Pre => Valid (H),
          Post => Find'Result <= Length (H) and then
                  (if Find'Result /= 0 then Item (H, Find'Result).Id = Id
                   else (for all P in 1 .. Length (H) => Item (H, P).Id /= Id));

   --  Add N as the newest record (its Id is assigned here) and toast it
   --  unless Quiet (Critical is always toasted). Id is No_Notification only
   --  once every Serial is spent (never in practice); nothing is changed then.
   procedure Post
     (H : in out History; N : Notification; Now_Ms : Interfaces.Unsigned_64;
      Id : out Serial)
     with Pre  => Valid (H),
          Post => Valid (H) and then
                  (if Id /= No_Notification then
                     Length (H) = Count'Min (Length (H'Old) + 1, Capacity)
                     and then Item (H, 1).Id = Id
                     and then Item (H, 1).Level = N.Level
                     and then Unread (H) = Count'Min (Unread (H'Old) + 1, Capacity)
                   else Length (H) = Length (H'Old));

   --  Remove the record Id (and its toast); nothing if it is not there.
   procedure Dismiss (H : in out History; Id : Serial)
     with Pre  => Valid (H),
          Post => Valid (H) and then
                  (for all P in 1 .. Length (H) => Item (H, P).Id /= Id) and then
                  Length (H) = (if Find (H'Old, Id) /= 0 then Length (H'Old) - 1
                                else Length (H'Old));

   procedure Clear (H : in out History)
     with Pre  => Valid (H),
          Post => Valid (H) and then Length (H) = 0 and then Toast (H) = No_Notification;

   --  The panel was opened: every record has been seen.
   procedure Mark_Read (H : in out History)
     with Pre  => Valid (H),
          Post => Valid (H) and then Unread (H) = 0 and then Length (H) = Length (H'Old);

   procedure Set_Quiet (H : in out History; On : Boolean)
     with Pre  => Valid (H),
          Post => Valid (H) and then Quiet (H) = On and then Length (H) = Length (H'Old);

   --  The toasted record (No_Notification for none) and when it ends.
   function Toast (H : History) return Serial;
   function Toast_Due (H : History) return Interfaces.Unsigned_64;

   --  End the toast if its time is up; Changed when it ended.
   procedure Expire_Toast
     (H : in out History; Now_Ms : Interfaces.Unsigned_64; Changed : out Boolean)
     with Pre  => Valid (H),
          Post => Valid (H) and then Length (H) = Length (H'Old) and then
                  (if Changed then Toast (H) = No_Notification);

   --  The person dismissed the toast; the record stays in the history.
   procedure Hide_Toast (H : in out History)
     with Pre  => Valid (H),
          Post => Valid (H) and then Toast (H) = No_Notification and then
                  Length (H) = Length (H'Old);

private
   type Slots is array (Position) of Notification;

   type History is record
      Items     : Slots;
      Used      : Count := 0;
      --  The newest Unseen records are unread.
      Unseen    : Count := 0;
      Last_Id   : Serial := No_Notification;
      Evicted   : Interfaces.Unsigned_64 := 0;
      Toasted   : Serial := No_Notification;
      Toast_End : Interfaces.Unsigned_64 := Never;
      Is_Quiet  : Boolean := False;
   end record;

   --  Records are kept newest first with strictly decreasing Ids, every Id
   --  assigned and no newer than Last_Id (so Ids are unique).
   function Valid (H : History) return Boolean is
     (H.Unseen <= H.Used and then
      (for all P in 1 .. H.Used => H.Items (P).Id /= No_Notification) and then
      (for all P in 1 .. H.Used => H.Items (P).Id <= H.Last_Id) and then
      (for all P in 1 .. H.Used =>
         (for all Q in P + 1 .. H.Used => H.Items (P).Id > H.Items (Q).Id)));

   function Length (H : History) return Count is (H.Used);
   function Unread (H : History) return Count is (Count'Min (H.Unseen, H.Used));
   function Dropped (H : History) return Interfaces.Unsigned_64 is (H.Evicted);
   function Quiet (H : History) return Boolean is (H.Is_Quiet);
   function Item (H : History; P : Position) return Notification is (H.Items (P));
   function Toast (H : History) return Serial is (H.Toasted);
   function Toast_Due (H : History) return Interfaces.Unsigned_64 is (H.Toast_End);
end Desktop_Notifications;
