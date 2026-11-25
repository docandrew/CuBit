with Interfaces; use Interfaces;
with CuBit.Graphics_Report;
with CuBit.Handles;
with CuBit.Messages;
with CCL_Native_Bindings;

--  graphics.* on CuBit: devmgr's renderer report through the
--  graphics-observer endpoint (CuBit.Graphics_Report): the summary, then
--  every page of every entry against the generation it answered; a report
--  that changed meanwhile is read again (at most Attempts times).
package body CCL_Graphics is
   package GR renames CuBit.Graphics_Report;
   use type CuBit.Messages.Call_Ending;

   Attempts : constant := 3;
   --  devmgr answers from memory; this bounds a busy devmgr.
   Answer_Budget_Milliseconds : constant := 2_000;
   Next_Id : Unsigned_64 := 1;

   function Available return Boolean is (CuBit.Handles.Valid (CCL_Native_Bindings.Graphics_Observer));

   type Outcome is (Done, Changed, Failed);

   procedure Call (Msg : in out CuBit.Messages.Message; Label : Unsigned_32; Answered, Refused : out Boolean) is
      Id : constant Unsigned_64 := Msg.words (GR.Request_Word);
      Tag : CuBit.Messages.MessageTag;
      Replied : Boolean;
   begin
      Tag := CuBit.Messages.capCall
        (CCL_Native_Bindings.Graphics_Observer, Msg,
         CuBit.Messages.Deadline_After (Answer_Budget_Milliseconds));
      Replied := CuBit.Messages.Ending_Of (Tag) = CuBit.Messages.Call_Answered
        and then Msg.words (GR.Request_Word) = Id;
      Answered := Replied and then Tag.label = Label;
      Refused := Replied and then Tag.label = GR.Refused_Label;
   end Call;

   function Request (Label : Unsigned_32; Length : Unsigned_8) return CuBit.Messages.Message is
      Msg : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Msg.tag := (label => Label, length => Length, protocol => 0, ipc => <>, reserved => 0);
      Msg.words (GR.Request_Word) := Next_Id;
      Next_Id := (if Next_Id = Unsigned_64 (Integer_64'Last) then 1 else Next_Id + 1);
      return Msg;
   end Request;

   procedure Read_Once
     (Entries : out CCL.Interfaces.Graphics.Entry_Array; Count : out Natural; Result : out Outcome;
      Why : out CuBit.Failures.Failure)
   is
      Msg : CuBit.Messages.Message := Request (GR.Summary_Label, GR.Request_Words);
      Answered, Refused : Boolean;
      Generation, Listed : Unsigned_64;
      Wire : GR.Entry_Wire;
   begin
      Entries := [others => (others => <>)];
      Count := 0;
      Result := Failed;
      Why := (others => <>);
      Call (Msg, GR.Summary_Label, Answered, Refused);
      if not Answered then
         Why := CuBit.Failures.Failed (CuBit.Failures.Unavailable, "graphics: devmgr did not answer its summary");
         return;
      end if;
      Generation := Msg.words (GR.Generation_Word);
      Listed := Msg.words (GR.Count_Word);
      if Listed > Unsigned_64 (Entries'Length) then
         Why := CuBit.Failures.Failed
           (CuBit.Failures.Exhausted, "graphics:" & Unsigned_64'Image (Listed) & " entries do not fit one result");
         return;
      end if;
      for I in 1 .. Natural (Listed) loop
         Wire := [others => 0];
         for Page in GR.Page_Index loop
            Msg := Request (GR.Entry_Label, GR.Entry_Request_Words);
            Msg.words (GR.Generation_Word) := Generation;
            Msg.words (GR.Index_Word) := Unsigned_64 (I);
            Msg.words (GR.Page_Word) := Unsigned_64 (Page);
            Call (Msg, GR.Entry_Label, Answered, Refused);
            if not Answered then
               if Refused and then Msg.words (1) = GR.Refusal'Enum_Rep (GR.Stale_Generation) then
                  Result := Changed;
               else
                  Why := CuBit.Failures.Failed
                    (CuBit.Failures.Unavailable, "graphics: devmgr did not answer an entry's page");
               end if;
               return;
            end if;
            for Offset in 0 .. GR.Page_Words - 1 loop
               Wire (GR.Page_Words * Page + Offset) := Msg.words (GR.Payload_Word + Offset) and GR.Maximum_Word;
            end loop;
         end loop;
         if not GR.Valid (Wire) then
            Why := CuBit.Failures.Failed (CuBit.Failures.Refused, "graphics: devmgr sent an entry it never writes");
            return;
         end if;
         Entries (Entries'First + I - 1) := GR.Decode (Wire);
      end loop;
      Count := Natural (Listed);
      Result := Done;
   end Read_Once;

   procedure Read
     (Entries : out CCL.Interfaces.Graphics.Entry_Array; Count : out Natural;
      Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      Result : Outcome := Changed;
   begin
      Entries := [others => (others => <>)];
      Count := 0;
      Success := False;
      Why := (others => <>);
      if not Available then
         Why := CuBit.Failures.Failed
           (CuBit.Failures.Not_Granted, "graphics: this program has no endpoint to devmgr's renderer report",
            "the program's manifest must request (service ""graphics-observer"" ""graphics-observer"") " &
            "from the startup plan");
         return;
      end if;
      for Attempt in 1 .. Attempts loop
         Read_Once (Entries, Count, Result, Why);
         exit when Result /= Changed;
      end loop;
      case Result is
         when Done => Success := True;
         when Changed =>
            Why := CuBit.Failures.Failed (CuBit.Failures.Unavailable, "graphics: the report kept changing while read");
         when Failed => null;
      end case;
   end Read;
end CCL_Graphics;
