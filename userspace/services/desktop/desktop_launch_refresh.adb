with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with Compositor_Requests;
with Compositor_Refresh;
with Desktop_Timing_Policy;

package body Desktop_Launch_Refresh is
   package CR renames Compositor_Requests;
   package RF is new Compositor_Refresh (Desktop_Launch.Maximum_Entries);
   use type RF.Phase;
   package MG renames CuBit.Memory_Grants;
   Schedule : RF.State;
   Flight : CR.State;
   Grant : MG.Grant_Reference;
   Buffer_Address : System.Address := System.Null_Address;
   Attempted, Initialized : Boolean := False;
   Candidate : Desktop_Launch.Menu;
   type Key_Text is record
      Text : String (1 .. 64) := [others => ' '];
      Length : Natural range 0 .. 64 := 0;
   end record;
   Names : array (1 .. Desktop_Launch.Maximum_Entries) of Key_Text;

   procedure Quarantine is
   begin
      CR.Quarantine (Flight);
      debugPrint ("desktop: menu refresh uncertain; buffer retained" & ASCII.LF);
   end Quarantine;

   procedure Initialize is
      Raw, Aligned : Unsigned_64;
      OK : Boolean;
   begin
      if Attempted then return; end if;
      Attempted := True;
      Raw := syscall (SYSCALL_SBRK, 3 * 4096);
      if Raw > Unsigned_64'Last - 3 * 4096 then Quarantine; return; end if;
      Aligned := (Raw + 4095) and not Unsigned_64 (4095);
      Buffer_Address := To_Address (Integer_Address (Aligned));
      MG.Create_Via_Capability (CAP_SLOT_CONFIG, Buffer_Address, 2, True, Grant, OK);
      if not OK then Quarantine; return; end if;
      Initialized := True;
   end Initialize;

   procedure Request is
   begin RF.Request (Schedule); end Request;

   function Token return Unsigned_64 is (CR.Token (Flight));

   procedure Pump (Sequence : in out Unsigned_64) is
      Msg : Message := NULL_MESSAGE;
      New_Token : Unsigned_64;
      Accepted : Boolean;
      Prefix : constant String := "desktop.launch.";
   begin
      if not Initialized or else not CR.Available (Flight) then return; end if;
      if RF.Current (Schedule) = RF.Idle and then RF.Requested (Schedule) then
         Candidate := (others => <>);
         RF.Start (Schedule);
      end if;
      if RF.Current (Schedule) not in RF.Listing | RF.Reading then return; end if;
      declare
         Buffer : String (1 .. 4096) with Import, Address => Buffer_Address;
      begin
         Buffer := [others => ASCII.NUL];
         if RF.Current (Schedule) = RF.Listing then
            Buffer (1 .. Prefix'Length) := Prefix;
            Msg.tag.label := 16#0603#;
            Msg.words (2) := Prefix'Length;
         else
            declare K : Key_Text renames Names (RF.Index (Schedule));
            begin
               Buffer (1 .. K.Length) := K.Text (1 .. K.Length);
               Msg.tag.label := 16#0600#;
               Msg.words (2) := Unsigned_64 (K.Length);
            end;
         end if;
      end;
      Msg.tag.length := 4;
      Msg.words (0) := Grant.slot;
      Msg.words (1) := Grant.generation;
      CR.Allocate (Sequence, New_Token);
      CR.Begin_Request (Flight, New_Token, Accepted);
      if not Accepted then Quarantine; return; end if;
      if not capSubmit (CAP_SLOT_CONFIG, Msg, New_Token) then Quarantine; end if;
   end Pump;

   procedure Collect (C : CompletionEntry) is
      Envelope : constant Boolean := C.valid and then C.status = COMPLETION_OK and then
        C.msg.tag.length = 1 and then C.msg.tag.flags = 0 and then C.msg.tag.reserved = 0 and then
        C.msg.words (1) = 0 and then C.msg.words (2) = 0 and then C.msg.words (3) = 0;
      Success : constant Boolean := Envelope and then C.msg.tag.label = 16#F000# and then
        (if RF.Current (Schedule) = RF.Listing then C.msg.words (0) <= 256
         else RF.Current (Schedule) = RF.Reading and then C.msg.words (0) <= 4096);
      -- These errors follow successful Return_Acquisition in Config. F001
      -- also covers return failure and cannot authorize buffer reuse.
      Returned_Error : constant Boolean := Envelope and then C.msg.words (0) = 0 and then
        (C.msg.tag.label in 16#F007# | 16#F060# or else
         (RF.Current (Schedule) = RF.Listing and then C.msg.tag.label = 16#F061#));
   begin
      CR.Complete (Flight, C.token, Success or Returned_Error);
      if not CR.Available (Flight) then Quarantine; return; end if;
      if RF.Current (Schedule) = RF.Listing then
         declare
            Buffer : String (1 .. 4096) with Import, Address => Buffer_Address;
            Pos : Positive := 1;
            Named : RF.Item_Count := 0;
            Valid : Boolean := True;
         begin
            if Success then
               for I in 1 .. Natural (C.msg.words (0)) loop
                  declare First : constant Positive := Pos;
                  begin
                     while Pos <= Buffer'Last and then Buffer (Pos) /= ASCII.NUL loop
                        Pos := Pos + 1;
                     end loop;
                     if Pos > Buffer'Last or else Pos = First then Valid := False; exit; end if;
                     if Named < Names'Last and then Pos - First in 1 .. 64 then
                        Named := Named + 1;
                        Names (Named).Text (1 .. Pos - First) := Buffer (First .. Pos - 1);
                        Names (Named).Length := Pos - First;
                     end if;
                     Pos := Pos + 1;
                  end;
               end loop;
            end if;
            if not Valid then Named := 0; end if;
            for I in 2 .. Named loop
               declare Item : constant Key_Text := Names (I); J : Natural := I - 1;
               begin
                  while J >= 1 and then Names (J).Text (1 .. Names (J).Length) >
                    Item.Text (1 .. Item.Length)
                  loop Names (J + 1) := Names (J); J := J - 1; end loop;
                  Names (J + 1) := Item;
               end;
            end loop;
            RF.Listed (Schedule, Named);
         end;
      elsif RF.Current (Schedule) = RF.Reading then
         if Success and then C.msg.words (0) in 1 .. 512 then
            declare
               Source : String (1 .. Natural (C.msg.words (0))) with Import, Address => Buffer_Address;
               Item : Desktop_Launch.Entry_Info;
               OK : Boolean;
            begin
               Desktop_Launch.Decode (Source, Item, OK);
               if OK then Desktop_Launch.Append (Candidate, Item); end if;
            end;
         end if;
         RF.Read_Item (Schedule);
      end if;
      if RF.Current (Schedule) = RF.Ready and then Desktop_Timing_Policy.Enabled then
         debugPrint ("desktop: menu refresh ready count=" & Candidate.Count'Image & ASCII.LF);
      end if;
   end Collect;

   function Can_Take (Visible : Boolean) return Boolean is
     (not Visible and then not CR.Faulted (Flight) and then RF.Current (Schedule) = RF.Ready);

   procedure Take (Visible : Boolean; Items : out Desktop_Launch.Menu; Updated : out Boolean) is
      Published : Boolean;
   begin
      Items := (others => <>);
      Updated := False;
      if CR.Faulted (Flight) then return; end if;
      RF.Publish (Schedule, Visible, Published);
      if Published and then Candidate.Count > 0 then
         Items := Candidate;
         Updated := True;
         if Desktop_Timing_Policy.Enabled then
            debugPrint ("desktop: menu refresh published count=" & Candidate.Count'Image & ASCII.LF);
         end if;
      end if;
   end Take;
end Desktop_Launch_Refresh;
