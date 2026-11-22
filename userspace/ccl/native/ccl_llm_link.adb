with System.Storage_Elements;
with CuBit.Llm_Protocol;
with CCL_Native_Bindings;

package body CCL_Llm_Link is
   use Interfaces;
   use type System.Address;
   use type CCL_Llm.Text_Access;
   package P renames CuBit.Llm_Protocol;

   Page_Bytes : constant := 4_096;
   --  The first content allocation; it doubles from here as pieces arrive.
   First_Content_Bytes : constant := 16_384;

   --  CuAlloc, fallible: a window that cannot be had refuses the ask.
   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate_zeroed";
   procedure Heap_Free (Item : Unsigned_64)
     with Import, Convention => C, External_Name => "cualloc_free";

   type Window_Record is record
      Base : Unsigned_64 := 0;
      Bytes : Natural := 0;
      Content : CCL_Llm.Text_Access := null;
      Content_Length : Natural := 0;
      Lost : Boolean := False;
   end record;
   Windows : array (Slot_Index) of Window_Record;
   States : array (Slot_Index) of Slot_State;

   --  Pull tokens: this marker, the slot and the exchange's low bits, so
   --  no other submitter's token (small, increasing) is taken for one.
   Token_Marker : constant Unsigned_64 := 16#4C4C_4D00_0000_0000#;
   Marker_Mask : constant Unsigned_64 := 16#FFFF_FF00_0000_0000#;
   Slot_Shift : constant := 32;
   Slot_Mask : constant Unsigned_64 := 16#FF#;
   Exchange_Mask : constant Unsigned_64 := 16#FFFF_FFFF#;

   function Window_Address (Slot : Slot_Index) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (Windows (Slot).Base)));
   function Window_Size (Slot : Slot_Index) return Natural is (Windows (Slot).Bytes);
   function State (Slot : Slot_Index) return Slot_State is (States (Slot));
   procedure Set_State (Slot : Slot_Index; Item : Slot_State) is
   begin
      States (Slot) := Item;
   end Set_State;

   procedure Open_Window (Slot : Slot_Index; Bytes : Positive; Opened : out Boolean) is
      Rounded : constant Natural := ((Bytes + Page_Bytes - 1) / Page_Bytes) * Page_Bytes;
   begin
      Close_Window (Slot);
      Windows (Slot).Base := Allocate_Zeroed (Unsigned_64 (Rounded), Page_Bytes);
      Opened := Windows (Slot).Base /= 0;
      Windows (Slot).Bytes := (if Opened then Rounded else 0);
   end Open_Window;

   procedure Close_Window (Slot : Slot_Index) is
      W : Window_Record renames Windows (Slot);
   begin
      if W.Base /= 0 then
         declare
            Area : String (1 .. W.Bytes) with Import, Address => Window_Address (Slot);
         begin
            Area := [others => Character'Val (0)];
         end;
         Heap_Free (W.Base);
      end if;
      CCL_Llm.Free (W.Content);
      W := (others => <>);
   end Close_Window;

   procedure Take_Content (Slot : Slot_Index; Content : out CCL_Llm.Text_Access; Length : out Natural) is
   begin
      Content := Windows (Slot).Content;
      Length := Windows (Slot).Content_Length;
      Windows (Slot).Content := null;
      Windows (Slot).Content_Length := 0;
   end Take_Content;

   function Content_Lost (Slot : Slot_Index) return Boolean is (Windows (Slot).Lost);

   --  Append the window's first Count bytes to Slot's content, growing it
   --  (doubling) up to the ceiling; past it the content is lost, and said so.
   procedure Append_Content (Slot : Slot_Index; Count : Natural) is
      W : Window_Record renames Windows (Slot);
      Piece : String (1 .. Count) with Import, Address => Window_Address (Slot);
   begin
      if W.Lost then
         return;
      elsif W.Content_Length + Count > CCL_Llm.MAXIMUM_CONTENT then
         W.Lost := True;
         CCL_Llm.Free (W.Content);
         W.Content_Length := 0;
         return;
      end if;
      if W.Content = null or else W.Content_Length + Count > W.Content'Length then
         declare
            Capacity : Natural := (if W.Content = null then First_Content_Bytes else W.Content'Length);
            Grown : CCL_Llm.Text_Access;
         begin
            while Capacity < W.Content_Length + Count loop
               Capacity := Natural'Min (2 * Capacity, CCL_Llm.MAXIMUM_CONTENT);
            end loop;
            Grown := new String (1 .. Capacity);
            if W.Content /= null then
               Grown (1 .. W.Content_Length) := W.Content (1 .. W.Content_Length);
               CCL_Llm.Free (W.Content);
            end if;
            W.Content := Grown;
         end;
      end if;
      W.Content (W.Content_Length + 1 .. W.Content_Length + Count) := Piece;
      W.Content_Length := W.Content_Length + Count;
   end Append_Content;

   function Token_Of (Slot : Slot_Index) return Unsigned_64 is
     (Token_Marker or Shift_Left (Unsigned_64 (Slot), Slot_Shift) or
      (States (Slot).Exchange and Exchange_Mask));

   procedure Pull (Slot : Slot_Index) is
      S : Slot_State renames States (Slot);
      M : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      if not S.Used or else S.Pull_Pending or else S.Text_Ready > 0 or else S.Done then
         return;
      end if;
      M.tag := (label => P.Pull_Operation, length => 2, protocol => 0, ipc => <>, reserved => 0);
      M.words := [S.Exchange, Unsigned_64 (Windows (Slot).Bytes), 0, 0];
      S.Pull_Pending := CuBit.Messages.capSubmit (CCL_Native_Bindings.Llm, M, Token_Of (Slot));
   end Pull;

   procedure Deliver (Receipt : CuBit.Messages.CompletionEntry; Consumed : out Boolean) is
      Slot_Word : constant Unsigned_64 := Shift_Right (Receipt.token, Slot_Shift) and Slot_Mask;
   begin
      Consumed := False;
      if (Receipt.token and Marker_Mask) /= Token_Marker or else Slot_Word > Unsigned_64 (Slot_Index'Last) then
         return;
      end if;
      Consumed := True;
      declare
         Slot : constant Slot_Index := Slot_Index (Slot_Word);
         S : Slot_State renames States (Slot);
         Size : constant Unsigned_64 := Unsigned_64 (Windows (Slot).Bytes);
      begin
         if not S.Used or else not S.Pull_Pending or else Receipt.token /= Token_Of (Slot) then
            return;
         end if;
         S.Pull_Pending := False;
         if Receipt.valid and then Receipt.msg.tag.label = P.Reply_Text
           and then Receipt.msg.words (0) in 1 .. Size
         then
            S.Text_Ready := Natural (Receipt.msg.words (0));
         elsif Receipt.valid and then Receipt.msg.tag.label = P.Reply_Content
           and then Receipt.msg.words (0) in 1 .. Size
         then
            --  A piece of the captured content: keep it and ask for more.
            Append_Content (Slot, Natural (Receipt.msg.words (0)));
            Pull (Slot);
         elsif Receipt.valid and then Receipt.msg.tag.label = P.Reply_Done then
            S.Done := True;
         else
            --  The service failed the pull: the exchange is over, untold.
            declare
               Area : array (0 .. Windows (Slot).Bytes - 1) of Unsigned_8
                 with Import, Address => Window_Address (Slot);
            begin
               Area := [others => 0];
               Area (P.Summary_Stop_Offset) := Unsigned_8 (P.Stop_Reason'Enum_Rep (P.Disconnected));
            end;
            S.Done := True;
         end if;
      end;
   end Deliver;
end CCL_Llm_Link;
