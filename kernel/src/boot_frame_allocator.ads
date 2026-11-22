pragma Ada_2022;

-- Single-threaded boot reservation core. Addresses and firmware admission
-- policy belong to the caller; this component owns the actual packed bitmap.
generic
   Last_Frame : Positive;
package Boot_Frame_Allocator with SPARK_Mode, Pure is
   subtype Frame is Natural range 0 .. Last_Frame;
   subtype Payload_Frame is Frame range 1 .. Last_Frame;
   subtype Request_Size is Payload_Frame;
   type State is private;

   function Is_Free (This : State; Item : Frame) return Boolean
     with Inline_Always;
   function Highest (This : State) return Frame with Inline_Always;

   procedure Initialize (This : out State) with
     Post => Highest (This) = 0 and then
       (for all Item in Frame => not Is_Free (This, Item));

   -- Repeated admission is idempotent. Once reservations have begun, firmware
   -- admission must not reopen a frame that could already belong to metadata.
   procedure Admit (This : in out State; Item : Payload_Frame) with
     Pre => Highest (This) = 0,
     Post => Highest (This) = 0 and then Is_Free (This, Item) and then
       (for all Other in Frame =>
          (if Other /= Item then
             Is_Free (This, Other) = Is_Free (This'Old, Other)));

   -- First=0 is exhaustion, never a frame. No partial reservation on failure.
   procedure Reserve
     (This : in out State; Size : Request_Size; First : out Frame) with
     Post =>
       (if First = 0 then This = This'Old
        else First <= Last_Frame - Size + 1 and then
          Highest (This) = Frame'Max (Highest (This'Old), First + (Size - 1))
          and then
          (for all Item in Frame =>
             (if Item >= First and then Item - First < Size then
                Is_Free (This'Old, Item) and then not Is_Free (This, Item)
              else Is_Free (This, Item) = Is_Free (This'Old, Item))));

   -- Diagnostic only: no cached count to become inconsistent with the bitmap.
   function Free_Count (This : State) return Frame;

private
   type Frame_Bitmap is array (Frame) of Boolean with Component_Size => 1;
   type State is record
      Available : Frame_Bitmap := [others => False];
      High_Water : Frame := 0;
   end record;

   function Is_Free (This : State; Item : Frame) return Boolean is
     (This.Available (Item));
   function Highest (This : State) return Frame is (This.High_Water);
end Boot_Frame_Allocator;
