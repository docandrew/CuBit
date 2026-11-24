pragma Ada_2022;
with AML_Names;
with AML_Buffer_Layouts;
with Ada.Unchecked_Deallocation;
package body AML_Data with SPARK_Mode is
   use AML_Decode;
   use AML_Objects;
   use type Byte;
   use type Integer_Value;
   package String_Reader is new AML_Objects.String_Initializers
     (AML_Byte_Sources.Storage, AML_Byte_Sources.Source_Length, AML_Byte_Sources.Source_Byte);
   procedure Load_Bound_From_Source
     (Store : in out AML_Objects.State; Data : Bytes;
      Source : aliased AML_Byte_Sources.Storage; Source_First : Natural;
      Width : Integer_Width; Environment : in out Context; ID : out Object_ID;
      Consumed : out Natural; Status : out AML_Decode.Status)
   is
      -- Keep the capacity-sized transaction frame off the integer-only path.
      procedure Load_Transactional is
      -- Only allocations made by this parse are journaled. Committed older
      -- objects are immutable here; package writes target newly born packages.
      type Journal_Node;
      type Journal_Owner is access Journal_Node;
      type Journal_Node (Byte_Count, Element_Count : Natural) is limited record
         Before : Construction_Receipt (Byte_Count, Element_Count);
         Previous : Journal_Owner;
      end record;
      procedure Free is new Ada.Unchecked_Deallocation (Journal_Node, Journal_Owner);
      Head : Journal_Owner := null;
      Max_New : constant Natural := Natural'Min
        (Data'Length, Store.Object_Capacity - Live_Count (Store));
      Born : array (Positive range 1 .. Max_New) of Object_ID := [others => No_Object];
      Born_Count : Natural range 0 .. Max_New := 0;
      Initial_Live : constant Object_ID := Live_Count (Store);
      Plan : Construction_Plan;
      -- The allocator chooses the lowest reusable slot. Without collection or
      -- retirement inside a parse, successive allocated slots increase.
      function Original_Is_Live (Value : Object_ID) return Boolean is
         Low : Natural := 0;
         High : Natural := Born_Count;
         Middle : Positive;
      begin
         if not Is_Live (Store, Value) then return False; end if;
         while Low < High loop
            Middle := Low + (High - Low) / 2 + 1;
            if Born (Middle) = Value then return False;
            elsif Born (Middle) < Value then Low := Middle;
            else High := Middle - 1; end if;
         end loop;
         return True;
      end Original_Is_Live;
      procedure Prepare (Bytes : Integer_Value; Elements : Natural;
                         Result : out Allocation_Status) is
      begin
         Plan_Construction (Store, Bytes, Elements, Plan, Result);
         if Result /= Allocated then return; end if;
         if Born_Count = Max_New then Result := Object_Limit; return; end if;
         -- Fully initialized owned node, sized only to the overwritten spans.
         -- No store mutation occurs until its before-image allocation succeeds.
         Head := new Journal_Node'
           (Byte_Count => Construction_Bytes (Plan),
            Element_Count => Construction_Elements (Plan),
            Before => Capture_Construction (Store, Plan), Previous => Head);
      exception
         when Storage_Error => Result := Storage_Unavailable;
      end Prepare;
      procedure Finish_Allocation (Value : Object_ID) is
      begin
         pragma Assert (Born_Count < Max_New);
         pragma Assert (Born_Count = 0 or else Born (Born_Count) < Value);
         Born_Count := Born_Count + 1;
         Born (Born_Count) := Value;
      end Finish_Allocation;
      procedure Close_Journal (Rollback : Boolean) is
         Old : Journal_Owner;
      begin
         while Head /= null loop
            Old := Head;
            Head := Old.Previous;
            Old.Previous := null;
            if Rollback then Restore_Construction (Store, Old.Before); end if;
            Free (Old);
         end loop;
      end Close_Journal;
      function Copied_Context (Item : Context) return Context is (Item);
      pragma No_Inline (Copied_Context);
      Candidate_Context : Context := Copied_Context (Environment);
      type Frame is record
         Object : Object_ID := 0;
         Limit : Natural := 0;
         Next : Natural := 0;
      end record;
      Frames : array (Positive range 1 .. 64) of Frame := [others => <>];
      Depth : Natural range 0 .. 64 := 0;
      Offset : Natural := 0;
      Root : Object_ID := 0;
      Item : Object_ID;
      Limit, Finish, Size : Natural;
      Is_Package : Boolean;
      Op : Byte;
      P : Package_Result;
      N : Integer_Result;
      Package_Count : Count_Result;
      Member : Member_Result;
      S : String_Span_Result;
      B : AML_Buffer_Layouts.Layout_Result;
      Allocation : Allocation_Status;
      procedure Execute_Transaction is
   begin
      ID := 0; Consumed := 0; Status := Malformed;
      if not AML_Byte_Sources.Corresponds (Source, Source_First, Data) then return; end if;
      Status := Truncated;
      loop
         pragma Loop_Invariant (Valid (Store));
         pragma Loop_Invariant (Live_Count (Store) = Initial_Live + Born_Count);
         pragma Loop_Invariant (Root = No_Object or else Is_Live (Store, Root));
         pragma Loop_Invariant (Root = 0 or Offset > 0);
         pragma Loop_Invariant (Offset <= Data'Length);
         pragma Loop_Invariant
           (for all J in 1 .. Depth =>
              Is_Live (Store, Frames (J).Object)
              and then Kind (Store, Frames (J).Object) = Package_Object
              and then Frames (J).Next <= Length (Store, Frames (J).Object)
              and then Offset <= Frames (J).Limit and then Frames (J).Limit <= Data'Length);
         pragma Loop_Invariant
           (for all J in 1 .. Depth =>
              (for all K in J .. Depth => Frames (K).Limit <= Frames (J).Limit));
         pragma Loop_Variant (Decreases => Data'Length - Offset, Decreases => Depth);
         if Depth = 0 and Root /= 0 then
            Environment := Candidate_Context; ID := Root; Consumed := Offset; Status := Accepted;
            return;
         elsif Depth > 0 and then Offset = Frames (Depth).Limit then
            Depth := Depth - 1;
         else
            Limit := (if Depth = 0 then Data'Length else Frames (Depth).Limit);
            if Offset = Limit then return; end if;
            if Depth > 0 and then Frames (Depth).Next = Length (Store, Frames (Depth).Object) then
               Status := Malformed; return;
            end if;
            Op := Data (Data'First + Offset);
            Is_Package := Op in 16#12# | 16#13#;
            Finish := Offset;
            if Is_Package then
               if Depth = 64 then Status := Limit_Exceeded; return; end if;
               Offset := Offset + 1;
               if Offset = Limit then return; end if;
               P := Read_Package (Data (Data'First + Offset .. Data'First + (Limit - 1)));
               if P.Kind /= Accepted then Status := P.Kind; return; end if;
               Finish := Offset + P.Extent;
               Offset := Offset + P.Encoding_Bytes;
               if Offset = Finish then Status := Truncated; return; end if;
               if Op = 16#12# then
                  Size := Natural (Data (Data'First + Offset));
                  Offset := Offset + 1;
               else
                  Package_Count := Read_Count (Environment,
                    Data (Data'First + Offset .. Data'First + (Finish - 1)), Width);
                  if Package_Count.Kind /= Accepted then Status := Package_Count.Kind; return; end if;
                  if Package_Count.Consumed = 0 or else Package_Count.Consumed > Finish - Offset then
                     Status := Malformed; return;
                  end if;
                  if Package_Count.Value > Integer_Value (Max_Elements) then Status := Limit_Exceeded; return; end if;
                  Size := Natural (Package_Count.Value);
                  Offset := Offset + Package_Count.Consumed;
               end if;
               Prepare (0, Size, Allocation);
               if Allocation = Allocated then
                  Construct_Package (Store, Plan, Item); Finish_Allocation (Item);
               end if;
            elsif Depth > 0 and then
              (AML_Names.Lead (Op) or else Op in 16#5C# | 16#5E# | 16#2E# | 16#2F#)
            then
               Read_Member
                 (Candidate_Context, Frames (Depth).Object, Frames (Depth).Next,
                  Data (Data'First + Offset .. Data'First + (Limit - 1)), Member);
               if Member.Kind /= Accepted then Status := Member.Kind; return; end if;
               if Member.Consumed = 0 or else Member.Consumed > Limit - Offset
                 or else (Member.Binding = Resolved_Member and then
                   not Original_Is_Live (Member.ID))
                 or else (Member.Binding = Deferred_Member and then Member.ID /= No_Object)
               then Status := Malformed; return; end if;
               -- Ordinary package construction preserves aliases. CopyObject
               -- supplies deep copying later; binding must retain this identity.
               Item := Member.ID;
               Allocation := Allocated;
               Offset := Offset + Member.Consumed;
            elsif Op = 16#0D# then
               S := Read_String_Span (Data (Data'First + Offset .. Data'First + (Limit - 1)));
               if S.Kind /= Accepted then Status := S.Kind; return; end if;
               Prepare (Integer_Value (S.Length), 0, Allocation);
               if Allocation = Allocated then
                  String_Reader.Construct (Store, Plan, Source,
                    Source_First + Offset + S.Payload_Offset, Item);
                  Finish_Allocation (Item);
               end if;
               Offset := Offset + S.Consumed;
            elsif Op = 16#11# then
               B := AML_Buffer_Layouts.Read
                 (Data (Data'First + Offset .. Data'First + (Limit - 1)), Width);
               if B.Kind /= Accepted then Status := B.Kind; return; end if;
               Prepare (B.Effective_Length, 0, Allocation);
               if Allocation = Allocated then
                  if B.Initializer_Length = 0 then
                     Construct_Buffer (Store, Plan, Bytes'(1 .. 0 => 0), Item);
                  else
                     Construct_Buffer (Store, Plan,
                       Data (Data'First + Offset + B.Initializer_Offset ..
                             Data'First + Offset + (B.Consumed - 1)), Item);
                  end if;
                  Finish_Allocation (Item);
               end if;
               Offset := Offset + B.Consumed;
            else
               N := Read_Integer (Data (Data'First + Offset .. Data'First + (Limit - 1)), Width);
               if N.Kind /= Accepted then Status := N.Kind; return; end if;
               Offset := Offset + N.Consumed;
               Prepare (0, 0, Allocation);
               if Allocation = Allocated then
                  Construct_Integer (Store, Plan, N.Value, Item,
                    Literal_Origin (Data (Data'First + (Offset - N.Consumed))));
                  Finish_Allocation (Item);
               end if;
            end if;
            if Allocation /= Allocated then Status := Limit_Exceeded; return; end if;
            if Depth = 0 then
               Root := Item;
            else
               Set_Element (Store, Frames (Depth).Object, Frames (Depth).Next, Item);
               Frames (Depth).Next := Frames (Depth).Next + 1;
            end if;
            if Is_Package then
               Depth := Depth + 1;
               Frames (Depth) := (Object => Item, Limit => Finish, Next => 0);
            end if;
         end if;
      end loop;
      end Execute_Transaction;
   begin
      Execute_Transaction;
      Close_Journal (Rollback => Status /= Accepted);
      end Load_Transactional;
      pragma No_Inline (Load_Transactional);
      Literal : Integer_Result;
      Item : Object_ID;
      Allocation : Allocation_Status;
   begin
      ID := No_Object;
      Consumed := 0;
      Status := Malformed;
      Literal := Read_Integer (Data, Width);
      if Literal.Kind = Accepted then
         -- Preserve the original source-correspondence admission rule, including
         -- trailing bytes. Do not allocate before it succeeds.
         if not AML_Byte_Sources.Corresponds (Source, Source_First, Data) then return; end if;
         New_Integer (Store, Literal.Value, Item, Allocation,
           Literal_Origin (Data (Data'First)));
         if Allocation = Allocated then
            ID := Item;
            Consumed := Literal.Consumed;
            Status := Accepted;
         else
            Status := Limit_Exceeded;
         end if;
      else
         -- Composite values and malformed/truncated encodings retain the
         -- existing parser and its complete rollback/status behavior.
         Load_Transactional;
      end if;
   end Load_Bound_From_Source;
   procedure Load_Bound
     (Store : in out AML_Objects.State; Data : Bytes;
      Width : Integer_Width; Environment : in out Context; ID : out Object_ID;
      Consumed : out Natural; Status : out AML_Decode.Status)
   is
      Source : aliased constant AML_Byte_Sources.Storage :=
        (Length => Data'Length, Bytes => Data);
      procedure Load_Source is new Load_Bound_From_Source (Context, Read_Count, Read_Member);
   begin
      Load_Source (Store, Data, Source, 0, Width, Environment, ID, Consumed, Status);
   end Load_Bound;
   type Empty_Context is null record;
   function Literal_Count
     (Environment : Empty_Context; Data : Bytes; Width : Integer_Width)
      return Count_Result
     with Post => (if Literal_Count'Result.Kind = Accepted then
       Literal_Count'Result.Consumed <= Data'Length)
   is
      pragma Unreferenced (Environment);
      N : constant Integer_Result := Read_Integer (Data, Width);
   begin
      if N.Kind = Accepted then return (Accepted, N.Value, N.Consumed); end if;
      return (Kind => N.Kind, others => <>);
   end Literal_Count;
   procedure Reject_Member (Environment : in out Empty_Context;
      Package_ID : Object_ID; Element : Natural; Data : Bytes; Result : out Member_Result)
   is
      pragma Unreferenced (Environment, Package_ID, Element, Data);
   begin Result := (Kind => Unsupported, others => <>); end Reject_Member;
   procedure Load_Literals is new Load_Bound (Empty_Context, Literal_Count, Reject_Member);
   procedure Load
     (Store : in out AML_Objects.State; Data : Bytes;
      Width : Integer_Width; ID : out Object_ID;
      Consumed : out Natural; Status : out AML_Decode.Status)
   is
      Environment : Empty_Context;
   begin
      Load_Literals (Store, Data, Width, Environment, ID, Consumed, Status);
   end Load;
end AML_Data;
