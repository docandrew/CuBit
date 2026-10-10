pragma Ada_2022;
with AML_Names;
package body AML_Data with SPARK_Mode is
   use AML_Decode;
   use AML_Objects;
   use type Byte;
   use type Integer_Value;
   procedure Load_Bound
     (Store : in out AML_Objects.State; Data : Bytes;
      Width : Integer_Width; Environment : in out Context; ID : out Object_ID;
      Consumed : out Natural; Status : out AML_Decode.Status)
   is
      Candidate : AML_Objects.State := Store;
      Candidate_Context : Context := Environment;
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
      S : String_Result;
      B : Buffer_Result;
      Allocation : Allocation_Status;
   begin
      ID := 0; Consumed := 0; Status := Truncated;
      loop
         pragma Loop_Invariant (Valid (Candidate));
         pragma Loop_Invariant (Live_Count (Candidate) >= Live_Count (Store));
         pragma Loop_Invariant (Root = No_Object or else Is_Live (Candidate, Root));
         pragma Loop_Invariant (Root = 0 or Offset > 0);
         pragma Loop_Invariant (Offset <= Data'Length);
         pragma Loop_Invariant
           (for all J in 1 .. Slot_Bound (Store) => (if Is_Live (Store, J) then
              Kind (Candidate, J) = Kind (Store, J)
              and then Length (Candidate, J) = Length (Store, J)));
         pragma Loop_Invariant
           (for all J in 1 .. Depth =>
              Is_Live (Candidate, Frames (J).Object)
              and then Kind (Candidate, Frames (J).Object) = Package_Object
              and then Frames (J).Next <= Length (Candidate, Frames (J).Object)
              and then Offset <= Frames (J).Limit and then Frames (J).Limit <= Data'Length);
         pragma Loop_Invariant
           (for all J in 1 .. Depth =>
              (for all K in J .. Depth => Frames (K).Limit <= Frames (J).Limit));
         pragma Loop_Variant (Decreases => Data'Length - Offset, Decreases => Depth);
         if Depth = 0 and Root /= 0 then
            Store := Candidate; Environment := Candidate_Context; ID := Root; Consumed := Offset; Status := Accepted;
            return;
         elsif Depth > 0 and then Offset = Frames (Depth).Limit then
            Depth := Depth - 1;
         else
            Limit := (if Depth = 0 then Data'Length else Frames (Depth).Limit);
            if Offset = Limit then return; end if;
            if Depth > 0 and then Frames (Depth).Next = Length (Candidate, Frames (Depth).Object) then
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
               New_Package (Candidate, Size, Item, Allocation);
            elsif Depth > 0 and then
              (AML_Names.Lead (Op) or else Op in 16#5C# | 16#5E# | 16#2E# | 16#2F#)
            then
               Read_Member
                 (Candidate_Context, Frames (Depth).Object, Frames (Depth).Next,
                  Data (Data'First + Offset .. Data'First + (Limit - 1)), Member);
               if Member.Kind /= Accepted then Status := Member.Kind; return; end if;
               if Member.Consumed = 0 or else Member.Consumed > Limit - Offset
                 or else (Member.Binding = Resolved_Member and then
                   not Is_Live (Store, Member.ID))
                 or else (Member.Binding = Deferred_Member and then Member.ID /= No_Object)
               then Status := Malformed; return; end if;
               -- Ordinary package construction preserves aliases. CopyObject
               -- supplies deep copying later; binding must retain this identity.
               Item := Member.ID;
               Allocation := Allocated;
               Offset := Offset + Member.Consumed;
            elsif Op = 16#0D# then
               S := Read_String (Data (Data'First + Offset .. Data'First + (Limit - 1)));
               if S.Kind /= Accepted then Status := S.Kind; return; end if;
               Offset := Offset + S.Consumed;
               declare
                  L : constant Natural := S.Length;
                  Text : Bytes (1 .. L);
               begin
                  for J in Text'Range loop Text (J) := Character'Pos (S.Text (J)); end loop;
                  New_Bytes (Candidate, String_Object, Text, Item, Allocation);
               end;
            elsif Op = 16#11# then
               B := Read_Buffer (Data (Data'First + Offset .. Data'First + (Limit - 1)), Width);
               if B.Kind /= Accepted then Status := B.Kind; return; end if;
               Offset := Offset + B.Consumed;
               New_Bytes (Candidate, Buffer_Object, B.Content (1 .. B.Length), Item, Allocation);
            else
               N := Read_Integer (Data (Data'First + Offset .. Data'First + (Limit - 1)), Width);
               if N.Kind /= Accepted then Status := N.Kind; return; end if;
               Offset := Offset + N.Consumed;
               New_Integer (Candidate, N.Value, Item, Allocation,
                 Literal_Origin (Data (Data'First + (Offset - N.Consumed))));
            end if;
            if Allocation /= Allocated then Status := Limit_Exceeded; return; end if;
            if Depth = 0 then
               Root := Item;
            else
               Set_Element (Candidate, Frames (Depth).Object, Frames (Depth).Next, Item);
               Frames (Depth).Next := Frames (Depth).Next + 1;
            end if;
            if Is_Package then
               Depth := Depth + 1;
               Frames (Depth) := (Object => Item, Limit => Finish, Next => 0);
            end if;
         end if;
      end loop;
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
