pragma Ada_2022;
package body AML_Objects.Package_References with SPARK_Mode is
   procedure Make
     (Store : State; Source : Object_ID; Index : AML_Decode.Integer_Value;
      Ref : out Reference; Status : out Result_Status)
   is
   begin
      Ref := No_Reference;
      if Source = No_Object or else Source > Count (Store) then
         Status := Invalid_Object; return;
      end if;
      if Kind (Store, Source) /= Package_Object then
         Status := Wrong_Kind; return;
      end if;
      if Index > AML_Decode.Integer_Value (Natural'Last) then
         Status := Out_Of_Bounds; return;
      end if;
      pragma Assert (AML_Decode.Integer_Value (Natural (Index)) = Index);
      if Natural (Index) >= Length (Store, Source) then
         Status := Out_Of_Bounds; return;
      end if;
      Ref := (Present => True, Source => Source, Index => Natural (Index));
      Status := Ready;
   end Make;
   procedure Read
     (Store : State; Ref : Reference; Value : out Object_ID;
      Status : out Result_Status)
   is
   begin
      Value := 0;
      if not Is_Valid (Store, Ref) then Status := Invalid_Reference; return; end if;
      Value := Element (Store, Owner (Ref), Offset (Ref));
      Status := Ready;
   end Read;
   procedure Write
     (Store : in out State; Ref : Reference; Value : Object_ID;
      Status : out Result_Status)
   is
   begin
      if not Is_Valid (Store, Ref) then Status := Invalid_Reference; return; end if;
      if Value > Count (Store) then Status := Invalid_Value; return; end if;
      Set_Element (Store, Owner (Ref), Offset (Ref), Value);
      Status := Ready;
   end Write;
end AML_Objects.Package_References;
