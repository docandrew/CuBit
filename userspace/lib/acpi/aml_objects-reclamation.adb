with AML_Index_Handles;
package body AML_Objects.Reclamation with SPARK_Mode is
   use type AML_Identity.Identity;
   use type AML_References.Reference_Kind;
   function Reclaimed_State (Store, Prior : State; Keep : Keep_Set) return Boolean is
      Bytes : Natural range 0 .. Max_Bytes := 0;
      Elements : Natural range 0 .. Max_Elements := 0;
      Live : Object_ID := 0;
   begin
      if Store.Used /= Prior.Used or else Store.Generation_Limit /= Prior.Generation_Limit then return False; end if;
      for I in 1 .. Max_Objects loop
         if Store.Objects (I).Occupied /= Keep (I)
           or else Store.Objects (I).Stamp /= Prior.Objects (I).Stamp then return False; end if;
         if not Keep (I) then
            if Store.Objects (I) /= Object_Record'(Stamp => Prior.Objects (I).Stamp, others => <>) then return False; end if;
         else
            if not Is_Live (Prior, I) then return False; end if;
            Live := Live + 1;
            case Prior.Objects (I).Tag is
               when Integer_Object | Reference_Object =>
                  if Store.Objects (I) /= Prior.Objects (I) then return False; end if;
               when String_Object | Buffer_Object =>
                  if Store.Objects (I) /= (Prior.Objects (I) with delta First => Bytes)
                    or else Prior.Objects (I).Size > Max_Bytes - Bytes then return False; end if;
                  for J in 1 .. Prior.Objects (I).Size loop
                     if Store.Bytes (Bytes + J) /= Prior.Bytes (Prior.Objects (I).First + J) then return False; end if;
                  end loop;
                  Bytes := Bytes + Prior.Objects (I).Size;
               when Package_Object =>
                  if Store.Objects (I) /= (Prior.Objects (I) with delta First => Elements)
                    or else Prior.Objects (I).Size > Max_Elements - Elements then return False; end if;
                  for J in 1 .. Prior.Objects (I).Size loop
                     if Store.Elements (Elements + J) /= Prior.Elements (Prior.Objects (I).First + J) then return False; end if;
                  end loop;
                  Elements := Elements + Prior.Objects (I).Size;
            end case;
         end if;
      end loop;
      return Store.Live_Used = Live and then Store.Bytes_Used = Bytes and then Store.Elements_Used = Elements
        and then (for all I in Bytes + 1 .. Max_Bytes => Store.Bytes (I) = 0)
        and then (for all I in Elements + 1 .. Max_Elements => Store.Elements (I) = No_Object);
   end Reclaimed_State;
   procedure Reclaim
     (Store : in out State; Owner : AML_Identity.Identity; Keep : Keep_Set;
      Scratch : in out Workspace; Status : out Reclaim_Status)
   is
      Bytes : Natural range 0 .. Max_Bytes := 0;
      Elements : Natural range 0 .. Max_Elements := 0;
      Live : Object_ID := 0;
      Container : Object_ID;
      Address : AML_Object_Identifiers.Object_Address;
      Index : Natural;
      R : AML_References.Reference;
   begin
      Status := Invalid_Owner;
      if Owner = AML_Identity.No_Identity then return; end if;
      Status := Invalid_Keep_Set;
      for I in 1 .. Max_Objects loop
         if Keep (I) and then not Is_Live (Store, I) then return; end if;
      end loop;
      for I in 1 .. Store.Used loop
         if Keep (I) then
            case Store.Objects (I).Tag is
               when Package_Object =>
                  Status := Unclosed_Package;
                  for J in 1 .. Store.Objects (I).Size loop
                     Container := Store.Elements (Store.Objects (I).First + J);
                     if Container /= No_Object and then not Keep (Container) then return; end if;
                  end loop;
                  Status := Storage_Limit;
                  if Store.Objects (I).Size > Max_Elements - Elements then return; end if;
                  Elements := Elements + Store.Objects (I).Size;
               when String_Object | Buffer_Object =>
                  Status := Storage_Limit;
                  if Store.Objects (I).Size > Max_Bytes - Bytes then return; end if;
                  Bytes := Bytes + Store.Objects (I).Size;
               when Reference_Object =>
                  R := Store.Objects (I).Reference;
                  if AML_References.Belongs_To (R, Owner) then
                     Container := No_Object;
                     if AML_References.Kind (R) = AML_References.Byte_Slot then
                        Address := AML_Index_Handles.Address (AML_References.Byte_Item (R));
                        Index := AML_Index_Handles.Offset (AML_References.Byte_Item (R));
                        if Matches_Address (Store, Address) then
                           Container := AML_Object_Identifiers.Slot_Of (Address);
                           if Store.Objects (Container).Tag not in Byte_Kind or else Index >= Store.Objects (Container).Size then Container := No_Object; end if;
                        end if;
                     elsif AML_References.Kind (R) = AML_References.Package_Slot then
                        Address := AML_Index_Handles.Address (AML_References.Package_Item (R));
                        Index := AML_Index_Handles.Offset (AML_References.Package_Item (R));
                        if Matches_Address (Store, Address) then
                           Container := AML_Object_Identifiers.Slot_Of (Address);
                           if Store.Objects (Container).Tag /= Package_Object or else Index >= Store.Objects (Container).Size then Container := No_Object; end if;
                        end if;
                     end if;
                     Status := Unclosed_Container_Reference;
                     if Container /= No_Object and then not Keep (Container) then return; end if;
                  end if;
               when Integer_Object => null;
            end case;
         end if;
      end loop;
      -- All rejection checks precede mutation. Scratch belongs to the caller;
      -- no callback or evaluator-visible publication occurs before commit.
      Bytes := 0; Elements := 0;
      Scratch.Bytes := [others => 0]; Scratch.Elements := [others => 0];
      for I in 1 .. Max_Objects loop
         Scratch.Objects (I) := (Stamp => Store.Objects (I).Stamp, others => <>);
         if Keep (I) then
            Scratch.Objects (I) := Store.Objects (I); Live := Live + 1;
            case Store.Objects (I).Tag is
               when String_Object | Buffer_Object =>
                  Scratch.Objects (I).First := Bytes;
                  for J in 1 .. Store.Objects (I).Size loop
                     Scratch.Bytes (Bytes + J) := Store.Bytes (Store.Objects (I).First + J);
                  end loop;
                  Bytes := Bytes + Store.Objects (I).Size;
               when Package_Object =>
                  Scratch.Objects (I).First := Elements;
                  for J in 1 .. Store.Objects (I).Size loop
                     Scratch.Elements (Elements + J) := Store.Elements (Store.Objects (I).First + J);
                  end loop;
                  Elements := Elements + Store.Objects (I).Size;
               when Integer_Object | Reference_Object => null;
            end case;
         end if;
      end loop;
      Store.Objects := Scratch.Objects; Store.Bytes := Scratch.Bytes; Store.Elements := Scratch.Elements;
      Store.Live_Used := Live; Store.Bytes_Used := Bytes; Store.Elements_Used := Elements;
      Status := Reclaimed;
   end Reclaim;
end AML_Objects.Reclamation;
