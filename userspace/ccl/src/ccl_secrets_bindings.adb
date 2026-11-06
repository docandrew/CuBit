with CCL.Interfaces.Secrets;
with CCL.Objects;
with CCL.Objects.Views;
with CCL_Secrets;
with CuBit.Failures;

package body CCL_Secrets_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Objects.Build_Result;
   use type CCL.Host_Values.Value_Kind;
   package S renames CCL.Interfaces.Secrets;
   use type S.Operation;
   package Views renames CCL.Objects.Views;

   Contracts : S.Contracts;

   function Handles (Binding : Unsigned_32) return Boolean is
     (for some Op in S.Operation => Binding = S.Binding_Of (Op));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      S.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, S.SECRET_KEY, Contracts.Secret);
      CCL.Catalog.Resolve_Schema (Catalog, S.STATE_KEY, Contracts.State);
      Success := CCL.Objects.Is_Bound (Contracts.Secret) and then CCL.Objects.Is_Bound (Contracts.State);
      for Op in S.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "secrets." & S.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, S.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   function Failure_Of (Result : CCL_Secrets.Call_Result) return CuBit.Failures.Failure is
     (case Result is
         when CCL_Secrets.Not_Granted => CuBit.Failures.Failed
           (CuBit.Failures.Not_Granted, "secrets: the program has no secrets service",
            "its manifest must request (service ""secrets"" ""secrets"")"),
         when CCL_Secrets.Bad_Name => CuBit.Failures.Failed
           (CuBit.Failures.Invalid_Argument, "secrets: a secret name is 1 .. 64 of a-z, 0-9 and -"),
         when others => CuBit.Failures.Failed
           (CuBit.Failures.Unavailable, "secrets: secrets.svc did not answer"));

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Object : Views.Snapshot;
      Captured : Boolean := False;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then return; end if;
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Contracts.Secret, Argument.Object, Captured);
      end if;
      if not Captured then
         Reply.Why := CuBit.Failures.Failed (CuBit.Failures.Invalid_Argument, "secrets: takes a Secret");
         return;
      end if;
      declare
         Name : constant String := Views.Text (Object, Views.Field (Object, Views.Root (Object), S.SECRET_NAME));
         Op : constant S.Operation := S.Operation'Val (Binding - S.FIRST_BINDING);
         Result : CCL_Secrets.Call_Result;
         use type CCL_Secrets.Call_Result;
      begin
         case Op is
            when S.Set =>
               declare
                  Asked : CCL_Secrets.Store_Result;
                  use type CCL_Secrets.Store_Result;
               begin
                  CCL_Secrets.Request_Value (Name, Asked);
                  if Asked = CCL_Secrets.Stored then
                     --  Entry is armed; the reference itself is the answer.
                     Reply := (Value => Argument, Success => True, Why => <>);
                  else
                     Reply.Why := CuBit.Failures.Failed
                       ((if Asked = CCL_Secrets.Not_Granted then CuBit.Failures.Not_Granted
                         else CuBit.Failures.Refused), "secrets: " & CCL_Secrets.Explain (Asked));
                  end if;
               end;
            when S.Forget =>
               CCL_Secrets.Forget (Name, Result);
               if Result = CCL_Secrets.Done then
                  Reply := (Value => Argument, Success => True, Why => <>);
               else
                  Reply.Why := Failure_Of (Result);
               end if;
            when S.Status | S.Revoke_Use =>
               if Op = S.Revoke_Use then
                  CCL_Secrets.Revoke_Use (Name, Result);
                  if Result /= CCL_Secrets.Done then
                     Reply.Why := Failure_Of (Result);
                     return;
                  end if;
               end if;
               declare
                  Stored : Boolean;
                  Holder : String (1 .. CCL_Secrets.MAXIMUM_NAME);
                  Holder_Length : Natural;
                  Image : CCL.Objects.Image := CCL.Objects.Empty (Contracts.State);
                  Step : CCL.Objects.Build_Result := CCL.Objects.Added;
               begin
                  CCL_Secrets.Status (Name, Stored, Holder, Holder_Length, Result);
                  if Result /= CCL_Secrets.Done then
                     Reply.Why := Failure_Of (Result);
                     return;
                  end if;
                  CCL.Objects.Append (Image, CCL.Objects.Product_Cell (S.STATE_FIELDS), Step);
                  if Step = CCL.Objects.Added then CCL.Objects.Append_Text (Image, Name, Step); end if;
                  if Step = CCL.Objects.Added then
                     CCL.Objects.Append (Image, CCL.Objects.Boolean_Cell (Stored), Step);
                  end if;
                  if Step = CCL.Objects.Added then
                     CCL.Objects.Append_Text (Image, Holder (1 .. Holder_Length), Step);
                  end if;
                  if Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Contracts.State) then
                     Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
                  else
                     Reply.Why := CuBit.Failures.Failed (CuBit.Failures.Exhausted, "secrets: the state does not fit");
                  end if;
               end;
         end case;
      end;
   end Invoke;
end CCL_Secrets_Bindings;
