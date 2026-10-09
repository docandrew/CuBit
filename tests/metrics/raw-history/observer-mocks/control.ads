with Interfaces; use Interfaces;
package Control is
 type Mode is (Success, Bad_Page, Failed_Call, Replaced_Endpoint, Denied, Bad_Length, Bad_Flags, Bad_Reserved, Mismatched_Tag, Too_Many_Rows, Reversed_Cursor, Gap_Overflow);
 Current : Mode := Success;
 Identity : Unsigned_64 := 2 ** 32 + 77;
 Calls, Creates, Revokes : Natural := 0;
 Create_OK, Retired, Revoke_OK : Boolean := True;
end Control;
