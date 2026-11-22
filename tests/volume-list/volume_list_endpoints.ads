with Interfaces; use Interfaces;
package Volume_List_Endpoints with Pure is
   function Valid (Item : Unsigned_64) return Boolean is (Item /= 0);
end Volume_List_Endpoints;
