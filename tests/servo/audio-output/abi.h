typedef unsigned long U;
static inline U call6(U n,U a,U b,U c,U d,U e,U f){
 U r;register U r10 __asm__("r10")=d;register U r8 __asm__("r8")=e;register U r9 __asm__("r9")=f;
 __asm__ volatile("syscall":"=a"(r):"a"(n),"D"(a),"S"(b),"d"(c),"r"(r10),"r"(r8),"r"(r9):"rcx","r11","memory");return r;
}
static inline U call(U n,U a,U b,U c){return call6(n,a,b,c,0,0,0);}
static inline void say(const char*s){U n=0;while(s[n])n++;call(12,1,(U)s,n);}
