#include <inttypes.h>
#include <wchar.h>
typedef void * (*fd4fun)(void*, void*);
typedef void **clo;
extern void *fd4_mkclosure(void*, int, ...);
extern uint64_t fd4_printn(uint64_t);
extern uint64_t fd4_sub(uint64_t, uint64_t);

uint64_t fd4_fix____f__x_____48 (clo fd4_fix__clos_____49, uint64_t fd4_x) {
  return ({({
    fd4fun fd4_f = fd4_fix__clos_____49;
    (fd4_x
    ? ({
        clo fd4_app_____50 = fd4_f;
        (uint64_t)(((fd4fun) (fd4fun)((fd4_app_____50)[0]))( (void *)fd4_app_____50
        , (void *)({
          fd4_sub(fd4_x, 1);
        }) ));
      })
    : fd4_x);
  });});
}

fd4fun fd4_f;
uint64_t fd4_a;
uint64_t* fd4main() {
  fd4_f = (fd4_mkclosure(fd4_fix____f__x_____48, 0));
  fd4_a = (({
    wprintf(L"hola");
    fd4_printn(({({
      clo fd4_app_____51 = fd4_f;
      (uint64_t)(((fd4fun) (fd4fun)((fd4_app_____51)[0]))( (void *)fd4_app_____51
      , (void *)10 ));
    });}));
  }));
  return 0;
}