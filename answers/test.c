#include <inttypes.h>
#include <wchar.h>
typedef void * (*fd4fun)(void*, void*);
typedef void **clo;
extern void *fd4_mkclosure(void*, int, ...);
extern uint64_t fd4_printn(uint64_t);
extern uint64_t fd4_sub(uint64_t, uint64_t);

uint64_t fd4_fun____n_____50 (clo fd4_clos__lam_____51, uint64_t fd4_n) {
  return ({({
    uint64_t fd4_m = (uint64_t)((fd4_clos__lam_____51)[1]);
    ({
      fd4fun fd4_mul = (fd4fun)((fd4_clos__lam_____51)[2]);
      ({
        uint64_t fd4_m = (uint64_t)((fd4_clos__lam_____51)[3]);
        (fd4_n
        ? fd4_m + ({
            clo fd4_app_____52 = ({
              clo fd4_app_____53 = fd4_mul;
              (fd4fun)(((fd4fun) (fd4fun)((fd4_app_____53)[0]))( (void *)fd4_app_____53
              , (void *)fd4_m ));
            });
            (uint64_t)(((fd4fun) (fd4fun)((fd4_app_____52)[0]))( (void *)fd4_app_____52
            , (void *)({
              fd4_sub(fd4_n, 1);
            }) ));
          })
        : 1);
      });
    });
  });});
}

fd4fun fd4_fix____mul__m_____48 (clo fd4_fix__clos_____49, uint64_t fd4_m) {
  return ({({
    fd4fun fd4_mul = fd4_fix__clos_____49;
    fd4_mkclosure(fd4_fun____n_____50, 3, fd4_m, fd4_mul, fd4_m);
  });});
}

fd4fun fd4_mul;
uint64_t fd4_x;
uint64_t* fd4main() {
  fd4_mul = (fd4_mkclosure(fd4_fix____mul__m_____48, 0));
  fd4_x = (({
    wprintf(L"Papu, anda ");
    fd4_printn(({({
      clo fd4_app_____54 = ({
        clo fd4_app_____55 = fd4_mul;
        (fd4fun)(((fd4fun) (fd4fun)((fd4_app_____55)[0]))( (void *)fd4_app_____55
        , (void *)3 ));
      });
      (uint64_t)(((fd4fun) (fd4fun)((fd4_app_____54)[0]))( (void *)fd4_app_____54
      , (void *)11 ));
    });}));
  }));
  return 0;
}