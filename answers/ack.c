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
      fd4fun fd4_ack = (fd4fun)((fd4_clos__lam_____51)[2]);
      ({
        uint64_t fd4_m = (uint64_t)((fd4_clos__lam_____51)[3]);
        ({
          fd4fun fd4_ack = (fd4fun)((fd4_clos__lam_____51)[4]);
          ({
            uint64_t fd4_m = (uint64_t)((fd4_clos__lam_____51)[5]);
            ({
              fd4fun fd4_ack = (fd4fun)((fd4_clos__lam_____51)[6]);
              ({
                uint64_t fd4_m = (uint64_t)((fd4_clos__lam_____51)[7]);
                (fd4_m
                ? (fd4_n
                  ? ({
                      clo fd4_app_____54 = ({
                        clo fd4_app_____55 = fd4_ack;
                        (fd4fun)(((fd4fun) (fd4fun)((fd4_app_____55)[0]))( (void *)fd4_app_____55
                        , (void *)({
                          fd4_sub(fd4_m, 1);
                        }) ));
                      });
                      (uint64_t)(((fd4fun) (fd4fun)((fd4_app_____54)[0]))( (void *)fd4_app_____54
                      , (void *)({
                        clo fd4_app_____56 = ({
                          clo fd4_app_____57 = fd4_ack;
                          (fd4fun)(((fd4fun) (fd4fun)((fd4_app_____57)[0]))( (void *)fd4_app_____57
                          , (void *)fd4_m ));
                        });
                        (uint64_t)(((fd4fun) (fd4fun)((fd4_app_____56)[0]))( (void *)fd4_app_____56
                        , (void *)({
                          fd4_sub(fd4_n, 1);
                        }) ));
                      }) ));
                    })
                  : ({
                      clo fd4_app_____52 = ({
                        clo fd4_app_____53 = fd4_ack;
                        (fd4fun)(((fd4fun) (fd4fun)((fd4_app_____53)[0]))( (void *)fd4_app_____53
                        , (void *)({
                          fd4_sub(fd4_m, 1);
                        }) ));
                      });
                      (uint64_t)(((fd4fun) (fd4fun)((fd4_app_____52)[0]))( (void *)fd4_app_____52
                      , (void *)1 ));
                    }))
                : fd4_n + 1);
              });
            });
          });
        });
      });
    });
  });});
}

fd4fun fd4_fix____ack__m_____48 (clo fd4_fix__clos_____49, uint64_t fd4_m) {
  return ({({
    fd4fun fd4_ack = fd4_fix__clos_____49;
    fd4_mkclosure( fd4_fun____n_____50
    , 7
    , fd4_m
    , fd4_ack
    , fd4_m
    , fd4_ack
    , fd4_m
    , fd4_ack
    , fd4_m );
  });});
}

fd4fun fd4_ack;
uint64_t fd4_x;
uint64_t* fd4main() {
  fd4_ack = (fd4_mkclosure(fd4_fix____ack__m_____48, 0));
  fd4_x = (({
    uint64_t fd4_print_____49_50 = ({
      clo fd4_app_____49_48 = ({
        clo fd4_app_____49_49 = fd4_ack;
        (fd4fun)(((fd4fun) (fd4fun)((fd4_app_____49_49)[0]))( (void *)fd4_app_____49_49
        , (void *)3 ));
      });
      (uint64_t)(((fd4fun) (fd4fun)((fd4_app_____49_48)[0]))( (void *)fd4_app_____49_48
      , (void *)11 ));
    });
    ({
      wprintf(L"Papu, anda ");
      fd4_printn(({fd4_print_____49_50;}));
    });
  }));
  return 0;
}